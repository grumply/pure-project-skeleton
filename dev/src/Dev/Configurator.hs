{-# language BlockArguments #-}
module Dev.Configurator (configurator) where

import Control.Configure
import Data.Compiler
import Data.Duration
import Data.Log
import Data.Process
import Data.Project

import Pure.Elm as Elm hiding (Start,Raw,distribute)

import System.FSNotify

import Data.Traversable
import Prelude hiding (log)
import System.FilePath

data Msg = Start | Configure Project | Stop

configurator :: WatchManager -> View
configurator = Elm.run (App [Start] [] [Stop] Nothing update view)
  where
    update Start mgr _mdl = do
      let 
        actionable f
          | ".hs"    <- takeExtension f = True
          | ".dhall" <- takeExtension f = True
          | otherwise                   = False

        dispatch prj ev
          | ".dhall" <- takeExtension (eventPath ev) 
          = print ("dhall changed for " ++ project prj) >> command (Configure prj) 
            -- update configuration changes in frontend

          | ".hs" <- takeExtension (eventPath ev)
          , Modified {} <- ev 
          = pure ()
            -- ignore module change

          | otherwise
          = print ("hs file added or removed for " ++ project prj) >> command (Configure prj)
            -- .hs addition or deletion

        watcher prj = 
          watchTree mgr (project prj) 
            (actionable . takeFileName . eventPath) 
            (dispatch prj)

      rootConfigWatcher <- watchDir mgr "."
        ((==) ".dhall" . takeExtension . takeFileName. eventPath)
        (const (command (Configure Shared)))

      watchers <- for (enumFrom Frontend) watcher

      let stop = foldl (>>) rootConfigWatcher watchers

      pure (Just stop)

    update Stop _ (Just stop) = do
      stop 
      pure Nothing

    update (Configure Backend) _ mdl = do
      config Backend GHC
      config Test GHC
      pure mdl

    update (Configure Frontend) _ mdl = do
      config Frontend GHC
      config Test GHC
      pure mdl

    update (Configure Shared) _ mdl = do
      config Shared GHC
      config Backend GHC
      config Test GHC
      pure mdl

    update (Configure Test) _ mdl = do
      config Test GHC
      pure mdl

    update _ _ mdl = pure mdl

    view _ _ = Null

