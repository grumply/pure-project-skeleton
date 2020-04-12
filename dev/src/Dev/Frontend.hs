module Dev.Frontend (frontend) where

import Control.Build
import Control.Configure
import Control.Distribute
import Data.Compiler
import Data.Duration
import Data.Log
import Data.Process
import Data.Project

import Pure.Elm as Elm hiding (Start,Raw,distribute)

import System.FSNotify

import Control.Concurrent
import Data.Foldable
import System.FilePath

import Prelude hiding (log)

data Msg = Start | Build | Stop

data Model = Model
  { watchers :: IO ()
  , building :: Maybe ThreadId
  }

frontend :: WatchManager -> View
frontend = Elm.run (App [Start] [] [Stop] mdl update view)
  where
    mdl = Model (pure ()) Nothing

    update Start mgr _mdl = do
      let 
        actionable (takeExtension -> ext) = 
          ext == ".cabal" || ext == ".hs"

        dispatch ev@(takeFileName . eventPath -> p)
          | "frontend.cabal" <- p  = command Build
          | "shared.cabal"   <- p  = config Frontend GHCJS
          | Modified {}      <- ev = command Build
          | otherwise              = pure ()

        watch prj =
          watchTree mgr (project prj)
            (actionable . takeFileName . eventPath)
            dispatch

      watch Frontend
      watch Shared

      update Build mgr mdl

    update Stop _ mdl = do
      watchers mdl
      pure mdl { watchers = pure () }

    update Build _ mdl = do
      for_ (building mdl) killThread
      let status = Running "building"
      log ( Event Frontend status )
      tid <- forkIO $
        withDuration $ \dur ->
          withProcessResult
            (build Frontend GHCJS)
            failure
            (success dur)
      pure mdl { building = Just tid }
      where

        failure = log . Event Frontend . Bad "build failure" . Just

        success dur _ = do
          t <- dur
          let status = "build success (" <> t <> ")"
          log ( Event Frontend (Good status) )
          distribute
          pure ()

    view _ _ = Null

