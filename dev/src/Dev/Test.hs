module Dev.Test (test) where

import Control.Build
import Control.Configure
import qualified Control.Run
import Data.Compiler
import Data.Duration
import Data.Log
import Data.Process
import Data.Project

import Pure.Elm as Elm hiding (Start,Raw)

import System.FSNotify

import Control.Concurrent
import Data.Traversable
import Data.Foldable
import Prelude hiding (log)
import System.FilePath

data Msg = Start | Build | Run String | Died ProcessResult | Stop

data Model = Model
  { watchers :: IO ()
  , building :: Maybe ThreadId
  , running  :: Maybe (ThreadId,Process)
  }

test :: WatchManager -> View
test = Elm.run (App [Start] [] [Stop] mdl update view)
  where
    mdl = Model (pure ()) Nothing Nothing

    update Start mgr mdl = do
      let 
        actionable (takeExtension -> ext) = 
          ext == ".cabal" || ext == ".hs"

        dispatch ev@(takeExtension . eventPath -> e)
          | ".cabal"    <- e  = command Build
          | Modified {} <- ev = command Build
          | otherwise         = pure ()

        watch prj = 
          watchTree mgr (project prj) 
            (actionable . takeFileName . eventPath) 
            dispatch

      watchers <- for (enumFrom Frontend) watch

      let stop = foldl (>>) (pure ()) watchers

      update Build mgr mdl { watchers = stop }

    update Stop _ mdl = do
      watchers mdl
      pure mdl { watchers = pure () }

    update Build mgr mdl = do
      for_ (building mdl) killThread
      let status = Running "building"
      log ( Event Test status )
      tid <- forkIO $ 
        withDuration $ \dur ->
          withProcessResult
            (build Test GHC)
            failure 
            (success dur)
      pure mdl { building = Just tid }
      where

        failure = log . Event Test . Bad "build failure" . Just

        success dur _ = do
          t <- dur
          let status = "build success (" <> t <> ")"
          command (Run status)

    update (Died pr) _ mdl = do
      withProcessResult (pure pr) failure success
      pure mdl { running = Nothing }
      where
        failure = log . Event Test . Bad "test died" . Just

        success _ = do
          let status = "test completed successfully"
          log ( Event Test (Good status) )

    update (Run status) _ mdl = do
      log ( Event Test (Running "stopping") )
      for_ (running mdl) $ \(tid,p) -> do
        killThread tid
        kill p 
      p <- Control.Run.test
      log ( Event Test (Good status) )
      tid <- forkIO (await p >>= command . Died)
      pure mdl { running = Just (tid,p) }
         
    view _ _ = Null
