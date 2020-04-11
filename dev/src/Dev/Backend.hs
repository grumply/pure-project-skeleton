module Dev.Backend (backend) where

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
import Data.Foldable
import Prelude hiding (log)
import System.FilePath

import Prelude hiding (log)

data Msg = Start | Build | Run String | Died ProcessResult | Stop

data Model = Model
  { watchers :: IO ()
  , building :: Maybe ThreadId
  , running  :: Maybe (ThreadId,Process)
  }

backend :: WatchManager -> View
backend = Elm.run (App [Start] [] [Stop] mdl update view)
  where
    mdl = Model (pure ()) Nothing Nothing

    update Start mgr mdl = do
      let 
        actionable (takeExtension -> ext) = 
          ext == ".cabal" || ext == ".hs"

        dispatch ev@(takeFileName . eventPath -> p)
          | "backend.cabal" <- p  = command Build
          | "shared.cabal"  <- p  = pure ()
          | Modified {}     <- ev = command Build
          | otherwise             = pure ()

        watch prj = 
          watchTree mgr (project prj) 
            (actionable . takeFileName . eventPath) 
            dispatch

      watch Backend
      watch Shared

      update Build mgr mdl

    update Stop _ mdl = do
      watchers mdl
      pure mdl { watchers = pure () }

    update Build mgr mdl = do
      for_ (building mdl) killThread
      let status = Running "building"
      log ( Event Backend status )
      tid <- forkIO $ 
        withDuration $ \dur ->
          withProcessResult
            (build Backend GHC)
            failure 
            (success dur)
      pure mdl { building = Just tid }
      where

        failure = log . Event Backend . Bad "build failure" . Just

        success dur _ = do
          t <- dur
          let status = "build success (" <> t <> ")"
          command (Run status)

    update (Died pr) _ mdl = do
      withProcessResult (pure pr) failure success
      pure mdl { running = Nothing }
      where
        failure = log . Event Backend . Bad "backend died" . Just

        success _ = do
          let status = "backend stopped successfully"
          log ( Event Backend (Good status) )

    update (Run status) _ mdl = do
      log ( Event Backend (Running "stopping") )
      for_ (running mdl) $ \(tid,p) -> do
        killThread tid
        kill p 
      p <- Control.Run.backend
      log ( Event Backend (Good status) )
      tid <- forkIO (await p >>= command . Died)
      pure mdl { running = Just (tid,p) }
         
    view _ _ = Null
