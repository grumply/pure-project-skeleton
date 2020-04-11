module Data.Process 
  ( Process
  , spawn
  , kill
  , ProcessResult
  , proc
  , pattern Success, pattern Failure
  , await, withProcessResult
  ) where

import Control.Monad
import System.Process hiding (proc)
import System.Exit
import System.IO (Handle,hGetContents)

type Process = (Handle, Handle, Handle, ProcessHandle)

spawn :: String -> IO Process
spawn = runInteractiveCommand

kill :: Process -> IO ()
kill p@(_,_,_,ph) = terminateProcess ph

type ProcessResult = (ExitCode, String, String)

await :: Process -> IO ProcessResult
await (_,outh,errh,ph) = do
  ec <- waitForProcess ph
  o <- hGetContents outh
  e <- hGetContents errh
  pure (ec,o,e)

proc :: String -> IO ProcessResult
proc = await <=< spawn

pattern Success :: String -> ProcessResult 
pattern Success s <- (ExitSuccess,s,_)

pattern Failure :: String -> ProcessResult 
pattern Failure f <- (ExitFailure _,_,f)

withProcessResult :: IO ProcessResult -> (String -> IO a) -> (String -> IO a) -> IO a
withProcessResult c failure success = c >>= \case
  Success s -> success s
  Failure f -> failure f


