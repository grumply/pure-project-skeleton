module Data.Log (Event(..),Status(..),log) where

import Data.Project
import Control.Clear

import Data.Map as Map

import Prelude hiding (log)
import Data.Foldable
import Data.IORef
import Data.List (sortOn)
import System.IO
import System.IO.Unsafe

{-# NOINLINE logStatus #-}
logStatus :: IORef (Map Project Event)
logStatus = unsafePerformIO $ newIORef mempty

addLogMessage :: Event -> IO [(Project,Event)]
addLogMessage ev = 
  atomicModifyIORef' logStatus $ \ls ->
    let ls' = Map.insert (eventProject ev) ev ls
    in (ls',sortOn (eventStatus . snd) (Map.toList ls'))

data Event = Event Project Status 

eventProject :: Event -> Project
eventProject (Event prj _) = prj

eventStatus :: Event -> Status
eventStatus (Event _ st) = st

data Status = Bad String (Maybe String) | Running String | Good String
  deriving (Eq,Ord)

statusMessage :: Status -> String
statusMessage (Bad     m _) = m
statusMessage (Running m  ) = m
statusMessage (Good    m  ) = m

statusEmoji :: Status -> String
statusEmoji Bad     {} = "\x1F534"
statusEmoji Running {} = "\x1F7E1"
statusEmoji Good    {} = "\x1F7E2"

projectStatus :: Project -> Status -> String
projectStatus prj status = statusEmoji status <> " <" <> project prj <> "> " <> statusMessage status

log :: Event -> IO ()
log ev = do
  ms <- addLogMessage ev
  clear
  putStrLn $ unlines 
    [ errors prj m | (_,Event prj (Bad _ (Just m))) <- ms ]
  traverse_ (write . snd) ms
  where
    errors :: Project -> String -> String
    errors prj = unlines . fmap (("<" <> project prj <> "> ") <>) . lines

    write :: Event -> IO ()
    write (Event prj status) = putStrLn (projectStatus prj status)

