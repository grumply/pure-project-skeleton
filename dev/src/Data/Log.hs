module Data.Log (Event(..),Status(..),log) where

import Data.Project
import Control.Clear

import Data.Map as Map

import Prelude hiding (log)
import Control.Concurrent
import Data.Foldable
import Data.List (sortOn)
import System.IO
import System.IO.Unsafe

{-# NOINLINE logStatus #-}
logStatus :: MVar (Map Project Event)
logStatus = unsafePerformIO $ newMVar mempty

data Event = Event Project Status

eventProject :: Event -> Project
eventProject (Event prj _) = prj

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
log ev =
  modifyMVar_ logStatus $ \ls -> do
    let 
      ls' = Map.insert (eventProject ev) ev ls
      ms  = sortOn (project . fst) (Map.toList ls')
    clear
    hFlush stdout
    putStrLn $ unlines 
      [ errors prj m | (_,Event prj (Bad _ (Just m))) <- ms ]
    hFlush stdout
    traverse_ (write . snd) ms
    pure ls'
  where
    errors :: Project -> String -> String
    errors prj = unlines . fmap (("<" <> project prj <> "> ") <>) . lines

    write :: Event -> IO ()
    write (Event prj status) = putStrLn (projectStatus prj status)

