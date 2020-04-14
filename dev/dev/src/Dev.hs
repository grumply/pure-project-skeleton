{-# language BlockArguments, ImplicitParams #-}
module Dev (module Dev, module System.FilePath, module System.FilePath.Glob) where

import Pure.Data.Time
import Pure.Data.Txt
import Pure.Data.Txt.Interpolate

import System.FSNotify hiding (Action)

import Data.Map.Strict as Map

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.IO
import System.IO.Unsafe
import Text.Printf

import System.FilePath.Glob

import Prelude

import Debug.Trace

type Name = (?name :: String)

name :: Name => String
name = ?name

type File = (?file :: String)

file :: File => String
file = ?file

dir :: File => String
dir = takeDirectory file

data Interrupt = NoInterrupt | Interrupt | InterruptWith ((File,Name) => IO ())

interruptible :: Interrupt -> Bool
interruptible NoInterrupt = False
interruptible _ = True

runInterrupt :: Interrupt -> ((File,Name) => IO ())
runInterrupt (InterruptWith f) = f
runInterrupt _ = pure ()

data Action = Action Interrupt String (Name => Event -> Maybe (IO ()))

group :: String -> [Action] -> [Action]
group g = fmap (\(Action b nm f) -> Action b (g <> "." <> nm) f)

pattern Restartable :: String -> (Name => Event -> Maybe (IO ())) -> Action
pattern Restartable nm f = Action Interrupt nm f

pattern Interruptible :: String -> (Name => Event -> Maybe (IO ())) -> ((File,Name) => IO ()) -> Action
pattern Interruptible nm f g = Action (InterruptWith g) nm f

pattern Uninterruptible :: String -> (Name => Event -> Maybe (IO ())) -> Action
pattern Uninterruptible nm f = Action NoInterrupt nm f

first :: [a -> Maybe b] -> (a -> Maybe b)
first fs = \a -> listToMaybe $ catMaybes $ fs <*> pure a

tracer :: (Name, File) => a -> a
tracer = traceShow (?name,?file)

infixr 0 |%
(|%) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|%) glob f = let p = compile glob in \ev ->
  case ev of
    Modified path _ _ | match p path -> Just (let ?file = eventPath ev in f)
    _ -> Nothing

infixr 0 |+
(|+) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|+) glob f = let p = compile glob in \ev ->
  case ev of
    Added path _ _ | match p path -> Just (let ?file = eventPath ev in f)
    _ -> Nothing

infixr 0 |-
(|-) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|-) glob f = let p = compile glob in \ev ->
  case ev of
    Removed path _ _ | match p path -> Just (let ?file = eventPath ev in f)
    _ -> Nothing

infixr 0 |*
(|*) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|*) glob f = let p = compile glob in \ev ->
  case ev of
    Removed path _ _ | match p (eventPath ev) -> Just (let ?file = eventPath ev in f)
    Added   path _ _ | match p (eventPath ev) -> Just (let ?file = eventPath ev in f)
    _                                         -> Nothing

infixr 0 |$
(|$) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|$) glob f = let p = compile glob in \ev ->
  case ev of
    _ | match p (eventPath ev) -> Just (let ?file = eventPath ev in f)
      | otherwise              -> Nothing


defaultMain :: FilePath -> [Action] -> IO ()
defaultMain dir as =
  withManagerConf defaultConfig { confDebounce = Debounce (realToFrac 0.075) } $ \mgr -> do
    actions <- newMVar Map.empty
    cd <- getCurrentDirectory
    watchTree mgr dir (const True) $ \(mapEventPath (makeRelative cd) -> ev) -> do
      for_ as $ \(Action interrupt nm f) -> let { ?name = nm; ?file = eventPath ev } in
        let
          start g as = do
            tid <- forkIO (g >> modifyMVar_ actions (continue g))
            pure (Map.insert nm (tid,False) as)

          stop = pure . Map.delete nm

          -- run awaiting build or clean up
          continue g as
            | Just (_,True) <- Map.lookup nm as = start g as
            | otherwise                         = stop as

        in
          for_ (f ev) $ \g -> do
            print nm
            modifyMVar_ actions $ \as ->
              case Map.lookup nm as of
                Nothing -> start g as
                Just (tid,_)
                  | interruptible interrupt -> killThread tid >> runInterrupt interrupt >> start g as
                  | otherwise -> pure (Map.insert nm (tid,True) as)
    forever (threadDelay 1000000)

mapEventPath :: (FilePath -> FilePath) -> Event -> Event
mapEventPath f ev =
  case ev of
    Added    path t b -> Added    (f path) t b
    Modified path t b -> Modified (f path) t b
    Removed  path t b -> Removed  (f path) t b
    Unknown  path t s -> Unknown  (f path) t s

{-# NOINLINE output #-}
output :: MVar (Map String String,Map String String)
output = unsafePerformIO (newMVar (Map.empty,Map.empty))

writeOutput :: Map String String -> Map String String -> IO ()
writeOutput (synopsis -> ss) ms = do
  putStrLn "\ESC[2J"
  for_ ms putStrLn
  putStrLn ss
  hFlush stdout

synopsis :: Map String String -> String
synopsis ms
  | Data.Foldable.all isGood ms = '\x1F7E2' : " all good"
  | otherwise = Prelude.unlines (Map.elems (Map.filter (not . isGood) ms))
  where
    isGood ('\x1F7E2' : _) = True
    isGood _ = False

data Status
  = Good String
  | Running String
  | Bad String

status :: Name => Status -> IO ()
status s = do
  modifyMVar_ output $ \(Map.insert ?name (s' s) -> ss,ms) ->
    writeOutput ss ms >> pure (ss,ms)
  where
    s' (Good    msg) = '\x1F7E2' : (" <" <> ?name <> "> " <> msg)
    s' (Bad     msg) = '\x1F534' : (" <" <> ?name <> "> " <> msg)
    s' (Running msg) = '\x1F7E1' : (" <" <> ?name <> "> " <> msg)

message :: Name => String -> IO ()
message m = do
  modifyMVar_ output $ \(ss,Map.insert ?name msg -> ms) ->
    writeOutput ss ms >> pure (ss,ms)
  where
    msg = Prelude.unlines $ fmap (("<" <> ?name <> "> ") <>) (Prelude.lines m)

clear :: Name => IO ()
clear =
  modifyMVar_ output $ \(ss,Map.delete ?name -> ms) ->
    writeOutput ss ms >> pure (ss,ms)

type Process = (Handle, Handle, Handle, ProcessHandle)

spawn :: String -> IO Process
spawn s = runInteractiveCommand s

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

proc_ :: String -> IO ()
proc_ = void . Dev.proc

pattern Success :: String -> ProcessResult
pattern Success s <- (ExitSuccess,s,_)

pattern Failure :: String -> String -> ProcessResult
pattern Failure out err <- (ExitFailure _,out,err)

withProcessResult :: IO ProcessResult -> (String -> String -> IO a) -> (String -> IO a) -> IO a
withProcessResult c failure success = c >>= \case
  Success out     -> success out
  Failure out err -> failure out err

withDuration :: (IO String -> IO a) -> IO a
withDuration f = do
  start <- time
  f $ do
    end <- time

    let
      Seconds ss (Milliseconds ms _) = end - start

      dur | 0 <- ms   = show ss <> " seconds"
          | otherwise = show ss <> "." <> printf "%03d" ms <> " seconds"

    dur `seq` pure dur
