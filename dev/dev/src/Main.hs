{-# language QuasiQuotes, NoMonomorphismRestriction, ImplicitParams #-}
module Main where

import Dev

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= \case
  ["--ghcjs"] -> defaultMain "app" frontend
  _           -> defaultMain "app" (backend ++ shared ++ tests)

frontend :: [Action]
frontend = withProject "frontend" simpleProjectGHCJS

backend :: [Action]
backend = withProject "backend" simpleProjectGHC

-- needed to update the shared.cabal to trigger 
-- other projects to reconfigure/rebuild
shared :: [Action]
shared = withProject "shared" simpleConfigureOnlyGHC

tests :: [Action]
tests = withProject "test" $
  let
    builder after = first
      [ "app/**/*.cabal" |% (build "cabal.project" after)
      , "app/**/*.hs"    |% (build "cabal.project" after)
      ]
        
  in
    defaultProject configureGHC builder test

--------------------------------------------------------------------------------
-- Level-4; default compiler-specific project configurations

simpleProjectGHC :: Project => [Action]
simpleProjectGHC = defaultProject configureGHC buildGHC run

simpleProjectGHCJS :: Project => [Action]
simpleProjectGHCJS = defaultProject configureGHCJS buildGHCJS distribute

simpleConfigureOnlyGHC :: Project => [Action]
simpleConfigureOnlyGHC = defaultProject configureGHC (\_ -> emptyMatcher) (pure ())

simpleConfigureOnlyGHCJS :: Project => [Action]
simpleConfigureOnlyGHCJS = defaultProject configureGHCJS (\_ -> emptyMatcher) (pure ())

--------------------------------------------------------------------------------
-- Level-3; compiler-specific project configurations

configureGHC :: (Project,Name) => Matcher
configureGHC = configMatcher (configure "cabal.project")

buildGHC :: (Project,Name) => IO () -> Matcher
buildGHC = buildMatcher . build "cabal.project"

configureGHCJS :: (Project,Name) => Matcher
configureGHCJS = configMatcher (configure "cabal-ghcjs.project")

buildGHCJS :: (Project,Name) => IO () -> Matcher
buildGHCJS = buildMatcher . build "cabal-ghcjs.project"

--------------------------------------------------------------------------------
-- Level-2; project primitives

buildMatcher :: Project => (File => IO ()) -> Matcher
buildMatcher build = first
  [ "app/shared/shared.cabal"            |% build
  , [i|app/#{project}/#{project}.cabal|] |% build
  , [i|app/shared/**/*.hs|]              |% build
  , [i|app/#{project}/**/*.hs|]          |% build
  ]

configMatcher :: Project => (File => IO ()) -> Matcher
configMatcher config = first
  [ "app/config.dhall"               |% config
  , [i|app/#{project}/config.dhall|] |% config
  , [i|app/#{project}/**/*.hs|]      |* config
  ]

defaultProject :: Project => Matcher -> (IO () -> Matcher) -> (Name => IO ()) -> [Action]
defaultProject configure build afterBuild = 
  group project
    [ Restartable "configure" configure
    , Restartable "build"     (build afterBuild)
    ]

--------------------------------------------------------------------------------
-- Level-1; shell commands with status updates

configure :: (Project,Name) => String -> IO ()
configure pf = withDuration $ \dur -> do
  status (Running [i|running configure|])
  (ec,out,err) <- proc
    [i|dhall-to-yaml <<< ./app/#{project}/config.dhall > ./app/#{project}/.package.yaml && \
      hpack --force ./app/#{project}/.package.yaml && \
      cabal new-configure #{project} --disable-documentation --enable-optimization=1 --builddir=./.dist-newstyle/#{project} --project-file=#{pf}
    |]
  t <- ec `seq` dur
  case ec of
    ExitFailure (-15) -> clear
    ExitFailure _     -> message (Prelude.unlines [out,err]) >> status (Bad [i|configuration failed (#{t})|])
    ExitSuccess       -> status (Good [i|configuration finished (#{t})|])

build :: (Project,Name) => String -> IO () -> IO ()
build pf onSuccess = withDuration $ \dur -> do
  clear
  status (Running [i|running build|])
  (ec,out,err) <- proc [i|cabal new-build #{project} --disable-documentation --enable-optimization=1 --builddir=./.dist-newstyle/#{project} --project-file=#{pf}|]
  t <- ec `seq` dur
  case ec of
    ExitFailure (-15) -> clear
    ExitFailure _     -> message (Prelude.unlines [out,err]) >> status (Bad [i|build failed (#{t})|])
    ExitSuccess       -> status (Good [i|build finished (#{t})|]) >> onSuccess

distribute :: (Project,Name) => IO ()
distribute = withDuration $ \dur -> do
  status (Running [i|running distribute|])
  (ec,out,err) <- proc [i|(rm ./#{path}/index.html || true) && cp ./#{path}/* ./dist/|]
  t <- ec `seq` dur
  case ec of
    ExitFailure (-15) -> clear
    ExitFailure _     -> message (Prelude.unlines [out,err]) >> status (Bad [i|distribute failed (#{t})|])
    ExitSuccess       -> status (Good [i|distribute finished (#{t})|])
  where
    path :: String
    path = [i|.dist-newstyle/#{project}/build/*/ghcjs-*/#{project}-*/x/#{project}/build/#{project}/#{project}.jsexe|]

run :: (Project,Name) => IO ()
run = withDuration $ \dur -> do
  clear
  status (Good [i|running #{project}|])
  ec <- procPipe [i|./#{path}|]
  t <- ec `seq` dur
  case ec of
    ExitFailure (-15) -> clear
    ExitFailure _     -> status (Bad [i|#{project} died (#{t})|])
    ExitSuccess       -> status (Good [i|#{project} finished (#{t})|])
  where
    path :: String
    path = [i|.dist-newstyle/#{project}/build/*/ghc-*/#{project}-*/x/#{project}/build/#{project}/#{project}|]

test :: Name => IO ()
test = withDuration $ \dur -> do
  clear
  status (Running [i|running test|])
  ec <- procPipe [i|./#{path}|]
  t <- ec `seq` dur
  case ec of
    ExitFailure (-15) -> clear
    ExitFailure _     -> status (Bad [i|tests failed (#{t})|])
    ExitSuccess       -> clear >> status (Good [i|tests finished (#{t})|])
  where
    path :: String
    path = [i|.dist-newstyle/test/build/*/ghc-8.6.5/test-*/x/test/build/test/test|]

--------------------------------------------------------------------------------
-- Level-0; configuration

type Project = (?project :: String)

withProject :: String -> (Project => a) -> a
withProject prj a = let ?project = prj in a

project :: Project => String
project = ?project

