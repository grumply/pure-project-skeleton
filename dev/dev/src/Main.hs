{-# language QuasiQuotes, NoMonomorphismRestriction, ImplicitParams #-}
module Main where

import Dev

import System.Environment

main :: IO ()
main = getArgs >>= \case
  ["--ghcjs"] -> defaultMain "app" frontend
  _           -> defaultMain "app" (backend ++ shared ++ test)

frontend :: [Action]
frontend = withProject "frontend" simpleProjectGHCJS

backend :: [Action]
backend = withProject "backend" simpleProjectGHC

-- needed to update the shared.cabal to trigger 
-- other projects to reconfigure/rebuild
shared :: [Action]
shared = withProject "shared" simpleConfigureOnlyGHC

test :: [Action]
test = withProject "test" $
  let
    builder after = first
      [ "app/**/*.cabal" |% (build "cabal.project" after)
      , "app/**/*.hs"    |% (build "cabal.project" after)
      ]
        
  in
    defaultProject configureGHC builder runTest

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
  status (Running [i|running|])
  pr <- proc
    [i|dhall-to-yaml <<< ./app/#{project}/config.dhall > ./app/#{project}/.package.yaml && \
      hpack --force ./app/#{project}/.package.yaml
      cabal new-configure #{project} --disable-documentation --enable-optimization=1 --builddir=./.dist-newstyle/#{project} --project-file=#{pf}
    |]
  t <- dur
  withProcessResult (pure pr)
    (\out err -> message (unlines [err,out]) >> status (Bad  [i|configuration failure (#{t})|]))
    (\out -> clear                           >> status (Good [i|configured (#{t})|]) )

build :: (Project,Name) => String -> IO () -> IO ()
build pf onSuccess = withDuration $ \dur -> do
  status (Running [i|running|])
  pr <- proc [i|cabal new-build #{project} --builddir=./.dist-newstyle/#{project} --project-file=#{pf}|]
  t <- dur
  withProcessResult (pure pr)
    (\out err -> message (unlines [err,out]) >> status (Bad  [i|build failure (#{t})|]))
    (\out -> clear >> status (Good [i|built (#{t})|]) >> onSuccess)

distribute :: (Project,Name) => IO ()
distribute = withDuration $ \dur -> do
  pr <- proc [i|(rm ./#{path}/index.html || true) && cp ./#{path}/* ./dist/|]
  t <- dur
  withProcessResult (pure pr)
    (\out err -> message (unlines [err,out]) >> status (Bad [i|distribute failure (#{t})|]))
    (\out -> clear >> status (Good [i|distributed|]))
  where
    path :: String
    path = [i|.dist-newstyle/#{project}/build/*/ghcjs-*/#{project}-*/x/#{project}/build/#{project}/#{project}.jsexe|]

run :: (Project,Name) => IO ()
run = procPipe_ [i|./#{path}|]
  where
    path :: String
    path = [i|.dist-newstyle/#{project}/build/*/ghc-*/#{project}-*/x/#{project}/build/#{project}/#{project}|]

runTest :: Name => IO ()
runTest = withDuration $ \dur -> do
  pr <- proc [i|./#{path}|]
  t <- dur
  case pr of
    Success _ -> status $ Good $ "tests successful (" <> t <> ")"
    Failure out err -> do
      message (unlines [out,err])
      status $ Bad $ "test failure (" <> t <> ")"
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

