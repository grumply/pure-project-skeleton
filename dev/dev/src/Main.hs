{-# language QuasiQuotes, NoMonomorphismRestriction #-}
module Main where

import Pure.Data.Time
import Pure.Data.Txt (fromTxt,toTxt)
import Pure.Data.Txt.Interpolate

import Dev

import System.Environment

main :: IO ()
main = getArgs >>= \case
  ["--ghcjs"] -> defaultMain frontend
  _           -> defaultMain (backend ++ shared ++ test)

frontend :: [Action]
frontend = group "frontend"
  [ Restartable "configure" configure
  , Restartable "build" build
  ]
  where

    -- reconfigure frontend.cabal when a configuration changes or
    -- when a module is added or removed
    configure = first
      [ "app/config.dhall"          |% action
      , "app/frontend/config.dhall" |% action
      , "app/frontend/**/*.hs"      |* action
      ]
      where
        action = configureProject "frontend" "cabal-ghcjs.project"

    -- rebuild frontend when the frontend or shared cabal files change or
    -- when a module is modified in frontend or shared
    build = first
      [ "app/shared/shared.cabal"     |% action
      , "app/frontend/frontend.cabal" |% action
      , "app/shared/**/*.hs"          |% action
      , "app/frontend/**/*.hs"        |% action
      ]
      where
        action = buildProject "frontend" "cabal-ghcjs.project" distributeJS

backend :: [Action]
backend = group "backend"
  [ Restartable "configure" configure
  , Restartable "build" build
  ]
  where

    -- reconfigure backend.cabal when a configuration changes or
    -- when a module is added or removed
    configure = first
      [ "app/config.dhall"         |% run
      , "app/backend/config.dhall" |% run
      , "app/backend/**/*.hs"      |* run
      ]
      where
        run = configureProject "backend" "cabal.project"

    -- rebuild backend when the backend or shared cabal files change or
    -- when a module is modified in backend or shared
    build = first
      [ "app/shared/shared.cabal"   |% action
      , "app/backend/backend.cabal" |% action
      , "app/shared/**/*.hs"        |% action
      , "app/backend/**/*.hs"       |% action
      ]
      where
        action = buildProject "backend" "cabal.project" runBackend

shared :: [Action]
shared = group "shared"
  [ Restartable "configure" configure ]
  where
    -- reconfigure backend.cabal when a configuration changes or
    -- when a module is added or removed
    configure = first
      [ "app/config.dhall"        |% run
      , "app/shared/config.dhall" |% run
      ]
      where
        run = configureProject "shared" "cabal.project"

test :: [Action]
test = group "test"
  [ Restartable "configure" configure
  , Restartable "build" build
  ]
  where

    -- reconfigure test.cabal when a configuration changes or
    -- when a module is added or removed
    configure = first
      [ "app/config.dhall"      |% run
      , "app/test/config.dhall" |% run
      , "app/test/**/*.hs"      |* run
      ]
      where
        run = configureProject "test" "cabal.project"

    -- rebuild test when frontend, backend, shared, or test cabal files change
    -- or when a module is modified in frontend, backend, shared, or test
    build = first
      [ "app/**/*.cabal" |% action
      , "app/**/*.hs"    |% action
      ]
      where
        action = buildProject "test" "cabal.project" runTest

configureProject :: Name => String -> String -> IO ()
configureProject prj pf = withDuration $ \dur -> do
  status (Running [i|running|])
  pr <- proc
    [i|dhall-to-yaml <<< ./app/#{prj}/config.dhall > ./app/#{prj}/.package.yaml && \
      hpack --force ./app/#{prj}/.package.yaml
    |]
  t <- dur
  withProcessResult (pure pr)
    (\out err -> message (unlines [err,out]) >> status (Bad  [i|configuration failure (#{t})|]))
    (\out -> clear                           >> status (Good [i|configured (#{t})|]) )

buildProject :: Name => String -> String -> IO () -> IO ()
buildProject prj pf onSuccess = withDuration $ \dur -> do
  status (Running [i|running|])
  pr <- proc [i|cabal new-build #{prj} --enable-optimization=1 --builddir=./.dist-newstyle/#{prj} --project-file=#{pf}|]
  t <- dur
  withProcessResult (pure pr)
    (\out err -> message (unlines [err,out]) >> status (Bad  [i|build failure (#{t})|]))
    (\out -> clear >> status (Good [i|built (#{t})|]) >> onSuccess)

distributeJS :: Name => IO ()
distributeJS = withDuration $ \dur -> do
  pr <- proc [i|(rm ./#{path}/index.html || true) && cp ./#{path}/* ./dist/|]
  t <- dur
  withProcessResult (pure pr)
    (\out err -> message (unlines [err,out]) >> status (Bad [i|distribute failure (#{t})|]))
    (\out -> clear >> status (Good [i|distributed|]))
  where
    path :: String
    path = [i|.dist-newstyle/frontend/build/*/ghcjs-*/frontend-*/x/frontend/build/frontend/frontend.jsexe|]

runBackend :: Name => IO ()
runBackend = proc_ [i|./#{path}|]
  where
    path :: String
    path = [i|.dist-newstyle/backend/build/*/ghc-*/backend-*/x/backend/build/backend/backend|]

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

cleanupInplace :: String -> String -> IO ()
cleanupInplace prj lib = proc_
  [i|rm ./.dist-newstyle/#{prj}/build/*/*/#{lib}-*/package.conf.inplace || true|]

project :: File => String
project = splitPath file !! 1

cabal :: File => Bool
cabal = match (compile "**/*.cabal") file

dhall :: File => Bool
dhall =  match (compile "**/*.dhall") file
      || match (compile    "*.dhall") file

haskell :: File => Bool
haskell = match (compile "**/*.hs") file
