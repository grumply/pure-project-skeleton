{-# language QuasiQuotes #-}
module Control.Configure (config) where

import Data.Compiler
import Data.Duration
import Data.Log
import Data.Process
import Data.Project

import Pure.Data.Txt
import Pure.Data.Txt.Interpolate

import Prelude hiding (log)

configure :: Project -> Compiler -> IO ProcessResult
configure (project -> p) (projectFile -> pf) = proc
  [i|dhall-to-yaml <<< ./#{p}/config.dhall > ./#{p}/.package.yaml && \
     hpack --force ./#{p}/.package.yaml && \
     cabal new-configure #{p} --enable-optimization=1 --builddir=./dist-newstyle/#{p} --project-file=#{pf}
  |]

config :: Project -> Compiler -> IO ()
config prj compiler = do
  let status = Running "configuring"
  log ( Event prj status )
  withDuration $ \dur ->
    withProcessResult
      (configure prj compiler)
      failure 
      (success dur)
  where

    failure = log . Event prj . Bad "configuration failure" . Just

    success dur _ = do
      t <- dur
      let status = "configuration success (" <> t <> ")"
      log ( Event prj (Good status) )
