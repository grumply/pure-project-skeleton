{-# language QuasiQuotes #-}
module Control.Build (build) where

import Data.Compiler
import Data.Process
import Data.Project

import Pure.Data.Txt
import Pure.Data.Txt.Interpolate

build :: Project -> Compiler -> IO ProcessResult
build (project -> p) (projectFile -> pf) = proc 
  [i|cabal new-build --builddir=./dist-newstyle/#{p} --project-file=#{pf} #{p}|]

