module Data.Compiler where

data Compiler = GHC | GHCJS

projectFile :: Compiler -> String
projectFile GHCJS = "cabal-ghcjs.project"
projectFile GHC   = "cabal.project"
