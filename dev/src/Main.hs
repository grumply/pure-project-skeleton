module Main where

import Data.Compiler
import Data.Opts
import Dev.Backend
import Dev.Configurator
import Dev.Frontend
import Dev.Test

import Pure.Elm 

import System.FSNotify

import Control.Monad

main :: IO ()
main = do
  os <- getOpts
  withManager $ \mgr -> do
    inject body $ Div <||>
      case compiler os of
        GHC -> 
          [ configurator mgr
          , backend mgr
          , test mgr
          ]
        GHCJS ->
          [ frontend mgr
          ]
    forever (delay (Minutes 10 0))


