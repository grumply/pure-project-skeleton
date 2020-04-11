{-# language QuasiQuotes #-}
module Control.Run (backend,test) where

import Data.Process

import Pure.Data.Txt
import Pure.Data.Txt.Interpolate

backend :: IO Process
backend = spawn 
  [i|./dist-newstyle/backend/build/*/ghc-8.6.5/backend-*/x/backend/build/backend/backend|]

test :: IO Process
test = spawn 
  [i|./dist-newstyle/test/build/*/ghc-8.6.5/test-*/x/test/build/test/test|]
