{-# LANGUAGE TemplateHaskell #-}
module Main where

import Pure.Test

main :: IO (Maybe ())
main = run suite

suite :: Test sync ()
suite = tests
  [ test_reverse
  ]

test_reverse :: Test sync ()
test_reverse = expect (original == doubleReversed)
  where
    original = [1,2,3]
    doubleReversed = reverse (reverse original)
