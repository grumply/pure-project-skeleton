module Data.Duration where

import Pure.Data.Time

import Text.Printf

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

