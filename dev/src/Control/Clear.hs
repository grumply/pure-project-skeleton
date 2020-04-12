module Control.Clear (clear) where

clear :: IO ()
clear = putStrLn "\ESC[2J"
