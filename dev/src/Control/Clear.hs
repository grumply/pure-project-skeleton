module Control.Clear (clear) where

clear :: IO ()
clear = putStr "\ESC[2J"
