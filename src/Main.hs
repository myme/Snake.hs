module Main where

import Snake

import System.Random

main :: IO ()
main = do
    gen <- newStdGen
    startGUI gen "levels/lvl0.txt"
