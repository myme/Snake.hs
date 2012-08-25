module Main where

import Snake

import System.Random

main :: IO ()
main = do
    gen <- newStdGen
    let (g, _) = placeApple initialGrid gen
    print g
    return ()
