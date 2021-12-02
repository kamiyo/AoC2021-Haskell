module Main where
import System.IO
import System.Environment

import Day01

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ "./inputs/day" ++ head args ++ ".txt"
    let
        day = read $ head args :: Int
        part = read $ (head . tail) args :: Int
        runSolution
             | day == 1 && part == 1 = day01_1
             | day == 1 && part == 2 = day01_2
             | otherwise = (\x -> return ())
    runSolution input
        
