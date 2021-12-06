module Main where
import System.IO
import System.Environment

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

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
             | day == 2 && part == 1 = day02_1
             | day == 2 && part == 2 = day02_2
             | day == 3 && part == 1 = day03_1
             | day == 3 && part == 2 = day03_2
             | day == 4 && part == 1 = day04_1
             | day == 4 && part == 2 = day04_2
             | day == 5 && part == 1 = day05_1
             | day == 5 && part == 2 = day05_2
             | day == 6 && part == 1 = day06_1
             | day == 6 && part == 2 = day06_2
             | otherwise = (\x -> return ())
    runSolution input

