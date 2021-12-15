module Main where
import System.IO
import System.Environment

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14

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
             | day == 7 && part == 1 = day07_1
             | day == 7 && part == 2 = day07_2
             | day == 8 && part == 1 = day08_1
             | day == 8 && part == 2 = day08_2
             | day == 9 && part == 1 = day09_1
             | day == 9 && part == 2 = day09_2
             | day == 10 && part == 1 = day10_1
             | day == 10 && part == 2 = day10_2
             | day == 11 && part == 1 = day11_1
             | day == 11 && part == 2 = day11_2
             | day == 12 && part == 1 = day12_1
             | day == 12 && part == 2 = day12_2
             | day == 13 && part == 1 = day13_1
             | day == 13 && part == 2 = day13_2
             | day == 14 && part == 1 = day14_1
             | day == 14 && part == 2 = day14_2
             | otherwise = (\x -> return ())
    runSolution input

