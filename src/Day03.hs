module Day03
    ( day03_1
    , day03_2
    ) where

import Data.Char (digitToInt)
import Data.List (transpose)

reverseBinaryListToDecimal :: [Int] -> Int -> Int
reverseBinaryListToDecimal [] _ = 0
reverseBinaryListToDecimal (x:xs) power =
    x * (2 ^ power) + reverseBinaryListToDecimal xs (succ power)

binaryListToDecimal :: [Int] -> Int
binaryListToDecimal binList = reverseBinaryListToDecimal (reverse binList) 0

day03_1 :: String -> IO ()
day03_1 input = do
    let 
        codes = map (\vs -> map digitToInt vs :: [Int]) $ lines input
        listLength = length codes
        transposed = transpose codes
        gamma = map (fromEnum . (> listLength `div` 2) . sum) transposed
        epsilon = map (negate . pred) gamma
    print gamma
    print epsilon
    print $ binaryListToDecimal gamma * binaryListToDecimal epsilon

doIter :: [[Int]] -> Int -> Int -> [Int]
doIter [code] _ _ = code
doIter codes position selection =
    let transposed = transpose codes
        positionSum = sum (transposed !! position)
        codesLength = length codes
        toKeep = if fromIntegral positionSum >= (fromIntegral codesLength / 2.0)
                    then selection
                    else 1 - selection
        filteredList = filter (\code -> (code !! position) == toKeep) codes
    in doIter filteredList (succ position) selection

startIter :: [[Int]] -> Int -> [Int]
startIter codes = doIter codes 0

day03_2 :: String -> IO ()
day03_2 input = do
    let
        codes = map (\vs -> map digitToInt vs :: [Int]) $ lines input
        o2Gen = binaryListToDecimal $ startIter codes 1
        co2Scrub = binaryListToDecimal $ startIter codes 0
    print $ o2Gen * co2Scrub