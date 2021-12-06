{-# LANGUAGE TupleSections #-}
module Day06 
    ( day06_1
    , day06_2
    ) where
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List
import Debug.Trace

doIter :: [Int] -> Int -> [Int]
doIter fishCounts 0 = fishCounts
doIter fishCounts generations =
    let
        numToSpawn = head fishCounts
        before = take 6 (tail fishCounts)
        after = last fishCounts
        new6 = (fishCounts !! 7) + numToSpawn
    in doIter (before ++ [new6, after, numToSpawn]) (pred generations)

runIter :: String -> Int -> Int 
runIter input gens = 
    let fishes = sort $ map read $ splitOn "," input :: [Int]
        fishCounts = foldr (\ val acc ->
                            let before = take val acc
                                after = drop (succ val) acc
                                item = succ (acc !! val)
                            in before ++ [item] ++ after) (replicate 9 0 :: [Int]) fishes
    in sum $ doIter fishCounts gens

day06_1 :: String -> IO ()
day06_1 input = do
    print $ runIter input 80
    return ()

day06_2 :: String -> IO ()
day06_2 input = do
    print $ runIter input 256
    return ()