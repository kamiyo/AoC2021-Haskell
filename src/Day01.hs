module Day01
    ( day01_1
    , day01_2
    ) where

import Data.Foldable (Foldable(foldl'))
import qualified Data.Vector.Unboxed as V

accumulator :: (Ord a, Num a, Enum p) => p -> a -> p
accumulator acc val
    | val > 0 = succ acc
    | otherwise = acc

day01_1 :: String -> IO ()
day01_1 input = do
    let
        depths = V.fromList (map read $ lines input :: [Int])
        result = V.zipWith (-) (V.tail depths) (V.init depths)
        count = V.foldl' accumulator 0 result
    print count

day01_2 :: String -> IO ()
day01_2 input = do
    let
        depths = V.fromList (map read $ lines input :: [Int])
        results = V.zipWith3
            (\a b c -> a + b + c)
            (V.init $ V.init depths)
            (V.init $ V.tail depths)
            (V.tail $ V.tail depths)
        result = V.zipWith (-) (V.tail results) (V.init results)
        count = V.foldl' accumulator 0 result
    print count
