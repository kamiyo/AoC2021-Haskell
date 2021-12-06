module Day05
    ( day05_1
    , day05_2
    ) where
import Data.Maybe
import Text.Read
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (sortBy)

commaToTuple :: String -> (Int, Int)
commaToTuple input =
    let nums = map read (splitOn "," input) :: [Int]
    in (head nums, last nums)

coordsAreAxial :: [(Int, Int)] -> Bool
coordsAreAxial coords =
    let
        start = head coords
        end = last coords
    in (fst start == fst end) || (snd start == snd end)

genRange :: [(Int, Int)] -> [(Int, Int)]
genRange endPoints =
    let (x0, y0) = head endPoints
        (x1, y1) = last endPoints
        xdir = x1 - x0
        ydir = y1 - y0
        xs = case xdir of
                a | a > 0 -> [x0..x1]
                a | a < 0 -> [x0,(pred x0)..x1]
                _ -> [x1]
        ys = case ydir of
                a | a > 0 -> [y0..y1]
                a | a < 0 -> [y0,(pred y0)..y1]
                _ -> [y1]
        xlist = case length xs of
                    1 -> replicate (length ys) (head xs)
                    _ -> xs
        ylist = case length ys of
                    1 -> replicate (length xs) (head ys)
                    _ -> ys
    in zip xlist ylist

collectPaths :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
collectPaths points pointMap
  = foldl
      (\ pointMap point -> Map.insertWith (+) point 1 pointMap) pointMap
      points

day05_1 :: String -> IO ()
day05_1 input = do
    let
        parsed = map words $ lines input
        endPoints = map (\p -> [commaToTuple (head p), commaToTuple (last p)]) parsed
        onlyAxial = filter coordsAreAxial endPoints
        ranges = map genRange onlyAxial
        points = collectPaths (concat ranges) Map.empty
    print $ Map.size $ Map.filter (>= 2) points

day05_2 :: String -> IO ()
day05_2 input = do
    let
        parsed = map words $ lines input
        endPoints = map (\p -> [commaToTuple (head p), commaToTuple (last p)]) parsed
        ranges = map genRange endPoints
        points = collectPaths (concat ranges) Map.empty
    print $ Map.size $ Map.filter (>= 2) points
