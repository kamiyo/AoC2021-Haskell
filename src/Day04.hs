module Day04
    ( day04_1
    , day04_2
    ) where

import Data.List.Split (splitOn)
import Data.List (findIndices, elemIndices)

type Draws = [Int]
type Board = [[Int]]
type PointsBoard = [[[Int]]]

parseBoard :: [[Char]] -> [[[Int]]] -> [[[Int]]]
parseBoard [] boards = boards
parseBoard ("":xs) boards =
    parseBoard xs boards
parseBoard input boards =
    let
        board = map (map read . words) $ take 5 input :: [[Int]]
    in parseBoard (drop 5 input) (board : boards)

parseInput :: [String] -> (Draws, [Board])
parseInput input =
    let
        draws = map read $ splitOn "," (head input) :: [Int]
    in (draws, parseBoard (tail input) [])

findInBoard :: Board -> Int -> [(Int, Int)]
findInBoard board num =
    let
        cols = map (elemIndices num) board
        rows = findIndices (not . null) cols
    in [(r, c) | r <- rows, c <- cols !! r]

createPointsBoard :: Int -> PointsBoard
createPointsBoard numBoards = replicate numBoards (replicate 5 (replicate 5 0))

day04_1 :: String -> IO ()
day04_1 input = do
    let
        byLine = lines input
        table = parseInput byLine
    print $ findInBoard (head (snd table)) 0
    return ()

day04_2 :: String -> IO ()
day04_2 input = do
    return ()