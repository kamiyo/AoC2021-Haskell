module Day04
    ( day04_1
    , day04_2
    ) where

import Data.List.Split (splitOn)
import Data.List (findIndices, elemIndices, findIndex, transpose, elemIndex)
import Data.Maybe (mapMaybe)
import Debug.Trace

type Draws = [Int]
type Board = [[Int]]
type PointsBoard = [[Int]]

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

findInBoard :: Board -> Int -> Maybe (Int, Int)
findInBoard board num =
    let
        cols = map (elemIndices num) board
        row = findIndex (not . null) cols
    in row >>= (\r -> return (r, head (cols !! r)))

assocInBoard :: PointsBoard -> (Int, Int) -> Int -> PointsBoard
assocInBoard pb (row, col) newValue =
    let
        beforeRows = take row pb
        afterRows = drop (succ row) pb
        rowToChange = pb !! row
        beforeCols = take col rowToChange
        afterCols = drop (succ col) rowToChange
        newRow = beforeCols ++ [newValue] ++ afterCols
    in
        beforeRows ++ [newRow] ++ afterRows

createPointsBoards :: Int -> [PointsBoard]
createPointsBoards numBoards = replicate numBoards (replicate 5 (replicate 5 0))

checkBoard :: PointsBoard -> Bool
checkBoard pb =
    let
        sumRows = map sum pb
        sumCols = map sum (transpose pb)
        hasRow = elemIndex 5 sumRows
        hasCol = elemIndex 5 sumCols
    in case (hasRow, hasCol) of
        (Just _, _) -> True
        (_, Just _) -> True
        _ -> False

doIter :: [Int] -> [Board] -> [PointsBoard] -> (Int, Int, [PointsBoard])
doIter draws boards pointsBoards =
    let
        draw = head draws
        coords = map (`findInBoard` draw) boards
        newPoints = zipWith (\c b -> case c of
                                        Just coord -> assocInBoard b coord 1
                                        Nothing -> b) coords pointsBoards
        winner = elemIndex True $ map checkBoard newPoints
    in case winner of
        Just n -> (n, draw, newPoints)
        Nothing -> doIter (tail draws) boards newPoints

removeIdx :: Int -> [a] -> [a]
removeIdx idx boards =
    let
        beforeBoards = take idx boards
        afterBoards = drop (succ idx) boards
    in beforeBoards ++ afterBoards

removeIndices :: [Int] -> [a] -> [a]
removeIndices [] boards = boards
removeIndices (idx:idxs) boards =
    let
        beforeBoards = take idx boards
        afterBoards = drop (succ idx) boards
    in beforeBoards ++ removeIndices (map (\i -> i - 1) idxs) afterBoards

doLastIter :: [Int] -> [Board] -> [PointsBoard] -> (Int, Board, PointsBoard)
doLastIter draws boards pointsBoards =
    let
        draw = head draws
        coords = map (`findInBoard` draw) boards
        newPoints = zipWith (\c b -> case c of
                                        Just coord -> assocInBoard b coord 1
                                        Nothing -> b) coords pointsBoards
        winners = (elemIndices True $ map checkBoard newPoints)
        newBoards = removeIndices winners boards
    in case winners of
        [] -> doLastIter (tail draws) boards newPoints
        _ -> case newBoards of
            [] -> (draw, head boards, head newPoints)
            _ -> doLastIter (tail draws) newBoards (removeIndices winners newPoints)

day04_1 :: String -> IO ()
day04_1 input = do
    let
        byLine = lines input
        (draws, boards) = parseInput byLine
        pointsBoards = createPointsBoards (length boards)
        (winner, draw, finalPoints) = doIter draws boards pointsBoards
        winningBoard = concat (boards !! winner)
        mask = map (1 -) (concat (finalPoints !! winner))
        points = sum (zipWith (*) winningBoard mask)
    print winner
    print draw
    print points
    print $ points * draw

day04_2 :: String -> IO ()
day04_2 input = do
    let
        byLine = lines input
        (draws, boards) = parseInput byLine
        pointsBoards = createPointsBoards (length boards)
        (draw, winner, finalPoints) = doLastIter draws boards pointsBoards
        winningBoard = concat winner
        mask = map (1 -) (concat finalPoints)
        points = sum (zipWith (*) winningBoard mask)
    print winner
    print draw
    print points
    print $ points * draw