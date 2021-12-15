module Day13
    ( day13_1
    , day13_2
    ) where
import Data.List.Split (splitOn)
import Data.List (elemIndex, elemIndices, transpose)
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Matrix as M
import Debug.Trace (traceShow)

data Fold a = Horizontal a | Vertical a deriving(Show)

foldAlong :: Fold Int -> S.Set (Int, Int) -> S.Set (Int, Int)
foldAlong (Horizontal line) dots =
    let
        sets = S.partition (\(x, y) -> y < line) dots
        flippedSecond = S.map (\(x, y) -> (x, line - (y - line))) $ snd sets
    in S.union flippedSecond $ fst sets
foldAlong (Vertical line) dots =
    let
        sets = S.partition (\(x, y) -> x < line) dots
        flippedSecond = S.map (\(x, y) -> (line - (x - line), y)) $ snd sets
    in S.union flippedSecond $ fst sets

dotSetToMatrix :: S.Set (Int, Int) -> Int -> Int -> M.Matrix Int
dotSetToMatrix dots rows cols =
    let tempMatrix = M.matrix rows cols (const 0) :: M.Matrix Int
        result = S.foldl' (\acc (x, y) -> M.unsafeSet 1 (succ y, succ x) acc) tempMatrix dots
    in result

findMaxes :: S.Set (Int, Int) -> (Int, Int)
findMaxes dots =
    let coordList = unzip $ S.toList dots
        maxList = traceShow coordList $ map maximum [fst coordList, snd coordList]
    in (succ $ head maxList, succ $ last maxList)

day13_1 :: String -> IO ()
day13_1 input = do
    let
        dots = map (\l ->
                        let coords = splitOn "," l
                        in (head coords, last coords)
                    ) $ lines input
        separateIndex = head $ elemIndices ("","") dots
        parts = splitAt separateIndex dots
        coords = map (\c -> (read $ fst c :: Int, read $ snd c :: Int)) $ fst parts
        coordList = unzip coords
        maxes = map maximum [fst coordList, snd coordList]
        folds = map (
                        (\f -> case head f of
                                "y" -> Horizontal (read $ last f :: Int)
                                "x" -> Vertical (read $ last f :: Int)
                                _ -> error "Shouldnt' be here"
                        )
                        . splitOn "="
                        . (!! 2)
                        . splitOn " "
                        . fst
                    ) $ tail $ snd parts
        dotSet = S.fromList coords
        result = foldAlong (head folds) dotSet
        resultMaxes = findMaxes result
    print $ S.size result
    -- print $ dotSetToMatrix result (snd resultMaxes) (fst resultMaxes)

day13_2 :: String -> IO ()
day13_2 input = do
    let
        dots = map (\l ->
                        let coords = splitOn "," l
                        in (head coords, last coords)
                    ) $ lines input
        separateIndex = head $ elemIndices ("","") dots
        parts = splitAt separateIndex dots
        coords = map (\c -> (read $ fst c :: Int, read $ snd c :: Int)) $ fst parts
        coordList = unzip coords
        maxes = map maximum [fst coordList, snd coordList]
        folds = map (
                        (\f -> case head f of
                                "y" -> Horizontal (read $ last f :: Int)
                                "x" -> Vertical (read $ last f :: Int)
                                _ -> error "Shouldnt' be here"
                        )
                        . splitOn "="
                        . (!! 2)
                        . splitOn " "
                        . fst
                    ) $ tail $ snd parts
        dotSet = S.fromList coords
        result = foldl' (\acc f -> foldAlong f acc) dotSet folds
        resultMaxes = findMaxes result
    print $ S.size result
    print $ dotSetToMatrix result (snd resultMaxes) (fst resultMaxes)
