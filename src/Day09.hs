module Day09
    ( day09_1
    , day09_2
    ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T
import qualified Data.Matrix as M
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Maybe (catMaybes, isJust, fromJust)
import Debug.Trace (traceShow)
import Data.List (elemIndex, sortBy)
import qualified Data.Bifunctor as B

readChars :: String -> [Int]
readChars inputs = map (read . T.unpack) $ T.chunksOf 1 (T.pack inputs)

day09_1 :: String -> IO ()
day09_1 input = do
    let
        heightList = map readChars $ lines input
        height = length heightList
        width = length (head heightList)
        heightFlatten = concat heightList
        heightMap = M.fromList height width heightFlatten
        mins = M.mapPos (\(r, c) a ->
                            -- let rowStart = max 1 (pred r)
                            --     rowEnd = min height (succ r)
                            --     colStart = max 1 (pred c)
                            --     colEnd = min width (succ c)
                            --     sub = M.submatrix rowStart rowEnd colStart colEnd heightMap
                            let up = M.safeGet (pred r) c heightMap
                                down = M.safeGet (succ r) c heightMap
                                left = M.safeGet r (pred c) heightMap
                                right = M.safeGet r (succ c) heightMap
                                surrounding = catMaybes [up, down, left, right]
                                sub = catMaybes [Just a, up, down, left, right]
                            in case minimum sub of
                                x | x == a -> case elemIndex a surrounding of
                                                Just _ -> Nothing
                                                Nothing -> Just a
                                _ -> Nothing) heightMap
        filteredMins = catMaybes $ M.toList mins
        riskLevel =  sum $ map succ filteredMins
    print riskLevel

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Enum)

stepFlow :: M.Matrix Int -> (Int, Int) -> S.Set (Int, Int) -> ((Int, Int), S.Set (Int, Int))
stepFlow heightMap (row, col) temp =
    let
        curr = fromJust (M.safeGet row col heightMap)
        up = M.safeGet (pred row) col heightMap
        down = M.safeGet (succ row) col heightMap
        left = M.safeGet row (pred col) heightMap
        right = M.safeGet row (succ col) heightMap
        surrounding = map
                        (B.first fromJust)
                        (filter (isJust . fst)
                                (zip
                                    [up, down, left, right]
                                    [(pred row, col), (succ row, col), (row, pred col), (row, succ col)]))
        min = minimum surrounding
    in case min of
        (height, coord) | height <= curr -> stepFlow heightMap coord (S.insert coord temp)
        (height, coord) -> ((row, col), S.insert (row, col) temp)

stepFlow' :: M.Matrix Int -> (Int, Int) -> S.Set (Int, Int) -> Map.Map (Int, Int) (S.Set (Int, Int)) -> ((Int, Int), S.Set (Int, Int))
stepFlow' heightMap (row, col) temp accumulator =
    let
        curr = fromJust (M.safeGet row col heightMap)
        up = M.safeGet (pred row) col heightMap
        down = M.safeGet (succ row) col heightMap
        left = M.safeGet row (pred col) heightMap
        right = M.safeGet row (succ col) heightMap
        surrounding = map
                        (B.first fromJust)
                        (filter (isJust . fst)
                                (zip
                                    [up, down, left, right]
                                    [(pred row, col), (succ row, col), (row, pred col), (row, succ col)]))
        min = minimum surrounding
    in case min of
        (height, coord) | height <= curr -> 
            let found = Map.toAscList $ Map.filter (S.member coord) accumulator
            in case found of 
                [] -> stepFlow' heightMap coord (S.insert coord temp) accumulator
                [(final, _)] -> (final, S.insert (row, col) temp)
                _ -> error "shouldn't be here"
        (height, coord) -> ((row, col), S.insert (row, col) temp)

day09_2 :: String -> IO ()
day09_2 input = do
    let
        heightList = map readChars $ lines input
        height = length heightList
        width = length (head heightList)
        heightFlatten = concat heightList
        heightMap = M.fromList height width heightFlatten
        coordList = [(r, c) | r <- [1..height], c <- [1..width]]
        -- flows' adds paths to an accumulating map, and each flow iteration
        -- terminates if it reaches a previously visited path
        flows' = foldr (\(r, c) accumulator ->
                            let v = M.unsafeGet r c heightMap
                                found = Map.size $ Map.filter (S.member (r, c)) accumulator
                                (final, coordsPassed) = stepFlow' heightMap (r, c) (S.singleton (r, c)) accumulator
                            in case v of
                                    x | x == 9 -> accumulator
                                    _ -> case found of
                                            0 -> Map.insertWith S.union final coordsPassed accumulator
                                            _ -> accumulator
                        ) Map.empty coordList
        -- flows maps each coord into a flow path, then you would use Map.fromListWith S.union
        -- to merge the flows.
        flows = filter (\(k, v) -> S.size v /= 0) $ M.toList $ M.mapPos
                        (\(r, c) a ->
                            case a of
                                x | x == 9 -> ((r, c), S.empty)
                                _ -> stepFlow heightMap (r, c) (S.singleton (r, c))
                        ) heightMap
        -- consolidatedFlows = Map.toAscList $ Map.fromListWith S.union flows
        consolidatedFlows = Map.toAscList flows'
        orderedFlows = sortBy (\(lk, lv) (rk, rv) -> S.size rv `compare` S.size lv) consolidatedFlows
        top3Flows = take 3 orderedFlows
        solution = foldr (\(_, v) acc -> S.size v * acc) 1 top3Flows
    print solution
