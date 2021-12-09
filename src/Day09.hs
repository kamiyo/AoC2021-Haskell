module Day09
    ( day09_1
    , day09_2
    ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T
import qualified Data.Matrix as M
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow)
import Data.List (elemIndex)

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
    return ()

day09_2 :: String -> IO ()
day09_2 input =
    return ()
