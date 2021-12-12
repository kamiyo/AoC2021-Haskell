{-# LANGUAGE TupleSections #-}
module Day11
    ( day11_1
    , day11_2
    ) where

import Utils (readChars)
import qualified Data.Matrix as M
import Debug.Trace (traceShow)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.State

data FlashState a = No a | NotYet a | Flashed a deriving (Show)

createSurrounding :: Int -> Int -> [(Int, Int)]
createSurrounding row col =
    [(y, x) | y <- [(pred row)..(succ row)], x <- [(pred col)..(succ col)], y /= row && x /= col]

incrementFlashChain :: (Int, Int) -> State ((), M.Matrix Int)
incrementFlashChain coord@(r, c) = do
    modify (\m -> M.unsafeSet coord (M.unsafeGet r c m) m)
    ()

propogateFlashes :: M.Matrix Int -> M.Matrix (FlashState Int) -> [(Int, Int)] -> M.Matrix Int
propogateFlashes octopodes flashMap coordList =
    let tempAccumulator = M.zero (M.nrows octopodes) (M.ncols octopodes) :: M.Matrix Int
        accumulatingState = (tempAccumulator, flashMap)
        newAccumulator = foldl' (\acc (r, c) ->
                                        let curr = M.unsafeGet r c octopodes
                                            coords = createSurrounding r c
                                            surrounding = mapMaybe (\coord@(rr, cc) ->
                                                                        fmap (coord,) (M.safeGet rr cc octopodes)
                                                                    ) coords
                                        in case curr of
                                            x | x <= 9 -> acc
                                            _ -> foldl' (\acc' (coord, val) ->
                                                            M.setElem (succ val) coord acc') acc surrounding
                                        ) tempAccumulator coordList
    in octopodes

step :: M.Matrix Int -> Int -> [(Int, Int)] -> M.Matrix Int
step octopodes 0 _ = octopodes
step octopodes steps coordList =
    let updated = M.mapPos (\_ o -> o + 1) octopodes
        maxOct = foldl' max 0 updated
    in case maxOct of
        x | x <= 9 -> step updated (pred steps) coordList
        _ -> octopodes

day11_1 :: String -> IO ()
day11_1 input = do
    let
        octopodList = map readChars $ lines input
        height = length octopodList
        width = length (head octopodList)
        octopodes = concat octopodList
        octopodMatrix = M.fromList height width octopodes

        result = step octopodMatrix 1 [(y, x) | y <- [1..height], x <- [1..width]]
    print result

day11_2 :: String -> IO ()
day11_2 input = do
    return ()