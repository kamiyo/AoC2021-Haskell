{-# LANGUAGE TupleSections #-}
module Day11
    ( day11_1
    , day11_2
    ) where

import Utils (readChars)
import qualified Data.Matrix as M
import Debug.Trace (traceShow)
import Data.Foldable (foldl', find)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.State
import qualified Control.Arrow as Data.Bifunctor
import qualified Control.Monad

data FlashState a = No a | NotYet a | Flashed a deriving (Show)
anyState :: FlashState a -> a
anyState (No a) = a
anyState (NotYet a) = a
anyState (Flashed a) = a

createSurrounding :: Int -> Int -> [(Int, Int)]
createSurrounding row col =
    [(y, x) | y <- [(pred row)..(succ row)], x <- [(pred col)..(succ col)], (y, x) /= (row, col)]

incrementFlashChain :: (Int, Int) -> State (M.Matrix Int) ()
incrementFlashChain coord@(r, c) = do
    modify (\m -> M.unsafeSet (M.unsafeGet r c m) coord m)
    return ()

createFlashMap :: M.Matrix Int -> M.Matrix (FlashState Int)
createFlashMap = M.mapPos (\_ a -> case a of
                                      n | n <= 9 -> No a
                                      _ -> NotYet a)


propogateFlashes :: M.Matrix (FlashState Int) -> State ([(Int, Int)], Int) (M.Matrix (FlashState Int))
propogateFlashes flashMap = do
    state <- get
    let tempAccumulator = (M.zero (M.nrows flashMap) (M.ncols flashMap) :: M.Matrix Int)
        newAccumulator = foldl' (\acc (r, c) ->
                                    let curr = M.unsafeGet r c flashMap
                                        coords = createSurrounding r c
                                        surrounding = mapMaybe (\coord@(rr, cc) ->
                                                                    fmap (const  coord) (M.safeGet rr cc flashMap)
                                                                ) coords
                                    in case curr of
                                        x | anyState x <= 9 -> acc
                                        NotYet _            -> foldl' (\acc' coord@(rr, cc) ->
                                                                    M.setElem (succ (M.unsafeGet rr cc acc')) coord acc') acc surrounding
                                        _                   -> acc
                                    ) tempAccumulator (fst state)
        newFlashMap = M.mapPos (\(r, c) a ->
                                        let curr = M.unsafeGet r c flashMap
                                            newValue = a + anyState curr
                                        in case curr of
                                            No _ | newValue > 9 -> NotYet newValue
                                            No _ -> No newValue
                                            NotYet _ -> Flashed newValue
                                            Flashed _ -> Flashed newValue
                                    ) newAccumulator
        needFlashing = find (\f ->
                                    let ff = f
                                    in case ff of
                                        NotYet _ -> True
                                        _ -> False) newFlashMap
    case needFlashing of
        Nothing -> return newFlashMap
        Just _ -> propogateFlashes newFlashMap


step :: M.Matrix Int -> Int -> State ([(Int, Int)], Int) (M.Matrix Int)
step octopodes 0 = return octopodes
step octopodes steps = do
    let updated = M.mapPos (\_ o -> o + 1) octopodes
        maxOct = foldl' max 0 updated
        flashMap = M.mapPos (\_ o -> No o) updated
    case maxOct of
        x | x <= 9 -> step updated (pred steps)
        _ -> do
            afterFlashMap <- propogateFlashes flashMap
            let countFlash = foldl' (\acc o -> case o of
                                            Flashed _ -> succ acc
                                            _ -> acc) 0 afterFlashMap
                afterFlash = M.mapPos (\_ o -> case anyState o of
                                        x | x > 9 -> 0
                                        x -> x) afterFlashMap
            modify (\(coords, flashes) -> (coords, flashes + countFlash))
            step afterFlash (pred steps)

stepUp :: M.Matrix Int -> Int -> State ([(Int, Int)], Int) (M.Matrix Int)
stepUp octopodes steps = do
    let updated = M.mapPos (\_ o -> o + 1) octopodes
        maxOct = foldl' max 0 updated
        flashMap = M.mapPos (\_ o -> No o) updated
    case maxOct of
        x | x <= 9 -> stepUp updated (succ steps)
        _ -> do
            afterFlashMap <- propogateFlashes flashMap
            let
                allFlashed = foldl' (\acc o -> case o of
                                                Flashed _ -> acc && True
                                                _ -> acc && False) True afterFlashMap
                afterFlash = M.mapPos (\_ o -> case anyState o of
                                        x | x > 9 -> 0
                                        x -> x) afterFlashMap
            state <- get
            Control.Monad.when allFlashed $ put (fst state, steps)
            if allFlashed
                then return afterFlash
                else stepUp afterFlash (succ steps)

day11_1 :: String -> IO ()
day11_1 input = do
    let
        octopodList = map readChars $ lines input
        height = length octopodList
        width = length (head octopodList)
        octopodes = concat octopodList
        octopodMatrix = M.fromList height width octopodes
        (result, (_, flashes)) = runState (step octopodMatrix 100) ([(y, x) | y <- [1..height], x <- [1..width]], 0)
    print flashes
    print result

day11_2 :: String -> IO ()
day11_2 input = do
    let
        octopodList = map readChars $ lines input
        height = length octopodList
        width = length (head octopodList)
        octopodes = concat octopodList
        octopodMatrix = M.fromList height width octopodes
        (result, (_, steps)) = runState (stepUp octopodMatrix 1) ([(y, x) | y <- [1..height], x <- [1..width]], 0)
    print steps
    print result