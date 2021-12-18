module Day15
  ( day15_1,
    day15_2,
  ) where

import Data.Array (Array, Ix)
import qualified Data.Array as A
import Data.Set (Set)
import qualified Data.Set as S
import Utils
import Data.List (transpose, findIndex, elemIndex, sortBy)
import Control.Monad.State (State, MonadState (get, put), runState, execState)
import Control.Monad (when, foldM, foldM_, liftM)
import GHC.Base (compareWord)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (Foldable(foldl'))
import Debug.Trace (traceShow)
import qualified Data.Bifunctor

getNeighbors :: Matrix Int -> Coordinate -> [Coordinate]
getNeighbors grid (cr, cc) =
  let up = (pred cr, cc)
      right = (cr, succ cc)
      down = (succ cr, cc)
      left = (cr, pred cc)
      (minBound, maxBound) = A.bounds grid
      filtered = filter (\(r, c) -> not
                                    (  r < fst minBound
                                    || r > fst maxBound
                                    || c < snd minBound
                                    || c > snd maxBound
                                    )) [up, right, down, left]
  in filtered

buildPaths :: Matrix Int -> (Int, Int) -> Int -> [(Int, Int)] -> State (Int, (Int, Int), [(Int, Int)]) ()
buildPaths caveMatrix coord riskTotal currentPath = do
  (currentMin, (nrows, ncols), _) <- get
  let newRisk = riskTotal + caveMatrix A.! coord
      end = (nrows, ncols)
      newPathChain = currentPath ++ [coord]

  case newRisk of
    x | x >= currentMin -> return ()
    _ ->
      case coord of
        c | c == end -> -- At the end
          put (newRisk, (nrows, ncols), newPathChain)
        _ -> do
          let dirs = filter (isNothing . (`elemIndex` currentPath)) $ getNeighbors caveMatrix coord
              sortedDirs = sortBy (\l r -> (caveMatrix A.! l) `compare` (caveMatrix A.! r)) dirs
          mapM_ (\dir -> buildPaths caveMatrix dir newRisk newPathChain) dirs
          -- loop through possible paths [up, right, down, left] filter by possibility,
          -- but have to store coords passed through, so we can check for no intersections
          -- run buildPaths through all

type Coordinate = (Int, Int)
type SearchState = (Map Coordinate Coordinate, Map Coordinate Int, Heap (Heap.Entry Int Coordinate))
type Matrix e = Array Coordinate e

manhattanDist :: Coordinate -> Coordinate -> Int
manhattanDist a b = abs (fst a - fst b) + abs (snd a - snd b)

aStarSearch :: Matrix Int -> Coordinate -> Coordinate -> State SearchState ()
aStarSearch caveMatrix start end = do
  (parents, costs, openHeap) <- get
  case Heap.uncons openHeap of
    Nothing -> return $ traceShow "here" ()
    Just (Heap.Entry currentPrio currentCoord, rest) -> do
      case currentCoord of
        a | a == end -> return ()
        _ -> do
          put (parents, costs, rest)
          let neighbors = getNeighbors caveMatrix currentCoord
          mapM_ (\nextNeighbor -> do
            (parents, costs, rest) <- get
            let newCost = costs Map.! currentCoord + caveMatrix A.! nextNeighbor
            case Map.lookup nextNeighbor costs of
              next | isNothing next || isJust next && newCost < fromJust next -> do
                let priority = newCost + manhattanDist nextNeighbor end
                put (Map.insert nextNeighbor currentCoord parents, Map.insert nextNeighbor newCost costs, Heap.insert (Heap.Entry priority nextNeighbor) rest)
              _ -> return ()) neighbors
          aStarSearch caveMatrix start end

startAStarSearch :: Matrix Int -> SearchState
startAStarSearch caveMatrix =
  let
    (start, end) = A.bounds caveMatrix
    openHeap = Heap.singleton (Heap.Entry 0 start)
    parents = Map.singleton start start
    costs = Map.singleton start 0
  in execState (aStarSearch caveMatrix start end) (parents, costs, openHeap)

day15_1 :: String -> IO ()
day15_1 input = do
  let cave = map readChars $ lines input
      height = length cave
      width = length $ head cave
      caveMatrix = A.listArray ((0, 0), (99, 99)) (concat cave) :: Matrix Int
      (parents, costs, heap) = startAStarSearch caveMatrix
  print $ costs Map.! (99, 99)

getNeighbors' :: Matrix Int -> Coordinate -> [Coordinate]
getNeighbors' grid (cr, cc) =
  let up = (pred cr, cc)
      right = (cr, succ cc)
      down = (succ cr, cc)
      left = (cr, pred cc)
      filtered = filter (\(r, c) -> not
                                    (  r < 0
                                    || r > 499
                                    || c < 0
                                    || c > 499
                                    )) [up, right, down, left]
  in filtered

(!) :: Matrix Int -> Coordinate -> Int
(!) grid (r, c) =
  let
    size = ((\(_, (mr, mc)) -> (succ mr, succ mc)) . A.bounds) grid
    modded = (\(r', c') -> Data.Bifunctor.bimap (mod r') (mod c') size) (r, c)
    block = (\(r', c') -> Data.Bifunctor.bimap (div r') (div c') size) (r, c)
    currentVal = grid A.! modded
    newVal = currentVal + fst block + snd block
  in succ $ mod (pred newVal) 9

aStarSearch' :: Matrix Int -> Coordinate -> Coordinate -> State SearchState ()
aStarSearch' caveMatrix start end = do
  (parents, costs, openHeap) <- get
  case Heap.uncons openHeap of
    Nothing -> return $ traceShow "here" ()
    Just (Heap.Entry currentPrio currentCoord, rest) -> do
      case currentCoord of
        a | a == end -> return ()
        _ -> do
          put (parents, costs, rest)
          let neighbors = getNeighbors' caveMatrix currentCoord
          mapM_ (\nextNeighbor -> do
            (parents, costs, rest) <- get
            let newCost = costs Map.! currentCoord + caveMatrix ! nextNeighbor
            case Map.lookup nextNeighbor costs of
              next | isNothing next || isJust next && newCost < fromJust next -> do
                let priority = newCost + manhattanDist nextNeighbor end
                put (Map.insert nextNeighbor currentCoord parents, Map.insert nextNeighbor newCost costs, Heap.insert (Heap.Entry priority nextNeighbor) rest)
              _ -> return ()) neighbors
          aStarSearch' caveMatrix start end

startAStarSearch' :: Matrix Int -> SearchState
startAStarSearch' caveMatrix =
  let
    (start, end) = A.bounds caveMatrix
    openHeap = Heap.singleton (Heap.Entry 0 start)
    parents = Map.singleton start start
    costs = Map.singleton start 0
  in execState (aStarSearch' caveMatrix start (499, 499)) (parents, costs, openHeap)

day15_2 :: String -> IO ()
day15_2 input = do
  let
    cave = map readChars $ lines input
    height = length cave
    width = length $ head cave
    caveMatrix = A.listArray ((0, 0), (99, 99)) (concat cave) :: Matrix Int
    (parents, costs, heap) = startAStarSearch' caveMatrix
  print $ costs Map.! (499, 499)
  return ()