module Day12
    ( day12_1
    , day12_2
    )where
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Map ((!))
import Data.Char (isLower, isUpper)
import Data.List (elemIndex, elemIndices)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow)

data Graph = Node String [Graph]

traverseGraph :: String -> M.Map String (S.Set String) -> [String] -> Maybe [[String]]
traverseGraph "end" _ resultSet = Just [resultSet ++ ["end"]]
traverseGraph node nodeMap resultSet =
    let
        paths = nodeMap M.! node

        pathsWithoutRepeats =
          S.filter
            ( \path ->
                all isUpper path
                  || ( case elemIndex path resultSet of
                         Nothing -> True
                         Just _ -> False
                     )
            )
            paths
        newResultSet = resultSet ++ [node]
        nextPaths = S.map (\path -> traverseGraph path nodeMap newResultSet) pathsWithoutRepeats
    in case S.size pathsWithoutRepeats of
        0 -> Nothing
        _ -> (Just . concat . catMaybes) (S.toList nextPaths)

checkIfHasTwo :: [String] -> Bool
checkIfHasTwo paths =
    let pathSet = S.fromList paths
        greaterThanTwo = S.map (\cave ->
                                    case all isLower cave of
                                        True -> ((>= 2) . length . (`elemIndices` paths)) cave
                                        False -> False) pathSet
        foundAny = foldl' (||) False greaterThanTwo
    in foundAny


traverseGraph' :: String -> M.Map String (S.Set String) -> [String] -> Maybe [[String]]
traverseGraph' "end" _ resultSet = Just [resultSet ++ ["end"]]
traverseGraph' node nodeMap resultSet =
    let
        paths = nodeMap M.! node
        newResultSet = resultSet ++ [node]
        pathsWithoutRepeats =
          S.filter
            ( \path ->
                let
                    currentIsBig = all isUpper path
                    alreadyHasTwo = checkIfHasTwo newResultSet
                    alreadyInList = case elemIndex path resultSet of
                         Nothing -> False
                         Just _ -> True
                in path /= "start" && (currentIsBig || not alreadyInList || not alreadyHasTwo)
            )
            paths
        nextPaths = S.map (\path -> traverseGraph' path nodeMap newResultSet) pathsWithoutRepeats
    in case S.size pathsWithoutRepeats of
        0 -> Nothing
        _ -> (Just . concat . catMaybes) (S.toList nextPaths)

day12_1 :: String -> IO ()
day12_1 input = do
    let
        paths = map (splitOn "-") $ lines input
        nodeSet = S.fromList $ concat paths
        graph =
          foldl'
            ( \graph path ->
                let left = head path
                    right = last path
                    toInsert = [(left, right), (right, left)] :: [(String, String)]
                 in foldl'
                      ( \acc (k, v) ->
                          M.insert k (S.insert v (M.findWithDefault S.empty k acc)) acc
                      )
                      graph
                      toInsert
            )
            (M.empty :: M.Map String (S.Set String))
            paths
        allPaths = case traverseGraph "start" graph [] of
                    Nothing -> error "Oh no"
                    Just x -> x
    print $ length allPaths

day12_2 :: String -> IO ()
day12_2 input = do
    let
        paths = map (splitOn "-") $ lines input
        nodeSet = S.fromList $ concat paths
        graph =
          foldl'
            ( \graph path ->
                let left = head path
                    right = last path
                    toInsert = [(left, right), (right, left)] :: [(String, String)]
                 in foldl'
                      ( \acc (k, v) ->
                          M.insert k (S.insert v (M.findWithDefault S.empty k acc)) acc
                      )
                      graph
                      toInsert
            )
            (M.empty :: M.Map String (S.Set String))
            paths
        allPaths = case traverseGraph' "start" graph [] of
                    Nothing -> error "Oh no"
                    Just x -> x
        setAllPaths = S.fromList allPaths
    print $ S.size setAllPaths