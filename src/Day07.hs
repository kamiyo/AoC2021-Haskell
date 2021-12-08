module Day07
    ( day07_1
    , day07_2
    ) where
import Data.List.Split
import Data.List (sort)
import Debug.Trace

data Dir = Up | Down

minimize :: [Int] -> Int -> Maybe Int -> Maybe Dir -> Int
minimize crabs partition prev dir =
    let
        fuel = sum (map (\c -> abs (partition - c)) crabs)
    in case prev of
        Nothing ->  let
                        right = minimize crabs (succ partition) (Just fuel) (Just Up)
                        left = minimize crabs (pred partition) (Just fuel) (Just Down)
                    in min right (min fuel left)
        Just p | fuel < p -> minimize crabs (case dir of
                                                Just Up -> succ partition
                                                Just Down -> pred partition
                                                _ -> error "error!") (Just fuel) dir
        Just p -> p

minimizeProportionalFuel :: [Int] -> Int -> Maybe Int -> Maybe Dir -> Int
minimizeProportionalFuel crabs partition prev dir =
    let
        fuel = sum (map (\c ->
                            let dist = abs (partition - c)
                            in (dist * succ dist) `div` 2)
                    crabs)
    in case prev of
        Nothing ->  let
                        right = minimizeProportionalFuel crabs (succ partition) (Just fuel) (Just Up)
                        left = minimizeProportionalFuel crabs (pred partition) (Just fuel) (Just Down)
                    in min right (min fuel left)
        Just p | fuel < p -> minimizeProportionalFuel crabs (case dir of
                                                Just Up -> succ partition
                                                Just Down -> pred partition
                                                _ -> error "error!") (Just fuel) dir
        Just p -> p

day07_1 :: String -> IO ()
day07_1 input = do
    let
        crabs = sort (map read $ splitOn "," input :: [Int])
        lowestFuel = minimize crabs (crabs !! (length crabs `div` 2)) Nothing Nothing
    print lowestFuel

day07_2 :: String -> IO ()
day07_2 input = do
    let
        crabs = sort (map read $ splitOn "," input :: [Int])
        lowestFuel = minimizeProportionalFuel crabs (crabs !! (length crabs `div` 2)) Nothing Nothing
    print lowestFuel