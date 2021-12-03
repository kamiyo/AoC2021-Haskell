{-# LANGUAGE DuplicateRecordFields #-}

module Day02
    ( day02_1
    , day02_2
    ) where
import Data.Foldable (Foldable(foldl'))

data Position = Position { horizontal :: Int
                         , depth :: Int } deriving (Show)
type Command = (String, Int)

data PositionAim = PositionAim { horizontal :: Int
                               , depth :: Int
                               , aim :: Int } deriving (Show)

moveSub :: Position -> Command -> Position
moveSub acc (dir, amount)
    | dir == "forward" = acc { horizontal = horizontal (acc :: Position) + amount }
    | dir == "down" = acc { depth = depth (acc :: Position) + amount }
    | dir == "up" = acc { depth = depth (acc :: Position) - amount }
    | otherwise = acc

aimAndMoveSub :: PositionAim -> Command -> PositionAim
aimAndMoveSub acc (dir, amount)
    | dir == "down" = acc { aim = aim acc + amount }
    | dir == "up" = acc { aim = aim acc - amount }
    | dir == "forward" = acc { horizontal = horizontal (acc :: PositionAim) + amount
                             , depth = depth (acc :: PositionAim) + aim acc * amount }
    | otherwise = acc

day02_1 :: String -> IO ()
day02_1 input = do
    let
        directions = map ((\ [d,q] -> (d, read q :: Int)) . words) $ lines input
        finalPosition = foldl' moveSub (Position 0 0) directions
    print finalPosition
    print $ horizontal (finalPosition :: Position) * depth (finalPosition :: Position)

day02_2 :: String -> IO ()
day02_2 input = do
    let
        directions = map ((\ [d,q] -> (d, read q :: Int)) . words) $ lines input
        finalPosition = foldl' aimAndMoveSub (PositionAim 0 0 0) directions
    print finalPosition
    print $ horizontal (finalPosition :: PositionAim) * depth (finalPosition :: PositionAim)
