module Day13
    ( day13_1
    , day13_2
    ) where
import Data.List.Split (splitOn)
import Data.List (elemIndex, elemIndices)
import qualified Data.Set as S

data Fold a = Horizontal a | Vertical a deriving(Show)

day13_1 :: String -> IO ()
day13_1 input = do
    let
        dots = map (\l ->
                        let coords = splitOn "," l
                        in (head coords, last coords)
                    ) $ lines input
        separateIndex = head $ elemIndices ("","") dots
        parts = splitAt separateIndex dots
        coords = fst parts
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
    mapM_ print folds

day13_2 :: String -> IO ()
day13_2 input = do
    return ()