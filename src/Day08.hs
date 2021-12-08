module Day08
    ( day08_1
    , day08_2
    ) where
import Data.List.Split
import Data.List (findIndices, find, elemIndices)
import Data.Set (Set)
import qualified Data.Set as Set

day08_1 :: String -> IO ()
day08_1 input = do
    let
        entries = map (map words . splitOn "|") $ lines input
        digits = map last entries
        contains = map (findIndices (\e -> case length e of
                                           l | l `elem` [2, 3, 4, 7] -> True
                                           _ -> False )
                        ) digits
        totalCount = length $ concat contains
    mapM_ print contains
    print totalCount

{-|
    l2 -> 1
    l3 -> 7
    l4 -> 4
    l7 -> 8
    l5 contains l2 -> 3
    difference of l3 and l2 -> top line
    difference of l4 and l2 -> left and mid (lm)
    intersection of l5s -> horizontals
    intersection of lm and horizontals = mid
    horizontals - top - mid = bottom
    l4 - mid - l2 -> ul
    l5 contains ul -> 5
    the other l5 -> 2
    l6 doesn't have mid -> 0
    l7 - horizontals - l4 -> ll
    l6 doesn't have ll -> 9
    last l6 -> 6


-}

deduceNumbers :: [Set Char] -> [Set Char]
deduceNumbers numSet =
    let
        one = head $ filter (\s -> Set.size s == 2) numSet
        seven = head $ filter (\s -> Set.size s == 3) numSet
        four = head $ filter (\s -> Set.size s == 4) numSet
        eight = head $ filter (\s -> Set.size s == 7) numSet
        three = head $ filter (\s -> Set.size s == 5 && one `Set.isSubsetOf` s) numSet
        topLine = Set.difference seven one
        upperLeftAndMid = Set.difference four one
        horizontals = foldr1 Set.intersection (filter (\s -> Set.size s == 5) numSet)
        midLine = Set.intersection upperLeftAndMid horizontals
        bottomLine = Set.difference horizontals (Set.union topLine midLine)
        upperLeft = Set.difference upperLeftAndMid midLine
        five = head $ filter (\s -> Set.size s == 5 && upperLeft `Set.isSubsetOf` s) numSet
        two = head $ filter (\s -> Set.size s == 5 && s /= five && s/= three) numSet
        zero = head $ filter (\s -> Set.size s == 6 && Set.disjoint s midLine) numSet
        lowerLeft = Set.difference eight (Set.union horizontals four)
        nine = head $ filter (\s -> Set.size s == 6 && Set.disjoint s lowerLeft) numSet
        six = head $ filter (\s -> Set.size s == 6 && s /= nine && s /= zero) numSet
    in [zero, one, two, three, four, five, six, seven, eight, nine]

numListToDecimal :: (Integral t, Num p) => t -> [p] -> p
numListToDecimal _ [] = 0
numListToDecimal power (x:xs) =
    x * 10 ^ power + numListToDecimal (succ power) xs

day08_2 :: String -> IO ()
day08_2 input = do
    let
        entries = map (map (map Set.fromList . words) . splitOn "|") $ lines input
        observs = map head entries
        solutions = map deduceNumbers observs
        digits = map last entries
        answers = map (numListToDecimal 0 . reverse . concat) $ zipWith (\ds sol -> map (`elemIndices` sol) ds) digits solutions
    print $ sum answers