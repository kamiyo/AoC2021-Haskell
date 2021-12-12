module Day10
    ( day10_1
    , day10_2
    ) where
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Foldable (Foldable(foldl'))
import Data.List (sort)

parseLine :: String -> [Char] -> Maybe Char
parseLine [] _ = Nothing
parseLine (x:xs) [] = parseLine xs [x]
parseLine (x:xs) currentOpens =
    case (x, last currentOpens) of
        (')', co) -> case co of
                        '(' -> parseLine xs (init currentOpens)
                        _ -> Just x
        (']', co) -> case co of
                        '[' -> parseLine xs (init currentOpens)
                        _ -> Just x
        ('}', co) -> case co of
                        '{' -> parseLine xs (init currentOpens)
                        _ -> Just x
        ('>', co) -> case co of
                        '<' -> parseLine xs (init currentOpens)
                        _ -> Just x
        _ -> parseLine xs (currentOpens ++ [x])

isIncomplete :: String -> [Char] -> Bool
isIncomplete [] _ = True
isIncomplete (x:xs) [] = isIncomplete xs [x]
isIncomplete (x:xs) currentOpens =
    case (x, last currentOpens) of
        (')', co) -> case co of
                        '(' -> isIncomplete xs (init currentOpens)
                        _ -> False
        (']', co) -> case co of
                        '[' -> isIncomplete xs (init currentOpens)
                        _ -> False
        ('}', co) -> case co of
                        '{' -> isIncomplete xs (init currentOpens)
                        _ -> False
        ('>', co) -> case co of
                        '<' -> isIncomplete xs (init currentOpens)
                        _ -> False
        _ -> isIncomplete xs (currentOpens ++ [x])

closeSet = S.fromList [')', ']', '}', '>']

doComplete :: String -> [Char] -> [Char]
doComplete [] acc = reverse $ map (\c -> case c of
                                '(' -> ')'
                                '[' -> ']'
                                '{' -> '}'
                                '<' -> '>'
                                _ -> error "not supposed to be here") acc
doComplete (x:xs) [] = doComplete xs [x]
doComplete (x:xs) currentOpens =
    case x of
        c | S.member c closeSet -> doComplete xs (init currentOpens)
        _ -> doComplete xs (currentOpens ++ [x])


day10_1 :: String -> IO ()
day10_1 input = do
    let
        syntaxLines = lines input
        illegals = mapMaybe (`parseLine` []) syntaxLines
        points = foldr (\l r ->
                            let points = case l of
                                            ')' -> 3
                                            ']' -> 57
                                            '}' -> 1197
                                            '>' -> 25137
                                            _ -> error "shouldn't be here"
                            in r + points) 0 illegals
    print points

day10_2 :: String -> IO ()
day10_2 input = do
    let
        syntaxLines = lines input
        incompletes = filter (`isIncomplete` []) syntaxLines
        needed = map (`doComplete` []) incompletes
        points = sort $ map (foldl' (\acc c ->
                        let points = case c of
                                        ')' -> 1
                                        ']' -> 2
                                        '}' -> 3
                                        '>' -> 4
                                        _ -> error "shouldn't be here"
                        in (5 * acc) + points) 0) needed
        middle = points !! (length points `div` 2)
    print middle
