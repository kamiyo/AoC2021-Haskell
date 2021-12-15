module Day14
  ( day14_1,
    day14_2,
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.Function
import Data.List (elemIndices, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as S

polymerize :: String -> Map.Map String String -> String -> String
polymerize [x] _ result = result ++ [x]
polymerize template rules result =
  let firstTwo = take 2 template
      toInsert = rules Map.! firstTwo
      newResult = result ++ [head template] ++ toInsert
   in polymerize (tail template) rules newResult

score :: String -> Int
score chain =
  let set = S.fromList chain
      lengths = sort $ map (length . (`elemIndices` chain)) (S.toList set)
   in last lengths - head lengths

day14_1 :: String -> IO ()
day14_1 input = do
  let inputLines = lines input
      template = head inputLines
      rules =
        ( foldl' (\acc val -> Map.insert (head val) (last val) acc) Map.empty
            . map (splitOn " -> ")
            . drop 2
        )
          inputLines
      iter x = take (succ x) (iterate (\temp -> polymerize temp rules "") template)
  print $ score $ last $ iter 10

chainToMap :: String -> Map.Map String Int -> Map.Map String Int
chainToMap [c] result = result
chainToMap chain result =
  let firstTwo = take 2 chain
      newResult = Map.insertWith (+) firstTwo 1 result
   in chainToMap (tail chain) newResult

polymerize' :: Map.Map String Int -> Map.Map String [String] -> Map.Map String Int
polymerize' chainMap ruleMap =
  Map.foldlWithKey'
    ( \acc key val ->
        let toInsert = ruleMap Map.! key
            newMap = foldl' (\acc' insert -> Map.insertWith (+) insert val acc') acc toInsert
         in newMap
    )
    Map.empty
    chainMap

calculateFrequencies :: Map.Map String Int -> [Int]
calculateFrequencies chainMap =
  let ks = (S.toList . S.fromList . concat . Map.keys) chainMap
      freqs =
        ( Map.map ((`div` 2) . (+ 1))
        . Map.foldlWithKey'
          ( \acc key val ->
              foldl' (\acc' letter -> Map.insertWith (+) letter val acc') acc key
          )
          Map.empty)
          chainMap
      freqList = (map snd . Map.toList) freqs
   in freqList

day14_2 :: String -> IO ()
day14_2 input = do
  let inputLines = lines input
      template = head inputLines
      rules =
        ( foldl'
            ( \acc val ->
                let toInsert = [(head . head) val : last val, last val ++ [(last . head) val]]
                 in Map.insert (head val) toInsert acc
            )
            Map.empty
            . map (splitOn " -> ")
            . drop 2
        )
          inputLines
      templateMap = chainToMap template Map.empty
      -- iter x = take (succ x) (iterate (\temp -> polymerize temp rules "") template)
      result = take 41 (iterate (`polymerize'` rules) templateMap)
  print $ last result
  print $ (\fl -> maximum fl - minimum fl) $ calculateFrequencies (last result)
