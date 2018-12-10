import Control.Applicative
import Control.Arrow
import Data.List (nub)

import qualified Data.Map as M

type Moves = M.Map String String
type State = String

mapLine2 :: String -> [String]
mapLine2 xs = nub $ map (map (xs !!)) idxs
  where idxs = [ [0, 1, 2, 3]
               , [2, 3, 0, 1]
               , [1, 3, 0, 2]
               , [0, 2, 1, 3]
               , [3, 2, 1, 0]
               , [1, 0, 3, 2]
               , [2, 0, 3, 1]
               , [3, 1, 2, 0]
               ]

mapLine3 :: String -> [String]
mapLine3 xs = nub $ map (map (xs !!)) idxs
  where idxs = [ [0, 1, 2, 3, 4, 5, 6, 7, 8]
               , [6, 7, 8, 3, 4, 5, 0, 1, 2]
               , [2, 5, 8, 1, 4, 7, 0, 3, 6]
               , [0, 3, 6, 1, 4, 7, 2, 5, 8]
               , [8, 7, 6, 5, 4, 3, 2, 1, 0]
               , [2, 1, 0, 5, 4, 3, 8, 7, 6]
               , [6, 3, 0, 7, 4, 1, 8, 5, 2]
               , [8, 5, 2, 7, 4, 1, 6, 3, 0]
               ]

parseInput2 :: [String] -> Moves
parseInput2 = M.fromList .
              concatMap (\(xs, y) -> [(x, y) | x <- xs]) .
              map (first mapLine2 . splitAt 4) .
              filter ((== 13) . length)

parseInput3 :: [String] -> Moves
parseInput3 = M.fromList .
              concatMap (\(xs, y) -> [(x, y) | x <- xs]) .
              map (first mapLine3 . splitAt 9) .
              filter ((== 25) . length)

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral

chunk :: Int -> [a] -> [[a]]
chunk n xs = helper xs
  where helper [] = []
        helper xs = take n xs : helper (drop n xs)

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:_:xs) = x : everyOther xs

partition :: Int -> [a] -> [[a]]
partition n xs =
  let d = isqrt $ length xs
      rows = chunk d xs
      rowBlocks = chunk n (map (chunk n) rows)
  in concatMap collect rowBlocks
  where
    collect block
      | all null block = []
      | otherwise =
          concatMap head block : collect (map tail block)

melt :: [[a]] -> [a]
melt xs =
  let n = isqrt $ length $ head xs
      d = isqrt $ length xs
      blocks = map (chunk n) xs
  in concatMap collect $ chunk d blocks
  where
    collect row
      | all null row = []
      | otherwise =
          concatMap head row ++ collect (map tail row)

parseInput :: [String] -> Moves
parseInput xs = (parseInput2 xs) `M.union` (parseInput3 xs)

testInput :: String
testInput = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

initState :: State
initState = ".#...####"

step :: Moves -> State -> State
step moves state =
  let d = if length state `mod` 2 == 0 then 2 else 3
      blocks = partition d state
  in melt $ map (moves M.!) blocks

countOn :: State -> Int
countOn = length . filter (== '#')

main :: IO ()
main = do
--  let input = map (filter (\c -> c `elem` ".#")) . lines $ testInput
  input <- map (filter (\c -> c `elem` ".#")) . lines <$> readFile "day21.txt"
  let moves = parseInput input
  print $ countOn $ (!! 5) $ iterate (step moves) initState
  print $ countOn $ (!! 18) $ iterate (step moves) initState

