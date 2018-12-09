import Control.Applicative
import Control.Arrow
import Data.List (nub)

import qualified Data.Map as M

type Moves2 = M.Map String [String]
type Moves3 = M.Map String [String]
type Moves = M.Map String [String]

-- 0 1
-- 2 3
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

-- 0 1 2
-- 3 4 5
-- 6 7 8
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

parseInput2 :: [String] -> Moves2
parseInput2 = M.fromList .
              concatMap (\(xs, y) -> [(x, [y]) | x <- xs]) .
              map (first mapLine2 . splitAt 4) .
              filter ((== 13). length)

parseInput3 :: [String] -> Moves3
parseInput3 = M.fromList .
              concatMap (\(xs, ys) -> [(x, ys) | x <- xs]) .
              map (first mapLine3 . second partition2 . splitAt 9) .
              filter ((== 25). length)
  where partition2 :: String -> [String]
        partition2 xs = map (extract . flip drop xs) [0, 2, 8, 10]
          where extract xs = map (xs !!) [0, 1, 4, 5]

parseInput :: [String] -> Moves
parseInput xs = (parseInput2 xs) `M.union` (parseInput3 xs)

testInput :: String
testInput = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

initState :: [String]
initState = [".#...####"]

step :: Moves -> [String] -> [String]
step ms = concatMap (ms M.!)

countOn :: [String] -> Int
countOn = length . filter (== '#') . concat

main :: IO ()
main = do
--  let input = map (filter (\c -> c `elem` ".#")) . lines $ testInput
  input <- map (filter (\c -> c `elem` ".#")) . lines <$> readFile "day21.txt"
  let moves = parseInput input
  print (moves M.! "#######.#")
  mapM_ (print . countOn) $ (take 7) $ iterate (step moves) initState
