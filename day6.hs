{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

step :: [Int] -> [Int]
step xs =
  let maxValue = maximum xs
      index = length $ takeWhile (/= maxValue) xs
      xs' = take index xs ++ 0 : drop (index + 1) xs
      ys = replicate (index + 1) 0 ++ replicate maxValue 1 ++ repeat 0
      n = length xs
  in zipWith (+) (drop n ys) $ zipWith (+) xs' ys

solve :: S.Set [Int] -> [Int] -> Int
solve visited xs =
  if xs `S.member` visited
  then S.size visited
  else solve (S.insert xs visited) (step xs)

solve2 :: M.Map [Int] Int -> [Int] -> Int -> Int
solve2 visited xs !i =
  case M.lookup xs visited of
    Just n -> i - n
    Nothing -> solve2 (M.insert xs i visited) (step xs) (i + 1)

main :: IO ()
main = do
  input <- map (read :: String -> Int) . words <$> readFile "day6.txt"
  print input
  print $ solve S.empty input
  print $ solve2 M.empty input 0
