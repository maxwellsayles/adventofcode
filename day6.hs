import Control.Applicative
import Data.List
import qualified Data.Set as S

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

main :: IO ()
main = do
  input <- map (read :: String -> Int) . words <$> readFile "day6.txt"
  print input
  print $ solve S.empty input
