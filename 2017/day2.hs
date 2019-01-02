import Control.Applicative
import Control.Arrow
import Data.List
import Prelude hiding (divMod)

divMod :: Int -> Int -> (Int, Int)
divMod x y =
  if x > y then (x `div` y, x `mod` y) else (y `div` x, y `mod` x)

special :: [Int] -> Int
special xs =
  head $
  map fst $
  filter ((== 0) . snd) $
  [divMod x y | (x:ys) <- tails xs, y <- ys]

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> readFile "day2.txt"

  -- part 1
  print $ sum $ map (uncurry (-) . (maximum &&& minimum)) input

  -- part 2
  print $ sum $ map special input
