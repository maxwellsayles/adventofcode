import Control.Applicative
import Data.List (nub)

valid :: Eq a => [a] -> Bool
valid xs = (length $ nub xs) == length xs

main :: IO ()
main = do
  input <- map words . lines <$> readFile "day4.txt"
  print $ length $ filter valid input
