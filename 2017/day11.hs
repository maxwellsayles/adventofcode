import Control.Applicative
import Control.Arrow
import Data.List (foldl', groupBy)

splitCSV :: String -> [String]
splitCSV = words . map (\c -> if c == ',' then ' ' else c)

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) "nw"
  | even x    = (x - 1, y)
  | otherwise = (x - 1, y - 1)
move (x, y) "ne"
  | even x    = (x + 1, y)
  | otherwise = (x + 1, y - 1)
move (x, y) "sw"
  | even x    = (x - 1, y + 1)
  | otherwise = (x - 1, y)
move (x, y) "se"
  | even x    = (x + 1, y + 1)
  | otherwise = (x + 1, y)
move (x, y) "n" = (x, y - 1)
move (x, y) "s" = (x, y + 1)

main :: IO ()
main = do
  input <- splitCSV <$> readFile "day11.txt"

  let pos = foldl' move (0, 0) input
  print $ uncurry max pos

  let allPos = scanl move (0, 0) input
  print $ uncurry max $ maximum *** maximum $ unzip allPos
