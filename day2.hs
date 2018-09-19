import Control.Applicative
import Control.Arrow

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> readFile "day2.txt"

  -- part 1
  print $ sum $ map (uncurry (-) . (maximum &&& minimum)) input
