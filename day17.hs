import Data.Foldable (foldl')

input :: Int
input = 355
--input = 3

step :: [Int] -> Int -> [Int]
step xs v =
  let n = length xs
      m = (input + 1) `mod` n
  in  m `seq` v : drop m xs ++ take m xs

-- https://www.reddit.com/r/adventofcode/comments/7kch29/2017_day_17_is_there_a_pattern_to_part_2/
-- "Do you need the buffer at all for this problem?"
update :: (Int, Int) -> Int -> (Int, Int)
update (pos, one) v =
  let pos' = (pos + input) `mod` v + 1
  in pos' `seq` (pos', if pos' == 1 then v else one)

main :: IO ()
main = do
  print $ (!! 1) $ foldl' step [0] [1..2017]
  print $ snd $ foldl' update (0, 0) [1..50000000]
