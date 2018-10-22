import Data.Foldable (foldl')

input :: Int
input = 355
--input = 3

step :: [Int] -> Int -> [Int]
step xs v =
  let n = length xs
  in  take (n + 1) $ v : drop (input + 1) (cycle xs)

main :: IO ()
main = do
  print $ (!! 1) $ foldl' step [0] [1..2017]
