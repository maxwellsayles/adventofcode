import qualified Data.IntSet as IS

firstRepeat :: [Int] -> Int -> IS.IntSet -> Int
firstRepeat (x:xs) acc visited
  | IS.member acc visited = acc
  | otherwise = firstRepeat xs (acc + x) (IS.insert acc visited)

main :: IO ()
main = do 
  input <- (map read . lines . filter (/= '+')) `fmap` readFile "day01.txt"
  print $ sum input
  print $ firstRepeat (cycle input) 0 IS.empty
