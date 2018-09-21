input :: Int
input = 289326

stepCount :: Int -> Int
stepCount n =
  let s = (ceiling $ sqrt $ fromIntegral n) `div` 2 * 2 - 1
      m = (s + 1) `div` 2
      p = abs $ (n - s ^ 2) `mod` (s + 1) - m
  in  p + m

main :: IO ()
main = do
  print $ stepCount input
