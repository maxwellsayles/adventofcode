import Control.Applicative

import qualified Data.IntMap as IM

parseInput :: String -> IM.IntMap Int
parseInput = IM.fromList .
             map (\[x, y] -> (read x, read y)) .
             map words .
             lines .
             filter (':' /= )

pos :: IM.IntMap Int -> Int -> Int -> Maybe Int
pos map depth time = do
  range <- IM.lookup depth map
  let n = (range - 1) * 2
      y = time `mod` n
  return $! if y >= range then n - y else y

cost :: IM.IntMap Int -> Int -> Int -> Int
cost map depth time =
  case IM.lookup depth map of
    Just range -> let n = (range - 1) * 2
                  in if time `mod` n == 0 then range * depth else 0
    _ -> 0

main :: IO ()
main = do
  input <- parseInput <$> readFile "day13.txt"
  let maxLayer = fst $ IM.findMax input
  print $ sum $ zipWith (cost input) [0..maxLayer] [0..]
