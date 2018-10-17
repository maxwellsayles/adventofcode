import Control.Applicative

import qualified Data.IntMap as IM

parseInput :: String -> IM.IntMap Int
parseInput = IM.fromList .
             map (\[x, y] -> (read x, read y)) .
             map words .
             lines .
             filter (':' /= )

cost :: IM.IntMap Int -> Int -> Int -> Int
cost map depth time =
  case IM.lookup depth map of
    Just range -> let n = (range - 1) * 2
                  in if time `mod` n == 0 then range * depth else 0
    _ -> 0

safe :: IM.IntMap Int -> Int -> Bool
safe input startTime =
  all (\(depth, range) -> (depth + startTime) `mod` (range * 2 - 2) /= 0) $ IM.toList input

main :: IO ()
main = do
--  let input = IM.fromList [(0, 3), (1, 2), (4, 4), (6, 4)]
  input <- parseInput <$> readFile "day13.txt"
  let maxLayer = fst $ IM.findMax input
  print $ sum $ zipWith (cost input) [0..maxLayer] [0..]
  print $ head $ filter (safe input) [0..]
