import Control.Applicative

parseInput :: String -> [(Int, Int)]
parseInput = map (\[x, y] -> (read x, read y)) .
             map words .
             lines .
             filter (':' /= )

cost :: [(Int, Int)] -> Int
cost = sum . map (\(d, r) -> if d `mod` (r * 2 - 2) == 0 then d * r else 0)

safe :: Int -> [(Int, Int)] -> Bool
safe t = all (\(d, r) -> (d + t) `mod` (r * 2 - 2) /= 0)

main :: IO ()
main = do
--  let input = IM.fromList [(0, 3), (1, 2), (4, 4), (6, 4)]
  input <- parseInput <$> readFile "day13.txt"
  print $ cost input
  print $ head $ filter (flip safe input) [0..]
