import qualified Data.Map as M

parseInput :: [[a]] -> M.Map (Int, Int) a
parseInput rows =
  let height = length rows
      width = length $ head rows
  in M.fromList $
     concatMap (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) $
     zip [-height..height] $
     map (zip [-width..width]) rows

main :: IO ()
main = do
  input <- lines `fmap` readFile "day22.txt"
  print $ parseInput input

