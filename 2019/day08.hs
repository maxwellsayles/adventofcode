import Control.Arrow
import Data.List (minimumBy)
import Data.List.Split
import Data.Ord (comparing)

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

main :: IO ()
main = do
  let width = 25
  let height = 6
  let area = width * height

  contents <- init <$> readFile "day08.txt"
  let layers = chunksOf area contents
  let zs = map (countIf (== '0')) layers
  print $
    uncurry (*) $
    countIf (== '1') &&& countIf (== '2') $
    snd $
    minimumBy (comparing fst) $
    zip zs layers
