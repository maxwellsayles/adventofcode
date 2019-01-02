import Data.Char (digitToInt)
import Data.Text (pack, strip, unpack)

main :: IO ()
main = do
  input <- (unpack . strip . pack) `fmap` readFile "day1.txt"
  print $
    sum $
    map (digitToInt . fst) $
    filter (uncurry (==)) $
    zip input (tail $ cycle input)
  print $
    sum $
    map (digitToInt . fst) $
    filter (uncurry (==)) $
    zip input (drop (length input `div` 2) $ cycle input)
