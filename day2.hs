import Control.Applicative
import Control.Arrow

main :: IO ()
main = do
  input <- sum .
    map (uncurry (-) .
         (maximum &&& minimum) .
         map (read :: String -> Integer) .
         words) .
    lines <$>
    readFile "day2.txt"
  print input
