import Control.Applicative
import Control.Arrow

main :: IO ()
main = do
  print =<<
    sum .
    map (uncurry (-) .
         (maximum &&& minimum) .
         map read .
         words) .
    lines <$>
    readFile "day2.txt"
