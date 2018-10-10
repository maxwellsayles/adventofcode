import Control.Applicative
import Control.Arrow
import Data.Foldable (foldl')

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

toEdge :: String -> (Int, [Int])
toEdge = (head &&& drop 2) . map read . words . filter (/= ',')

connected :: IM.IntMap [Int] -> IS.IntSet -> Int -> IS.IntSet
connected edges acc x
  | x `IS.member` acc = acc
  | otherwise = foldl' (connected edges) (IS.insert x acc) (edges IM.! x)

main :: IO ()
main = do
  input <- IM.fromList . map toEdge . lines <$> readFile "day12.txt"
  print $ IS.size $ connected input IS.empty 0
