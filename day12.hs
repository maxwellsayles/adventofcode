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


allConnected :: IM.IntMap [Int] -> IS.IntSet -> [IS.IntSet]
allConnected edges set
  | IS.null set = []
  | otherwise =
      let component = connected edges IS.empty (head $ IS.elems set)
          set' = IS.difference set component
      in  component : allConnected edges set'

main :: IO ()
main = do
  input <- IM.fromList . map toEdge . lines <$> readFile "day12.txt"
  print $ IS.size $ connected input IS.empty 0
  print $ length $ allConnected input $ IS.fromList $ IM.keys input
