import Data.Bits (popCount, testBit, xor)
import Data.Char (ord)
import Data.Foldable (foldl')
import Text.Printf (printf)

import qualified Data.Vector as V

data State = State { statePos :: Int
                   , stateSkip :: Int
                   , stateBuffer :: [Int]
                   }

stepBuffer :: Int -> Int -> [Int] -> [Int]
stepBuffer pos len xs =
  let n = length xs
      xs' = take n $ drop pos $ cycle xs
      ys = (reverse $ take len xs') ++ drop len xs'
  in  take n $ drop (n - pos) $ cycle ys

stepState :: State -> Int -> State
stepState (State pos skip xs) len =
  let pos' = (pos + skip + len) `mod` (length xs)
      skip' = skip + 1
      xs' = stepBuffer pos len xs
  in State pos' skip' xs'

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

toHex :: Int -> String
toHex = printf "%02x"

hashBytes :: String -> [Int]
hashBytes input =
  let initState = State 0 0 [0..255]
      lengths = (map ord input) ++ [17, 31, 73, 47, 23]
      finalState = foldl' stepState initState (concat $ replicate 64 lengths)
      ys = stateBuffer finalState
      bs = map (foldl' xor 0) (chunk 16 ys)
  in bs

hashString :: String -> String
hashString = concatMap toHex . hashBytes

hashPopCount :: String -> Int
hashPopCount = sum . map popCount . hashBytes

type Grid = V.Vector (V.Vector Int)

grid :: String -> Grid
grid input =
  let row i = input ++ "-" ++ show i
  in V.fromList $ map (V.fromList . hashBytes . row) [0..127]

isSet :: Int -> Int -> Grid -> Bool
isSet x y g =
  let bx = x `div` 8
      bi = x `mod` 8
      b = (g V.! y) V.! bx
  in testBit b bi

main :: IO ()
main = do
  let input = "amgozmfv"
  let row i = input ++ "-" ++ show i
  print $ sum $ map (hashPopCount . row) [0..127]
  let g = grid input
  print $ length $ filter id [isSet x y g | x <- [0..127], y <- [0..127]]
