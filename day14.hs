import Data.Bits (popCount, xor)
import Data.Char (ord)
import Data.Foldable (foldl')
import Text.Printf (printf)

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

main :: IO ()
main = do
  let input = "amgozmfv"
  let row i = input ++ "-" ++ show i
  print $ sum $ map (hashPopCount . row) [0..127]
