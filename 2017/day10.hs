import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable (foldl')
import Text.Printf (printf)

input :: [Int]
input = [129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108]

input2 :: [Int]
input2 = let input = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"
         in (map ord input) ++ [17, 31, 73, 47, 23]

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

main :: IO ()
main = do
  let initState = State 0 0 [0..255]
      finalState = foldl' stepState initState input
      xs = stateBuffer finalState
  print $ (xs !! 0) * (xs !! 1)

  let finalState2 = foldl' stepState initState (concat $ replicate 64 input2)
      ys = stateBuffer finalState2
      bs = map (foldl' xor 0) (chunk 16 ys)
      hash = concatMap toHex bs
  putStrLn hash
