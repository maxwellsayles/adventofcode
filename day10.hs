import Data.Foldable (foldl')

input :: [Int]
input = [129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108]

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

main :: IO ()
main = do
  let initState = State 0 0 [0..255]
      finalState = foldl' stepState initState input
      xs = stateBuffer finalState
  print $ (xs !! 0) * (xs !! 1)
