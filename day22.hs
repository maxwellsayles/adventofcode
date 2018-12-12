import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import qualified Data.Map as M

type Grid = M.Map (Int, Int) Char

type Health = Char

data State = State { pos :: (Int, Int)
                   , dir :: (Int, Int)
                   } deriving Show

infected :: Health
infected = '#'

clean :: Health
clean = '.'

initState :: State
initState = State (0, 0) (0, -1)

turnLeft :: State -> State
turnLeft (State pos (dx, dy)) = State pos (dy, -dx)

turnRight :: State -> State
turnRight (State pos (dx, dy)) = State pos (-dy, dx)

forward :: State -> State
forward (State (x, y) (dx, dy)) = State (x + dx, y + dy) (dx, dy)

flipHealth :: Grid -> State -> Grid
flipHealth grid state
  | health == infected = M.insert p clean grid
  | otherwise = M.insert p infected grid
  where
    p = pos state
    health = lookup grid state

lookup :: Grid -> State -> Health
lookup grid state = clean `fromMaybe` (M.lookup (pos state) grid)

step :: Grid -> State -> (Grid, State)
step grid state
  | health == infected =
      let state' = forward $ turnRight state
      in  (grid', state')
  | otherwise =
      let state' = forward $ turnLeft state
      in (grid', state')
  where
    health = lookup grid state
    grid' = flipHealth grid state

parseInput :: [String] -> Grid
parseInput rows =
  let height = length rows
      width = length $ head rows
  in M.fromList $
     concatMap (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) $
     zip [-height..height] $
     map (zip [-width..width]) rows

main :: IO ()
main = do
  input <- lines `fmap` readFile "day22.txt"
  print $ parseInput input

