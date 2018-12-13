import Data.Maybe (fromMaybe)
import Prelude hiding (lookup, reverse)

import qualified Data.Map as M

type Grid = M.Map (Int, Int) Char

type Health = Char

data State = State { pos :: (Int, Int)
                   , dir :: (Int, Int)
                   } deriving Show

clean :: Health
clean = '.'

weakened :: Health
weakened = 'W'

infected :: Health
infected = '#'

flagged :: Health
flagged = 'F'

initState :: State
initState = State (0, 0) (0, -1)

turnLeft :: State -> State
turnLeft (State pos (dx, dy)) = State pos (dy, -dx)

turnRight :: State -> State
turnRight (State pos (dx, dy)) = State pos (-dy, dx)

reverse :: State -> State
reverse (State pos (dx, dy)) = State pos (-dx, -dy)

forward :: State -> State
forward (State (x, y) (dx, dy)) = State (x + dx, y + dy) (dx, dy)

stepHealth :: Grid -> State -> Grid
stepHealth grid state = M.insert p health' grid
  where
    p = pos state
    health = lookup grid state
    health' | health == clean = weakened
            | health == weakened = infected
            | health == infected = flagged
            | health == flagged = clean

lookup :: Grid -> State -> Health
lookup grid state = clean `fromMaybe` (M.lookup (pos state) grid)

step :: Grid -> State -> (Grid, State)
step grid state = (grid', state')
  where
    grid' = stepHealth grid state
    state'
      | health == clean = forward $ turnLeft state
      | health == weakened = forward state
      | health == infected = forward $ turnRight state
      | health == flagged = forward $ reverse state
    health = lookup grid state

parseInput :: [String] -> Grid
parseInput rows =
  let height = length rows
      width = length $ head rows
      h = height `div` 2
      w = width `div` 2
  in M.fromList $
     concatMap (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) $
     zip [-h .. h] $
     map (zip [-w .. w]) rows

isWeakened :: Grid -> State -> Bool
isWeakened grid state = lookup grid state == weakened

main :: IO ()
main = do
  input <- (parseInput . lines) `fmap` readFile "day22.txt"
  let walk = iterate (uncurry step) (input, initState)
  print $ length $ filter (uncurry isWeakened) $ take 10000000 walk

