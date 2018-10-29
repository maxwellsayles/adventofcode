import Control.Arrow (second)
import Data.Char (isAlpha)
import Data.Maybe (catMaybes)

import qualified Data.Vector as V

data State = State { stateX :: Int
                   , stateY :: Int
                   , stateDX :: Int
                   , stateDY :: Int
                   }
  deriving Show

type Grid = V.Vector (V.Vector Char)

cell :: Grid -> Int -> Int -> Char
cell grid x y
  | y < 0 || y >= V.length grid = ' '
  | x < 0 || x >= V.length (grid V.! y) = ' '
  | otherwise = grid V.! y V.! x

directions :: Grid -> Int -> Int -> [(Int, Int)]
directions grid x y =
  catMaybes [ check 1 0
            , check (-1) 0
            , check 0 1
            , check 0 (-1)
            ]
  where
    check dx dy =
      let v = cell grid (x + dx) (y + dy)
      in if v /= ' '
         then Just (dx, dy)
         else Nothing

turn :: Grid -> State -> State
turn grid state@(State x y dx dy) =
  let (dx', dy') = head $
                   filter (/= (-dx, -dy)) $
                   directions grid x y
  in state { stateDX = dx', stateDY = dy' }

step :: State -> State
step (State x y dx dy) = State (x + dx) (y + dy) dx dy

path :: Grid -> State -> [State]
path grid state@(State x y dx dy)
  | v == '+' = 
    let stateTurn = turn grid state
    in state : path grid (step stateTurn)
  | v == '|' || v == '-' || isAlpha v = state : path grid (step state)
  | otherwise = []
  where
    v = cell grid x y

isAlphaState :: Grid -> State -> Maybe Char
isAlphaState grid (State x y _ _)
  | isAlpha v = Just v
  | otherwise = Nothing
  where
    v = cell grid x y

getXY :: State -> (Int, Int)
getXY (State x y _ _) = (x, y)

main :: IO ()
main = do
  grid <- (V.fromList . map V.fromList . lines) `fmap` readFile "day19.txt"
  let startX = length $ takeWhile (\x -> grid V.! 0 V.! x == ' ') [0..]
  let state = State startX 0 0 1
  let ps = path grid state
  putStrLn $ catMaybes $ map (isAlphaState grid) ps
  print $ length ps
  
