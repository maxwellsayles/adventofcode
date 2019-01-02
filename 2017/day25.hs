import Data.Maybe (fromMaybe)

import qualified Data.IntMap as IM
import qualified Data.Map as M

data State = State Char Int Tape deriving Show
data Transition = Transition Int Int Char
type Tape = IM.IntMap Int
type TransitionMap = M.Map (Char, Int) Transition

tapeLookup :: Int -> Tape -> Int
tapeLookup idx tape = 0 `fromMaybe` IM.lookup idx tape

stepCount :: Int
stepCount = 12399302

transitionMap :: TransitionMap
transitionMap = M.fromList $
  map (\(s, u, v, d, t) -> ((s, u), Transition v d t))
        [ ('A', 0, 1,  1, 'B')
        , ('A', 1, 0,  1, 'C')
        , ('B', 0, 0, -1, 'A')
        , ('B', 1, 0,  1, 'D')
        , ('C', 0, 1,  1, 'D')
        , ('C', 1, 1,  1, 'A')
        , ('D', 0, 1, -1, 'E')
        , ('D', 1, 0, -1, 'D')
        , ('E', 0, 1,  1, 'F')
        , ('E', 1, 1, -1, 'B')
        , ('F', 0, 1,  1, 'A')
        , ('F', 1, 1,  1, 'E')
        ]

step :: State -> State
step (State s idx tape) =
  let (Transition v d t) = transitionMap M.! (s, tapeLookup idx tape)
      tape' = IM.insert idx v tape
      idx' = idx + d
  in State t idx' tape'

initState :: State
initState = State 'A' 0 IM.empty

stateTape :: State -> Tape
stateTape (State _ _ tape) = tape

main :: IO ()
main = do
  print $ sum $ IM.elems $ stateTape $ (!! stepCount) $ iterate step initState
