import Control.Arrow (second)

import qualified Data.IntMap as IM
import qualified Data.Set as S

type Port = Int
type Pipe = (Port, Port)
data Pipes = Pipes [Pipe] deriving Show

data State = State { currentPort :: Port
                   , bridge :: [Pipe]
                   , pipes :: Pipes
                   } deriving Show

parsePipe :: String -> Pipe
parsePipe s = let (x, y) = break (== '/') s
              in (read x, read $ tail y)

addPipe :: Pipe -> Pipes -> Pipes
addPipe p (Pipes ps) = Pipes (p:ps)

delPipe :: Pipe -> Pipes -> Pipes
delPipe p (Pipes ps) = Pipes (uncurry (++) $ second tail $ break (== p) ps)

findPipes :: Port -> Pipes -> [Pipe]
findPipes p (Pipes ps) = filter (\(x, y) -> x == p || y == p) ps

makeStateFromPipes :: Pipes -> State
makeStateFromPipes = State 0 []

nextPort :: Port -> Pipe -> Port
nextPort port (x, y) = if port == x then y else x

bridgeStrength :: [Pipe] -> Int
bridgeStrength = sum . map (\(x, y) -> x + y)

solveBridge :: State -> [State]
solveBridge state =
  let nextPipes = findPipes (currentPort state) (pipes state)
      nextStates = map (\pipe ->
                          State (nextPort (currentPort state) pipe)
                                (pipe : bridge state)
                                (delPipe pipe (pipes state))) nextPipes
  in if null nextPipes
     then [state]
     else concatMap solveBridge nextStates

main :: IO ()
main = do
  initState <- (makeStateFromPipes . Pipes . map parsePipe . lines) `fmap` readFile "day24.txt"
  let bridges = map bridge $ solveBridge initState
  print $ maximum $ map bridgeStrength bridges
  let longest = maximum $ map length bridges
  print $ maximum $ map bridgeStrength $ filter (\b -> length b == longest) bridges
