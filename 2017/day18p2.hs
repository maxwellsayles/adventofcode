import Control.Applicative
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V

data Operand = Literal Int | Register Char deriving Show

instance Read Operand where
  readsPrec _ s@(x:_)
    | isDigit x || x == '-' = [(Literal $ read s, "")]
    | otherwise = [(Register $ head s, "")]

data Instruction = Snd Operand
                 | Set Operand Operand
                 | Add Operand Operand
                 | Mul Operand Operand
                 | Mod Operand Operand
                 | Rcv Operand
                 | Jgz Operand Operand
                 deriving Show

instance Read Instruction where
  readsPrec _ s = [(parse $ words s, "")]
    where parse ["snd", op] = Snd (read op)
          parse ["set", op1, op2] = Set (read op1) (read op2)
          parse ["add", op1, op2] = Add (read op1) (read op2)
          parse ["mul", op1, op2] = Mul (read op1) (read op2)
          parse ["mod", op1, op2] = Mod (read op1) (read op2)
          parse ["rcv", op] = Rcv (read op)
          parse ["jgz", op1, op2] = Jgz (read op1) (read op2)

data State = State { snds :: [Int]
                   , rcvs :: S.Seq Int
                   , ip :: Int
                   , instructions :: V.Vector Instruction
                   , registers :: M.Map Char Int
                   }
  deriving Show

tokenize :: String -> [Instruction]
tokenize = map read . lines

initState :: String -> State
initState s = let instructions = V.fromList $ tokenize s
              in  State [] S.empty 0 instructions M.empty

getValue :: M.Map Char Int -> Operand -> Int
getValue _ (Literal x) = x
getValue m (Register r) = fromMaybe 0 $ M.lookup r m

step :: State -> State -> (State, State)
step state otherState = execute $ instructions state V.! ip state
  where
    execute (Snd op) =
      let v = getValue regs op 
          rcvs' = rcvs otherState S.|> v
          otherState' = otherState { rcvs = rcvs' }
          snds' = v : snds state
          state' = state { ip = ip', snds = snds' }
      in (state', otherState')

    execute (Rcv (Register r)) =
      let (x S.:< rcvs') = S.viewl $ rcvs state
          regs' = M.insert r x regs
      in  ret $ state { rcvs = rcvs', registers = regs', ip = ip' }

    execute (Set (Register r1) op2) =
      let v2 = getValue regs op2
          regs' = M.insert r1 v2 regs
      in  ret $ state { registers = regs', ip = ip' }

    execute (Add op1 op2) = binary op1 op2 (+)
    execute (Mul op1 op2) = binary op1 op2 (*)
    execute (Mod op1 op2) = binary op1 op2 (mod)

    execute (Jgz op1 op2) =
      let v1 = getValue regs op1
          v2 = getValue regs op2
      in  ret $ if v1 > 0
                then state { ip = ip state + v2 }
                else state { ip = ip' }

    ip' = ip state + 1
    regs = registers state

    binary x1@(Register r1) x2 op =
      let v1 = getValue regs x1
          v2 = getValue regs x2
          regs' = M.insert r1 (op v1 v2) regs
      in ret $ state { registers = regs', ip = ip' }

    ret state = (state, otherState)

-- True if state is on rcv and the rcvs queue is empty
isBlocked :: State -> Bool
isBlocked state =
  case instructions state V.! ip state of
    Rcv _ -> S.null $ rcvs state
    _ -> False

solve2 state1 state2
  | isBlocked state1 && isBlocked state2 = length $ snds state2
  | isBlocked state1 = uncurry solve2 $ step state2 state1
  | isBlocked state2 = uncurry solve2 $ step state1 state2
  | otherwise = uncurry solve2 $ (uncurry . flip) step $ step state1 state2

main :: IO ()
main = do
  state1 <- initState <$> readFile "day18.txt"
  let state2 = state1 { registers = M.fromList [('p', 1)] }
  print $ solve2 state1 state2

