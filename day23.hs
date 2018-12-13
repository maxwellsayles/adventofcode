import Control.Applicative
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import qualified Data.Map as M
import qualified Data.Vector as V

data Operand = Literal Int | Register Char deriving Show

instance Read Operand where
  readsPrec _ s@(x:_)
    | isDigit x || x == '-' = [(Literal $ read s, "")]
    | otherwise = [(Register $ head s, "")]

data Instruction = Set Operand Operand
                 | Sub Operand Operand
                 | Mul Operand Operand
                 | Jnz Operand Operand
                 deriving Show

instance Read Instruction where
  readsPrec _ s = [(parse $ words s, "")]
    where parse ["set", op1, op2] = Set (read op1) (read op2)
          parse ["sub", op1, op2] = Sub (read op1) (read op2)
          parse ["mul", op1, op2] = Mul (read op1) (read op2)
          parse ["jnz", op1, op2] = Jnz (read op1) (read op2)

data State = State { ip :: Int
                   , instructions :: V.Vector Instruction
                   , registers :: M.Map Char Int
                   }

tokenize :: String -> [Instruction]
tokenize = map read . lines

initState :: String -> State
initState s = let instructions = V.fromList $ tokenize s
              in  State 0 instructions M.empty

getValue :: M.Map Char Int -> Operand -> Int
getValue _ (Literal x) = x
getValue m (Register r) = fromMaybe 0 $ M.lookup r m

step :: State -> State
step state = execute $ instructions state V.! ip state
  where
    execute (Snd op) =
      state { snds = getValue regs op : snds state, ip = ip' }

    execute (Rcv op)
      | getValue regs op /= 0 =
          let lastSound = head $ snds state
          in  state { rcvs = lastSound : rcvs state, ip = ip' }
      | otherwise = state { ip = ip' }

    execute (Set (Register r1) op2) =
      let v2 = getValue regs op2
          regs' = M.insert r1 v2 regs
      in state { registers = regs', ip = ip' }

    execute (Add op1 op2) = binary op1 op2 (+)
    execute (Mul op1 op2) = binary op1 op2 (*)
    execute (Mod op1 op2) = binary op1 op2 (mod)

    execute (Jgz op1 op2) =
      let v1 = getValue regs op1
          v2 = getValue regs op2
      in  if v1 > 0
          then state { ip = ip state + v2 }
          else state { ip = ip' }

    ip' = ip state + 1
    regs = registers state

    binary x1@(Register r1) x2 op =
      let v1 = getValue regs x1
          v2 = getValue regs x2
          regs' = M.insert r1 (op v1 v2) regs
      in state { registers = regs', ip = ip' }


solve1 :: State -> Int
solve1 state
  | not $ null $ rcvs state = head $ rcvs state
  | otherwise = solve1 $ step state

main :: IO ()
main = do
  input <- initState <$> readFile "day23.txt"
  print $ solve1 input
