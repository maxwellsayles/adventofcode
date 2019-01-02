import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import qualified Data.IntSet as IS
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
                   } deriving Show

tokenize :: String -> [Instruction]
tokenize = map read . lines

initState :: String -> State
initState s = let instructions = V.fromList $ tokenize s
              in  State 0 instructions M.empty

getValue :: M.Map Char Int -> Operand -> Int
getValue _ (Literal x) = x
getValue m (Register r) = fromMaybe 0 $ M.lookup r m

currentInstruction :: State -> Instruction
currentInstruction state = instructions state V.! ip state

step' :: State -> State
step' state
  | ip state == 8 =
    let b = getValue regs (Register 'b')
        f' = if IS.member b poi then 1 else 0
        regs' = M.insert 'f' f' regs
    in state { registers = regs', ip = 24 }
  | otherwise = step state
  where
    regs = registers state


step :: State -> State
step state = execute $ currentInstruction state
  where
    execute (Set (Register r1) op2) =
      let v2 = getValue regs op2
          regs' = M.insert r1 v2 regs
      in state { registers = regs', ip = ip' }

    execute (Sub op1 op2) = binary op1 op2 (-)
    execute (Mul op1 op2) = binary op1 op2 (*)

    execute (Jnz op1 op2) =
      let v1 = getValue regs op1
          v2 = getValue regs op2
      in  if v1 /= 0
          then state { ip = ip state + v2 }
          else state { ip = ip' }

    ip' = ip state + 1
    regs = registers state

    binary x1@(Register r1) x2 op =
      let v1 = getValue regs x1
          v2 = getValue regs x2
          regs' = M.insert r1 (op v1 v2) regs
      in state { registers = regs', ip = ip' }

isFinished :: State -> Bool
isFinished state = ip state < 0 || ip state >= V.length (instructions state)

runCopro :: State -> [State]
runCopro state
  | isFinished state = []
  | otherwise = state : runCopro (step state)

isMul :: Instruction -> Bool
isMul (Mul _ _) = True
isMul _ = False

primes :: [Int]
primes = 2 : 3 : filter (\x -> not $
                               any (\y -> x `mod` y == 0) $
                               takeWhile (\y -> y * y <= x) primes) [5, 7 ..]

maxPrime :: Int
maxPrime = 57 * 100000 + 17000

poi :: IS.IntSet
poi = IS.fromList $ takeWhile (<= maxPrime) primes

main :: IO ()
main = do
  input <- initState `fmap` readFile "day23.txt"

  print $ length $ filter isMul $ map currentInstruction $ runCopro input

  let input2 = input { registers = M.fromList [('a', 1)] }
  let solve2 state | isFinished state = state
                   | otherwise = solve2 $ step' state
  print $ getValue (registers (solve2 input2)) (Register 'h')
