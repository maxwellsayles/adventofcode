import Control.Applicative
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M

data Operation = Operation String (Int -> Int)
data Condition = Condition String (Int -> Bool)
data Instruction = Instruction Operation Condition

toInstruction :: String -> Instruction
toInstruction l =
  let xs = words l

      opConst = read $ xs !! 2
      op = case xs !! 1 of
        "dec" -> (-)
        "inc" -> (+)
      operation = Operation (xs !! 0) (flip op opConst)

      condConst = read $ xs !! 6
      cond = case xs !! 5 of
        "==" -> (==)
        "!=" -> (/=)
        "<" -> (<)
        ">" -> (>)
        "<=" -> (<=)
        ">=" -> (>=)
      condition = Condition (xs !! 4) (flip cond condConst)

  in Instruction operation condition

type State = M.Map String Int

evalCond :: Condition -> State -> Bool
evalCond (Condition reg pred) state =
  let val = 0 `fromMaybe` (reg `M.lookup` state)
  in pred val

evalOp :: Operation -> State -> State
evalOp (Operation reg op) state =
  let val = 0 `fromMaybe` (reg `M.lookup` state)
  in M.insert reg (op val) state

evalInst :: State -> Instruction -> State
evalInst state (Instruction op cond) =
  if evalCond cond state then evalOp op state else state

main :: IO ()
main = do
  instructions <- map toInstruction . lines <$> readFile "day8.txt"

  let solution = foldl' evalInst M.empty instructions
  print $ maximum $ M.elems solution

  let solutions = scanl evalInst M.empty instructions
  print $ maximum $ concatMap M.elems solutions
