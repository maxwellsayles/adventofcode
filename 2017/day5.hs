{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import qualified Data.IntMap as IM

stepCount :: IM.IntMap Int -> Int -> Int -> Int
stepCount m ip count =
  case IM.lookup ip m of
    Nothing -> count
    Just steps -> stepCount (IM.update (Just . succ) ip m) (ip + steps) (count + 1)

stepCount2 :: IM.IntMap Int -> Int -> Int -> Int
stepCount2 m ip !count =
  case IM.lookup ip m of
    Nothing -> count
    Just steps ->
      if steps < 3
      then stepCount2 (IM.update (Just . succ) ip m) (ip + steps) (count + 1)
      else stepCount2 (IM.update (Just . pred) ip m) (ip + steps) (count + 1)

main :: IO ()
main = do
  input <- map (read :: String -> Int) . lines <$> readFile "day5.txt"
  let initSteps = IM.fromList $ zip [0..] input
  print $ stepCount initSteps 0 0
  print $ stepCount2 initSteps 0 0
