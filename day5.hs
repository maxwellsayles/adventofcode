import Control.Applicative
import qualified Data.IntMap as IM

stepCount :: IM.IntMap Int -> Int -> Int -> Int
stepCount m ip count =
  case IM.lookup ip m of
    Nothing -> count
    Just steps -> stepCount (IM.update (Just . succ) ip m) (ip + steps) (count + 1)

main :: IO ()
main = do
  input <- map (read :: String -> Int) . lines <$> readFile "day5.txt"
  let initSteps = IM.fromList $ zip [0..] input
  print $ stepCount initSteps 0 0
