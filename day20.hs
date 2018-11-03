import Control.Applicative
import Data.List (sortBy, sort)
import Data.Ord (comparing)

data Particle = Particle Vector3 Vector3 Vector3 deriving Show
data Vector3 = Vector3 Int Int Int deriving Show

vector3ToHeavy :: Vector3 -> [Int]
vector3ToHeavy (Vector3 x y z) = reverse $ sort $ map abs [x, y, z]

parseLine :: String -> Particle
parseLine line =
  let [px, py, pz, vx, vy, vz, ax, ay, az] =
        map read $
        words $
        map (\c -> if c == ',' then ' ' else c) $
        filter (\c -> not $ c `elem` "pva=<> ") line
  in Particle (Vector3 px py pz) (Vector3 vx vy vz) (Vector3 ax ay az)

particleToSortedList :: Particle -> [Int]
particleToSortedList (Particle ps vs as) =
  (vector3ToHeavy as) ++ (vector3ToHeavy vs) ++ (vector3ToHeavy ps)

main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "day20.txt"
  print $ fst $ head $ sortBy (comparing snd) $ zip [0..] $
    map particleToSortedList input
