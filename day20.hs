{-
Here's some math. Given a start point `p`, a start velocity `v`, and a start
acceleration `a`, the point after `t` moves is at

x = p + vt + at(t-1)/2
x = p + vt + at^2/2 - at/2
x = p + (v-a/2)t + (a/2)t^2
x = (2p + (2v-a)t + at^2) / 2

Let `(q, w, b)` be the second triple corresponding to `(p, v, a)`. Substituting
the above and solving for `t` yields:

q + (w-b/2)t + (b/2)t^2 = p + (v-a/2)t + (a/2)t^2
0 = p-q + (v-w-(a-b)/2)t + ((a-b)/2)t^2

This gives the coefficients of the quadratic formula

A = (a-b)/2
B = v-w-A
C = p-q

So for each pair, take one axis, compute `t` for where the two points collide 
on that axis, make sure `t` is positive and integral, and compute the two points
at time `t` and see if they collide.
-}


import Control.Applicative
import Data.List (sortBy, sort, tails)
import Data.Ord (comparing)

import qualified Data.Set as S

data Vector3 = Vector3 Int Int Int deriving (Eq, Ord, Show)
data Particle = Particle Vector3 Vector3 Vector3 deriving (Eq, Ord, Show)

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

location :: Particle -> Int -> (Int, Int, Int)
location (Particle (Vector3 px py pz) (Vector3 vx vy vz) (Vector3 ax ay az)) t =
  let px' = (2 * px + (2 * vx - ax) * t + ax * t * t) `div` 2
      py' = (2 * py + (2 * vy - ay) * t + ay * t * t) `div` 2
      pz' = (2 * pz + (2 * vz - az) * t + az * t * t) `div` 2
  in (px', py', pz')


collide1 :: (Double, Double, Double) -> (Double, Double, Double) -> [Double]
collide1 (p1, v1, a1) (p2, v2, a2) =
  let a = (a1 - a2) / 2
      b = v1 - v2 - a
      c = p1 - p2
      delta = b^2 - 4 * a * c
      s = sqrt delta
  in  [(-b + s) / (2 * a), (-b - s) / (2 * a)]

collideX :: Particle -> Particle -> [Double]
collideX (Particle (Vector3 p1 _ _) (Vector3 v1 _ _) (Vector3 a1 _ _))
         (Particle (Vector3 p2 _ _) (Vector3 v2 _ _) (Vector3 a2 _ _)) =
  let p1' = fromIntegral p1
      v1' = fromIntegral v1
      a1' = fromIntegral a1
      p2' = fromIntegral p2
      v2' = fromIntegral v2
      a2' = fromIntegral a2
  in collide1 (p1', v1', a2') (p2', v2', a2')

collideT :: Particle -> Particle -> Int -> Bool
collideT part1 part2 t =
  let part1' = location part1 t
      part2' = location part2 t
  in part1' == part2'

isIntegral :: Double -> Bool
isIntegral d = d == fromIntegral (round d)

collide :: Particle -> Particle -> Bool
collide part1 part2 =
  any (collideT part1 part2) $
  map round $
  filter isIntegral $
  filter (> 0) $
  collideX part1 part2

collisions :: [Particle] -> [Particle]
collisions ps =
  concatMap (\(x, y) -> [x, y]) $
  filter (uncurry collide) $
  [(x, y) | (x:ys) <- tails ps, y <- ys]

main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "day20.txt"
  print $ fst $ head $ sortBy (comparing snd) $ zip [0..] $
    map particleToSortedList input

  let leftover = S.fromList input `S.difference` (S.fromList $ collisions input)
  print $ S.size leftover
  
