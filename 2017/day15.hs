{-# LANGUAGE BangPatterns #-}

import Data.Bits

cap :: Int
cap = 2147483647

factorA :: Int
factorA = 16807

factorB :: Int
factorB = 48271

initA :: Int
--initA = 65
initA = 289

initB :: Int
--initB = 8921
initB = 629

stepA :: Int -> Int
stepA !x = (x * factorA) `rem` cap

stepB :: Int -> Int
stepB !x = (x * factorB) `rem` cap

generatorA :: [Int]
generatorA = tail $ iterate stepA initA

generatorB :: [Int]
generatorB = tail $ iterate stepB initB

generatorA' :: [Int]
generatorA' = filter (\x -> x .&. 3 == 0) $ tail $ iterate stepA initA

generatorB' :: [Int]
generatorB' = filter (\x -> x .&. 7 == 0) $ tail $ iterate stepB initB

eq16 :: Int -> Int -> Bool
eq16 x y =
  let x' = x .&. 65535
      y' = y .&. 65535
  in x' == y'

main :: IO ()
main = do
  print $ length $ filter (uncurry eq16) $ take 40000000 $ zip generatorA generatorB
  print $ length $ filter (uncurry eq16) $ take 5000000 $ zip generatorA' generatorB'
