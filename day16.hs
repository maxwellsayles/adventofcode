import Control.Applicative ((<$>))
import Control.Arrow ((***), second)
import Data.Foldable (foldl')

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving Show

tokenize :: String -> [Move]
tokenize = map convert . words . map (\c -> if c == ',' then ' ' else c)
  where
    convert ('s' : xs) = Spin $ read xs
    convert ('x' : xs) = uncurry Exchange $ (read *** read) $ second tail $ span (/= '/') xs
    convert ('p' : x : '/' : [y]) = Partner x y

update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ x : drop (i + 1) xs

step :: String -> Move -> String
step xs (Spin c) =
  let n = length xs
  in drop (n - c) xs ++ take (n - c) xs
step xs (Exchange i j) =
  let x = xs !! i
      y = xs !! j
  in  update i y $ update j x xs
step xs (Partner a b) =
  map (\c -> if c == a then b else if c == b then a else c) xs

main :: IO ()
main = do
  input <- tokenize <$> readFile "day16.txt"
  putStrLn $ foldl' step ['a'..'p'] input
