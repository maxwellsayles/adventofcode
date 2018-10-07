import Control.Applicative

sanitize :: String -> String
sanitize "" = ""
sanitize (x:xs)
  | x == '!' = sanitize $ tail xs
  | otherwise = x : sanitize xs

groups :: Int -> String -> [Int]
groups _ "" = []
groups level (x:xs)
  | x == '<' = groups level (tail $ dropWhile (/= '>') xs)
  | x == '{' = level : groups (level + 1) xs
  | x == '}' = groups (level - 1) xs
  | otherwise = groups level xs

main :: IO ()
main = do
  input <- init . sanitize <$> readFile "day9.txt"
  print $ sum $ groups 1 input
