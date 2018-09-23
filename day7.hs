{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S
import qualified Data.Text as T hiding (concatMap)

getNames :: [Text] -> [Text]
getNames = map (head . T.words)

supports :: Text -> [Text]
supports l =
  let ws = T.words l
  in if length ws == 2
     then []
     else T.splitOn ", " (T.unwords (drop 3 ws))

supported :: [Text] -> Set Text
supported = S.fromList . concatMap supports

main :: IO ()
main = do
  input <- T.lines . T.pack <$> readFile "day7.txt"
  let names = S.fromList $ getNames input
  putStrLn $ T.unpack $ head $ S.toList $ S.difference names (supported input)
