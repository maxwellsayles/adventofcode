{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

import qualified Data.Map as M
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

supported :: [Text] -> S.Set Text
supported = S.fromList . concatMap supports

getName :: Text -> Text
getName = head . T.words

getWeight :: Text -> Int
getWeight = read . T.unpack . T.tail . T.init . (!! 1) . T.words

nameToWeight :: [Text] -> M.Map Text Int
nameToWeight = M.fromList . map (getName &&& getWeight)

nameToSupports :: [Text] -> M.Map Text [Text]
nameToSupports = M.fromList . map (getName &&& supports)

nameToStackWeight :: M.Map Text [Text]
                  -> M.Map Text Int
                  -> Text
                  -> M.Map Text Int
nameToStackWeight supportMap weightMap root =
  helper root M.empty
  where
    helper name acc =
      let supports = supportMap M.! name
          weight = weightMap M.! name
          acc' = foldr helper acc supports
          childStackWeights = map (acc' M.!) supports
          stackWeight = weight + sum childStackWeights
      in M.insert name stackWeight acc'

maybeImbalanced :: M.Map Text [Text]
                -> M.Map Text Int
                -> Text
                -> Maybe (Text, Int)
maybeImbalanced supportMap stackWeightMap name =
  let supports = supportMap M.! name
      groups = sortBy (comparing length) $
               groupBy ((==) `on` snd) $
               sortBy (comparing snd) $
               map (id &&& (stackWeightMap M.!)) supports
  in case length groups of
    1 -> Nothing
    2 -> Just (fst $ head $ groups !! 0, snd $ head $ groups !! 1)

solve2 :: M.Map Text [Text] -- nodes supported by a node
       -> M.Map Text Int -- weight of the node
       -> M.Map Text Int -- weight the node supports
       -> Text -- root node
       -> Int -- different of the imbalanced node
solve2 supportMap weightMap stackWeightMap root =
  helper root 0
  where
    helper name v =
      case maybeImbalanced supportMap stackWeightMap name of
        Nothing -> (weightMap M.! name) + v - (stackWeightMap M.! name)
        Just (name, v') -> helper name v'

main :: IO ()
main = do
  input <- T.lines . T.pack <$> readFile "day7.txt"
  let names = S.fromList $ getNames input
      root = head $ S.toList $ S.difference names (supported input)
      supportMap = nameToSupports input
      weightMap = nameToWeight input
      stackWeightMap = nameToStackWeight supportMap weightMap root
  putStrLn $ T.unpack root
  print $ solve2 supportMap weightMap stackWeightMap root
