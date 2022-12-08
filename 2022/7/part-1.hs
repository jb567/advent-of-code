{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
import Data.Map (Map)
import Data.List (tails)
import qualified Data.Map as Map ((!), elems, insertWith, singleton)
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text as T (stripPrefix, pack)
import qualified Data.Text.Read as T (decimal)

type State = ([Text], Map [Text] Int)

read' :: State -> Text -> State
-- ls does not change the state
read' (_, m) "$ cd /" = ([],  m)
read' (_:cd, m) "$ cd .." = (cd,  m)
read' (cd, m) (T.stripPrefix "$ cd " -> Just dir) = (dir:cd, m)
read' (cd, m) (T.stripPrefix "$dir " -> Just dir) = (cd, Map.insertWith (const id) (dir:cd) 0 m)
read' (cd, m) (T.decimal -> Right (size, _ )) =  (cd, foldl (flip $ Map.insertWith (+) `flip` size) m $ tails cd)
read' s _ = s



getInput :: FilePath -> IO [Text]
getInput path = do contents <- readFile path
                   return $ map T.pack (lines contents)

run :: IO()
run = do
      file <- getInput "input.txt"
      print $ foldl (+) 0 $ filter (<= 100000) $ Map.elems $ snd $ foldl read' ([], Map.singleton [] 0) file
