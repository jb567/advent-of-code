{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text as T (stripPrefix, pack)
import qualified Data.Text.Read as T (decimal, signed)

operation :: Int -> [Text] -> [Int]
operation _ [] = []
operation x ("noop":xs) = x : operation x xs
operation x ((T.stripPrefix "addx " -> Just y):xs) =  x:x:(operation (x + addv) xs)
                                                where addv = (\(Right (z,_)) -> z) $ T.signed T.decimal y

cyclesToInspect :: [Int]
cyclesToInspect = [20+x*40 | x <- [0..]]

results :: [Text] -> Int
results x = foldl (+) 0 $ zipWith cycleMap [0..] cycles
          where cycleMap i v | i == 20 || ((i - 20) `mod` 40 == 0) = i * (cycles!!i)
                             | otherwise                = 0
                cycles = operation 1 x


getInput :: FilePath -> IO [Text]
getInput path = do contents <- readFile path
                   return $ map T.pack (lines contents)

run :: IO()
run = do
      file <- getInput "input.txt"
      print $ results file
