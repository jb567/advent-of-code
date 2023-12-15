{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
import Data.Text (Text)
import qualified Data.Text as T (stripPrefix, unpack, pack, chunksOf, intercalate)
import qualified Data.Text.Read as T (decimal, signed)

operation :: Int -> [Text] -> [Int]
operation _ [] = []
operation x ("noop":xs) = x : operation x xs
operation x ((T.stripPrefix "addx " -> Just y):xs) =  x:x:(operation (x + addv) xs)
                                                where addv = (\(Right (z,_)) -> z) $ T.signed T.decimal y

cyclesToInspect :: [Int]
cyclesToInspect = [20+x*40 | x <- [0..]]

results :: [Text] -> Text
results z = T.intercalate "\n" . T.chunksOf 40 $ T.pack $ zipWith pixel [1..] cycles where
        pixel i x = if abs (x - (i - 1) `mod` 40) < 2 then '#' else '.'
	cycles = operation 1 z


getInput :: FilePath -> IO [Text]
getInput path = do contents <- readFile path
                   return $ map T.pack (lines contents)

run :: IO()
run = do
      file <- getInput "input.txt"
      putStr . T.unpack $ results file
