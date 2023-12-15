import qualified Data.Text as T
import Data.Text (Text) 
import Data.Char (isDigit) 
import Data.Text.Read 
import System.IO
import Control.Monad

sumCal :: Text -> Int
sumCal = (foldl (+) 0) . (map (num . parseNumber)) . splitLines

splitLines :: Text -> [Text]
splitLines = T.splitOn (T.pack "\n")

parseNumber :: Text -> Text
parseNumber x = T.pack [firstNumber (T.unpack x), firstNumber (reverse $ T.unpack x)]

firstNumber :: [Char] -> Char
firstNumber (x:xs) | isDigit x = x
		   | otherwise = firstNumber xs

num :: Text -> Int
num = either (error . show) fst . signed decimal

main = do
	handle <- openFile "test.txt" ReadMode
	contents <- hGetContents handle
	print $ sumCal (T.pack contents)
	hClose handle
	handle <- openFile "input.txt" ReadMode
	contents <- hGetContents handle
	print $ sumCal (T.pack contents)
	hClose handle
