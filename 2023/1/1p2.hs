import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Text (Text) 
import Data.List (isPrefixOf, foldl')
import Data.Char (isDigit) 
import Data.Text.Read 
import System.IO
import Control.Monad

sumCal :: Text -> Int
sumCal = (foldl (+) 0) . (map (num . parseNumber)) . splitLines

splitLines :: Text -> [Text]
splitLines = T.splitOn (T.pack "\n")

parseNumber :: Text -> Text
parseNumber x | (T.unpack x) == [] = T.pack "0"
parseNumber x = T.pack [firstNumber (T.unpack x), lastNumber (reverse $ T.unpack x)]

lastNumber :: [Char] -> Char
lastNumber x | isPrefixOf (reverse "one") x = '1'
             | isPrefixOf (reverse "two") x = '2'
             | isPrefixOf (reverse "three") x = '3'
             | isPrefixOf (reverse "four") x = '4'
             | isPrefixOf (reverse "five") x = '5'
             | isPrefixOf (reverse "six") x = '6'
             | isPrefixOf (reverse "seven") x = '7'
             | isPrefixOf (reverse "eight") x = '8'
             | isPrefixOf (reverse "nine") x = '9'
lastNumber (x:xs) | isDigit x = x
                  | otherwise = lastNumber xs

firstNumber :: [Char] -> Char
firstNumber x | isPrefixOf "one" x = '1'
	      | isPrefixOf "two" x = '2'
	      | isPrefixOf "three" x = '3'
	      | isPrefixOf "four" x = '4'
	      | isPrefixOf "five" x = '5'
	      | isPrefixOf "six" x = '6'
	      | isPrefixOf "seven" x = '7'
	      | isPrefixOf "eight" x = '8'
	      | isPrefixOf "nine" x = '9'
firstNumber (x:xs) | isDigit x = x
                   | otherwise = firstNumber xs

num :: Text -> Int
num = either (error . show) fst . signed decimal

main = do
	handle <- openFile "test2.txt" ReadMode
	contents <- hGetContents handle
	print $ sumCal (T.pack contents)
	hClose handle
	handle <- openFile "input.txt" ReadMode
	contents <- hGetContents handle
	print $ sumCal (T.pack contents)
	hClose handle
