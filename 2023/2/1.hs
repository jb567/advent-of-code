import Data.Text (Text, unpack, pack, splitOn, strip, empty)
import Data.Char (isDigit)
import Data.Text.Read (signed, decimal)
import Control.Monad
import System.IO

data Colour = RED | GREEN | BLUE deriving (Eq, Show)

type Draw = (Colour, Int)

type Round = [Draw]
type Game = (Int, [Round])

--will be three long, one for red, green and blue
type MinCubes = (Int, Int, Int)
maxOfColour' :: Colour -> Round -> Int
maxOfColour' c r = foldl max 0 (map (\(_, x) -> x) $ filter (\(x, _) -> x == c) r)

maxOfColour :: Colour -> Game -> Int
maxOfColour c = (foldl max 0) . (map (maxOfColour' c)) . snd

minCub :: Game -> MinCubes
minCub g = (maxOfColour RED g, maxOfColour GREEN g, maxOfColour BLUE g)

withinPossibility :: Game -> Bool
withinPossibility game | r > 12    = False
                       | g > 13    = False
                       | b > 14    = False
                       | otherwise = True
                       where (r,g,b) = minCub game

sumValidIDs :: [Game] -> Int
sumValidIDs = (foldl (+) 0) . (map fst) . (filter withinPossibility)

-------------
-- PARSING --
-------------


num :: [Char] -> Int
num = (either (error . show) fst . signed decimal) . pack

parseGame :: Text -> Game
parseGame t = (id, parseRounds rounds)
            where [gid, rounds] = splitOn (pack ":") t
                  id = num (filter isDigit (unpack gid))

parseRounds :: Text -> [Round]
parseRounds r = map parseRound $ splitOn (pack ";") r

parseRound :: Text -> Round
parseRound =  (map (parseHand . strip)) . splitOn (pack ",")

parseHand :: Text -> Draw
parseHand h = (col, count)
             where count = num $ filter isDigit $ unpack h
                   c = unpack . strip . pack $ filter (not . isDigit) $ unpack h 
                   col | c == "red" = RED
                       | c == "green" = GREEN
                       | c == "blue" = BLUE

parseAllGames :: Text -> [Game]
parseAllGames = (map parseGame) . (filter (\x -> x /= empty)) . (splitOn (pack "\n"))
--------------------------
-- STICKING IT TOGETHER --
--------------------------

main = do
	handle <- openFile "fulltest.txt" ReadMode
	contents <- hGetContents handle
	print . sumValidIDs $ parseAllGames (pack contents)
	--print . sumValidIDs $ parseAllGames (pack contents)
	hClose handle
	--handle <- openFile "input.txt" ReadMode
	--contents <- hGetContents handle
	--print $ sumCal (pack contents)
	--hClose handle

