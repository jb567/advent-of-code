import Data.Char (isDigit, digitToInt, isSymbol)
import Data.List (intersect)
import Data.Text (pack, unpack, splitOn)
import Control.Monad
import System.IO



-- The algorithm will be
-- 1. Parse all the numbers
-- 2. Store them with all their co-ords
-- 3. Lookup the symbols and their adjacents
-- 4. Add them up
--


type NumPos = (Int, [Coords])-- deriving Show
type Coords = (Int, Int)


parseLine :: [Char] -> Int -> Int -> [NumPos]
parseLine [] _ _ = []
parseLine s@(x':xs) x y | not (isDigit x') = parseLine xs (x+1) y
                        | otherwise        = (num, arr): parseLine (drop len s) (x+len) y
                        -- figure out how long this is
                        where (num, len) = digitsAndLength s 0 0
                              arr = map (\z -> (z, y)) $ map (+ x) [0..(len-1)]

digitsAndLength :: [Char] -> Int -> Int -> (Int, Int)
digitsAndLength [] x y = (x,y)
digitsAndLength (x:xs) d l | not (isDigit x) = (d, l)
                           | otherwise       = digitsAndLength xs z (l+1)
                           where z = d * 10 + (digitToInt x)


-- Find symbols
--
-- Finds the positions
findSymbols :: [Char] -> Int -> Int -> [Coords]
findSymbols [] _ _ = []
findSymbols (h:t) x y | h == '.'        = findSymbols t (x+1) y
                      | not (isDigit h) = (x,y): findSymbols t (x+1) y
                      | otherwise       = findSymbols t (x+1) y

findAdjacent :: (Int, Int) -> [(Int, Int)]
findAdjacent (x, y) = [ (i, j) | i <- [x-1..x+1],
                                 j <- [y-1..y+1]]

sumUp :: [Coords] -> [NumPos] -> Int
sumUp symbols numbers = foldl (+) 0 $ map fst $ filter z numbers
                      where i y = (snd y) `intersect` ((map findAdjacent symbols) >>= id)
                            z y = not $ null (i y)


parseFile :: [Char] -> ([NumPos], [Coords])
parseFile file = (numbers, symbols)
                where lines      = zip [0..] $ splitOn' "\n" file
                      splitOn' s = (map unpack) . (splitOn $ pack s) . pack
                      numbers    = (map (\(n, str) -> parseLine str 0 n) lines)   >>= id
                      symbols    = (map (\(n, str) -> findSymbols str 0 n) lines) >>= id

process :: [Char] -> Int
process = (uncurry (flip sumUp)) . parseFile

main :: IO()
main = do
         handle <- openFile "fulltest.txt" ReadMode
         contents <- hGetContents handle
         print $ process contents
         --print $ sumUp (snd fil) (fst fil)
         hClose handle
