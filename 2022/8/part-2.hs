import Data.List (transpose, tails, findIndex)

score :: [Char] -> [Int]
score row = [maybe (length xs) succ $ findIndex (>= x) xs | (x:xs) <- tails row]

row :: [Char] -> [Int]
row r = zipWith (*) (score r) (reverse $ score $ reverse r)

visibilityBoard :: [[Char]] -> [[Int]]
visibilityBoard b = zipWith (zipWith (*)) rows cols
                  where rows = map row b
                        cols = transpose $ map row (transpose b)

getInput :: FilePath -> IO [String]
getInput p = do
             file <- readFile p
             return $ lines file

go :: IO()
go = do
     board <- getInput "input.txt"
     print $ maximum $ concat (visibilityBoard board)


