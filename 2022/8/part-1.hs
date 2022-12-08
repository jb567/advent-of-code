import Data.List (scanl, transpose)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f l = map (uncurry f) $ zip [0..] l


visible :: [Char] -> [Bool]
visible l = zipWith (<) l' xs
          where l'@(x:xs) = scanl max minBound l

row :: [Char] -> [Bool]
row r = zipWith (||) (visible r) (reverse $ visible $ reverse r)

visibilityBoard :: [[Char]] -> [[Bool]]
visibilityBoard b = zipWith (zipWith (||)) rows cols
                  where rows = map row b
                        cols = transpose $ map row (transpose b)

getInput :: FilePath -> IO [String]
getInput p = do
             file <- readFile p
             return $ lines file

go :: IO()
go = do
     board <- getInput "input.txt"
     print $ length $ filter id $ concat (visibilityBoard board)


