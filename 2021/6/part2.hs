import qualified Data.Map.Strict as Map

type Fish = Map.Map Int Int

maxLife = 6
newFish = 8

emptyFish :: Fish
emptyFish = foldl (\f i -> Map.insert i 0 f) Map.empty [0..newFish]

day :: [(Int, Int)] -> Fish -> Fish
day [] m          = m
day ((l, c):xs) m | l == 0    = day xs $ add c newFish $ add c maxLife m
                  | otherwise = day xs $ add c (l-1) m
                  where
                    e = Map.lookup l m
                    add x = Map.adjust (+ x)

process :: Fish -> Int -> Fish
process f c = foldl (proc) f [1..c]
            where proc fish _ = day (Map.foldlWithKey (\l k v -> (k,v):l) [] fish) emptyFish

test = Map.fromList [(4, 1), (3, 2), (1, 1), (2, 1)]

startFish = [1,1,1,3,3,2,1,1,1,1,1,4,4,1,4,1,4,1,1,4,1,1,1,3,3,2,3,1,2,1,1,1,1,1,1,1,3,4,1,1,4,3,1,2,3,1,1,1,5,2,1,1,1,1,2,1,2,5,2,2,1,1,1,3,1,1,1,4,1,1,1,1,1,3,3,2,1,1,3,1,4,1,2,1,5,1,4,2,1,1,5,1,1,1,1,4,3,1,3,2,1,4,1,1,2,1,4,4,5,1,3,1,1,1,1,2,1,4,4,1,1,1,3,1,5,1,1,1,1,1,3,2,5,1,5,4,1,4,1,3,5,1,2,5,4,3,3,2,4,1,5,1,1,2,4,1,1,1,1,2,4,1,2,5,1,4,1,4,2,5,4,1,1,2,2,4,1,5,1,4,3,3,2,3,1,2,3,1,4,1,1,1,3,5,1,1,1,3,5,1,1,4,1,4,4,1,3,1,1,1,2,3,3,2,5,1,2,1,1,2,2,1,3,4,1,3,5,1,3,4,3,5,1,1,5,1,3,3,2,1,5,1,1,3,1,1,3,1,2,1,3,2,5,1,3,1,1,3,5,1,1,1,1,2,1,2,4,4,4,2,2,3,1,5,1,2,1,3,3,3,4,1,1,5,1,3,2,4,1,5,5,1,4,4,1,4,4,1,1,2]

mapStartFish :: [Int] -> Fish -> Fish
mapStartFish [] m     = m
mapStartFish (x:xs) m = mapStartFish xs $ Map.adjust (+ 1) x m

main = print $ Map.foldl (+) 0 $ process (mapStartFish startFish emptyFish) 256
--main = print $ process test 256
--main = print $ day (Map.foldlWithKey (\a k v -> (k,v):a) [] test) emptyFish
