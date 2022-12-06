data WinLoss = Win | Loss | Draw deriving (Eq, Show)
data Hand =  Rock | Paper | Scissor deriving (Eq, Show)

handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissor = 3

-- 0 = loss
-- 1 = draw
-- 2 = win
winner :: Hand -> Hand -> WinLoss
winner a     b       | a == b = Draw
winner Paper Rock    = Win
winner Rock  Scissor = Win
winner Scissor Paper = Win
winner a       b     = Loss

winnerMultiplier :: Hand -> Hand -> Int
winnerMultiplier a b | wl == Win  = 2
                     | wl == Draw = 1
                     | wl == Loss = 0
                     where wl = winner a b

scoreRound :: (Hand, WinLoss) -> (Int, Int)
scoreRound (p1,p2) = (handScore p1 + 3 * winnerMultiplier p1 h2, handScore h2 + 3 * winnerMultiplier h2 p1)
                   where 
                         h2 =  head $ filter (\x -> (winner x p1) == p2) [Rock, Paper, Scissor]


score :: [(Hand, WinLoss)] -> Int
score x = foldl (+) 0 $ map snd $ map scoreRound x



inp = [(Scissor,Win), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Paper,Draw), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Draw), (Rock,Win), (Rock,Win), (Paper,Loss), (Rock,Draw), (Rock,Loss), (Paper,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Paper,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Paper,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Rock,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Draw), (Scissor,Draw), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Rock,Draw), (Scissor,Loss), (Rock,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Draw), (Scissor,Win), (Rock,Win), (Scissor,Win), (Rock,Win), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Rock,Draw), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Paper,Loss), (Paper,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Rock,Draw), (Paper,Loss), (Paper,Win), (Rock,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Draw), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Draw), (Scissor,Loss), (Scissor,Draw), (Paper,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Draw), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Rock,Win), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Paper,Win), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Paper,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Draw), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Draw), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Scissor,Win), (Rock,Draw), (Rock,Draw), (Scissor,Loss), (Paper,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Rock,Win), (Paper,Win), (Rock,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Paper,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Draw), (Rock,Loss), (Rock,Win), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Paper,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Paper,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Paper,Loss), (Rock,Draw), (Rock,Draw), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Scissor,Draw), (Paper,Draw), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Draw), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Paper,Win), (Scissor,Draw), (Paper,Loss), (Rock,Draw), (Paper,Loss), (Paper,Draw), (Rock,Draw), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Win), (Rock,Win), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Paper,Loss), (Paper,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Paper,Draw), (Rock,Draw), (Paper,Loss), (Paper,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Draw), (Paper,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Paper,Loss), (Scissor,Win), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Rock,Win), (Paper,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Paper,Win), (Paper,Loss), (Rock,Loss), (Scissor,Draw), (Paper,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Paper,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Win), (Rock,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Win), (Paper,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Paper,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Rock,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Win), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Paper,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Paper,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Draw), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Paper,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Paper,Loss), (Rock,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Rock,Win), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Paper,Win), (Scissor,Win), (Scissor,Win), (Rock,Draw), (Rock,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Draw), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Draw), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Draw), (Rock,Win), (Rock,Loss), (Scissor,Win), (Rock,Win), (Paper,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Win), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Rock,Win), (Rock,Loss), (Paper,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Scissor,Loss), (Paper,Win), (Rock,Loss), (Rock,Win), (Paper,Win), (Paper,Win), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Rock,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Paper,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Paper,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Draw), (Paper,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Paper,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Scissor,Win), (Rock,Draw), (Rock,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Draw), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Paper,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Paper,Draw), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Scissor,Draw), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Paper,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Paper,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Paper,Loss), (Paper,Win), (Scissor,Draw), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Paper,Draw), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Paper,Loss), (Rock,Win), (Rock,Loss), (Paper,Win), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Paper,Loss), (Paper,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Paper,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Paper,Draw), (Scissor,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Paper,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Paper,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Paper,Win), (Rock,Loss), (Paper,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Paper,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Draw), (Paper,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Scissor,Win), (Rock,Win), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Draw), (Scissor,Win), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Paper,Loss), (Rock,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Paper,Win), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Paper,Loss), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Paper,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Paper,Loss), (Scissor,Win), (Rock,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Paper,Win), (Rock,Loss), (Rock,Win), (Paper,Loss), (Rock,Win), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Draw), (Scissor,Win), (Rock,Win), (Scissor,Draw), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Paper,Draw), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Draw), (Rock,Draw), (Rock,Loss), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Paper,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Rock,Win), (Scissor,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Paper,Loss), (Rock,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Draw), (Scissor,Draw), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Rock,Win), (Paper,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Paper,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Rock,Win), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Draw), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Draw), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Paper,Win), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Paper,Loss), (Paper,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Paper,Loss), (Rock,Win), (Scissor,Win), (Paper,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Rock,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Win), (Rock,Draw), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Win), (Rock,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Paper,Win), (Scissor,Win), (Scissor,Win), (Rock,Draw), (Scissor,Loss), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Win), (Scissor,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Paper,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Paper,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Paper,Win), (Paper,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Draw), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Win), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Paper,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Paper,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Draw), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Draw), (Rock,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Paper,Draw), (Rock,Win), (Rock,Win), (Scissor,Win), (Scissor,Loss), (Rock,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Paper,Win), (Scissor,Win), (Paper,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Paper,Loss), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Draw), (Scissor,Win), (Rock,Win), (Paper,Win), (Rock,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Draw), (Rock,Win), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Loss), (Rock,Loss), (Paper,Draw), (Rock,Loss), (Scissor,Loss), (Paper,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Rock,Draw), (Rock,Draw), (Rock,Loss), (Rock,Win), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Rock,Loss), (Paper,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Win), (Scissor,Win), (Scissor,Win), (Paper,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Draw), (Rock,Loss), (Paper,Loss), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Paper,Draw), (Scissor,Draw), (Scissor,Draw), (Scissor,Draw), (Scissor,Loss), (Rock,Draw), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Paper,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Win), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Draw), (Rock,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Scissor,Win), (Rock,Win), (Scissor,Win), (Scissor,Draw), (Scissor,Win), (Rock,Loss), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Draw), (Paper,Loss), (Rock,Loss), (Rock,Win), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Rock,Win), (Rock,Draw), (Rock,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Draw), (Rock,Loss), (Rock,Loss), (Paper,Win), (Rock,Loss), (Scissor,Loss), (Scissor,Loss), (Scissor,Loss), (Rock,Loss), (Scissor,Loss), (Scissor,Win), (Scissor,Win), (Rock,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Loss), (Rock,Draw), (Scissor,Win), (Scissor,Loss), (Scissor,Loss), (Scissor,Draw), (Scissor,Win), (Rock,Draw), (Rock,Loss), (Scissor,Win), (Scissor,Win), (Scissor,Win), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Rock,Win), (Rock,Loss), (Scissor,Draw), (Scissor,Loss), (Scissor,Win), (Rock,Loss), (Rock,Loss), (Scissor,Win), (Rock,Loss), (Scissor,Loss), (Rock,Loss), (Rock,Draw), (Scissor,Loss)]