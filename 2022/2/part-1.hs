data Input = A | B | C | X | Y | Z deriving Eq
data Hand =  Rock | Paper | Scissor deriving Eq

handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissor = 3

inputToHand :: Input -> Hand
inputToHand A = Rock
inputToHand X = Rock
inputToHand B = Paper
inputToHand Y = Paper
inputToHand C = Scissor
inputToHand Z = Scissor

-- 0 = loss
-- 1 = draw
-- 2 = win
winnerMultiplier :: Hand -> Hand -> Int
winnerMultiplier a     b       | a == b = 1
winnerMultiplier Paper Rock    = 2
winnerMultiplier Rock  Scissor = 2
winnerMultiplier Scissor Paper = 2
winnerMultiplier a       b     = 0

scoreRound :: (Input, Input) -> (Int, Int)
scoreRound (p1,p2) = (handScore h1 + 3 * winnerMultiplier h1 h2, handScore h2 + 3 * winnerMultiplier h2 h1)
                   where 
			 h1 = inputToHand p1
                         h2 = inputToHand p2


score :: [(Input, Input)] -> Int
score x = foldl (+) 0 $ map snd $ map scoreRound x



inp = [(C,Z), (C,Z), (A,Y), (A,X), (C,Z), (C,Z), (A,X), (C,X), (A,X), (C,X), (A,X), (A,X), (A,X), (A,X), (C,Y), (A,X), (A,X), (A,Y), (C,Z), (C,X), (C,Z), (C,Y), (B,Y), (C,Y), (C,X), (C,X), (A,Z), (C,Y), (A,Z), (A,Z), (B,X), (A,Y), (A,X), (B,Y), (C,X), (A,X), (A,X), (C,Z), (C,Y), (B,X), (A,Y), (C,Z), (C,Y), (C,Z), (C,Z), (A,X), (C,Z), (C,Y), (C,X), (C,Z), (A,X), (C,Z), (A,Y), (A,X), (A,X), (A,X), (A,X), (C,X), (A,X), (A,X), (C,X), (A,X), (C,Y), (C,Y), (A,X), (C,Z), (C,Y), (A,X), (C,Z), (C,Y), (C,Z), (C,Z), (B,X), (C,Z), (C,X), (C,X), (B,Z), (A,X), (A,X), (C,Z), (C,X), (C,X), (A,X), (A,Y), (C,X), (C,Y), (A,X), (C,Z), (A,Z), (C,Z), (A,Z), (A,Z), (C,X), (A,X), (A,X), (C,Z), (C,Y), (C,Z), (A,X), (A,X), (C,X), (A,X), (C,X), (A,X), (A,Z), (A,Y), (C,Y), (A,Y), (C,Z), (A,X), (A,Y), (C,X), (A,Z), (C,Y), (C,Z), (C,Z), (A,Y), (C,Z), (A,Z), (C,Z), (A,Z), (A,Z), (A,X), (C,X), (A,Y), (A,X), (C,Z), (A,X), (C,Z), (C,Z), (A,X), (C,Z), (C,X), (A,Z), (A,X), (A,Z), (A,X), (C,Z), (A,X), (C,X), (A,X), (A,X), (C,X), (C,Z), (C,Y), (C,Z), (A,Y), (C,Z), (C,Z), (A,X), (C,X), (B,X), (C,X), (A,X), (C,Y), (C,Z), (C,Z), (C,Z), (A,Z), (C,Y), (A,X), (C,X), (B,X), (B,X), (C,Y), (C,X), (A,X), (C,X), (C,X), (A,Y), (C,Z), (A,X), (A,X), (A,X), (C,Z), (A,X), (C,Z), (C,Y), (C,Z), (C,X), (A,X), (C,Z), (A,X), (C,Z), (C,X), (C,X), (A,X), (C,Z), (C,Z), (C,X), (A,Y), (B,X), (B,Z), (A,Z), (C,Z), (A,X), (A,X), (A,X), (C,X), (C,X), (C,Y), (C,Z), (A,Y), (B,Z), (A,X), (C,X), (C,X), (C,X), (C,Y), (C,X), (A,X), (A,X), (A,Z), (A,X), (A,X), (C,Z), (C,Z), (C,Z), (A,X), (C,X), (C,X), (C,Y), (A,X), (C,Z), (A,Z), (C,X), (A,X), (C,X), (C,Z), (A,X), (A,X), (C,Z), (C,X), (C,Y), (A,X), (C,Y), (C,Z), (A,Z), (C,X), (A,X), (A,X), (A,X), (A,Z), (C,X), (C,X), (A,X), (A,X), (B,X), (C,Y), (C,X), (C,Z), (C,Y), (A,X), (C,X), (C,Z), (C,Y), (C,Y), (C,X), (C,Y), (B,X), (C,X), (C,Y), (A,X), (C,Z), (C,Y), (A,X), (A,X), (C,X), (A,X), (A,X), (A,Z), (C,X), (A,Z), (C,X), (C,X), (C,X), (C,Y), (A,Z), (C,Z), (A,X), (B,X), (C,X), (A,X), (A,Z), (C,X), (C,Z), (B,X), (C,X), (A,X), (A,Y), (A,Z), (C,Z), (C,Z), (C,X), (A,Z), (C,Z), (C,Z), (C,X), (C,Y), (C,Z), (C,Z), (C,X), (A,Z), (C,Z), (C,X), (A,X), (A,X), (A,X), (A,Z), (A,Z), (C,Z), (A,X), (C,X), (A,X), (C,X), (C,Z), (C,X), (C,X), (A,X), (C,X), (B,Y), (A,X), (B,X), (A,X), (C,X), (C,Z), (A,X), (C,X), (A,X), (A,X), (C,Z), (C,X), (C,X), (C,X), (C,Z), (A,X), (B,X), (A,Z), (C,X), (B,Z), (C,Z), (B,Z), (C,Z), (A,Y), (A,X), (B,Y), (A,X), (A,X), (C,Z), (C,X), (A,X), (B,X), (C,X), (B,Z), (C,Z), (A,X), (A,X), (A,X), (A,X), (A,X), (A,X), (A,X), (C,Y), (A,Y), (C,X), (C,Z), (C,X), (C,Y), (C,Z), (C,X), (A,X), (C,Z), (A,X), (A,Z), (C,X), (C,Z), (C,Y), (A,X), (C,X), (C,Z), (A,X), (A,Y), (C,X), (A,X), (C,Y), (A,Y), (C,Y), (A,Z), (C,Z), (A,X), (C,Z), (C,Z), (A,X), (C,Z), (A,Z), (C,X), (C,Z), (B,X), (C,X), (A,X), (A,Y), (A,X), (C,Z), (A,Y), (A,Y), (C,X), (B,Z), (C,X), (A,X), (A,X), (A,X), (A,X), (A,X), (C,X), (C,Z), (A,Z), (A,X), (C,Y), (C,Z), (A,X), (A,Z), (B,Z), (A,Z), (A,X), (C,Y), (C,X), (C,X), (C,X), (B,Z), (A,X), (C,Y), (A,X), (A,X), (A,X), (C,Y), (A,X), (B,X), (C,X), (C,X), (C,Z), (C,Z), (A,X), (C,Y), (A,Z), (A,X), (C,X), (A,X), (C,X), (C,X), (C,Z), (A,X), (C,Y), (C,Y), (A,X), (A,Z), (A,Z), (A,X), (C,X), (A,X), (C,Y), (B,X), (A,X), (C,Y), (C,X), (B,X), (C,Z), (A,X), (A,X), (C,X), (C,Z), (C,Z), (C,X), (C,Y), (C,Y), (A,X), (C,Z), (C,X), (C,Y), (C,X), (A,X), (A,X), (A,X), (B,Y), (C,X), (A,X), (C,X), (C,X), (A,X), (C,Z), (C,Z), (C,X), (B,X), (A,Y), (A,Y), (C,Z), (C,Y), (C,X), (C,Y), (B,Y), (A,Z), (C,X), (A,X), (A,Z), (A,Y), (C,X), (A,Z), (C,Z), (C,Y), (C,Z), (A,X), (C,X), (C,Z), (C,X), (C,X), (A,X), (C,Z), (A,Z), (C,X), (C,Y), (C,X), (A,X), (A,Z), (C,Z), (C,X), (A,Z), (B,Z), (C,Y), (B,X), (A,Y), (B,X), (B,Y), (A,Y), (A,X), (A,Z), (C,X), (C,X), (A,X), (C,X), (C,Z), (C,Y), (A,Z), (A,X), (C,Z), (A,X), (A,X), (A,Z), (A,X), (A,X), (C,Z), (A,X), (C,X), (A,X), (A,X), (A,Y), (C,Z), (A,X), (C,Z), (A,Z), (A,X), (A,X), (A,Y), (C,Z), (A,Z), (A,Z), (A,X), (C,X), (A,X), (A,X), (C,Z), (C,X), (C,X), (C,X), (C,Y), (C,X), (A,X), (B,X), (B,X), (C,Z), (C,Z), (C,Z), (C,X), (B,Y), (A,Y), (B,X), (B,X), (C,X), (C,X), (C,X), (A,X), (C,Y), (A,Z), (A,X), (C,X), (A,X), (A,X), (A,X), (B,X), (C,Y), (A,X), (C,Z), (A,X), (C,X), (A,X), (C,Z), (C,X), (A,X), (A,Z), (A,X), (A,X), (A,X), (C,Z), (A,Y), (B,X), (A,X), (B,X), (C,Z), (A,X), (A,X), (C,Y), (C,Z), (C,Z), (A,X), (C,X), (A,X), (A,X), (A,X), (A,X), (A,X), (C,X), (A,X), (C,Y), (C,X), (B,X), (A,X), (C,Y), (C,Z), (A,X), (A,X), (C,X), (A,Z), (A,X), (A,Z), (A,X), (C,Z), (A,X), (B,X), (C,X), (A,X), (C,X), (A,X), (C,Y), (C,Z), (A,X), (C,Z), (C,Z), (C,X), (B,X), (C,Z), (A,X), (A,Z), (C,X), (A,X), (C,X), (C,Z), (C,Z), (C,Y), (C,Z), (A,X), (C,X), (C,Z), (A,Z), (C,Z), (A,Z), (C,X), (C,Z), (A,X), (A,X), (B,X), (A,Z), (B,X), (C,Y), (C,Z), (C,X), (A,X), (C,Z), (C,Z), (C,Z), (B,X), (C,X), (C,X), (A,Z), (A,X), (C,Y), (A,X), (B,Z), (B,X), (A,X), (C,Y), (B,X), (C,Z), (C,Y), (C,Z), (A,X), (C,Y), (A,X), (A,X), (A,Z), (A,X), (A,X), (C,X), (C,Y), (C,Z), (C,Z), (C,Y), (B,Y), (C,Z), (A,X), (A,X), (C,X), (C,X), (C,X), (A,X), (C,Y), (A,X), (A,Y), (A,X), (C,X), (C,Y), (C,Y), (B,X), (A,X), (C,Z), (A,X), (C,Z), (C,X), (A,X), (A,Z), (C,X), (C,Y), (A,X), (A,Z), (A,Z), (C,Y), (A,X), (C,Z), (C,Z), (A,X), (A,Z), (B,X), (A,X), (C,Y), (C,X), (B,Z), (C,Z), (A,X), (C,Y), (A,X), (C,Y), (A,Z), (A,X), (C,X), (C,Y), (B,X), (A,X), (B,X), (A,X), (A,X), (B,Y), (A,X), (A,X), (C,X), (A,X), (C,X), (A,X), (A,X), (C,Y), (A,X), (C,Z), (C,Z), (A,Z), (C,X), (A,Z), (C,Y), (C,Z), (C,X), (A,X), (C,Z), (A,X), (C,Y), (A,X), (A,Z), (C,Y), (C,X), (C,X), (C,Z), (B,X), (A,X), (A,X), (A,Z), (C,Z), (C,X), (A,X), (C,X), (A,Z), (A,Z), (A,X), (C,X), (C,Z), (B,Z), (C,X), (C,X), (A,X), (A,X), (A,X), (C,Y), (C,X), (A,X), (C,Z), (A,X), (B,Y), (A,X), (A,X), (C,X), (B,X), (A,X), (C,X), (B,Y), (A,Z), (C,X), (C,X), (A,X), (A,X), (A,X), (C,Y), (C,X), (A,Z), (C,X), (A,X), (C,Z), (C,Z), (C,Y), (A,X), (A,X), (B,Z), (C,Y), (C,Z), (A,X), (C,Z), (A,X), (C,Z), (B,X), (A,Z), (A,Z), (C,X), (A,X), (C,Z), (C,X), (A,X), (A,X), (C,X), (C,Z), (C,X), (A,X), (C,X), (A,X), (A,X), (C,Z), (C,X), (A,X), (A,X), (A,X), (A,X), (C,Y), (A,X), (C,X), (C,Z), (C,Y), (C,Z), (C,X), (C,Z), (A,X), (C,X), (C,Z), (C,Z), (C,Z), (B,X), (C,X), (A,Z), (A,Z), (A,X), (C,X), (C,Z), (B,Z), (C,Z), (C,Z), (A,Y), (A,Y), (C,X), (A,X), (A,X), (A,Z), (A,X), (A,Y), (A,Z), (A,X), (A,X), (C,Z), (C,Z), (A,Y), (A,X), (B,X), (A,X), (A,X), (C,X), (A,X), (A,Z), (C,Z), (C,X), (C,Z), (C,X), (A,Z), (B,X), (A,X), (A,X), (C,X), (B,Y), (A,X), (C,Z), (A,X), (A,Y), (A,Z), (A,X), (C,Z), (A,Z), (B,X), (C,Z), (C,Y), (A,X), (C,Y), (A,X), (C,X), (C,X), (C,X), (A,X), (C,X), (C,Z), (C,Y), (A,Z), (C,X), (C,Y), (A,X), (A,X), (C,Z), (A,X), (C,X), (C,X), (A,X), (A,X), (A,Z), (C,X), (C,Z), (A,Z), (A,Z), (A,X), (B,X), (B,X), (A,X), (C,Z), (A,Z), (C,Z), (C,X), (C,Z), (B,X), (C,X), (C,X), (B,Z), (A,X), (A,Z), (B,Z), (B,Z), (C,X), (A,Z), (A,X), (A,Y), (C,X), (A,X), (C,Z), (A,X), (C,X), (A,X), (C,Z), (C,X), (C,X), (C,X), (C,Z), (B,Z), (A,X), (A,X), (A,X), (C,Z), (A,X), (A,X), (C,X), (B,Z), (C,X), (C,X), (A,X), (B,Y), (C,X), (C,X), (C,X), (A,X), (C,X), (A,Z), (C,Y), (B,Z), (A,X), (C,Z), (C,Z), (C,Z), (C,Z), (A,Y), (C,Z), (A,X), (A,X), (C,X), (C,X), (A,X), (C,X), (A,Z), (A,X), (C,Z), (C,X), (A,X), (C,Z), (C,X), (C,Z), (B,Z), (A,X), (C,Y), (C,Z), (A,X), (A,X), (C,Y), (A,Z), (C,X), (C,Z), (C,Z), (C,Y), (C,Z), (A,X), (C,X), (A,X), (A,X), (C,X), (A,X), (C,Z), (A,X), (A,Z), (A,X), (C,Z), (C,Z), (B,X), (C,Z), (A,Y), (A,Z), (A,X), (C,Y), (C,X), (C,Y), (A,X), (A,Z), (A,X), (A,X), (A,X), (C,X), (B,X), (C,X), (A,Y), (A,Y), (C,Z), (A,X), (A,X), (A,Z), (C,X), (A,X), (A,Z), (C,Z), (C,Z), (C,X), (C,X), (A,X), (A,X), (C,Z), (A,X), (C,Z), (C,Z), (B,Z), (A,X), (A,X), (C,Z), (A,X), (C,Z), (C,Y), (C,X), (A,X), (A,Z), (A,X), (A,X), (A,X), (C,Y), (C,Z), (A,X), (C,Y), (C,Z), (C,Z), (A,X), (A,Z), (A,X), (C,Z), (C,Y), (C,Z), (C,Y), (A,X), (C,X), (A,X), (A,X), (A,X), (C,X), (A,X), (A,X), (C,Y), (C,X), (A,X), (C,X), (A,Z), (A,X), (C,Z), (C,X), (B,Z), (C,Z), (A,X), (C,Y), (C,Z), (C,X), (A,X), (A,X), (B,Y), (A,X), (B,X), (C,X), (A,X), (A,X), (C,X), (C,Y), (A,X), (A,X), (A,X), (C,X), (C,Z), (A,X), (B,X), (C,Y), (C,Y), (A,X), (C,Y), (C,X), (A,X), (C,X), (A,X), (A,X), (A,X), (C,X), (C,X), (B,Z), (A,X), (C,X), (A,X), (C,Z), (A,X), (C,Y), (A,X), (C,Z), (C,Z), (C,Y), (A,X), (A,Z), (A,X), (C,X), (C,Z), (A,X), (C,Z), (C,X), (C,Z), (A,X), (C,Y), (A,X), (C,Z), (C,Y), (C,X), (A,X), (A,Y), (B,X), (A,X), (A,Y), (A,X), (A,X), (A,X), (A,X), (C,Z), (C,Y), (B,X), (A,X), (C,X), (A,X), (C,X), (A,X), (A,Y), (A,X), (C,Z), (A,Z), (C,X), (A,X), (A,X), (A,X), (C,Z), (C,Z), (C,Y), (C,X), (A,X), (C,X), (C,Y), (A,Z), (C,X), (A,X), (C,Y), (C,Z), (C,Z), (A,X), (A,X), (C,Z), (C,Z), (C,X), (C,X), (C,X), (C,Z), (C,Y), (C,X), (B,Z), (C,Z), (A,X), (C,Z), (A,X), (A,X), (C,X), (C,X), (C,Y), (A,Y), (A,X), (C,X), (A,X), (C,X), (C,X), (A,X), (A,X), (A,Y), (C,Z), (C,Y), (A,X), (A,X), (A,X), (A,X), (C,X), (C,Y), (A,X), (A,Y), (A,X), (A,X), (C,X), (A,Z), (C,Z), (C,Z), (C,Z), (C,Z), (A,X), (C,Z), (C,X), (C,X), (B,X), (C,X), (C,Z), (C,X), (A,Z), (C,Z), (B,X), (B,Z), (C,Y), (A,X), (B,X), (A,X), (A,X), (A,X), (C,X), (C,X), (A,X), (C,Y), (A,X), (C,Y), (B,Y), (C,Y), (A,X), (C,X), (C,Y), (C,Z), (C,X), (A,X), (A,X), (C,X), (B,X), (C,Z), (C,X), (A,X), (A,Z), (A,X), (C,Z), (C,X), (B,X), (A,Z), (A,X), (B,Z), (C,Z), (A,X), (C,Y), (A,X), (C,Z), (B,X), (C,X), (C,X), (C,Y), (A,X), (A,X), (A,X), (C,Z), (C,X), (B,Z), (C,Z), (B,X), (B,Z), (A,X), (A,Z), (A,X), (C,X), (C,X), (C,Z), (C,X), (C,X), (B,Y), (A,X), (A,X), (A,X), (A,X), (A,X), (A,X), (C,X), (A,Z), (A,Z), (C,X), (C,X), (A,X), (B,Y), (C,X), (C,X), (A,Z), (A,X), (B,X), (A,X), (C,X), (C,Z), (C,Y), (C,Z), (C,Z), (B,X), (C,Y), (C,X), (A,X), (A,X), (C,X), (C,Y), (C,Z), (A,X), (C,X), (A,X), (B,Z), (A,X), (C,Y), (C,X), (C,X), (A,X), (C,Z), (C,Z), (A,X), (C,X), (C,X), (C,X), (A,X), (A,Z), (C,X), (A,X), (C,Z), (A,X), (C,Z), (C,Z), (A,X), (C,Z), (A,Z), (C,X), (A,X), (A,X), (C,Z), (A,X), (A,X), (A,X), (C,X), (A,X), (A,X), (C,Z), (C,X), (C,Y), (B,X), (C,X), (C,Y), (C,X), (A,X), (A,Y), (A,X), (A,X), (A,Z), (C,Z), (A,X), (C,X), (C,X), (C,X), (C,X), (B,Z), (A,X), (B,Y), (C,X), (C,X), (C,Y), (C,X), (C,Z), (C,Z), (A,X), (C,X), (A,X), (C,X), (A,Z), (C,Y), (B,X), (A,X), (C,X), (C,Z), (C,Y), (B,X), (C,Y), (A,X), (C,X), (A,X), (A,X), (A,Z), (A,X), (B,X), (C,X), (A,X), (A,Y), (A,X), (C,X), (C,Z), (A,Z), (A,X), (A,X), (C,Z), (A,X), (C,X), (A,X), (C,X), (A,X), (C,Z), (C,Y), (A,X), (C,X), (A,X), (C,Z), (A,Y), (B,X), (C,Z), (C,Z), (A,X), (A,X), (A,Y), (A,Z), (A,X), (C,X), (C,Y), (C,Z), (A,X), (C,Z), (C,Z), (B,X), (A,X), (A,X), (A,X), (C,Y), (A,Z), (C,Z), (C,Z), (A,Z), (A,X), (A,X), (B,X), (C,X), (B,X), (A,X), (C,Z), (A,Y), (C,Z), (A,X), (C,X), (C,Z), (C,X), (A,X), (C,X), (A,X), (A,X), (C,Z), (A,Z), (C,X), (C,X), (B,Z), (C,Z), (A,Y), (A,X), (C,X), (C,Z), (A,X), (C,Z), (C,X), (C,Y), (C,X), (A,Y), (C,Z), (C,Z), (A,X), (A,X), (A,X), (C,Z), (C,Y), (C,Z), (A,X), (C,Z), (A,X), (C,X), (A,X), (A,Y), (A,X), (A,Z), (A,X), (A,Y), (A,X), (A,X), (A,X), (A,Z), (A,X), (C,Z), (C,Z), (C,Z), (C,X), (A,X), (A,Z), (C,X), (C,Z), (A,X), (C,X), (C,Y), (A,X), (C,X), (A,X), (C,Y), (A,X), (A,X), (C,Z), (C,X), (A,X), (C,Z), (A,X), (A,X), (C,Z), (C,X), (C,Z), (C,Y), (A,Y), (C,Z), (C,X), (B,X), (A,X), (A,X), (A,Z), (A,X), (A,X), (C,X), (C,Z), (A,X), (C,X), (A,Z), (B,X), (A,Z), (C,Z), (C,Z), (A,X), (B,X), (A,X), (A,Y), (A,X), (A,X), (C,Z), (A,X), (C,Y), (C,Z), (C,Y), (A,X), (A,X), (A,X), (A,X), (B,Z), (B,Z), (A,X), (C,X), (C,X), (A,X), (C,X), (C,Y), (B,X), (C,Y), (B,X), (A,X), (B,Z), (C,Z), (A,X), (A,X), (C,X), (A,Z), (A,X), (C,X), (C,Z), (A,X), (C,X), (C,X), (C,X), (C,Z), (C,Y), (A,X), (A,X), (A,X), (C,Z), (C,Z), (C,X), (C,Z), (B,X), (A,X), (C,Z), (C,Z), (C,Y), (A,X), (A,X), (C,Z), (A,Z), (C,X), (C,Z), (C,Y), (C,Z), (C,Z), (A,X), (C,Z), (C,Y), (C,X), (B,X), (C,Z), (A,Y), (C,Z), (C,X), (C,X), (A,X), (A,X), (A,X), (C,Z), (B,Z), (A,X), (A,Z), (B,X), (A,Z), (A,Z), (C,Z), (C,Z), (C,X), (C,Y), (C,Y), (C,Z), (A,Z), (C,Y), (C,X), (C,Y), (A,Z), (B,Y), (A,X), (C,Y), (A,X), (A,X), (A,Z), (C,Z), (C,Z), (C,Z), (C,X), (C,X), (C,Z), (C,Z), (A,X), (C,X), (A,Y), (A,Y), (A,X), (B,Z), (A,X), (C,X), (A,X), (C,X), (A,X), (A,X), (A,X), (C,X), (C,Y), (C,Z), (C,Z), (A,X), (C,Z), (A,X), (C,X), (C,X), (C,Z), (A,X), (A,X), (A,X), (C,X), (C,Z), (A,X), (A,X), (A,X), (A,X), (C,Z), (B,Y), (A,X), (A,X), (A,X), (C,Y), (A,X), (A,X), (A,Z), (C,X), (A,Z), (C,Z), (A,Z), (C,Z), (A,X), (A,Z), (A,X), (A,X), (A,X), (A,X), (A,Y), (C,X), (C,Z), (A,X), (A,Z), (C,X), (A,Y), (C,Z), (C,Y), (A,X), (B,X), (A,Y), (C,X), (A,X), (C,Z), (C,X), (A,X), (C,Z), (C,X), (A,X), (C,X), (C,X), (C,Z), (A,X), (A,Y), (C,Y), (C,Y), (A,X), (A,X), (A,X), (A,X), (A,X), (C,X), (A,Y), (C,Z), (C,Z), (C,Y), (A,X), (C,Z), (C,Z), (A,X), (C,X), (C,X), (C,Y), (A,X), (A,X), (C,X), (C,X), (A,Z), (A,X), (C,Z), (A,X), (C,Z), (A,X), (B,X), (A,Z), (B,X), (C,Z), (C,Z), (C,X), (A,X), (A,Z), (C,X), (A,X), (A,X), (C,X), (C,Y), (C,Z), (C,X), (C,X), (A,X), (C,Z), (C,Z), (C,X), (B,X), (C,Z), (A,X), (B,X), (A,Z), (C,X), (A,Z), (A,X), (A,X), (C,X), (A,X), (C,Z), (A,X), (A,X), (C,Z), (C,Z), (C,Z), (A,X), (C,X), (A,Y), (C,Y), (A,X), (A,X), (A,X), (A,X), (C,X), (C,Z), (A,X), (A,X), (A,X), (C,X), (C,Z), (A,X), (C,Z), (A,X), (A,Y), (C,Y), (C,X), (C,X), (A,X), (A,X), (A,X), (A,Z), (A,X), (C,Z), (A,X), (C,Y), (B,Z), (B,X), (A,X), (C,X), (C,Z), (A,X), (A,X), (C,Z), (B,X), (B,X), (C,X), (C,Y), (C,X), (B,X), (A,X), (A,X), (C,X), (A,Z), (A,X), (C,X), (C,X), (C,X), (A,X), (C,Z), (A,Y), (A,X), (C,Y), (A,X), (A,X), (C,Z), (A,Z), (A,Z), (C,X), (C,X), (C,Y), (C,Y), (C,X), (C,X), (C,Z), (A,X), (C,X), (A,X), (A,X), (A,X), (A,X), (C,Y), (C,Z), (A,X), (C,Z), (C,X), (C,X), (A,X), (A,X), (A,Z), (C,Y), (C,X), (A,X), (C,X), (A,X), (A,Z), (A,X), (A,Z), (A,X), (A,X), (A,X), (C,X), (A,Z), (A,X), (C,X), (C,Y), (A,Z), (A,X), (B,X), (C,X), (C,Y), (C,Z), (C,X), (C,Y), (C,X), (C,X), (A,X), (C,Y), (B,X), (A,Z), (C,Z), (B,Z), (C,X), (A,X), (C,Z), (A,X), (C,X), (A,X), (A,X), (A,X), (C,Z), (C,Y), (B,Z), (A,X), (C,X), (A,Y), (C,Z), (A,X), (A,X), (A,Z), (A,Z), (A,Y), (C,X), (C,Z), (C,X), (A,Z), (C,X), (C,Z), (C,X), (B,X), (A,X), (C,Z), (A,X), (A,X), (C,Z), (A,Z), (C,X), (C,Y), (C,X), (C,X), (C,Y), (A,X), (A,X), (C,X), (A,Z), (C,X), (C,Z), (C,X), (A,Z), (A,X), (A,Y), (A,X), (A,X), (B,Z), (C,Z), (C,Z), (A,Y), (C,X), (A,X), (B,X), (A,X), (A,Z), (C,Y), (C,Z), (C,X), (C,Z), (C,Z), (C,X), (C,X), (A,X), (C,X), (A,Z), (C,Z), (C,Z), (C,Z), (C,Y), (C,X), (A,X), (C,Z), (A,Z), (C,Z), (C,Z), (A,X), (C,X), (A,X), (C,Z), (A,X), (B,X), (A,X), (C,Z), (C,X), (C,X), (A,X), (C,Y), (A,X), (C,Z), (B,X), (C,Z), (C,X), (A,X), (C,X), (C,X), (C,Z), (A,X), (A,X), (C,Z), (C,Z), (C,X), (C,Z), (C,X), (A,X), (A,X), (C,Z), (C,X), (C,X), (A,X), (C,Z), (A,Z), (C,Z), (B,X), (C,X), (C,Y), (C,X), (C,Z), (A,X), (B,X), (C,Z), (A,X), (C,Z), (A,Z), (C,Z), (C,X), (A,X), (C,Z), (C,Z), (A,X), (A,X), (C,Z), (A,X), (C,Y), (A,Z), (C,X), (C,Z), (A,X), (B,Z), (B,X), (A,X), (A,Z), (A,X), (A,Z), (A,X), (A,X), (C,Y), (A,X), (C,Y), (C,X), (C,Y), (C,Y), (C,X), (A,X), (A,Z), (A,Z), (C,X), (C,Z), (B,X), (A,X), (C,Z), (A,Z), (C,X), (C,Z), (C,Y), (A,X), (A,X), (C,Z), (C,Z), (C,Y), (A,Z), (C,Z), (C,Y), (A,X), (C,X), (A,X), (C,X), (C,Z), (A,Z), (B,Z), (A,X), (A,X), (A,X), (C,X), (C,X), (A,Y), (A,Z), (C,X), (C,Z), (A,X), (B,X), (A,X), (A,X), (C,X), (C,X), (B,X), (A,X), (B,X), (C,Z), (A,Z), (C,Z), (A,X), (C,X), (C,Z), (C,Y), (A,Y), (A,Z), (C,Y), (C,Z), (A,X), (A,X), (C,X), (A,X), (C,Z), (C,Y), (A,X), (C,Z), (A,X), (A,X), (B,X), (A,X), (A,Z), (C,X), (C,Y), (A,Z), (C,Z), (A,X), (A,Z), (C,X), (C,Z), (C,X), (C,Z), (A,X), (A,X), (A,X), (C,X), (A,X), (C,Z), (C,X), (C,X), (C,X), (C,X), (A,X), (C,X), (B,Y), (A,Z), (A,Z), (C,Z), (C,X), (A,Y), (C,X), (A,X), (C,X), (C,Z), (B,Z), (C,Z), (B,X), (A,X), (A,Z), (A,X), (C,X), (A,X), (C,Z), (C,Z), (C,Z), (A,X), (C,Z), (A,X), (C,Z), (C,X), (C,X), (C,X), (B,X), (A,X), (A,X), (B,X), (C,X), (A,X), (C,Z), (A,Z), (C,Y), (C,Z), (A,Z), (B,Z), (A,Z), (C,Z), (C,Z), (C,X), (A,X), (A,X), (C,X), (C,X), (C,Y), (A,Y), (A,Z), (A,X), (C,Z), (C,Z), (C,Y), (C,X), (A,X), (A,X), (C,Y), (A,Z), (C,X), (C,X), (C,X), (A,X), (C,X), (C,X), (A,Z), (C,X), (C,Y), (A,Z), (C,Z), (C,Y), (A,Z), (C,Z), (A,X), (C,X), (C,Y), (C,X), (C,X), (C,Z), (C,X), (C,X), (C,Z), (C,X), (A,X), (B,Y), (A,X), (C,X), (B,Z), (C,Z), (C,X), (C,Z), (C,Z), (A,Y), (A,Y), (A,X), (A,Z), (C,Z), (A,X), (C,X), (C,Z), (A,Z), (A,X), (A,X), (A,X), (B,X), (A,X), (B,Z), (C,X), (A,X), (C,X), (A,Z), (C,X), (A,X), (C,Y), (A,X), (C,X), (C,Y), (A,Z), (C,Z), (C,Z), (B,X), (A,X), (C,X), (A,X), (C,X), (C,Y), (A,X), (C,Y), (A,X), (B,X), (C,Y), (A,X), (A,X), (C,X), (A,X), (B,Y), (C,Y), (C,Y), (C,Y), (C,X), (A,Y), (C,X), (C,Z), (A,X), (A,Z), (A,X), (A,X), (C,Z), (A,Z), (A,Z), (C,X), (A,X), (A,X), (B,X), (C,X), (C,Y), (C,X), (B,Z), (A,X), (C,X), (C,Y), (C,X), (C,Z), (C,Y), (A,X), (C,X), (A,X), (A,X), (A,Y), (C,Z), (C,Z), (C,Z), (A,X), (A,X), (A,Z), (A,X), (A,X), (A,Z), (A,X), (C,Y), (C,Y), (A,X), (A,X), (A,Z), (C,X), (A,X), (C,Z), (A,Z), (C,Z), (C,Y), (C,Z), (A,X), (C,Z), (C,X), (C,Z), (A,X), (C,Y), (C,Y), (B,X), (A,X), (A,Z), (C,X), (A,X), (A,X), (C,Z), (A,X), (C,X), (C,Z), (A,Z), (A,Y), (A,Y), (A,X), (C,X), (A,Y), (A,X), (A,X), (B,Z), (A,X), (C,X), (C,X), (C,X), (A,X), (C,X), (C,Z), (C,Z), (A,Z), (C,X), (C,X), (C,Y), (A,X), (C,X), (A,X), (A,X), (C,Y), (C,X), (A,X), (C,Y), (C,X), (C,X), (A,Y), (C,Z), (C,X), (C,X), (C,Y), (C,Z), (A,Y), (A,X), (C,Z), (C,Z), (C,Z), (C,X), (C,Z), (A,X), (A,X), (A,Z), (A,X), (C,Y), (C,X), (C,Z), (A,X), (A,X), (C,Z), (A,X), (C,X), (A,X), (A,Y), (C,X)]
