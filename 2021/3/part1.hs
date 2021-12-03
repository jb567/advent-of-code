find :: Int -> [Int] -> Int
find a b = foldl (\x y -> x + y) 0 $ map (\y -> if y == a then 1 else 0) b

epsilon :: [Int] -> Int
-- If nop
epsilon [] = 0
epsilon l  | find 0 l == length l	     = 0
	   | (find 1 digit) > (find 0 digit) = 2 * loop + 1
	   | otherwise                       = 2 * loop
	   where 
		digit = map (\x -> x `mod` 10) l
		remainder = map (\x -> x `div` 10) l
		loop = epsilon remainder


gamma :: [Int] -> Int
-- If nop
gamma [] = 0
gamma l  | find 0 l == length l	       = 0
	   | (find 1 digit) > (find 0 digit) = 2 * loop 
	   | otherwise                       = 2 * loop + 1
	   where 
		  digit = map (\x -> x `mod` 10) l
		  remainder = map (\x -> x `div` 10) l
		  loop = gamma remainder

power :: [Int] -> Int
power a = (gamma a) * (epsilon a)

main :: IO()
--main = print $ map (\x -> x `div` 2) [2,1,3]
main = print $ power [1001100101, 10100011100, 100000110001, 1111110101, 100010110101, 111010100100, 11011000110, 100000011101, 11001100111, 1011110, 10100011, 110100111110, 1101100101, 11011011101, 10000011010, 11100100100, 1111000011, 100111000111, 100111100011, 101100011011, 1110101101, 110010101000, 111110110111, 111100100010, 1001000000, 1111101011, 101100110011, 11010001111, 100010100001, 110011001010, 101000001100, 111010101100, 101010101100, 110011101000, 1110010110, 10101001001, 10101100, 10101101010, 111001101100, 10000000000, 11011111000, 1110110011, 101000001011, 110101100101, 111001010111, 110001111111, 10010100110, 11010011001, 11101100000, 100110010101, 111000101101, 110011110000, 11001011000, 1101000010, 1110011111, 100010101100, 10011100101, 11110000, 101100001111, 10001011000, 1101011101, 11110001110, 111110101101, 10000000011, 111000000011, 100010100, 111110011001, 111001010011, 11000100010, 100001110000, 111111000100, 10100101110, 10011100110, 101100110111, 101001101001, 1110111, 101100000001, 11010111011, 110011011111, 1001011001, 10110101100, 1100101100, 101001011101, 100101000010, 10101000010, 111101011110, 100010, 111010010010, 100001111011, 110000010011, 101111001, 111101100111, 110101011100, 111110101, 11110111001, 10000101011, 1001011000, 10110011100, 111111111000, 110100000101, 110101010011, 10011110010, 100110111100, 111000001101, 110001001001, 111101111100, 111111101001, 100101001010, 10101010100, 10010001111, 10110000, 11001110110, 111101111011, 11001011100, 110101010101, 110111011111, 110101111000, 110010100110, 110000000010, 11001100001, 11000110010, 10110100000, 11110000001, 100101011010, 100111111011, 1110000011, 100110011011, 100011101, 110000110110, 1000010, 11111111010, 1010100100, 11000100100, 1100001111, 10111100111, 10001101101, 11001010000, 1010101000, 1001100011, 11001010, 10010010100, 100001011000, 11011011, 111011110111, 100010010000, 111001110000, 11100010100, 101101011, 10000000010, 1101110100, 111010111111, 11000011110, 111110000110, 11011011011, 10001000100, 110111010010, 1110010100, 1001011010, 10111100011, 110110110101, 110101000101, 1011100011, 1111010110, 110111011000, 110001011110, 11101000001, 101111100000, 110001101110, 10101101100, 10010100010, 111101010, 10000001010, 11001000010, 100000010110, 111101010010, 111011001011, 10100010, 11111111001, 111001001011, 101001111010, 110100000001, 10010101111, 110011111001, 110111100110, 100111001111, 100101101001, 11111100100, 11111011011, 111100101101, 110010000111, 110110010000, 11101111011, 1101101000, 1001100110, 110001101000, 111100101110, 101110111, 11110101111, 100010010, 11010000011, 10010100001, 111001111110, 1111101010, 101110100011, 100111111001, 110110011010, 100001000, 111011010010, 11001101110, 1011011010, 101110111100, 110011010110, 1010100011, 111010111110, 11100010011, 100011010100, 110000111010, 110001010001, 1111101001, 10011100111, 101010111010, 110101001101, 100111000, 100101001001, 101110111011, 1110011000, 11000010010, 1000010001, 111011101, 100000111100, 1110000100, 11111111100, 100100100, 100101001000, 100011011110, 11110100010, 11110111110, 10001010110, 101000000, 1001100000, 10001101100, 110010110101, 100001000010, 100001001001, 111000011, 111000100010, 11101, 111111000110, 100111100100, 111100011101, 101011110011, 1010110101, 100000000110, 111100011001, 10110010010, 10010010, 1011111001, 1000011100, 11000011000, 111111110000, 100001110010, 11011110010, 110011101, 1101001010, 11010100111, 100000011100, 1000111011, 11010100110, 1111111, 1010010111, 11100001010, 110110001000, 110010101111, 111100010111, 100001111010, 101110010110, 10110011, 111001010100, 101101110110, 111111111100, 11101101011, 11011110100, 111010100011, 1101000110, 111101111001, 110001111000, 10110000111, 110110100101, 111101011100, 100001000011, 10011101000, 111110000, 111011110100, 11010111110, 101011001000, 111100110101, 100100001001, 100011110100, 10101010011, 10110011011, 100111001101, 111001010, 110111011011, 100000111011, 111011000001, 11111100011, 110010000010, 100110101111, 101001111000, 111001111010, 111111000, 111110100100, 111001111011, 100010110111, 110111001111, 100011001001, 110011001101, 100011101111, 11011110000, 101100011110, 110110111011, 11010100000, 111000000010, 10010011, 110101101001, 101011011101, 11101100110, 111111000010, 10110100101, 10010011110, 10000101111, 10110101001, 1111010101, 111000101110, 100000010111, 111001000101, 111101101100, 11000000110, 1110010, 110011010001, 11110011011, 100000010001, 100100001, 11110001100, 1101011111, 100000000001, 10011001, 110111000110, 100110110111, 11000110000, 100101000101, 100100111110, 101010100, 1110100111, 111011001010, 10111001001, 110101001110, 111000100111, 101011100, 100010111000, 111110111, 10110010100, 110101100100, 111000011111, 101001100, 1011111, 11001111101, 110001111001, 111111110100, 10100111111, 11001100110, 10110101101, 11111011001, 101000010100, 10001100101, 101000100101, 1001010010, 100011000, 111, 1110011001, 100000001011, 11011101001, 111001000000, 110011100000, 11001100011, 10011100, 111000000, 110011111, 10100011101, 1100010111, 100011000001, 1011000110, 10000011111, 11101001100, 101111011110, 101001001, 110100101100, 100010101, 10011001000, 10000010100, 111111010000, 101000111, 10010110, 1010000001, 100110110010, 10101100010, 111110010100, 111011111110, 10110110101, 11111000011, 1001001000, 101100001101, 100010101001, 10001101011, 11011101, 10110011111, 101001001010, 100100001111, 100110100011, 101001010, 100000100101, 11000100110, 110100000110, 10101, 11110010, 100010010101, 1000100111, 111010000101, 11000010, 101001110, 100110000, 101110010001, 101110100100, 111111001101, 111011101110, 10011000100, 1001000, 100010111110, 111100101001, 1011111101, 110000011, 100111111100, 111000000110, 1001011110, 111011110000, 110110000110, 110110011100, 10111111010, 11011001110, 110110001100, 111100010, 11001100101, 111111000011, 11001111000, 110111010100, 11111010, 11111101001, 11001011010, 10111101101, 1100100110, 1110101100, 1011011111, 110001010, 100100001101, 11001101111, 10000100110, 110110010100, 110010, 11000101101, 110101111011, 110011111011, 10010001000, 1101110000, 100111100000, 111001011100, 1000110010, 11000011100, 1110010111, 10111011101, 10001110001, 111111101110, 10111000, 1001000110, 101101100010, 10000000, 111100100100, 101111011010, 111110011111, 10011110100, 10010010001, 11100011101, 111100000100, 11110011000, 101100000, 101000111100, 101011110001, 1010000100, 1100101110, 10101010101, 111101110010, 11110100100, 1101100000, 110011, 100101100000, 101111100011, 101010101001, 100110110011, 110100010, 101010000100, 1011010011, 10100110101, 11101101, 100001001100, 111101101110, 100111101001, 10110111010, 11110000110, 100101011101, 1000, 111001011, 11001000101, 11101001111, 111010001011, 10011011111, 101100001011, 1110110001, 101110110100, 100100011000, 111010111, 111011111011, 11011100111, 110000110010, 101110110000, 10001111011, 10011111100, 110100110011, 11000111110, 10111010, 1110001001, 11001100000, 100100100110, 11111001010, 101000100, 101001010010, 10011101100, 110100101110, 10110001001, 111000011011, 10001110000, 11111011000, 10011000011, 1100001010, 100011100010, 11011111100, 10110101111, 101100001, 101101100000, 111000110101, 110011010100, 111011010, 11111010010, 111011111000, 100101110111, 111111, 11100110010, 110110011011, 10001011101, 100110110000, 110111110010, 10001111101, 100001101001, 110000111111, 10111110000, 100011001000, 11000101111, 111100100101, 10111010010, 111100110110, 101001100010, 110100010000, 110010111000, 11100010010, 1001, 10100011110, 110101010110, 111100100001, 111000010010, 110000010111, 1011100000, 1001010101, 110101101111, 1010010101, 10000110100, 111001011010, 1111100, 101101011111, 110100111011, 100111010000, 110001110100, 101011000110, 101100111110, 1010010001, 11111101011, 10100001001, 10110100001, 110000, 110010111111, 111010000100, 111100001111, 100100110000, 1000110001, 11010001011, 110101111010, 1110000001, 10100110, 10101111010, 11110100101, 11011100100, 11111, 100010011001, 100101000100, 110010011011, 11000000000, 1111111110, 1111010011, 100000110111, 100011011000, 110011111110, 10001000001, 11100011111, 101110001000, 101110000111, 1101100011, 111100100000, 101001110010, 100011110110, 100110101101, 11101100100, 111001011110, 110001101101, 100101101110, 111101001100, 100100100001, 100000011110, 11, 10100010111, 1010010110, 110010110000, 100011110101, 110110110110, 100101101100, 111011011, 1001001011, 101111010010, 11011010111, 10001100001, 111110101100, 10100100010, 1101101111, 11111100001, 100011011111, 110011111100, 100110001110, 101010100000, 100111011010, 101110101001, 101111001011, 110111000010, 100110001001, 11111101, 110100010011, 101001000010, 101101011100, 1110010010, 1111000010, 110011101110, 1010011000, 11001000011, 110001101111, 1011001011, 11001111111, 101010101, 101101100110, 110100101101, 11000111000, 11110100011, 10101110111, 110010001000, 101110100010, 111010001010, 11000001010, 1010110111, 110011111111, 11111101010, 101110000, 101101000001, 111110111001, 110010111011, 100111101111, 101111000100, 11101110000, 1101010010, 110110011101, 110000001, 1011111011, 11100001111, 110010101100, 110010010100, 110010100000, 110110000100, 10110011001, 100101101101, 10010010101, 101011111000, 100001001000, 11100011110, 101110010011, 110110001010, 110010110001, 110000000100, 11000000011, 11111000, 10111000111, 111000100, 111101100100, 101000101111, 1000000110, 100000001, 101001100101, 1001111110, 1000010011, 111100001110, 1010111011, 11110011101, 1111101000, 1110100, 110101000110, 101000010, 1110101010, 101101011101, 11100101001, 11110000000, 1001101011, 1001011111, 11101111101, 101111100111, 1110110110, 11010110001, 11111000001, 100101101, 1010010011, 10010110000, 111011101111, 110110000111, 1011110101, 100001101011, 110101100000, 10011111101, 101110010, 100100111010, 101101101111, 10110111001, 110100110110, 111111001111, 100110010000, 100010100000, 11001101000, 111010, 10001100000, 11001101, 111100011111, 100010111011, 10100111, 100101000001, 11001001010, 100101010001, 101001001101, 10011101001, 1000101, 10011000, 111011100001, 1100111010, 10011011000, 1011101, 10100111001, 1010110000, 11011101000, 10010111011, 100101100100, 10111000011, 1001111001, 11111011110, 11110111011, 101010010111, 101101111011, 1100011101, 11011010010, 11000100, 10010110010, 1111011110, 11100011001, 11011101111, 10110101010, 111011010000, 111111010110, 1100110110, 10000110111, 11110100, 11011000001, 101100101110, 100011001110, 11000010100, 110001011, 11000000001, 111100, 101100110101, 111110010011, 111110101001, 110110101111, 101110110101, 110101111001, 10001101, 111010101000, 10010100101, 1111101111, 100110100110, 1010000, 11110101, 110100111000, 101010111100, 10100, 10100010101, 10001100010, 100011010010, 11101001001, 10101111, 101000101, 1001101100, 111111100101, 100111111010, 110011101011, 11100000, 111111011100, 10100001010, 11100110, 110011111101, 1100000000, 11010101111, 101010101011, 111011000111, 10100110010, 11110110101, 10000011001, 100011010110, 1110, 10101101, 111110100011, 111110010000, 101011101010, 11011001101, 101011111111, 100101101011, 111001000100, 10010000101, 11001010101, 110001100010, 10100001101, 111111101101, 101101000111, 1110110101, 1101000101, 1000100110, 11011000, 101001001001, 10001001000, 110110111010, 111110011100, 111111100, 110101111, 1110001, 11101101010, 100001001101, 10100010011, 1001001110, 111110110101, 1110001011, 111010101110, 100111000000, 110100000010, 10111110, 100100000100, 111111001000, 111010001000, 101011100111, 1111111100, 1001101110, 11000001111, 1001100001, 1110000110, 111011011011, 1010101111, 101110110110, 110010000110, 1100010100, 110100111001, 111111110110, 100011001011, 111111000101, 111010010, 10100100110, 101010110110, 11110110100, 110110101100, 100100111111, 101101110100, 10110101110, 101001101101, 1101010011, 1000100101, 10111010011, 11110011, 10011111111, 101000000110, 10001111110, 100111010010, 10100010100, 110100001111, 1100000001, 10010110110, 11010011011, 101000010001, 101100010110, 111010011000, 1001101111, 111101010001, 10101111111, 111010110, 10111100100, 111101111, 110010100001, 100101111100, 10010010011, 100100001000, 10110000001, 11000110, 101100110100, 1100101, 10110101000, 101010100010, 101001110000, 101111000001, 110001111110, 101011010101, 111000010001, 10000101101, 101010011, 101100011100, 10101000000, 10001110010, 111000001010, 101001011100, 1101111101, 111000101011, 100100000000, 10001011111, 101000011, 111000011000, 100101, 110100001010, 11111111101, 100001010100, 110000011000, 11110011100, 110110, 101100100101, 110001110, 110101110100, 10011111011, 101011100110, 110110010101, 1110011100, 110101110000, 111001001111, 10101011, 10011100001, 100101010111, 111100101, 11001110, 111001000111, 11001100100, 1110001101, 110101010100, 100101100111, 11001000000, 100000110100, 100011101101, 100100110101, 11000110110, 101110011011, 110101100001]
