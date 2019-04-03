-- References
-- https://stackoverflow.com/questions/19867491/double-every-other-element-of-list-from-right-in-haskell

import Data.Char -- digitToInt

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n =
    if n > 0 then map toInteger (map digitToInt (show n)) 
    else []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- TODO: comprehend it, or the alternatives at SO
doubleSecondFromEnd :: [Integer] -> [Integer]
doubleSecondFromEnd [] = []  -- Do nothing on empty list
doubleSecondFromEnd n
  | length n `mod` 2 == 0 = head n * 2 : doubleSecondFromEnd (tail n)
  | otherwise      = head n : doubleSecondFromEnd (tail n)

-- TODO
-- sumDigits :: [Integer] -> Integer

main = do
    print (toDigits 1234)
    print (toDigitsRev 1234)
    print (toDigits 0)
    print (toDigits (-17))
    print (doubleSecondFromEnd [8, 7, 6, 5])
    print (doubleSecondFromEnd [1, 2, 3])