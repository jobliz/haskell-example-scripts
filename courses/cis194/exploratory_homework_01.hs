-- References
-- https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string
--      https://gist.github.com/thekarel/9913217
-- https://hoogle.haskell.org/?hoogle=Char%20-%3E%20Int
-- https://stackoverflow.com/questions/26847192/reverse-a-list-in-haskell
-- https://stackoverflow.com/questions/4061777/converting-int-to-integer/4061926

-- toDigits :: Integer -> [Integer]
-- toDigitsRev :: Integer -> [Integer]
-- digitToChar :: Integer -> Char

import Data.Char -- digitToInt
import Data.Typeable -- typeOf

-- own idea, but not necessary, i think
integerToString :: Int -> String
integerToString n = read $ show n :: String

toDigits :: Integer -> [Integer]
toDigits n = map toInteger (map digitToInt (show n))

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

main = do
    let test1 = read "3" :: Int
    let test2 = show 45
    let test3 = map digitToInt (show 12345678)
    let test4 = [1, 2, 3, 4]
    -- print (typeOf (integerToString 5))
    -- print (typeOf (digitToInt '2')) --
    print (typeOf test3)
    print (test3)
    print (reverse (test3))