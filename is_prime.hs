-- References:
-- https://stackoverflow.com/questions/4690762/determining-if-a-given-number-is-a-prime-in-haskell
-- https://stackoverflow.com/questions/7801407/read-from-stdin-in-haskell-using-io-readln
-- https://www.reddit.com/r/haskell/comments/288egd/im_trying_my_hardest_to_learn_this_language_and/
-- https://stackoverflow.com/questions/2468410/convert-string-to-integer-float-in-haskell
-- https://codereview.stackexchange.com/questions/181201/extracting-an-integer-command-line-argument-and-printing-a-sum-of-numbers-up-to

import System.Environment

isPrime :: Int -> Bool
isPrime k = null [ x | x <- [2..k - 1], k `mod` x == 0]

main = do
    args <- getArgs
    let arg1 = head args
    let n = (read arg1) :: Int
    let b = isPrime n
    print b