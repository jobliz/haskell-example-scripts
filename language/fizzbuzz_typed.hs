-- Reference:
-- https://github.com/jobliz/ocaml-example-scripts/blob/master/language/algebraic_fizzbuzz.ml
-- fizzbuzz_example_1.hs
-- https://stackoverflow.com/questions/3429291/what-is-the-difference-between-int-and-integer
--      "Integer" is arbitrary precision

import System.Environment

data FB = Fizz Int
    | Buzz Int
    | FizzBuzz Int
    | JustInt Int

fizzBuzz :: Int -> FB
fizzBuzz n | n `mod` 15 == 0 = FizzBuzz n
           | n `mod` 5  == 0 = Fizz n
           | n `mod` 3  == 0 = Buzz n
           | otherwise       = JustInt n

fbStringRep :: FB -> String
fbStringRep x = case x of
    Fizz x -> "Fizz"
    Buzz x -> "Buzz"
    FizzBuzz x -> "FizzBuzz"
    JustInt x -> "JustInt"

main = do
    args <- getArgs
    let arg1 = head args
    let n = (read arg1) :: Int
    print (fbStringRep (fizzBuzz n))