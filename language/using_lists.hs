-- References:
-- https://stackoverflow.com/questions/9014626/iterating-through-a-list-in-haskell
-- https://wiki.haskell.org/Fold
-- http://www.cse.unsw.edu.au/~en1000/haskell/hof.html
-- http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html
-- https://stackoverflow.com/questions/6688998/what-is-the-difference-between-form-and-form-in-haskell

import Data.Foldable -- imports forM_

-- prints each element of the list as in an imperative language
iterate_list xs = do
    forM_ xs $ \s -> do
        print s

double :: Int -> Int
double n = n * 2

main = do
    let l1 = [1, 2, 3]
    let l2 = map (+7) [1,2,3,4]
    let l3 = map double l2
    let some_evens = filter (even) [1..10]
    let total = foldr (+) 0 l3
    -- iterate_list l1
    -- iterate_list l2
    -- iterate_list l3
    -- print total
    -- print some_evens
    print "Finish!"