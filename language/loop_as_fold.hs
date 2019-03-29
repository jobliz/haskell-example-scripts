{- References: 
https://stackoverflow.com/questions/9014626/iterating-through-a-list-in-haskell
    * https://stackoverflow.com/a/9023214

-------------------------------
NOTE: Does not run as presented
-------------------------------

From StackOverflow:

The "correct" way to iterate is actually fold. Anything you might ever want to
 do with a list can be done with a fold. Let's consider what you want to do. 
 You're probably thinking of something like this:

for (row in xs):
  for (c in row):
    doSomething

The problem is, you're probably making use of mutable variables in doSomething. 
That's ok, we can deal with that. So suppose you have this.

def iter2d(xs):
  outerVar = outerInit
  for (row in xs):
    innerVar = innerInit(row)
    outerVar.adjust1(row)
    for (c in row):
      innerVar.adjust2(c)
      outerVar.adjust3(c, innerVar)
  return outerVar

Let's translate that to folds. And immutability.

Note: map is a specialization of fold
-}

iter2d :: [[Char]] -> Something
iter2d xs = foldl' outerStep outerInit xs
  where outerInit = ... -- same as outerInit above
        outerStep acc row = fst $ foldl' innerStep innerInit' row)
          where innerInit' = ((adjust1 acc row), innerInit row)
        innerInit row = ... -- same as innerInit above
        innerStep (outAcc, inAcc) c = (outAcc', inAcc')
          where inAcc' = adjust2 inAcc c
                outAcc' = adjust3 outAcc c inAcc'

main = do
    some <- iter2d [["one", "two"], ["three", "four"]]