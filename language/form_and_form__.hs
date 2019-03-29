-- https://stackoverflow.com/questions/6688998/what-is-the-difference-between-form-and-form-in-haskell

import Control.Monad (forM, forM_)

main = do
  let askName i = do
      putStrLn $ "What's your name (" ++ (show i) ++ ")"
      name <- getLine
      return name
  -- if you use forM_ instead it would output "Results = ()"
  results <- forM [1,2,3] askName
  putStrLn $ "Results = " ++ show results