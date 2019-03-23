-- References:
-- Haskell Programming From First Principles
-- https://www.devdungeon.com/content/command-line-arguments-haskell
-- https://stackoverflow.com/questions/11229854/how-can-i-parse-the-io-string-in-haskell
-- https://stackoverflow.com/questions/49055999/how-does-getargs-work

import System.Environment

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

extractHeadArg = do
    args <- getArgs
    return $ head args

main = do 
    args <- getArgs
    first <- extractHeadArg
    sayHello first