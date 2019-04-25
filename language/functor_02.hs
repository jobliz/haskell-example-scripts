-- From https://stackoverflow.com/questions/13134825/how-do-functors-work-in-haskell

echo1 :: IO ()
echo1 = do
    putStrLn "Say something!"
    whattheysaid <- getLine  -- getLine :: IO String
    putStrLn whattheysaid    -- putStrLn :: String -> IO ()

echo2 :: IO ()
echo2 = putStrLn "Say something!" 
        >> getLine >>= putStrLn

main = do
    echo1
    echo2