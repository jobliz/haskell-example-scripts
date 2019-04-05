-- https://stackoverflow.com/questions/11022163/haskell-read-lines-of-file
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines

module LogAnalysis where

import Data.Typeable -- typeOf
import System.Environment -- getArgs
import Log
import Data.List.Split -- splitOn, with cabal install split
import Data.List -- intercalate function

parseInformationMessage :: String -> LogMessage
parseInformationMessage s = do
    let elements = splitOn " " s
    let timestamp = (read (elements !! 1) :: Int) 
    let string = intercalate " " (tail (tail elements))
    LogMessage Info timestamp string

parseErrorMessage :: String -> LogMessage
parseErrorMessage s = do
    let elements = splitOn " " s
    let errorlevel = (read (elements !! 1) :: Int)
    let error = Error errorlevel 
    let timestamp = (read (elements !! 2) :: Int) 
    let string = intercalate " " (tail (tail (tail elements)))
    LogMessage error timestamp string

parseWarningMessage :: String -> LogMessage
parseWarningMessage s = do
    let elements = splitOn " " s
    let timestamp = (read (elements !! 1) :: Int) 
    let string = intercalate " " (tail (tail elements))
    LogMessage Warning timestamp string

parseMessage :: String -> LogMessage
parseMessage s = do
    let elements = splitOn " " s -- first item after splitting by spaces (message type)
    -- let key = (intercalate "" (head elements))   does not work
    let key = elements !! 0
    case key of -- todo: understand types, case does not work with single quotes when I expected it to
        "I" -> parseInformationMessage s
        "W" -> parseWarningMessage s
        "E" -> parseErrorMessage s
        _  -> Unknown s

-- Doing smply "content <- readFile path" with a :: String -> [LogMessage] signature produces
--      Couldn't match type ‘IO’ with ‘[]’
-- because
-- readFile is an IO operation and cannot be used if the function's
-- output type is not IO(). You've gotta read it outside of this function
-- and then feed it here.
parseMessageFile :: String -> [LogMessage]
parseMessageFile content = do
    map parseMessage (lines content)
    

main = do
    args <- getArgs
    let path = (args !! 0)
    content <- readFile path -- takes the first element (at position 0)
    let notright = parseMessage "This is not in the right format"
    let messages = parseMessageFile content
    print notright
    mapM_ print messages