-- Reference:
-- https://stackoverflow.com/questions/10408046/implementing-read-for-a-left-associative-tree-in-haskell/10410178#10410178
-- Specific answer: https://stackoverflow.com/a/10410178/9930918

import System.Environment
import Text.Parsec
import Control.Applicative ((<*), (<$>))

data Tree = Branch Tree Tree | Leaf Char deriving (Eq, Show)
paren, tree, unit :: Parsec String st Tree

-- parenthesised tree or `Leaf <character>`
unit = paren <|> (Leaf <$> noneOf "()") <?> "group or literal"

-- normal tree between ( and )
paren = between (char '(') (char ')') tree  

-- all the units connected up left-associatedly
tree = foldl1 Branch <$> many1 unit

-- attempt to parse the whole input (don't short-circuit on the first error)
onlyTree = tree <* eof

read' str = case parse onlyTree "" str of
    Right tr -> tr
    Left er -> error (show er)

-- [i just wired it to stdin. it works!]
main = do
    args <- getArgs
    let string = (args !! 0) :: String
    print (read' string)