{- 
Transcribed from the markdown here: 
https://github.com/mrkkrp/markkarpov.com/blob/master/megaparsec/parsing-simple-imperative-language.md

Do:
cabal install parsec megaparsec parser-combinators

Use (todo: expand)

Run with runghc, type
    var := 42
Hit enter, and then control + D. It should say
    Assign "var" (IntConst 42)

Slightly larger example, arithmetic:
  x := 6;
  y := 4;
  z := x + y

It prints out:
  Seq [Assign "x" (IntConst 6),Assign "y" (IntConst 4),Assign "z" (ABinary Add (Var "x") (Var "y"))]
-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

{-

The language
============

The grammar for expressions is defined as follows:

a   ::= x | n | - a | a opa a
b   ::= true | false | not b | b opb b | a opr a
opa ::= + | - | * | /
opb ::= and | or
opr ::= > | <

Note that we have three groups of operators: arithmetic, Boolean and relational 
ones.

And now the definition of statements:

S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S

We probably want to parse that into some internal representation of the 
language (an abstract syntax tree). Therefore we need to define the data 
structures for the expressions and statements.

-}

-- Data structures
-- ===============

-- We need to take care of Boolean and arithmetic expressions and the 
-- appropriate operators. First let's look at the Boolean expressions:

data BExpr
    = BoolConst Bool
    | Not BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show)

-- Binary Boolean operators:

data BBinOp
    = And
    | Or
    deriving (Show)

-- Relational operators:

data RBinOp
    = Greater
    | Less
    deriving (Show)

-- Now we define the types for arithmetic expressions:

data AExpr
    = Var String
    | IntConst Integer
    | Neg AExpr
    | ABinary ABinOp AExpr AExpr
    deriving (Show)

-- and arithmetic operators:

data ABinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show)

-- Finally let's take care of the statements:

data Stmt
    = Seq [Stmt]
    | Assign String AExpr
    | If BExpr Stmt Stmt
    | While BExpr Stmt
    | Skip
    deriving (Show)

-- Since we don't need custom data in error messages and our input stream 
-- will be in the form of a String, the following definition of Parser will do:

type Parser = Parsec Void String

{-

Lexer
=====

Having all the data structures we can go on with writing the code to do the 
actual parsing. Here we will define lexemes of our language. When writing a 
lexer for a language it's always important to define what counts as whitespace 
and how it should be consumed. space from Text.Megaparsec.Char.Lexer module 
can be helpful here.

sc stands for “space consumer”. space takes three arguments: a parser that 
parses whitespace (but it should not accept empty input), a parser for line 
comments, and a parser for block (multi-line) comments. skipLineComment and 
skipBlockComment help with parsers that consume comments. (If our language 
didn't have block comments, we could pass empty from Control.Applicative as 
the third argument of space.)

-}

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Next, we will follow the strategy where whitespace will be consumed after 
-- every lexeme automatically, but not before it. Let's define a wrapper to 
-- achieve this:

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Perfect. Now we can wrap any parser in lexeme and it will consume any 
-- trailing whitespace with sc.

-- Since we often want to parse some “fixed” string, let's define one more 
-- parser called symbol. It will take a string as argument and parse this 
-- string and whitespace after it.

symbol :: String -> Parser String
symbol = L.symbol sc

-- With these tools we can create other useful parsers:

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

-- To parse various operators we can just use symbol, but reserved 
-- words and identifiers are a bit trickier. There are two things to note:
--
-- * Parsers for reserved words should check that the parsed reserved word 
--   is not a prefix of an identifier.
-- * Parsers of identifiers should check that parsed identifier is not a 
--   reserved word.
--
-- Let's express it in code:

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- identifier may seem complex, but it's actually simple. We just parse a 
-- sequence of characters where the first character is a letter and the rest 
-- is several characters where every one of them can be either a letter or a
-- digit. Once we have parsed such a string, we check if it's in the list of 
-- reserved words, fail with an informative message if it is, and return the 
-- result otherwise.
-- 
-- Note the use of try in identifier. This is necessary to backtrack to 
-- beginning of the identifier in cases when fail is evaluated. Otherwise 
-- expressions like many identifier would fail on such identifiers instead 
-- of just stopping.
--
-- And that's it, we have just written the lexer for our language, now we can
-- start writing the parser.

{-

Parser
======

As already mentioned, a program in this language is simply a statement, so the 
main parser should basically only parse a statement. But we must remember to 
take care of initial whitespace—our parsers only get rid of whitespace after 
the tokens!

-}

whileParser :: Parser Stmt
whileParser = between sc eof stmt

-- Because any statement might be actually a sequence of statements separated 
-- by semicolons, we use sepBy1 to parse at least one statement. The result is 
-- a list of statements. We also allow grouping statements with parentheses, 
-- which is useful, for instance, in the while loop.

stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi
  where
    -- if there's only one stmt return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l

-- A single statement is quite simple, it's either an if conditional, a while 
-- loop, an assignment or simply a skip statement. We use <|> to express choice.
-- So a <|> b will first try parser a and if it fails (without actually consuming 
-- any input) then parser b will be used. Note: this means that the order is
-- important.

stmt' :: Parser Stmt
stmt' = ifStmt
  <|> whileStmt
  <|> skipStmt
  <|> assignStmt
  <|> parens stmt

-- Now let's define the parsers for all the possible statements. This is quite
-- straightforward as we just use the parsers from the lexer and then use all
-- the necessary information to create appropriate data structures.

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond  <- bExpr
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  return (While cond stmt1)

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  void (symbol ":=")
  expr <- aExpr
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"

-- If you have a parser that might fail after consuming some input, and you 
-- still want to try the next parser, you should take a look at the try 
-- combinator. For instance try p <|> q will try parsing with p and if it 
-- fails, even after consuming the input, the q parser will be used as if 
-- nothing has been consumed by p. We don't need try with these alternatives 
-- because rword matches on keyword (such as if, while, skip) using string 
-- which backtracks automatically, and once we have matched a keyword, we're 
-- committed to that branch and don't want to backtrack anyway. As for 
-- identifier, it already has try in its definition.

{-

Expressions
===========

What's left is to parse the expressions. Fortunately we can use the 
Control.Monad.Combinators.Expr module from the parser-combinators package to
 do that. Let's define the arithmetic and Boolean expressions:

-}

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

-- Now we have to define the lists with operator precedence, associativity 
-- and what constructors to use in each case.

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]

-- In the case of prefix operators it is enough to specify which one should 
-- be parsed and what is the associated data constructor. Infix operators are 
-- defined similarly, but there are several variants of infix constructors for 
-- various associativity options. Note that the operator precedence depends 
-- only on the order of the elements in the list.
-- 
-- Finally we have to define the terms. In the case of arithmetic expressions, 
-- it is quite simple:

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var      <$> identifier
  <|> IntConst <$> integer

-- However, a term in a Boolean expression is a bit more tricky. In this case, 
-- a term can also be an expression with relational operator consisting of 
-- arithmetic expressions.

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr

-- Therefore we have to define a parser for relational expressions:

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (Greater <$ symbol ">")
  <|> (Less <$ symbol "<")

-- And that's it. We have a quite simple parser which is able to parse a few 
-- statements and arithmetic/boolean expressions.

{-

Notes
=====

If you want to experiment with the parser inside GHCi, the parseTest function 
might be handy. parseTest p input applies parser p on input input and prints 
results.

We could also define main function like so, then compile an executable and play
with it on the command line:

-}

main :: IO ()
main = do
  input <- getContents
  parseTest whileParser input