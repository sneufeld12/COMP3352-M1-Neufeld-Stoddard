{-# LANGUAGE OverloadedStrings #-}
-- this feature just overloads a string literal to convert between a haskell
-- String and Text by calling Text.pack and Text.unpack on them contextually
-- note this only occurs with string literals

module Parsers.NiParser where

import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Text as T
import Data.Text.IO as TIO
-- for setting up debugging
--import Text.Megaparsec.Debug

import ComposePasses
import NiPasses.N1
import CPasses.C0

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void


-- first we want to define how we handle whitespace, which
-- megaparsec helps through the Lexer package with a function
-- called space. This function requires:
--   1. how we define space, and we use space1
--      from Text.Megaparsec.Char.space1, which is 1 or more whitespace
--   2. how we ignore line comments, so we use skipLineComment, and in
--      Ni, we can use // to indicate a line comment
--   3. how we ignore block comments, so we use skipBlockComment, which
--      then allows us to define Ni block comments, which are delineated
--      with /* */
spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")


-- now that we have a space consumer, we can add define some helpers

-- lexeme will give us the lexeme from the parser, elminating whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- symbol will give us a symbol after eating up the space, you pass in
-- the text and it gives you a parser that will parse that Text literally
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- define character literals, which in ni is a single quoted character
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

-- string literals go between double quotes
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')


-- parse a variable, which begins with a letter or character and is
-- followed by 0 or more (M.many) alphanumeric characters--note the
-- (:) is used to cons the elements together
parseVar :: Parser Ni.Exp
parseVar =
  choice [
    try parseRead,
    try parseLet,
    Ni.Var <$> lexeme
  ((:) <$> letterChar
    <*> M.many (alphaNumChar <|> char '_' <|> char '-' <|> char '\'')
      <?> "variable")]

-- parses an integer (unsigned) and returns an Int
parseInt :: Parser Ni.Exp
parseInt = Ni.Int <$> lexeme L.decimal

-- parses parens by parsing what's between the parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- try to parse a read statement, which is a special keyword
-- in the Ni language to read an int. Note we have to use
-- try here so we can backtrack if this fails since like 'read1'
-- is a valid variable name
parseRead :: Parser Ni.Exp
parseRead = Ni.Read <$ symbol "read" <* notFollowedBy alphaNumChar

parseLet :: Parser Ni.Exp
parseLet = do
  symbol "let"
  symbol "ni"
  (Ni.Var sym) <- parseVar
  symbol "is"
  assn <- parseExp
  symbol "in"
  body <- parseExp
  symbol "end"
  return $ Ni.Let sym assn body


-- parses a term, which can be either a parenthesized expression,
-- a variable, or an integer
parseTerm :: Parser Ni.Exp
parseTerm = choice
  [ parens parseExp
  -- we use a try here because we need to backtrack if
  -- it turns out the let or read are prefixes to var names,
  -- and really the same holds for any keywords
  , try parseRead
  , try parseLet
  , parseVar
  , parseInt
  ]

-- create a parser for expressions, which we use the
-- parser-combinators library and makeExprParser within it
parseExp :: Parser Ni.Exp
parseExp = do
  try spaceConsumer
  makeExprParser parseTerm operatorTable

-- build binary parsers for our binary operators
binary :: Text -> (Ni.Exp -> Ni.Exp -> Ni.Exp) -> Operator Parser Ni.Exp
binary  name f = InfixL (f <$ symbol name)

-- build unary parsers (prefix/postfix) for our unary operators
prefix, postfix :: Text -> (Ni.Exp -> Ni.Exp) -> Operator Parser Ni.Exp
prefix  name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-- now construct the operator table, with room to grow!
operatorTable :: [[Operator Parser Ni.Exp]]
operatorTable =
  [ [ prefix "-" Ni.Negate
    , prefix "+" id
    ]
  --, [ binary "*" Ni.Mult
  --  , binary "/" Ni.Div
  --  ]
  , [ binary "+" Ni.Add
    --, binary "-" Ni.Sub
    ]
  ]

-- parses a program, creating an N1 program by parsing
-- its single subexpression
niParseProg :: Parser N1
niParseProg =
  Ni.Program <$> parseExp


-- parses a string and turns it into an N1
niParseStr :: String -> Result N1
niParseStr str =
  case parse niParseProg "(fromString)" (T.pack str) of
    Left bundle -> Left (errorBundlePretty bundle)
    Right res -> Right res

-- parses a string and turns it into an N1
niParse :: String -> Result N1
niParse str =
  case parse niParseProg "(fromString)" (T.pack str) of
    Left bundle -> Left (errorBundlePretty bundle)
    Right res -> Right res

-- take a file anme and parse it into an N1 AST
niParseFile :: String -> IO (Result N1)
niParseFile filename = do
  res <- parse niParseProg filename <$> TIO.readFile filename
  case res of
    Left bundle -> return $ Left (errorBundlePretty bundle)
    Right res -> return $ Right res
