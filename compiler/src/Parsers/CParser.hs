{-# LANGUAGE OverloadedStrings #-}
-- this feature just overloads a string literal to convert between a haskell
-- String and Text by calling Text.pack and Text.unpack on them contextually
-- note this only occurs with string literals

module Parsers.CParser where


import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text as T
import Data.Text.IO as TIO
-- for setting up debugging in the parser
--import Text.Megaparsec.Debug

import CompilerPasses
import CPasses.C0 as C

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
parseVar :: Parser C.Atom
parseVar = C.Var <$> lexeme
  ((:) <$> letterChar <*> M.many alphaNumChar <?> "variable")

-- parses an integer (unsigned) and returns an Int
parseInt :: Parser C.Atom
parseInt = C.Int <$> Parsers.CParser.lexeme L.decimal

-- parses parens by parsing what's between the parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (Parsers.CParser.symbol ")")

-- try to parse a read statement, which is a special keyword
-- in the Ni language to read an int. Note we have to use
-- try here so we can backtrack if this fails since like 'read1'
-- is a valid variable name
parseRead :: Parser C.Exp
parseRead = C.Read <$ symbol "read" <* notFollowedBy alphaNumChar

-- parseAssign :: Parser C.Exp
-- parseAssign = do
--   (C.Var sym) <- dbg "sym" parseVar
--   dbg "=" $ CParser.symbol "="
--   expr <- dbg "expr" parseExp
--   return $ C.Assign sym expr

-- parse an atom, which is an int or a var
parseAtom :: Parser C.Atom
parseAtom = choice [ parseVar, parseInt ]

-- parses an add, which has two atoms
parseAdd :: Parser C.Exp
parseAdd = do
  e1 <- parseAtom
  symbol "+"
  C.Add e1 <$> parseAtom

-- parses a sub, which has one atom
parseSub :: Parser C.Exp
parseSub = do
  symbol "-"
  C.Sub <$> parseAtom

-- parses an exp, which is an atom, read, sub, or add
-- also, note that order matters, we want to try to parse
-- an add before we fail and backtrack to just parsing an atom
parseExp :: Parser C.Exp
parseExp = choice
  [ try parseRead
  , parseSub
  , try parseAdd
  , Atm <$> parseAtom
  ]

parseStmt :: Parser C.Stmt
parseStmt = do
  (Var sym) <- parseVar
  symbol "="
  rhs <- parseExp
  symbol ";"
  return $ C.Assign sym rhs

parseReturn :: Parser C.Tail
parseReturn = do
  symbol "return"
  exp <- parseExp
  symbol ";"
  return $ C.Return exp

parseSeq :: Parser C.Tail
parseSeq = do
  stmt <- parseStmt
  C.Seq stmt <$> parseTail

parseTail :: Parser C.Tail
parseTail = choice
  [ parseReturn
  , parseSeq
  ]

parseLabel :: Parser String
parseLabel = do
  (Var sym) <- parseVar
  symbol ":"
  return sym

parseBlock :: Parser (String, C.Tail)
parseBlock = do
  (,) <$> parseLabel <*> parseTail

cParseProg :: Parser C.C0
cParseProg = do
  try spaceConsumer
  C.Program <$> some parseBlock

-- parses an
cParseStr :: String -> String --Result C.Tail
cParseStr str =
  case cParseResult str of
    Left msg -> Prelude.show msg
    Right res -> Prelude.show res

cParseResult :: String -> Result C0
cParseResult str =
  case parse cParseProg "(from string)" (T.pack str) of
    Left bundle -> Left $ errorBundlePretty bundle
    Right res -> Right res

-- take a file anme and parse it into an N1 AST
cParseFile :: String -> IO (Result C0)
cParseFile filename = do
  res <- parse cParseProg filename <$> TIO.readFile filename
  case res of
    Left bundle -> return $ Left (errorBundlePretty bundle)
    Right res -> return $ Right res
