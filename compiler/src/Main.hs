module Main where

import Data.Foldable
import Control.Monad
-- from optparse-applicative
import Options.Applicative as A

-- import Parsers.NiParser as Ni
-- import Parsers.CParser as C
import ComposePasses
import X86Passes.WriteToFile

-- basically a structure representing the entirety of the compiler
-- options we have, so lang, showing the AST, and a list of files to compile
data CompOpts = CompOpts
  { lang :: String
  , showAst :: Bool
  , outFile :: String
  , files :: [String]
  } 

-- create the parser, which is a combination of parsers here,
-- but really what we're trying to do is parse the values from
-- the command line so we can fill out the CompOpts above
compOpts :: A.Parser CompOpts
compOpts = CompOpts
  <$> strOption
      (  long "language"
      <> short 'l'
      <> metavar "LANGUAGE"
      <> value "ni"
      <> help "Source language to be compiled")
      -- a flag is present or not, the first argument is the value
      -- of the option if the flag is not there, the second is value
      -- if it gets included
  <*> flag False True
      (  long "ast"
      <> short 'a'
      <> help "Print the AST" )
      -- argument will parse anything that's not an option, but it
      -- can't start with -, because these look like options
  <*> strOption
      (   long "outFile"
      <>  short 'o'
      <>  metavar "OUTFILE"
      <>  value ""
      <>  help "file name to write output to"
      )
  <*> some (argument str
      ( metavar "FILES..."
      <> help "Source file(s) to compile"))

-- the long description of the program
opts :: ParserInfo CompOpts
opts = info (compOpts <**> helper)
  ( fullDesc
  <> progDesc "Compiles Ni programs"
  <> header "nic - a compiler for Ni programs"
  )

-- main entry point into our Haskell program
main :: IO ()
main = do
  -- figure out the command line options
  module Main where

import Data.Foldable
import Control.Monad
-- from optparse-applicative
import Options.Applicative as A

-- import Parsers.NiParser as Ni
-- import Parsers.CParser as C
import ComposePasses
import X86Passes.WriteToFile

-- basically a structure representing the entirety of the compiler
-- options we have, so lang, showing the AST, and a list of files to compile
data CompOpts = CompOpts
  { lang :: String
  , showAst :: Bool
  , outFile :: String
  , files :: [String]
  } 

-- create the parser, which is a combination of parsers here,
-- but really what we're trying to do is parse the values from
-- the command line so we can fill out the CompOpts above
compOpts :: A.Parser CompOpts
compOpts = CompOpts
  <$> strOption
      (  long "language"
      <> short 'l'
      <> metavar "LANGUAGE"
      <> value "ni"
      <> help "Source language to be compiled")
      -- a flag is present or not, the first argument is the value
      -- of the option if the flag is not there, the second is value
      -- if it gets included
  <*> flag False True
      (  long "ast"
      <> short 'a'
      <> help "Print the AST" )
      -- argument will parse anything that's not an option, but it
      -- can't start with -, because these look like options
  <*> strOption
      (   long "outFile"
      <>  short 'o'
      <>  metavar "OUTFILE"
      <>  value ""
      <>  help "file name to write output to"
      )
  <*> some (argument str
      ( metavar "FILES..."
      <> help "Source file(s) to compile"))

-- the long description of the program
opts :: ParserInfo CompOpts
opts = info (compOpts <**> helper)
  ( fullDesc
  <> progDesc "Compiles Ni programs"
  <> header "nic - a compiler for Ni programs"
  )

-- main entry point into our Haskell program
-- main entry point into our Haskell program
main :: IO ()
main = do
  -- figure out the command line options
  CompOpts { lang = lang, showAst = showAst, outFile = outFile, files = files}  do
    putStr $ "Parsing " ++ arg ++ "..."

    if lang == "ni" then do
      res <- Ni.niParseFile arg 
      case res of
        Left err -> do
          putStrLn "Failed."
          putStrLn err
        Right ast -> do
          putStrLn "Done."
          when showAst $ print ast
          -- now, run the passes
          case allPasses ast of
            Right x86 -> do
              if outFile == "" then
                writeToStdio x86
              else
                writeToFile outFile x86
            Left err -> putStrLn err
    else if lang == "c" then do
      res  do
          putStrLn "Failed."
          putStrLn err
        Right ast -> do
          putStrLn "Done."
          when showAst $ print ast
    else
      putStrLn "language must be 'ni' or 'c'"