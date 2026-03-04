module Main where

import Data.Foldable
import Control.Monad
import Options.Applicative as A

import Parsers.NiParser as Ni
import Parsers.CParser as C
import ComposePasses
import X86Passes.WriteToFile

-- Command line options structure
data CompOpts = CompOpts
  { lang :: String
  , showAst :: Bool
  , outFile :: String
  , files :: [String]
  }

-- Command line argument parser
compOpts :: A.Parser CompOpts
compOpts = CompOpts
  <$> strOption
      (  long "language"
      <> short 'l'
      <> metavar "LANGUAGE"
      <> value "ni"
      <> help "Source language to be compiled")
  <*> flag False True
      (  long "ast"
      <> short 'a'
      <> help "Print the AST" )
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

-- Program info
opts :: ParserInfo CompOpts
opts = info (compOpts <**> helper)
  ( fullDesc
  <> progDesc "Compiles Ni programs"
  <> header "nic - a compiler for Ni programs"
  )

-- Main entry point
main :: IO ()
main = do
  CompOpts { lang = lang', showAst = showAst', outFile = outFile', files = files' } <- execParser opts
  forM_ files' $ \arg -> do
    putStr $ "Parsing " ++ arg ++ "..."

    if lang' == "ni" then do
      res <- Ni.niParseFile arg
      case res of
        Left err -> do
          putStrLn "Failed."
          putStrLn err
        Right ast -> do
          putStrLn "Done."
          when showAst' $ print ast
          -- Run the full pipeline
          case allPasses ast of
            Right x86 -> do
              if outFile' == "" then
                writeToStdio x86
              else
                writeToFile outFile' x86
            Left err -> putStrLn err
    else if lang' == "c" then do
      res <- C.cParseFile arg
      case res of
        Left err -> do
          putStrLn "Failed."
          putStrLn err
        Right ast -> do
          putStrLn "Done."
          when showAst' $ print ast
    else
      putStrLn "language must be 'ni' or 'c'"
