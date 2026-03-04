module X86Passes.WriteToFile where

import System.IO
import X86Passes.X86b

writeToStdio :: X86b -> IO ()
writeToStdio = print

writeToFile :: String -> X86b -> IO ()
writeToFile filename x86 =
  withFile filename WriteMode (`writeToHandle` x86)

writeToHandle :: Handle -> X86b -> IO ()
writeToHandle handle x86 = do
  hPrint handle x86
  hClose handle
