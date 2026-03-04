module X86Passes.AssignHomes
  ( AHPass(..)
  , assignHomesPass
  , zipVarWithStackLocations
  ) where

import CompilerPasses (Result)
import qualified NiPasses.Env as Env
import NiPasses.Env (Env(..))
import X86Passes.X86b

data AHPass = AHPass
  { x86prog     :: X86b
  , env         :: Env Arg
  , localCount  :: Int
  } deriving (Eq, Show)

zipVarWithStackLocations :: [String] -> Reg -> Int -> Int -> [(String, Arg)]
zipVarWithStackLocations locals baseReg startOffset step =
  zip locals [ Mem baseReg (fromIntegral off) | off <- offsets ]
  where
    offsets = [startOffset, startOffset - step .. startOffset - step * (length locals - 1)]

assignHomesPass :: (X86b, [String]) -> Result AHPass
assignHomesPass (Program blocks, locals) =
  let bindings  = zipVarWithStackLocations locals RBP (-8) 8
      env0      = Env.fromList bindings
      nLocals   = length locals
  in do
    blocks' <- mapM (ahBlock env0) blocks
    pure $ AHPass (Program blocks') env0 nLocals

ahBlock :: Env Arg -> (Label, Block) -> Result (Label, Block)
ahBlock env0 (lbl, Block instrs) = do
  instrs' <- mapM (ahInstr env0) instrs
  pure (lbl, Block instrs')

ahInstr :: Env Arg -> Instr -> Result Instr
ahInstr env0 ins =
  case ins of
    Addq a1 a2 -> Addq <$> ahArg env0 a1 <*> ahArg env0 a2
    Subq a1 a2 -> Subq <$> ahArg env0 a1 <*> ahArg env0 a2
    Movq a1 a2 -> Movq <$> ahArg env0 a1 <*> ahArg env0 a2
    Negq a     -> Negq <$> ahArg env0 a
    Pushq a    -> Pushq <$> ahArg env0 a
    Popq a     -> Popq <$> ahArg env0 a
    Jmp l      -> pure (Jmp l)
    Retq       -> pure Retq
    Callq l n  -> pure (Callq l n)

ahArg :: Env Arg -> Arg -> Result Arg
ahArg env0 arg =
  case arg of
    Var name ->
      case Env.lookupEnv name env0 of
        Just home -> pure home
        Nothing   -> Left ("assignHomes: unbound variable " ++ show name)
    _ -> pure arg
