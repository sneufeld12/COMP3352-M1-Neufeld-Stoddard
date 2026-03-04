module X86Passes.X86bPasses
  ( SIPass(..)
  , instructionSelection
  , atmToArg
  , selectStmt
  , selectTail
  ) where

import CompilerPasses (Result)
import qualified CPasses.C0 as C0
import X86Passes.X86b

newtype SIPass = SIPass (X86b, [String])
  deriving (Eq, Show)

-- Convert C0 atom to X86b argument
atmToArg :: C0.Atom -> Arg
atmToArg (C0.Int n) = Imm n
atmToArg (C0.Var s) = Var s

-- Translate a C0 expression into x86 instructions, given a destination argument
selectStmt :: C0.Exp -> Arg -> [Instr]
selectStmt expr dst =
  case expr of
    C0.Atm atom -> [Movq (atmToArg atom) dst]
    C0.Read -> [Callq (Label "read_int") 0, Movq (Reg RAX) dst]
    C0.Sub atom -> [Movq (atmToArg atom) dst, Negq dst]
    C0.Add a1 a2 -> [Movq (atmToArg a1) dst, Addq (atmToArg a2) dst]

-- Walk the C0 Tail (linked list of Seq/Return)
selectTail :: C0.Tail -> [Instr]
selectTail tl =
  case tl of
    C0.Return expr -> selectStmt expr (Reg RAX) ++ [Jmp (Label "conclusion")]
    C0.Seq (C0.Assign sym expr) ctail -> selectStmt expr (Var sym) ++ selectTail ctail

-- Top-level instruction selection
instructionSelection :: C0.C0 -> [String] -> Result SIPass
instructionSelection (C0.Program blocks) lcls = do
  blocks' <- mapM selectBlock blocks
  Right $ SIPass (Program blocks', lcls)
  where
    selectBlock :: (String, C0.Tail) -> Result (Label, Block)
    selectBlock (lbl, tl) = Right (Label lbl, Block (selectTail tl))
