module X86Passes.PatchInstructions
  ( PIPass(..)
  , patchInstructions
  ) where

import CompilerPasses (Result)
import X86Passes.X86b

newtype PIPass = PIPass X86b
  deriving (Eq, Show)

patchInstructions :: X86b -> Result PIPass
patchInstructions (Program blocks) = do
  blocks' <- mapM piBlock blocks
  pure (PIPass (Program blocks'))

piBlock :: (Label, Block) -> Result (Label, Block)
piBlock (lbl, Block instrs) =
  pure (lbl, Block (concatMap piInstr instrs))

piInstr :: Instr -> [Instr]
piInstr ins =
  case ins of
    Movq src dst | src == dst -> []
    Movq (Mem r1 o1) (Mem r2 o2) ->
      [Movq (Mem r1 o1) (Reg RAX), Movq (Reg RAX) (Mem r2 o2)]
    Movq src dst -> [Movq src dst]
    Addq (Mem r1 o1) (Mem r2 o2) ->
      [Movq (Mem r1 o1) (Reg RAX), Addq (Reg RAX) (Mem r2 o2)]
    Subq (Mem r1 o1) (Mem r2 o2) ->
      [Movq (Mem r1 o1) (Reg RAX), Subq (Reg RAX) (Mem r2 o2)]
    Addq a b  -> [Addq a b]
    Subq a b  -> [Subq a b]
    Negq a    -> [Negq a]
    Pushq a   -> [Pushq a]
    Popq a    -> [Popq a]
    Callq l n -> [Callq l n]
    Jmp l     -> [Jmp l]
    Retq      -> [Retq]
