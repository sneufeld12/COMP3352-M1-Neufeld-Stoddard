module ComposePasses where

import NiPasses.N1 (N1)
import NiPasses.Uniquify (uniquify)
import NiPasses.RemoveComplexOperas (removeComplexOperas)
import CPasses.C0Passes (explicateControl, ECPass(..))
import CPasses.C0 (C0)
import X86Passes.X86bPasses (instructionSelection, SIPass(..))
import X86Passes.AssignHomes (assignHomesPass, AHPass(..))
import X86Passes.PatchInstructions (patchInstructions, PIPass(..))
import X86Passes.X86b
import CompilerPasses (Result)

type Pass a = a -> Result a

(>>>) :: Pass a -> Pass a -> Pass a
p1 >>> p2 = \input ->
  case p1 input of
    Left err -> Left err
    Right out -> p2 out

niPasses :: N1 -> Result N1
niPasses = uniquify >>> removeComplexOperas

allPasses :: N1 -> Result X86b
allPasses ast = do
  n1' <- niPasses ast
  ECPass c0prog lcls <- explicateControl n1'
  SIPass (x86vars, lcls') <- instructionSelection c0prog lcls
  AHPass x86homes _ _ <- assignHomesPass (x86vars, lcls')
  PIPass x86final <- patchInstructions x86homes
  Right x86final

-- ─── Pass wrappers for test pipeline ─────────────────────────────────────────

uniquifyPass :: N1 -> Result N1
uniquifyPass = uniquify

removeComplexOperasPass :: N1 -> Result N1
removeComplexOperasPass = removeComplexOperas

explicateControlPass :: N1 -> Result (C0, [String])
explicateControlPass ast = do
  ECPass c0prog lcls <- explicateControl ast
  Right (c0prog, lcls)

-- Adds prelude and conclusion blocks needed for liveness analysis
prelude :: (Label, Block)
prelude = (Label "_main", Block
    [ Pushq (Reg RBP)
    , Movq (Reg RSP) (Reg RBP)
    , Jmp (Label "start")])

conclusion :: (Label, Block)
conclusion = (Label "conclusion", Block
    [ Popq (Reg RBP)
    , Retq ])

selectInstructionsPass :: (C0, [String]) -> Result (X86b, [String])
selectInstructionsPass (c0prog, lcls) = do
  SIPass (Program blocks, lcls') <- instructionSelection c0prog lcls
  Right (Program $ prelude : conclusion : blocks, lcls')