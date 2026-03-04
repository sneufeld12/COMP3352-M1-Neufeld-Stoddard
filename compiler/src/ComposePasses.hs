module ComposePasses where

import NiPasses.N1 (N1)
import NiPasses.Uniquify (uniquify)
import NiPasses.RemoveComplexOperas (removeComplexOperas)
import CPasses.C0Passes (explicateControl, ECPass(..))
import X86Passes.X86bPasses (instructionSelection, SIPass(..))
import X86Passes.AssignHomes (assignHomesPass, AHPass(..))
import X86Passes.PatchInstructions (patchInstructions, PIPass(..))
import X86Passes.X86b (X86b)
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
