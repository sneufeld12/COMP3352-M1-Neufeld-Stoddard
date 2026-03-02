module ComposePasses where

import NiPasses.Uniquify
import NiPasses.RemoveComplexOperas
import NiPasses.N1
import NiPasses.PartialEvaluator (partialEvaluator)
import CompilerPasses

-- Pass Composition
type Pass a = a -> Result a

(>>>) :: Pass a -> Pass a -> Pass a
p1 >>> p2 = \input ->
  case p1 input of
    Left err -> Left err
    Right out -> p2 out

-- Ni pipeline
allPasses :: N1 -> Result N1
allPasses =
      uniquify
  -- >>> partialEvaluator
  >>> removeComplexOperas

