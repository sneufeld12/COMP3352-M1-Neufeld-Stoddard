-- Lab 04: Partial Evaluation (N1 -> N1)
--
-- IMPORTANT:
-- Lab 04 is about N1, NOT C0.
-- N1 has its own AST (defined below), its own module (this file),
-- and NO interpreter machinery in this skeleton code.
--
-- Do NOT add: Env, IO, Either, eval*, run*, or imports from InterpreterC0.
-- This is a compile-time AST rewrite pass only.

module NiPasses.PartialEvaluator
  ( N1(..)
  , Exp(..)
  , partialEvaluator
  ) where

--------------------------------------------------------------------------------
-- N1 AST (do not change)
--------------------------------------------------------------------------------

data Exp
  = IntLit Int
  | Var String
  | Read
  | Add Exp Exp
  | Sub Exp Exp
  | Let String Exp Exp
  deriving (Eq, Show)

newtype N1 = Program Exp
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- TODO 1 (REQUIRED): Implement the top-level pass
--------------------------------------------------------------------------------
-- Requirement:
--   partialEvaluator (Program e) must return Program (peExp e)
partialEvaluator :: N1 -> N1
partialEvaluator (Program e) = Program (peExp e)

--------------------------------------------------------------------------------
-- TODO 2 (REQUIRED): Implement peExp (recursive traversal)
--------------------------------------------------------------------------------
-- Rules:
--   * recurse into subexpressions first
--   * fold Add/Sub only when BOTH operands are IntLit
--   * never fold Var or Read
--   * Let: simplify rhs and body, but do NOT substitute variables
peExp :: Exp -> Exp
peExp (IntLit n) = IntLit n
peExp (Var x)    = Var x
peExp Read       = Read

peExp (Add e1 e2) =
  let e1' = peExp e1
      e2' = peExp e2
  in foldAdd e1' e2'

peExp (Sub e1 e2) =
  let e1' = peExp e1
      e2' = peExp e2
  in foldSub e1' e2'

peExp (Let x rhs body) =
  Let x (peExp rhs) (peExp body)

--------------------------------------------------------------------------------
-- TODO 3 (REQUIRED): Implement folding helpers
--------------------------------------------------------------------------------

foldAdd :: Exp -> Exp -> Exp
foldAdd (IntLit a) (IntLit b) = IntLit (a + b)
foldAdd e1 e2 = Add e1 e2

foldSub :: Exp -> Exp -> Exp
foldSub (IntLit a) (IntLit b) = IntLit (a - b)
foldSub e1 e2 = Sub e1 e2

