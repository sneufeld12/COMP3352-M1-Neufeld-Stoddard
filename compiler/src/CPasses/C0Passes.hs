module CPasses.C0Passes
  ( ECPass(..)
  , explicateControl
  , explicateTail
  , explicateAssign
  ) where

import CompilerPasses (Result)
import qualified NiPasses.N1 as N1
import CPasses.C0 as C0

data ECPass = ECPass { prog :: C0, locals :: [String] }
  deriving (Eq, Show)

-- Top-level function: convert N1 program to C0 program
explicateControl :: N1.N1 -> Result ECPass
explicateControl (N1.Program expr) =
  case explicateTail expr of
    Right (ctail, lcls) -> Right $ ECPass (Program [("start", ctail)]) lcls
    Left msg -> Left msg

-- Handle expressions in tail position (body of let, or top-level)
explicateTail :: N1.Exp -> Result (Tail, [String])
explicateTail expr =
  case expr of
    N1.Int x -> Right (Return (Atm (C0.Int x)), [])
    N1.Var x -> Right (Return (Atm (C0.Var x)), [])
    N1.Read -> Right (Return C0.Read, [])
    N1.Negate (N1.Int x) -> Right (Return (Sub (C0.Int x)), [])
    N1.Negate (N1.Var x) -> Right (Return (Sub (C0.Var x)), [])
    N1.Negate _ -> Left "explicateTail: Negate of complex expression (should not happen after RCO)"
    N1.Add e1 e2 ->
      case (e1, e2) of
        (N1.Int x, N1.Int y) -> Right (Return (C0.Add (C0.Int x) (C0.Int y)), [])
        (N1.Int x, N1.Var y) -> Right (Return (C0.Add (C0.Int x) (C0.Var y)), [])
        (N1.Var x, N1.Int y) -> Right (Return (C0.Add (C0.Var x) (C0.Int y)), [])
        (N1.Var x, N1.Var y) -> Right (Return (C0.Add (C0.Var x) (C0.Var y)), [])
        _ -> Left "explicateTail: Add operands must be atomic after RCO"
    N1.Let sym rhs body -> do
      (bodyTail, bodyLocals) <- explicateTail body
      (assignTail, assignLocals) <- explicateAssign sym rhs bodyTail
      Right (assignTail, assignLocals ++ bodyLocals)

-- Handle expressions NOT in tail position (RHS of let binding)
explicateAssign :: String -> N1.Exp -> Tail -> Result (Tail, [String])
explicateAssign sym expr ctail =
  case expr of
    N1.Int v -> Right (Seq (Assign sym (Atm (C0.Int v))) ctail, [sym])
    N1.Var v -> Right (Seq (Assign sym (Atm (C0.Var v))) ctail, [sym])
    N1.Read -> Right (Seq (Assign sym C0.Read) ctail, [sym])
    N1.Negate (N1.Int v) -> Right (Seq (Assign sym (Sub (C0.Int v))) ctail, [sym])
    N1.Negate (N1.Var v) -> Right (Seq (Assign sym (Sub (C0.Var v))) ctail, [sym])
    N1.Negate _ -> Left "explicateAssign: Negate of complex expression (should not happen after RCO)"
    N1.Add e1 e2 ->
      case (e1, e2) of
        (N1.Int x, N1.Int y) -> Right (Seq (Assign sym (C0.Add (C0.Int x) (C0.Int y))) ctail, [sym])
        (N1.Int x, N1.Var y) -> Right (Seq (Assign sym (C0.Add (C0.Int x) (C0.Var y))) ctail, [sym])
        (N1.Var x, N1.Int y) -> Right (Seq (Assign sym (C0.Add (C0.Var x) (C0.Int y))) ctail, [sym])
        (N1.Var x, N1.Var y) -> Right (Seq (Assign sym (C0.Add (C0.Var x) (C0.Var y))) ctail, [sym])
        _ -> Left "explicateAssign: Add operands must be atomic after RCO"
    N1.Let innerSym innerExpr innerBody -> do
      -- The inner body's value ultimately gets assigned to sym
      (bodyCont, bodyLocals) <- explicateAssign sym innerBody ctail
      -- The inner RHS gets assigned to innerSym, then continues to bodyCont
      (fullTail, exprLocals) <- explicateAssign innerSym innerExpr bodyCont
      Right (fullTail, exprLocals ++ bodyLocals)
