module NiPasses.RemoveComplexOperas where

import NiPasses.N1
import NiPasses.Env
import CompilerPasses

removeComplexOperas :: N1 -> Result N1
removeComplexOperas prog =
  case passRemoveComplexOperas (CState 0 (Right prog)) of
    CState _ (Right p') -> Right p'
    CState _ (Left err) -> Left err

getRCOResult :: CompilerResult RCOState N1 -> Result N1
getRCOResult (CState _ (Right p)) = Right p
getRCOResult (CState _ err)       = err

type RCOState = Integer
type RCOResult = CompilerResult RCOState Exp
type AtmResult = CompilerResult RCOState (Exp, [(String, Exp)])

freshName :: RCOState -> (String, RCOState)
freshName n = ("s" ++ show n, n + 1)

wrapLets :: [(String, Exp)] -> Exp -> Exp
wrapLets [] e = e
wrapLets ((name, expr):rest) e = Let name expr (wrapLets rest e)

passRemoveComplexOperas :: CompilerResult RCOState N1 -> CompilerResult RCOState N1
passRemoveComplexOperas (CState _ (Left msg)) = CState 0 (Left msg)
passRemoveComplexOperas (CState symCount (Right (Program expr))) =
  case rcoExp (CState symCount (Right expr)) of
    CState symCount' (Right expr') -> CState symCount' (Right (Program expr'))
    CState symCount' (Left msg)    -> CState symCount' (Left msg)

rcoExp :: RCOResult -> RCOResult
rcoExp (CState n (Left msg)) = CState n (Left msg)
rcoExp res@(CState _ (Right (Int _))) = res
rcoExp res@(CState _ (Right Read)) = res
rcoExp res@(CState _ (Right (Var _))) = res
rcoExp (CState n (Right (Let sym expr body))) =
  case rcoExp (CState n (Right expr)) of
    CState n1 (Left msg) -> CState n1 (Left msg)
    CState n1 (Right expr') ->
      case rcoExp (CState n1 (Right body)) of
        CState n2 (Left msg) -> CState n2 (Left msg)
        CState n2 (Right body') -> CState n2 (Right (Let sym expr' body'))
rcoExp (CState n (Right (Negate expr))) =
  case rcoAtm (CState n (Right (expr, []))) of
    CState n1 (Left msg) -> CState n1 (Left msg)
    CState n1 (Right (atomExpr, bindings)) ->
      CState n1 (Right (wrapLets bindings (Negate atomExpr)))
rcoExp (CState n (Right (Add e1 e2))) =
  case rcoAtm (CState n (Right (e1, []))) of
    CState n1 (Left msg) -> CState n1 (Left msg)
    CState n1 (Right (atom1, bindings1)) ->
      case rcoAtm (CState n1 (Right (e2, []))) of
        CState n2 (Left msg) -> CState n2 (Left msg)
        CState n2 (Right (atom2, bindings2)) ->
          CState n2 (Right (wrapLets (bindings1 ++ bindings2) (Add atom1 atom2)))

rcoAtm :: AtmResult -> AtmResult
rcoAtm (CState n (Left msg)) = CState n (Left msg)
rcoAtm res@(CState _ (Right (Int _, _))) = res
rcoAtm res@(CState _ (Right (Var _, _))) = res
rcoAtm (CState n (Right (Read, bindings))) =
  let (name, n') = freshName n
  in CState n' (Right (Var name, bindings ++ [(name, Read)]))
rcoAtm (CState n (Right (Negate expr, bindings))) =
  case rcoAtm (CState n (Right (expr, []))) of
    CState n1 (Left msg) -> CState n1 (Left msg)
    CState n1 (Right (atomExpr, innerBindings)) ->
      let (name, n2) = freshName n1
          negateExpr = Negate atomExpr
      in CState n2 (Right (Var name, bindings ++ innerBindings ++ [(name, negateExpr)]))
rcoAtm (CState n (Right (Add e1 e2, bindings))) =
  case rcoAtm (CState n (Right (e1, []))) of
    CState n1 (Left msg) -> CState n1 (Left msg)
    CState n1 (Right (atom1, bindings1)) ->
      case rcoAtm (CState n1 (Right (e2, []))) of
        CState n2 (Left msg) -> CState n2 (Left msg)
        CState n2 (Right (atom2, bindings2)) ->
          let (name, n3) = freshName n2
              addExpr = Add atom1 atom2
          in CState n3 (Right (Var name, bindings ++ bindings1 ++ bindings2 ++ [(name, addExpr)]))
rcoAtm (CState n (Right (Let sym expr body, bindings))) =
  case rcoExp (CState n (Right (Let sym expr body))) of
    CState n1 (Left msg) -> CState n1 (Left msg)
    CState n1 (Right letExpr) ->
      let (name, n2) = freshName n1
      in CState n2 (Right (Var name, bindings ++ [(name, letExpr)]))
