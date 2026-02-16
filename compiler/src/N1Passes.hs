module N1Passes where

import N1
import Env (Env, makeEnv, lookupEnv, extendEnv)
import CompilerPasses

-- this is the state we'll use for the uniquefy pass, it has
-- an environment which we'll adjust as we enter let expressions
-- and an Integer which will be used for generating symbol names
data UniquifyState = UState (Env String) Integer

-- a uniquify result is a compiler result with our state and an N1 Exp
type UniquifyResult = CompilerResult UniquifyState Exp

-- a helper function to pull out the result from the compiler result
getResult :: CompilerResult UniquifyState N1 -> Result N1
getResult (CState _ (Right p)) = Right p
getResult (CState _ err)       = err

getRCOResult :: CompilerResult RCOState N1 -> Result N1
getRCOResult (CState _ (Right p)) = Right p
getRCOResult (CState _ err)       = err

{--
  The uniquify pass transforms an N1 program into another
  N1 program, but ensures that every variable name is unique!
--}
uniquify :: N1 -> CompilerResult UniquifyState N1
uniquify (Program exp) =
  case uniquifyExp exp (UState Env.makeEnv 0) of
    CState state (Right exp') -> CState state (Right (Program exp'))
    CState state (Left msg)   -> CState state (Left msg)

{--
  uniquifyExp transforms an expression in N1 to another
  expression in N1 where the variables have been renamed. We
  pass UniquifyState along with the Exp and get a result. Note
  that this result also contains a UniquifyState, which allows
  us to update it from calls down further in the AST as we
  recurse through it.
--}
uniquifyExp :: Exp -> UniquifyState -> UniquifyResult

-- uniquifying an Int is simple, it doesn't change the state,
-- and just returns the int expression
uniquifyExp v@(Int _) state =
  CState state (Right v)

-- uniquifying Read is also straightforward, the state doesn't change
uniquifyExp r@Read state =
  CState state (Right r)

-- uniquifying a Negate requires uniquifying its subexpression
uniquifyExp (Negate exp) state =
  case uniquifyExp exp state of
    CState state' (Right res) -> CState state' (Right (Negate res))
    err                       -> err

uniquifyExp (Add x y) state =
  case uniquifyExp x state of
    CState state1 (Right x') ->
      case uniquifyExp y state1 of
        CState state2 (Right y') -> CState state2 (Right (Add x' y'))
        CState state2 (Left msg) -> CState state2 (Left msg)
    CState state1 (Left msg) -> CState state1 (Left msg)

uniquifyExp (Var sym) state@(UState env _) =
  case lookupEnv sym env of
    Just sym' -> CState state (Right (Var sym'))
    Nothing   -> CState state (Left ("Symbol '" ++ sym ++ "' not found"))

uniquifyExp (Let sym expr body) (UState env n) =
  let newName = "s" ++ show n
      stateForExpr = UState env (n + 1)  -- sym not visible in expr, but counter incremented
  in case uniquifyExp expr stateForExpr of
    CState (UState _ n') (Right expr') ->
      let env' = extendEnv sym newName env
          stateForBody = UState env' n'
      in case uniquifyExp body stateForBody of
           CState stateFinal (Right body') ->
             CState stateFinal (Right (Let newName expr' body'))
           CState stateFinal (Left msg) ->
             CState stateFinal (Left msg)

    CState state1 (Left msg) ->
      CState state1 (Left msg)

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
