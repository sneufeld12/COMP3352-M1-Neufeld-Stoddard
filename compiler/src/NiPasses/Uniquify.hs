module NiPasses.Uniquify where

import NiPasses.N1
import NiPasses.Env (Env, makeEnv, lookupEnv, extendEnv)
import CompilerPasses

uniquify :: N1 -> Result N1
uniquify prog =
  case passUniquify prog of
    CState _ (Right p') -> Right p'
    CState _ (Left err) -> Left err

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

{--
  The uniquify pass transforms an N1 program into another
  N1 program, but ensures that every variable name is unique!
--}
passUniquify :: N1 -> CompilerResult UniquifyState N1
passUniquify (Program exp) =
  case uniquifyExp exp (UState makeEnv 0) of
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