module N1Passes where

import N1
import Env (Env, makeEnv, lookupEnv, extendEnv)
import CompilerPasses

-- State for uniquify: environment + fresh counter
data UniquifyState = UState (Env String) Integer
  deriving (Eq, Show)

-- Extract result from a pass over Program
getResult :: CompilerResult st Program -> Result Program
getResult (CState _ res) = res

-- Top-level uniquify pass
uniquify :: Program -> CompilerResult UniquifyState Program
uniquify (Program e) =
  case uniquifyExp e (UState makeEnv 0) of
    CState st (Right e') -> CState st (Right (Program e'))
    CState st (Left msg) -> CState st (Left msg)

-- Generate a fresh variable name
freshName :: String -> UniquifyState -> (String, UniquifyState)
freshName x (UState env n) =
  let x' = x ++ "_" ++ show n
  in (x', UState env (n + 1))

-- Core uniquify function
uniquifyExp :: Exp -> UniquifyState -> CompilerResult UniquifyState Exp

uniquifyExp (Int n) st =
  CState st (Right (Int n))

uniquifyExp Read st =
  CState st (Right Read)

uniquifyExp (Var x) st@(UState env _) =
  case lookupEnv x env of
    Just x' -> CState st (Right (Var x'))
    Nothing -> CState st (Left ("Unbound variable: " ++ x))

uniquifyExp (Negate e) st =
  case uniquifyExp e st of
    CState st' (Right e') -> CState st' (Right (Negate e'))
    CState st' (Left msg) -> CState st' (Left msg)

uniquifyExp (Add e1 e2) st =
  case uniquifyExp e1 st of
    CState st1 (Right e1') ->
      case uniquifyExp e2 st1 of
        CState st2 (Right e2') -> CState st2 (Right (Add e1' e2'))
        CState st2 (Left msg)  -> CState st2 (Left msg)
    CState st1 (Left msg) -> CState st1 (Left msg)

uniquifyExp (Let x rhs body) st =
  case uniquifyExp rhs st of
    CState (UState env1 n1) (Right rhs') ->
      let (x', UState env1' n2) = freshName x (UState env1 n1)
          env'  = extendEnv x x' env1'
          stBody = UState env' n2
      in case uniquifyExp body stBody of
           CState st' (Right body') -> CState st' (Right (Let x' rhs' body'))
           CState st' (Left msg)    -> CState st' (Left msg)
    CState st1 (Left msg) -> CState st1 (Left msg)

-- Stub for later pass so tests compile
passRemoveComplexOperas :: CompilerResult s Program -> CompilerResult UniquifyState Program
passRemoveComplexOperas = error "removeComplexOperas not implemented yet"