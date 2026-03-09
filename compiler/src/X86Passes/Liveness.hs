module X86Passes.Liveness
  ( ReadWrite(..)
  , SrcDst(..)
  , getRWs
  , getLocs
  , liveAfter
  , liveBefore
  , liveSets
  , uncoverLiveBlock
  , uncoverLive
  ) where

import qualified Data.Set as Set
import NiPasses.Env (Env(..), makeEnv, lookupEnv, extendEnv, fromList)
import X86Passes.X86b

-- Read/Write Sets 
data ReadWrite = RW
  { readSet  :: Set.Set Arg
  , writeSet :: Set.Set Arg
  } deriving (Eq, Show)

instance Semigroup ReadWrite where
  RW r1 w1 <> RW r2 w2 = RW (Set.union r1 r2) (Set.union w1 w2)

instance Monoid ReadWrite where
  mempty = RW Set.empty Set.empty

-- Argument Role
data SrcDst = Src | Dst | SrcDst deriving (Eq, Show)

getRWs :: SrcDst -> Arg -> ReadWrite
getRWs _    (Imm _) = mempty
getRWs role arg     =
  case role of
    Src    -> RW (Set.singleton arg) Set.empty
    Dst    -> RW Set.empty           (Set.singleton arg)
    SrcDst -> RW (Set.singleton arg) (Set.singleton arg)

-- Per-Instruction Read/Write 
argRegs :: [Reg]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

callerSaved :: [Reg]
callerSaved = [RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11]

getLocs :: Instr -> ReadWrite
getLocs instr =
  case instr of
    Movq src dst ->
      getRWs Src src <> getRWs Dst dst

    Addq src dst ->
      getRWs Src src <> getRWs SrcDst dst

    Subq src dst ->
      getRWs Src src <> getRWs SrcDst dst

    Negq dst ->
      getRWs SrcDst dst

    -- Pushq reads RSP and src, writes RSP
    Pushq src ->
      RW (Set.fromList [Reg RSP, src]) (Set.singleton (Reg RSP))

    -- Popq reads RSP, writes RSP and dst
    Popq dst ->
      RW (Set.singleton (Reg RSP)) (Set.fromList [Reg RSP, dst])

    -- Retq reads and writes RSP
    Retq ->
      RW (Set.singleton (Reg RSP)) (Set.singleton (Reg RSP))

    -- Jmp does not read or write any locations
    Jmp _ ->
      mempty

    Callq _ k ->
      let readArgs  = fmap Reg (take (fromIntegral k) argRegs)
          writeArgs = fmap Reg callerSaved
      in RW (Set.fromList readArgs) (Set.fromList writeArgs)

-- Liveness 
liveAfter :: Instr -> Set.Set Arg -> Set.Set Arg
liveAfter _ after = after

liveBefore :: Env [Set.Set Arg]
           -> Instr
           -> Set.Set Arg
           -> Set.Set Arg
liveBefore env instr after =
  case instr of
    Jmp (Label lbl) ->
      case lookupEnv lbl env of
        Just (entrySet : _) -> entrySet
        _                   -> Set.empty
    _ ->
      let RW r w = getLocs instr
      in Set.union r (Set.difference after w)

liveSets :: Env [Set.Set Arg]
         -> [Instr]        -- instructions in reverse order
         -> Set.Set Arg    
         -> [Set.Set Arg]  
liveSets _   []             after = [after]
liveSets env (instr:instrs) after =
  let before = liveBefore env instr after
      rest   = liveSets env instrs before
  in after : rest

uncoverLiveBlock :: Env [Set.Set Arg]
                 -> Block
                 -> [Set.Set Arg]
uncoverLiveBlock env (Block instrs) =
  let reversed = reverse instrs
      sets     = liveSets env reversed Set.empty
  in reverse (tail sets) ++ [Set.empty]

getEntrySet :: Env [Set.Set Arg] -> Block -> Set.Set Arg
getEntrySet env (Block instrs) =
  let reversed = reverse instrs
      sets     = liveSets env reversed Set.empty
  in last sets

uncoverLive :: X86b
            -> [(Label, Block, [Set.Set Arg])]
uncoverLive (Program blocks) =
  snd $ foldl processBlock (makeEnv, []) blocks
  where
    processBlock (env, acc) (lbl@(Label name), blk) =
      let sets     = uncoverLiveBlock env blk
          entry    = getEntrySet env blk
          env'     = extendEnv name (entry : sets) env
          acc'     = acc ++ [(lbl, blk, sets)]
      in (env', acc')