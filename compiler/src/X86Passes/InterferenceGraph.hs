module X86Passes.InterferenceGraph
  ( instrInterference
  , interferenceBlock
  , buildInterference
  ) where

import qualified Data.Set as Set
import qualified Data.List as L
import Graph.Graph as G
import X86Passes.X86b
import X86Passes.Liveness (ReadWrite(..), getLocs)
import NiPasses.Env (Env, makeEnv, lookupEnv, extendEnv)

-- | Check if an argument is an immediate (we skip immediates in interference)
isImm :: Arg -> Bool
isImm (Imm _) = True
isImm _       = False

-- | Add interference edges between a destination and all live variables
-- Returns the updated graph with d as a vertex and edges to all applicable live vars
addInterferenceEdges :: Arg -> Set.Set Arg -> (Arg -> Bool) -> G.Graph Arg -> G.Graph Arg
addInterferenceEdges d liveAfter shouldSkip graph =
  let -- Filter out args we should skip
      liveVars = Set.filter (\v -> not (isImm v) && not (shouldSkip v)) liveAfter
  in -- Add undirected edges from d to all remaining live vars
     Set.foldl' (\g v -> G.addUndirectedEdge d v g) graph liveVars

-- | Compute interference from a single instruction
-- Takes an instruction, its live-after set, and the current graph
-- Returns the updated graph
instrInterference :: Instr -> Set.Set Arg -> G.Graph Arg -> G.Graph Arg
instrInterference instr liveAfter graph =
  case instr of
    -- Special case for Movq: don't add edge between d and s (source)
    Movq s d ->
      if isImm d
        then graph  -- Don't create vertices for immediates
        else
          -- For Movq, skip if v == d OR v == s (the movq optimization)
          let shouldSkip v = v == d || v == s
              liveVars = Set.filter (\v -> not (isImm v) && not (shouldSkip v)) liveAfter
          in if Set.null liveVars
               then graph  -- Don't add vertex if no edges (test expects G.empty)
               else addInterferenceEdges d liveAfter shouldSkip graph

    -- All other instructions: add edges between each written location and live vars
    _ ->
      let writeS = writeSet (getLocs instr)
          -- Filter out immediates from write set
          writes = Set.filter (not . isImm) writeS
      in Set.foldl' (\g d ->
           -- Only add vertex if there are edges to add
           let shouldSkip v = v == d
               liveVars = Set.filter (\v -> not (isImm v) && not (shouldSkip v)) liveAfter
           in if Set.null liveVars
                then g  -- No edges to add, don't add isolated vertex
                else addInterferenceEdges d liveAfter shouldSkip g
         ) graph writes

-- | Compute interference for an entire block
-- Takes a Block, the corresponding list of live-after sets, and current graph
-- Returns the updated graph
interferenceBlock :: Block -> [Set.Set Arg] -> G.Graph Arg -> G.Graph Arg
interferenceBlock (Block instrs) liveAfterSets graph =
  -- Pair each instruction with its live-after set and fold
  L.foldl' (\g (instr, liveAfter) -> instrInterference instr liveAfter g)
           graph
           (zip instrs liveAfterSets)

-- | Build interference graphs for all blocks in a program
-- Takes an X86b program and an Env mapping labels to live-after set lists
-- Returns an Env mapping labels to interference graphs
buildInterference :: X86b -> Env [Set.Set Arg] -> Env (G.Graph Arg)
buildInterference (Program blocks) liveSets =
  L.foldl' processBlock makeEnv blocks
  where
    processBlock graphsEnv (Label lbl, blk) =
      -- The liveness module returns [entry_set, live_after_0, live_after_1, ...]
      -- We need to skip the first element (entry set) and drop the trailing empty
      let liveAfts = case lookupEnv lbl liveSets of
                       Just sets -> drop 1 (init sets)  -- Skip entry, drop trailing {}
                       Nothing   -> []
          g = interferenceBlock blk liveAfts G.empty
      in extendEnv lbl g graphsEnv
