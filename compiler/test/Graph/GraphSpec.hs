module Graph.GraphSpec (spec) where

import Test.Hspec
import Graph.Graph as G
import X86Passes.X86b
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sort)

-- Helper to normalize graph output for order-independent comparison
-- Sorts both the outer list by vertex and inner edge lists
normalizeGraph :: (Ord v) => [(v, [v])] -> [(v, [v])]
normalizeGraph = sort . map (\(v, es) -> (v, sort es))

spec :: Spec
spec = do
  describe "Graph Tests:" $ do
    it "can make an empty graph" $ do
      G.empty `shouldBe` (G.Graph HM.empty :: G.Graph Int)
    it "can return an empty list from an empty graph" $ do
      toList G.empty `shouldBe` ([] :: [(Int,[Int])])
    it "can add a vertex to an empty list" $ do
      toList (addVertex (Reg RAX) [] G.empty) `shouldBe` [(Reg RAX,[])]
    it "can add a vertex with edges to an empty graph" $ do
      normalizeGraph (toList (addVertex (Reg RAX) [Var "a",Var "b"] G.empty))
        `shouldBe` normalizeGraph [(Reg RAX,[Var "a",Var "b"])]
    it "can convert to and from lists" $ do
      toList (fromList [(Reg RAX, [Var "a"])]) `shouldBe` [(Reg RAX, [Var "a"])]
    it "can add vertex to another graph" $ do
      normalizeGraph (toList (fromList [(Var "b", [Var "a",Var "c"]), (Var "c", [Var "a"])]))
        `shouldBe` normalizeGraph [(Var "b", [Var "a",Var "c"]), (Var "c", [Var "a"])]
    it "can add an edge to an empty graph" $ do
      toList (addEdge (Var "a") (Var "b") G.empty) `shouldBe` [(Var "a", [Var "b"])]
    it "can add an edge to an existing graph" $ do
      normalizeGraph (toList (addEdge (Var "a") (Var "b") (fromList [(Var "a", [Var "c"])])))
        `shouldBe` normalizeGraph [(Var "a", [Var "b", Var "c"])]
    it "can add an undirected edge to an existing graph" $ do
      normalizeGraph (toList (addUndirectedEdge (Var "b") (Var "c") (fromList [(Var "a", [Var "b"])])))
        `shouldBe` normalizeGraph [(Var "a", [Var "b"]),(Var "b", [Var "c"]),(Var "c", [Var "b"])]
    it "can add an undirected edge to an empty graph" $ do
      normalizeGraph (toList (addUndirectedEdge (Var "a") (Var "b") G.empty))
        `shouldBe` normalizeGraph [(Var "a", [Var "b"]),(Var "b", [Var "a"])]
    it "can make an undirected graph from a list" $ do
      normalizeGraph (toList (fromListUndirected [(Var "a", [Var "b", Var "c"])]))
        `shouldBe` normalizeGraph [(Var "a", [Var "b", Var "c"]),(Var "b", [Var "a"]),(Var "c", [Var "a"])]
    it "can query an edge" $ do
      hasEdge (Var "c") (Var "a") (fromListUndirected [(Var "a", [Var "b", Var "c"])])
        `shouldBe` True
    it "can query an edge" $ do
      hasEdge (Var "c") (Var "b") (fromListUndirected [(Var "a", [Var "b", Var "c"])])
        `shouldBe` False
    it "can query an vertex (true)" $ do
      hasVertex (Var "c") (fromListUndirected [(Var "a", [Var "b", Var "c"])])
        `shouldBe` True
    it "can query an vertex (false)" $ do
      hasVertex (Var "d") (fromListUndirected [(Var "a", [Var "b", Var "c"])])
        `shouldBe` False
