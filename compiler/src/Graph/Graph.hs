module Graph.Graph where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Data.Hashable
import qualified Data.List as L
import Data.Bifunctor (second)

newtype Graph v = Graph { vertices :: HM.HashMap v (HS.HashSet v) } deriving (Eq)

instance (Eq v, Hashable v, Show v) => Show (Graph v) where
  show g = "Graph.fromList " ++ show (toList g)

empty :: Graph v
empty = Graph { vertices = HM.empty }

addVertex :: (Eq v, Hashable v) => v -> [v] -> Graph v -> Graph v
addVertex vertex edgelist g@(Graph vertices) =
  case getVertexEdges vertex g of
    Just edges ->
      if null edgelist then g
      else Graph $ HM.insert vertex (edges `HS.union` HS.fromList edgelist) vertices
    Nothing -> Graph $ HM.insert vertex (HS.fromList edgelist) vertices

deleteVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
deleteVertex vertex (Graph vertices) =
  Graph $ HM.delete vertex vertices

deleteEdge ::  (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteEdge fromVertex toVertex (Graph vertices) =
  Graph $ HM.adjust (HS.delete toVertex) fromVertex vertices

deleteUndirectedEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteUndirectedEdge fromVertex toVertex graph =
  deleteEdge toVertex fromVertex $ deleteEdge fromVertex toVertex graph

replaceVertex :: (Eq v, Hashable v) => v -> [v] -> Graph v -> Graph v
replaceVertex vertex edgelist graph =
  addVertex vertex edgelist $ deleteVertex vertex graph

addEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
addEdge fromVertex toVertex (Graph vertices) =
  case HM.lookup fromVertex vertices of
    Nothing -> Graph $ HM.insert fromVertex (HS.singleton toVertex) vertices
    Just edges ->
      Graph $ HM.insert fromVertex (HS.insert toVertex edges) vertices

addUndirectedEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
addUndirectedEdge fromVertex toVertex g =
  addEdge toVertex fromVertex (addEdge fromVertex toVertex g)

hasVertex :: (Eq v, Hashable v) => v -> Graph v -> Bool
hasVertex vertex (Graph vertices) = HM.member vertex vertices

hasEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Bool
hasEdge fromVertex toVertex graph =
  case getVertexEdges fromVertex graph of
    Just edges -> HS.member toVertex edges
    Nothing -> False

getVertices :: (Eq v, Hashable v) => Graph v -> [v]
getVertices (Graph vertices) = HM.keys vertices

filterVertices :: (Eq v, Hashable v) => (v -> Bool) -> Graph v -> [(v, HS.HashSet v)]
filterVertices f (Graph vertices) =
  L.filter (\(v, _) -> f v) (HM.toList vertices)

getVertexEdges :: (Eq v, Hashable v) => v -> Graph v -> Maybe (HS.HashSet v)
getVertexEdges v (Graph vertices) = HM.lookup v vertices

fromList :: (Eq v, Hashable v) => [(v, [v])] -> Graph v
fromList [] = empty
fromList lst = L.foldl' (\acc (v, es) -> addVertex v es acc) empty lst

fromListUndirected :: (Eq v, Hashable v) => [(v, [v])] -> Graph v
fromListUndirected [] = empty
fromListUndirected lst =
  L.foldl' (\g (v, es) ->
    L.foldl' (flip (addUndirectedEdge v)) g es) empty lst

toList :: (Eq v, Hashable v) => Graph v -> [(v, [v])]
toList (Graph vertices) =
  let lst = HM.toList vertices in
    map (Data.Bifunctor.second HS.toList) lst
