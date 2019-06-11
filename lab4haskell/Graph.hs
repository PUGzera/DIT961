module Graph 
  ( Graph (Graph), Edge(..)
  , empty
  , addVertex, addVertices, addEdge, addBiEdge
  , adj, vertices, edges, v, e
  ) where

import Data.Map (Map, toList)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.PQueue.Min as PQ

data Edge a b = Edge
  { src   :: a
  , dst   :: a
  , label :: b
  } deriving Ord

instance (Show a, Show b) => Show (Edge a b) where
  show (Edge s d l) = concat ["(", show s, " - ", show l, " -> ", show d, ")"]

instance Eq a => Eq (Edge a b) where
  v == w = src v == src w && dst v == dst w

data Graph a b = Graph { adjMap :: Map a [Edge a b] } deriving (Show, Eq)

empty :: Graph a b
empty = Graph M.empty

alterMap :: (Map a [Edge a b] -> Map a [Edge a b]) -> Graph a b -> Graph a b
alterMap f g = g { adjMap = f (adjMap g) }

addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v g 
  | v `M.member` adjMap g = g
  | otherwise             = alterMap (M.insert v []) g

addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices vs g = foldr addVertex g vs

addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l = alterMap (M.update (Just . (Edge v w l :)) v)

addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l = addEdge v w l . addEdge w v l

adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v = fromMaybe [] . M.lookup v . adjMap

vertices :: Graph a b -> [a]
vertices = M.keys . adjMap

edges :: Graph a b -> [(a, a, b)]
edges = map (\(Edge v w l) -> (v, w, l)) . concat . M.elems . adjMap

v :: Graph a b -> Int
v = length . vertices

e :: Graph a b -> Int
e = length . edges

-- Test

g :: Graph Int Char
g = addEdge 1 2 'a'
  $ addEdge 1 3 'b'
  $ addEdge 3 4 'b'
  $ addEdge 3 6 'b'
  $ addEdge 4 2 'b'
  $ addEdge 4 6 'b'
  $ addEdge 5 2 'b'
  $ addEdge 5 4 'b'
  $ addEdge 5 7 'b'
  $ addEdge 7 6 'b'
  $ foldr addVertex empty [1..7]

