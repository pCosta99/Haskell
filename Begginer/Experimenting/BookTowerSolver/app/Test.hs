module Test where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree(Gr)
import Data.Graph.Inductive.Query.SP

lNodes :: [LNode String]
lNodes = zip [1..4] ["A","B","C","D"]

lEdges :: [LEdge Int]
lEdges = [(1,2,1),(1,3,1),(2,1,1),(3,4,1),(1,4,10)]

mygraph :: Gr String Int
mygraph = mkGraph lNodes lEdges

adjList :: [(String, Int, [Int])]
adjList = [("node1",1,[2,3]),("node2", 2, [1]),("node3", 3, [4]), ("node4", 4, [])]


