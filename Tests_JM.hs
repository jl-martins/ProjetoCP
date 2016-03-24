module Tests_JM where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

g2 :: Graph Int
g2 = Graph {nodes = fromList [1,2], 
            edges = fromList [Edge 1 1, Edge 1 2]}

forest1 :: Graph Int
forest1 = Graph {nodes = fromList [1,2,3,4,5,6],
                 edges = fromList [Edge 1 3, Edge 3 2, Edge 4 3, Edge 5 6]}

forest2 :: Graph Int
forest2 = Graph {nodes = nodes forest1,
                 edges = edges forest1 `Set.union` (Edge 5 1)}

forest3 :: Graph Int
forest3 = Graph 

test_isEmpty :: Test
test_isEmpty = assertBool "" (isEmpty Graph.empty)

test1_isForest :: Test
test1_isForest = isForest forest1 ~?= True

test2_isForest :: Test
test2_isForest = isForest g1 ~?= False -- falha a condição "isDAG"

test1_isSugraphOf :: Test
test1_isSugraphOf = isSubgraphOf g1 g2 ~?= True

test2_isSugraphOf :: Test
test2_isSugraphOf = isSubgraphOf g2 g1 ~?= False
