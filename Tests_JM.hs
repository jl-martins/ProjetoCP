module Tests_JM where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

-- Grafo já existente em Tests.hs . Coloquei aqui só para o poder usar
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

g2 :: Graph Int
g2 = Graph {nodes = fromList [1,2], 
            edges = fromList [Edge 1 1, Edge 1 2]}

forest :: Graph Int
forest = Graph {nodes = fromList [1,2,3,4,5,6],
                edges = fromList [Edge 1 3, Edge 3 2, Edge 4 3, Edge 5 6]}

{- Ao acrescentarmos a aresta (5,1) às arestas de 'forest', o vértice 5 passa a ter dois
   vértices adjacentes (os vértices 1 e 6), pelo que o grafo resultante não é uma floresta -}
g3 :: Graph Int
g3 = Graph {nodes = nodes forest,
                   edges = edges forest `Set.union` (fromList [Edge 5 1]) }

-- Testes
test_isEmpty :: Test
test_isEmpty = TestList [TestCase $ assertBool "" (isEmpty Graph.empty), 
                         isEmpty g1 ~?= False]


test_isForest :: Test
test_isForest = TestList [TestCase $ assertBool "" (isForest forest),
                          isForest g1 ~?= False, -- não é um DAG
                          isForest g3 ~?= False] -- o vértice 5 tem 2 vértices adjacentes

-- g1 é subrafo de g2 mas g2 não é subrafo de g1
test_isSugraphOf :: Test
test_isSugraphOf = TestList [isSubgraphOf g1 g2 ~?= True,
                             isSubgraphOf g2 g1 ~?= False]
