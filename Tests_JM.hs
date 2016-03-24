module Tests_JM where

import Graph
import Test.HUnit hiding (Path, path)
import Test.QuickCheck
import Data.Set as Set


-- Grafos usados nos testes unitários

-- Grafo já existente em Tests.hs . Coloquei aqui só para o poder usar
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

g2 :: Graph Int
g2 = Graph {nodes = fromList [1,2], 
            edges = fromList [Edge 1 1, Edge 1 2]
           }

forest :: Graph Int
forest = Graph {nodes = fromList [1,2,3,4,5,6],
                edges = fromList [Edge 1 3, Edge 3 2, Edge 4 3, Edge 5 6]
               }

{- Ao acrescentarmos a aresta (5,1) às arestas de 'forest', o vértice 5 passa a ter dois
   vértices adjacentes (os vértices 1 e 6), pelo que o grafo resultante não é uma floresta -}
g3 :: Graph Int
g3 = Graph {nodes = nodes forest,
            edges = edges forest `Set.union` (fromList [Edge 5 1])
           }

g4 :: Graph Int
g4 = Graph {nodes = nodes g2,
            edges = fromList [Edge 2 1]
           }

union_g4_g1 :: Graph Int
union_g4_g1 = Graph {nodes = fromList [1,2],
                     edges = fromList [Edge 1 1, Edge 2 1]
                    }

nodesGLarge :: Set Int
nodesGLarge = fromList [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

edgesGLarge :: Set (Edge Int)
edgesGLarge = fromList [Edge 1 1, Edge 1 3, Edge 2 1, Edge 3 2, Edge 3 5, Edge 4 1, Edge 4 2, 
                        Edge 4 12, Edge 4 13, Edge 5 6, Edge 5 8, Edge 6 7, Edge 6 8, Edge 6 10, 
                        Edge 7 10, Edge 8 9, Edge 8 10, Edge 9 11, Edge 10 9, Edge 10 11, Edge 10 14, 
                        Edge 11 12, Edge 11 14, Edge 12 13, Edge 13 11, Edge 13 15, Edge 14 13, Edge 15 14]

-- http://www.greatandlittle.com/studios/public/blowup-images/Dart/.directed_graph_m.jpg
gLarge :: Graph Int
gLarge = Graph {nodes = nodesGLarge, edges = edgesGLarge}

-- fim dos grafos usados nos testes unitários

-- Testes unitários
main = runTestTT tests

tests :: Test
tests = TestList [test_isEmpty, test_isForest, test_isSugraphOf, test_union, test_reachable, test_path]

test_isEmpty :: Test
test_isEmpty = TestList [TestCase $ assertBool "" (isEmpty Graph.empty), 
                         isEmpty g1 ~?= False
                        ]


test_isForest :: Test
test_isForest = TestList [TestCase $ assertBool "" (isForest forest),
                          isForest g1 ~?= False, -- não é um DAG
                          isForest g3 ~?= False -- o vértice 5 tem 2 vértices adjacentes
                         ] 

test_isSugraphOf :: Test
test_isSugraphOf = TestList [isSubgraphOf g1 g2 ~?= True,
                             isSubgraphOf (Graph.empty) g1 ~?= True, -- trivialmente válido
                             isSubgraphOf g2 g1 ~?= False,
                             isSubgraphOf g4 g2 ~?= False
                            ]

test_union :: Test
test_union = TestList [g1 `Graph.union` g1 ~?= g1,
                       Graph.empty `Graph.union` g1 ~?= g1,
                       g4 `Graph.union` g1 ~?= union_g4_g1
                      ]

test_reachable :: Test
test_reachable = TestList [reachable g4 1 ~?= fromList [1],
                           reachable forest 1 ~?= fromList [1, 2, 3]
                          ]

test_path :: Test
test_path = TestList [path gLarge 4 7 ~?= pathGLarge4_7,
                      path gLarge 1 4 ~?= Nothing]
    where pathGLarge4_7 :: Maybe (Path Int) -- caminho mais curto de 4 para 7 em gLarge
          pathGLarge4_7 = Just [Edge 4 1, Edge 1 3, Edge 3 5, Edge 5 6, Edge 6 7]

