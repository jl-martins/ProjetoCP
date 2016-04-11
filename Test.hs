--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A75273
-- Nome: João Carlos Mendes Pereira
-- Curso: MIEI
--
-- Aluno 2
-- Número: A68646
-- Nome: João Luís Alves Barreiros Martins
-- Curso: MIEI
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

--
-- Teste unitário
--
    
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

g2 :: Graph Int
g2 = Graph {nodes = fromList [1,2], 
            edges = fromList [Edge 1 1, Edge 1 2]
           }

{- Ao acrescentarmos a aresta (5,1) às arestas de 'forest1', o vértice 5 passa a ter dois
   vértices adjacentes (os vértices 1 e 6), pelo que o grafo resultante não é uma floresta -}
g3 :: Graph Int
g3 = Graph {nodes = nodes forest1,
            edges = edges forest1 `Set.union` (fromList [Edge 5 1])
           }

g4 :: Graph Int
g4 = Graph {nodes = nodes g2,
            edges = fromList [Edge 2 1]
           }

union_g4_g1 :: Graph Int
union_g4_g1 = Graph {nodes = fromList [1,2],
                     edges = fromList [Edge 1 1, Edge 2 1]
                    }

forest1 :: Graph Int
forest1 = Graph {nodes = fromList [1,2,3,4,5,6],
                 edges = fromList [Edge 1 3, Edge 3 2, Edge 4 3, Edge 5 6]
               }


nodesGLarge :: Set Int
nodesGLarge = fromList [1..15]

edgesGLarge :: Set (Edge Int)
edgesGLarge = fromList [Edge 1 1, Edge 1 3, Edge 2 1, Edge 3 2, Edge 3 5, Edge 4 1, Edge 4 2, 
                        Edge 4 12, Edge 4 13, Edge 5 6, Edge 5 8, Edge 6 7, Edge 6 8, Edge 6 10, 
                        Edge 7 10, Edge 8 9, Edge 8 10, Edge 9 11, Edge 10 9, Edge 10 11, Edge 10 14, 
                        Edge 11 12, Edge 11 14, Edge 12 13, Edge 13 11, Edge 13 15, Edge 14 13, Edge 15 14]

-- http://www.greatandlittle.com/studios/public/blowup-images/Dart/.directed_graph_m.jpg
gLarge :: Graph Int
gLarge = Graph {nodes = nodesGLarge, edges = edgesGLarge}

gInvalid :: Graph Int
gInvalid = Graph {nodes = fromList [1,2],
                  edges = fromList [Edge 1 3]
                 }

gDag :: Graph Int
gDag = Graph {nodes = fromList [1,2,3],
              edges = fromList [Edge 1 2, Edge 1 3]
             }

graphTopo :: Graph Int
graphTopo = Graph {nodes = fromList [1,2,3,4,5,6],
                   edges = fromList [Edge 1 3, Edge 3 2, Edge 3 5, Edge 3 6, Edge 4 3, Edge 5 6]
                  }

--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

tests :: Test
tests = TestList [test_swap, test_isDAG, test_adj, test_isPathOf, 
                  test_isEmpty, test_isForest, test_isSugraphOf, 
                  test_union, test_reachable, test_path, test_transpose, test_topo, test_bft]

test_swap :: Test
test_swap = swap (Edge 1 2) ~?= (Edge 2 1)

test_isValid :: Test
test_isValid = isValid gInvalid ~?= False

test_isDAG :: Test
test_isDAG = TestList [isDAG gInvalid ~?= False,
                       isDAG g1 ~?= False,
                       isDAG gDag ~?= True]

test_adj :: Test
test_adj = TestList [adj gLarge 4 ~?= Set.fromList [Edge 4 1, Edge 4 2, Edge 4 12, Edge 4 13],
                     adj g2 2 ~?= Set.fromList []]


test_isPathOf :: Test
test_isPathOf = TestList [isPathOf [] g1 ~?= True,
                          isPathOf [Edge 1 3] gDag ~?= True,
                          isPathOf [Edge 2 3] gDag ~?= False,
                          isPathOf [Edge 1 3, Edge 3 5, Edge 5 6, Edge 6 7, Edge 7 10, Edge 10 9, Edge 9 11, 
                                    Edge 11 14, Edge 14 13, Edge 13 15] gLarge ~?= True,
                          --falha 1a condiçao
                          isPathOf [Edge 1 12, Edge 12 13] gLarge ~?= False,
                          -- falha 2a
                          isPathOf [Edge 1 3, Edge 5 6] gLarge ~?= False,
                          -- falha na 3a
                          isPathOf [Edge 1 3, Edge 3 5, Edge 5 9] gLarge ~?= False]

test_transpose :: Test
test_transpose = TestList [transpose forest1 ~?= 
                 Graph {nodes = fromList [1,2,3,4,5,6],
                       edges = fromList [Edge 3 1, Edge 2 3, Edge 3 4, Edge 6 5]
                       }, 
                  transpose gDag ~?= Graph {nodes = fromList [1..3], edges = fromList [Edge 2 1, Edge 3 1]}]

test_topo :: Test
test_topo = TestList [topo Graph.empty ~?= ([] :: [Set Int]),
                      topo graphTopo ~?= Prelude.map fromList [[1,4], [3], [2, 5], [6]]
                      ]

test_isEmpty :: Test
test_isEmpty = TestList [TestCase $ assertBool "" (isEmpty Graph.empty), 
                         isEmpty g1 ~?= False
                        ]


test_isForest :: Test
test_isForest = TestList [TestCase $ assertBool "" (isForest forest1),
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
                           reachable forest1 1 ~?= fromList [1, 2, 3]
                          ]

test_path :: Test
test_path = TestList [path gLarge 4 7 ~?= pathGLarge4_7,
                      path gLarge 1 4 ~?= Nothing]
    where pathGLarge4_7 :: Maybe (Graph.Path Int) -- caminho mais curto de 4 para 7 em gLarge
          pathGLarge4_7 = Just [Edge 4 1, Edge 1 3, Edge 3 5, Edge 5 6, Edge 6 7]

test_bft :: Test 
test_bft = TestList [bft graphTopo (fromList [10]) ~?= Graph {nodes = fromList [10], edges = fromList []}, --propriedade indesejada do código??
                     bft graphTopo (fromList [1]) ~?= Graph {nodes = fromList [1,3,2,6,5], edges = fromList [Edge 3 1, Edge 2 3, Edge 5 3, Edge 6 3]}] 

main = runTestTT tests

--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas


instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = aux `suchThat` isValid
        where aux = do ns <- arbitrary
                       es <- arbitrary
                       return $ Graph {nodes = fromList ns, edges = fromList es}

instance Arbitrary ...
      gen x = suchThat Set.length$ formList arbitrary == x


-- versao inicial
{-
instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
     arbitrary = do nodes <- arbitrary :: [v] >>= return.remReps -- gera a lista de vertices
                    concat $ map (\x -> aux x nodes) nodes
                      where
                         remReps :: [a] -> [a]
                         remReps = toList . fromList
                         aux :: v -> vs -> Gen (Set v) -- ver se tem o nodes em scope
                         aux v nodes = do subL <- sublistOf nodes
                                          return . fromList $ map (Edge v) subL
-}
prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = arbitrary `suchThat` isDAG

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = arbitrary `suchThat` isForest

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g
