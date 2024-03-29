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
import Data.Maybe(fromJust)

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
test_isValid = TestList [isValid g1 ~?= True,
                         isValid gInvalid ~?= False]

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
                          isPathOf [Edge 1 3, Edge 3 5, Edge 5 6, Edge 6 7, Edge 7 10, Edge 10 9, 
                                    Edge 9 11, Edge 11 14, Edge 14 13, Edge 13 15] gLarge ~?= True,
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

main :: IO Counts
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

{- Original:-}
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
     arbitrary = do nodos <- resize 20 arbitrary -- scale (`div` 2) estava a dar muitos grafos vazios
                    arestas <- (sequence $ Prelude.map (aux nodos) nodos) >>= return . concat
                    return Graph {nodes = fromList nodos, edges = fromList arestas}  
                      where
                         aux :: (Arbitrary v) => [v] -> v -> Gen [Edge v]
                         aux nodos v = do let len = length nodos
                                          outD <- frequency (zip [1..(len+1)] (Prelude.map return [len,(len-1)..0]))
                                          perm <- shuffle nodos
                                          return $ Prelude.map (Edge v) (take outD perm)

-- Gerador de DAGs
-- baseado em http://mathematica.stackexchange.com/a/613

prop_dag :: Property
prop_dag = forAll (arbitraryDAG :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

arbitraryDAG :: (Arbitrary a, Ord a) => Gen (DAG a)
arbitraryDAG = do randomGraph <- arbitrary
                  ordem <- shuffle $ toList $ nodes randomGraph
                  return Graph {nodes = nodes randomGraph, edges = fromList $ Prelude.filter (ordemAleatoria ordem) $ toList $ edges randomGraph} 
                             where
                              ordemAleatoria :: Ord a => [a] -> Edge a -> Bool
                              ordemAleatoria ordem (Edge x y) = menor ordem x y
                              menor :: Ord a => [a] -> a -> a -> Bool
                              menor [] _ _ = undefined
                              menor (h:t) x y | x == y = False
                                              | h == x = True
                                              | h == y = False
                                              | otherwise = menor t x y                                           

-- Gerador de florestas
prop_forest :: Property
prop_forest = forAll (arbitraryForest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

arbitraryForest :: (Arbitrary a, Ord a) => Gen (Forest a)
arbitraryForest = do randomDAG <- arbitraryDAG
                     arestas <- sequence $ Prelude.map (\v -> if Set.null (adj randomDAG v) then return Nothing else
                                                              do  x <- elements(toList $ adj randomDAG v)
                                                                  frequency [(4, return $ Just x), (1, return Nothing)]
                                                       )
                                                       (toList $ nodes randomDAG)   
                     return Graph {nodes = nodes randomDAG, edges = fromList $ fromJustList $ arestas}
                     where
                        fromJustList = Prelude.map (\(Just x) -> x) . Prelude.filter (/= Nothing)

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

prop_swap1 :: Edge Int -> Bool
prop_swap1 (Edge a b) = swap (Edge a b) == Edge b a

-- Aplicar swap após swap é equivalente a não fazer nada.
prop_swap2 :: Edge Int -> Bool
prop_swap2 e = swap (swap e) == e

-- O resultado da função 'bft' é um grafo válido.
prop_isValid_bft :: Graph Int -> Bool
prop_isValid_bft g = isValid (bft g (nodes g))

-- O resultado da função 'bft' é uma floresta.
prop_isForest_bft :: Graph Int -> Bool
prop_isForest_bft g  = isForest $ bft g (nodes g)

-- A transposta da floresta produzida por 'bft g (nodes g)' é um subgrafo de g.
prop_transpBFT_isSubgraphOf :: Graph Int -> Bool
prop_transpBFT_isSubgraphOf g = transpose (bft g (nodes g)) `isSubgraphOf` g

-- A transposta de um grafo é um grafo válido.
prop_isValid_transpose :: Property
prop_isValid_transpose = forAll (arbitrary :: Gen (Graph Int)) (isValid . transpose)

-- A transposta da transposta de qualquer grafo é o próprio grafo.
prop_transpose2 :: Graph Int -> Bool
prop_transpose2 g = (transpose $ transpose g) == g

-- A transposta de um DAG é um DAG.
prop_transpose_DAG_isDAG :: Property
prop_transpose_DAG_isDAG = forAll (arbitraryDAG :: Gen (DAG Int)) (isDAG . transpose)

-- Qualquer grafo g1 é um subgrafo da sua união com um qualquer grafo g2 (e vice-versa).
prop_union_isSubgraphOf :: Graph Int -> Graph Int -> Property
prop_union_isSubgraphOf g1 g2 = g1 `isSubgraphOf` (Graph.union g1 g2) .&. g2 `isSubgraphOf` (Graph.union g1 g2)

-- A união de um grafo com ele próprio é o próprio grafo.
prop_union_self :: Graph Int -> Bool
prop_union_self g = (Graph.union g g) == g

-- Propriedade comutativa da união de grafos.
prop_commut_union :: Graph Int -> Graph Int -> Bool
prop_commut_union g1 g2 = (Graph.union g1 g2) == (Graph.union g2 g1)

-- A união das transpostas de 2 grafos é igual à transposta da união dos mesmos.
prop_transpose_union :: Graph Int -> Graph Int -> Bool
prop_transpose_union g1 g2 = (transpose g1) `Graph.union` (transpose g2) == transpose (g1 `Graph.union` g2)

-- Propriedade associativa da união de grafos.
prop_assoc_union :: Graph Int -> Graph Int -> Graph Int -> Bool
prop_assoc_union g1 g2 g3 = (Graph.union (Graph.union g1 g2) g3) == (Graph.union g1 (Graph.union g2 g3))

-- Dado um DAG 'g' e duas arestas 'a' e 'b' desse DAG, se existe uma aresta
-- de 'a' para 'b' então 'a' aparece antes de 'b' na ordenacão topológica de 'g'.
prop_topo :: Property
prop_topo = forAll (arbitraryDAG :: Gen(DAG Int)) 
                   (\g -> all (menorTopo$ topo g) (toList $ edges g))
                   where
                     menorTopo :: [Set Int] -> Edge Int ->  Bool
                     menorTopo [] _ = undefined
                     menorTopo (h:t) ed@(Edge a b) | member a h && notMember b h = True
                                                   | member a h && member b h = False
                                                   | otherwise = menorTopo t ed

-- Para uma ordenação toplógica não vazia, o grau de entrada 
-- de todos vértices da sua cabeça é necessariamente 0.
prop_null_inD_headTopo :: DAG Int -> Property
prop_null_inD_headTopo g = let t = topo g
                               inD :: Int -> DAG Int -> Int
                               inD v g = Set.size (Set.filter ((==v) . target) (edges g))
                           in ((not $ Prelude.null t) && (not $ Set.null (head t)))
                              ==> 
                              forAll (elements $ elems $ head t) (\v -> (inD v g) == 0)

-- Para qualquer vértice v, o conjunto dos vértices adjacentes ao 
-- mesmo é um subconjunto dos vértices alcançáveis a partir de v.
prop_adj_reachable :: Graph Int -> Property
prop_adj_reachable g = (not $ Graph.isEmpty g) 
                       ==> forAll (elements $ elems $ nodes g)
                           $ \v -> (Set.map target (adj g v)) `isSubsetOf` (reachable g v)

-- Se um vértice 'dst' for alcançável a partir de um vértice src, então o caminho
-- mais curto entre eles é um caminho (isPathOf) do grafo a que eles pertencem.
prop_path_isPathOf :: Graph Int -> Property
prop_path_isPathOf g = (not $ Graph.isEmpty g) ==>
                          forAll (elements $ elems $ nodes g) $ \src ->
                          forAll (elements $ elems $ nodes g) $ \dst ->
                          path g src dst /= Nothing ==> fromJust (path g src dst) `isPathOf` g

-- Seja 'y' um nodo alcançável a partir de um nodo 'x', tal que x /= y
-- e seja 'p' o caminho mais curto entre 'x' e 'y', temos que 'p' contém
-- o caminho mais curto entre o sucessor imediato de 'x' (em 'p') e o nodo 'y'.
prop_path_subPath :: Graph Int -> Property
prop_path_subPath g = (not $ Graph.isEmpty g) ==>
                          forAll (elements $ elems $ nodes g) $ \src ->
                          forAll (elements $ elems $ nodes g) $ \dst ->
                          prop_path_aux g src dst
    where
          prop_path_aux g x y | path g x y == Nothing   = label "Não há caminho" True
                              | x == y                  = label "Origem = Destino" True
                              | otherwise               = property $ fromJust (path g w y) == tail (fromJust (path g x y))
                        where w = (target . head) (fromJust (path g x y))

-- O conjunto de arestas no caminho entre 2 vértices é um subconjunto das arestas do grafo.
prop_path_isSubsetOf :: Graph Int -> Property
prop_path_isSubsetOf g = (not $ Graph.isEmpty g) ==>
                            forAll (elements $ elems $ nodes g) $ \src ->
                            forAll (elements $ elems $ nodes g) $ \dst ->
                            edgesPathSubsetOfedges g src dst
    where
          edgesPathSubsetOfedges g src dst =
              path g src dst /= Nothing ==> fromList (fromJust (path g src dst)) `isSubsetOf` (edges g)

-- Para cada destino 'dst' alcançável a partir de uma qualquer origem
-- 'src', a função 'path g src dst' não pode devolver Nothing.
prop_path_reachable :: Graph Int -> Property
prop_path_reachable g | Graph.isEmpty g   = label "Trivial" True
                      | otherwise         = forAll (elements $ elems $ nodes g) (existsPathToReachable g)
    where existsPathToReachable g src =
              forAll (elements $ elems $ reachable g src) ((/= Nothing) . (path g src))
