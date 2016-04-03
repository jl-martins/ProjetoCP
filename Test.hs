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

gInvalid :: Graph Int
gInvalid = Graph {nodes = fromList [1,2],
                  edges = fromList [(1,3)]
                 }

gDag :: Graph Int
gDag = Graph {nodes = fromList [1,2,3],
              edges = fromList [(1,2),(1,3)]
             }

-- Um exemplo de um teste unitário.
test_adj :: Test
test_adj = adj g1 1 ~?= fromList [Edge 1 1]

--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

tests :: Test
tests = TestList [test_swap, test_isDag, test_adj, test_isPathOf]

test_swap :: Test
test_swap = swap (Edge 1 2) ~?= (Edge 2 1)

test_isValid :: Test
test_isValid = isValid gInvalid ~?= False

test_isDag :: Test
test_isDag = TestList [isDag gInvalid ~?= False,
                       isDag g1 ~?= False,
                       isDag gDag ~?= True]

test_transpose :: Test 
test_transpose = transpose gDag ~?= Graph {nodes = fromList [1..3], edges = fromList[(2,1),(3,1)]}

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
                          isPathOf [Edge 1 12, Edge 12 13] ~?= False,
                          -- falha 2a
                          isPathOf [Edge 1 3, Edge 5 6] ~?= False,
                          -- falha na 3a
                          isPathOf [Edge 1 3, Edge 3 5, Edge 5 9] gLarge ~?= False]

-- falta completar o caso nao trivial
test_topo :: Test
test_topo = TestList [topo empty ~?= []]

-- bft, topo
      
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
