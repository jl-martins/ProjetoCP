g2 :: Graph
g2 = Graph {nodes = Set.empty, edges = Set.empty}

test_isEmpty :: Test
test_isEmpty = assertBool "" (isEmpty g2)

