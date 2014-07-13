:l DAGGraphing.hs ttrie.hs
let g = TTrie.fromList ["abc", "ade"]
let nodes_in_g = filter (not . is_empty) . zoldMap (\a -> [a]) $ g
let show' node = if isNothing . show_node $ node then Ly.empty else Ly.pack . fromJust . show_node $ node
let nodes = zip [0..] $ map show' nodes_in_g