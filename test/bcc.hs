module Main where

import Data.Graph.Inductive
import List

mkUndirectedUGraph v e = mkUGraph v [ (x, y) | (x0, y0) <- e, (x, y) <- [(x0, y0), (y0, x0)] ]

graph :: Gr () ()
graph = mkUndirectedUGraph [1..5] [(1, 2), (2, 3), (2, 4), (3, 5)]

bccEdges g = sort (concat $ map edges $ bcc g) == sort (edges g)

prop_bcc = bccEdges graph

main = do
  print prop_bcc
