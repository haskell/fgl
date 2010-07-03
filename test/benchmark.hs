{-
  Install microbench to build this program:
  http://hackage.haskell.org/cgi-bin/hackage-scripts/package/microbench

  % ghc -O --make benchmark
  % ./benchmark
  [1 of 1] Compiling Main             ( benchmark.hs, benchmark.o )
  Linking benchmark ...
  * insNode into AVL tree: ..................
    8.877ns per iteration / 112655.53 per second.
  * insNode into PATRICIA tree: .....................
    1.788ns per iteration / 559342.86 per second.
  * insEdge into AVL tree: ...........
    2833.029ns per iteration / 352.98 per second.
  * insEdge into PATRICIA tree: ...................
    4.625ns per iteration / 216224.60 per second.
  * gmap on AVL tree: ................
    32.754ns per iteration / 30530.57 per second.
  * gmap on PATRICIA tree: .....................
    1.623ns per iteration / 616056.37 per second.
  * nmap on AVL tree: ................
    35.455ns per iteration / 28204.95 per second.
  * nmap on PATRICIA tree: .....................
    1.713ns per iteration / 583758.06 per second.
  * emap on AVL tree: ...........
    4416.303ns per iteration / 226.43 per second.
  * emap on PATRICIA tree: ...................
    4.532ns per iteration / 220663.09 per second.
-}

import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Tree as AVL
import qualified Data.Graph.Inductive.PatriciaTree as Patricia
import Microbench


main :: IO ()
main = do microbench "insNode into AVL tree" insNodeAVL
          microbench "insNode into PATRICIA tree" insNodePatricia

          microbench "insEdge into AVL tree" insEdgeAVL
          microbench "insEdge into PATRICIA tree" insEdgePatricia

          microbench "gmap on AVL tree" gmapAVL
          microbench "gmap on PATRICIA tree" gmapPatricia

          microbench "nmap on AVL tree" nmapAVL
          microbench "nmap on PATRICIA tree" nmapPatricia

          microbench "emap on AVL tree" emapAVL
          microbench "emap on PATRICIA tree" emapPatricia


insNodeAVL :: Int -> AVL.UGr
insNodeAVL = insNodes' empty


insNodePatricia :: Int -> Patricia.UGr
insNodePatricia = insNodes' empty


{-# INLINE insNodes' #-}
insNodes' :: DynGraph gr => gr () b -> Int -> gr () b
insNodes' g 0 = g
insNodes' g n = let [v] = newNodes 1 g
                    g'  = insNode (v, ()) g
                in
                  insNodes' g' (n - 1)


insEdgeAVL :: Int -> AVL.UGr
insEdgeAVL n = insEdges' (insNodeAVL n) n


insEdgePatricia :: Int -> Patricia.UGr
insEdgePatricia n = insEdges' (insNodePatricia n) n


{-# INLINE insEdges' #-}
insEdges' :: DynGraph gr => gr a () -> Int -> gr a ()
insEdges' g 0 = g
insEdges' g n = let g' = insEdge (1, n, ()) g
                in
                  insEdges' g' (n - 1)


gmapAVL :: Int -> AVL.Gr Int ()
gmapAVL n
    = let g  = insNodeAVL n
          g' = gmap f g
          f (ps, v, _, ss) = (ps, v, v, ss)
      in
        g'


gmapPatricia :: Int -> Patricia.Gr Int ()
gmapPatricia n
    = let g  = insNodePatricia n
          g' = gmap f g
          f (ps, v, _, ss) = (ps, v, v, ss)
      in
        g'


nmapAVL :: Int -> AVL.Gr Int ()
nmapAVL n
    = let g   = insNodeAVL n
          g'  = nmap f g
          f _ = n
      in
        g'


nmapPatricia :: Int -> Patricia.Gr Int ()
nmapPatricia n
    = let g   = insNodePatricia n
          g'  = nmap f g
          f _ = n
      in
        g'


emapAVL :: Int -> AVL.Gr () Int
emapAVL n
    = let g   = insEdgeAVL n
          g'  = emap f g
          f _ = n
      in
        g'


emapPatricia :: Int -> Patricia.Gr () Int
emapPatricia n
    = let g   = insEdgePatricia n
          g'  = emap f g
          f _ = n
      in
        g'