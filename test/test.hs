module Main where

import Data.Graph.Inductive
import Data.Graph.Inductive.Example

main :: IO ()
main = return ()

m486 :: NodeMap String
m486 = fromGraph clr486

t1 :: Gr String ()
t1 = insMapEdge m486 ("shirt", "watch", ()) clr486

t2 :: Gr String ()
t2 = insMapEdge m486 ("watch", "pants", ()) t1

t3 :: Gr Char String
t3 = run_ empty $
    do insMapNodeM 'a'
       insMapNodeM 'b'
       insMapNodeM 'c'
       insMapEdgesM [('a', 'b', "right"),
		     ('b', 'a', "left"),
		     ('b', 'c', "down"),
		     ('c', 'a', "up")]

t4 :: Gr String ()
t4 = run_ clr486 $ insMapEdgeM ("shirt", "watch", ())
