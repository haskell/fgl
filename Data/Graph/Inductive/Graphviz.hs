-- | Simple graphviz output.
module Data.Graph.Inductive.Graphviz(
    Orient(..),
    graphviz, graphviz'
) where

import Data.Graph.Inductive.Graph

data Orient = Portrait | Landscape deriving (Eq, Show)

o2s :: Orient -> String
o2s Portrait = "\trotate = \"0\"\n"
o2s Landscape = "\trotate = \"90\"\n"

-- | Formats a graph for use in graphviz.
graphviz :: (Graph g, Show a, Show b) =>    g a b   -- ^ The graph to format
					 -> String  -- ^ The title of the graph
					 -> (Double, Double)	-- ^ The size
								-- of the page
					 -> (Int, Int)	-- ^ The width and
							-- height of the page
							-- grid
					 -> Orient  -- ^ The orientation of
						    -- the graph.
					 -> String

i2d :: Int -> Double
i2d = fromInteger . toInteger

graphviz g t (w, h) p@(pw', ph') o =
    let n = labNodes g
	e = labEdges g
	ns = concatMap sn n
	es = concatMap se e
	sz w' h' = if o == Portrait then show w'++","++show h' else show h'++","++show w'
	ps = show w++","++show h
	(pw, ph) = if o == Portrait then p else (ph', pw')
	--gs = show ((w*(i2d pw))-m)++","++show ((h*(i2d ph))-m)
	gs = sz (w*(i2d pw)) (h*(i2d ph))
    in "digraph "++t++" {\n"
	    ++"\tmargin = \"0\"\n"
	    ++"\tpage = \""++ps++"\"\n"
	    ++"\tsize = \""++gs++"\"\n"
	    ++o2s o
	    ++"\tratio = \"fill\"\n"
	    ++ns
	    ++es
	++"}"
    where sn (n, a) | sa == ""	= ""
		    | otherwise	= '\t':(show n ++ sa ++ "\n")
	    where sa = sl a
	  se (n1, n2, b) = '\t':(show n1 ++ " -> " ++ show n2 ++ sl b ++ "\n")

-- | Format a graph for graphviz with reasonable defaults: title of \"fgl\",
-- 8.5x11 pages, one page, landscape orientation
graphviz' :: (Graph g, Show a, Show b) => g a b -> String
graphviz' g = graphviz g "fgl" (8.5,11.0) (1,1) Landscape

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
	    | otherwise	     = s
sq ('\'':s) | last s == '\'' = init s
	    | otherwise	     = s
sq s                         = s

sl :: (Show a) => a -> String
sl a =
    let l = sq (show a)
    in if (l /= "()") then (" [label = \""++l++"\"]") else ""
