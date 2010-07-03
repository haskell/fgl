-- (c) 2000 - 2002 by Martin Erwig [see file COPYRIGHT]
-- | Maximum Independent Node Sets

module Data.Graph.Inductive.Query.Indep (
    indep
) where


import Data.Graph.Inductive.Graph


first :: (a -> Bool) -> [a] -> a
first p = head . filter p

indep :: DynGraph gr => gr a b -> [Node]
indep g | isEmpty g = []
indep g = if length i1>length i2 then i1 else i2
          where vs          = nodes g 
                m           = maximum (map (deg g) vs) 
                v           = first (\v'->deg g v'==m) vs 
                (Just c,g') = match v g 
                i1          = indep g'
                i2          = v:indep (delNodes (neighbors' c) g')

