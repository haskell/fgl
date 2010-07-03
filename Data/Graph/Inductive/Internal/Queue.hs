module Data.Graph.Inductive.Internal.Queue(
    -- * Type
    Queue(..),
    -- * Operations
    mkQueue, queuePut, queuePutList, queueGet, queueEmpty
) where


data Queue a = MkQueue [a] [a]

mkQueue :: Queue a
mkQueue = MkQueue [] []

queuePut :: a -> Queue a -> Queue a
queuePut item (MkQueue ins outs) = MkQueue (item:ins) outs

queuePutList :: [a] -> Queue a -> Queue a
queuePutList [] q     = q
queuePutList (x:xs) q = queuePutList xs (queuePut x q)

queueGet :: Queue a -> (a, Queue a)
queueGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
queueGet (MkQueue ins []) = queueGet (MkQueue [] (reverse ins))

queueEmpty :: Queue a -> Bool
queueEmpty (MkQueue ins outs) = (null ins) && (null outs)
