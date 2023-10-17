module Core.Utils where

import Data.List (find)

-- Addr

type Addr = Int

aNull :: Addr
aNull = 0

aIsNull :: Addr -> Bool
aIsNull = (== aNull) 

aShow :: Addr -> String
aShow a = "#" ++ (show a)


-- Heap

data Heap a = Heap
    { hSize  :: Int
    , hFree  :: [Addr]
    , hMap   :: [(Addr, a)]
    } deriving Show

-- returns an initialized empty heap
hInitial :: Heap a
hInitial = Heap 0 [1..] []

-- adds element to heap, returns new heap and added element address
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap _ [] _) _ = error "heap should have a non-empty free address list"
hAlloc (Heap size (addr:free) mapping) n = (heap', addr)
    where heap' = Heap (size+1) free mapping' 
          mapping' = (addr,n):mapping

-- update addr-to-object mapping in the heap
hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free mapping) a n = Heap size free mapping'
    where mapping' = (a,n) : [m | m <- mapping, fst m /= a]

-- remove object at given address
hRemove :: Heap a -> Addr -> Heap a
hRemove (Heap size free mapping) a = Heap (size-1) (a:free) mapping'
    where mapping' = [m | m <- mapping, fst m /= a]

-- lookup given address
hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ mapping) a = 
    case mn of
        Nothing -> error ("can't find node " ++ aShow a ++ " in the heap")
        Just e  -> snd e
    where mn = find (\(k,_) -> k == a) mapping 

-- get all addresses
hAddrs :: Heap a -> [Addr]
hAddrs (Heap _ _ mapping) = map fst mapping

