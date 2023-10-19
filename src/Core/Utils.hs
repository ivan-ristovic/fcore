module Core.Utils where

import Data.List (find)

-- Utils

mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml _ acc []     = (acc, [])
mapAccuml f acc (x:xs) = (acc'', x':xs')
    where (acc' , x' ) = f acc x
          (acc'', xs') = mapAccuml f acc' xs


-- Addr

type Addr = Int

aNull :: Addr
aNull = 0

aIsNull :: Addr -> Bool
aIsNull = (== aNull) 

aShow :: Addr -> String
aShow a = "#" ++ (show a)

aLookup :: Eq a => [(a, b)] -> a -> b -> b
aLookup lst k def = case mv of Nothing     -> def
                               Just (_, v) -> v
    where mv = find (\(k',_) -> k' == k) lst

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
hLookup (Heap _ _ mapping) a = aLookup mapping a err
    where err = error ("can't find node " ++ aShow a ++ " in the heap")

-- get all addresses
hAddrs :: Heap a -> [Addr]
hAddrs (Heap _ _ mapping) = map fst mapping

