import Data.Array
import Data.List(sort,tails)

class EOF a where
 eof :: a

instance EOF Char where eof = '\0'
instance EOF Int  where eof = -1

tag :: (EOF a) => [a] -> [a]
tag xs = xs ++ [eof]

rrot :: [a] -> [a]
rrot xs = [last xs] ++ init xs

lrot :: [a] -> [a]
lrot (x:xs) = xs ++ [x]

transform :: (EOF a, Ord a) => [a] -> ([a],Int)
transform xs = (lrot[x | i <- ps, let x = xa ! i, x /= eof], k-1)
 where
  n   = length xs
  k   = length(takeWhile(/=0)ps)
  xa  = listArray (0,n) (rrot xs')
  xs' = tag xs
  ps  = map snd (sort(zip(tails xs')[0..n]))

