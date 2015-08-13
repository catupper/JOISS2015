{-# OPTIONS -Wall #-}
{-  Note: the source code on the book fails -}
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

transform :: (EOF a, Ord a) => [a] -> ([a],Int)
transform xs = ([xa!i | i <- ps], k)
 where 
  n   = length xs
  k   = length(takeWhile (/=0) ps)
  xa  = listArray(0,n) (rrot xs')
  xs' = tag xs
  ps  = map snd(sort(zip(tails xs')[0..n]))

untransform :: Ord a => ([a],Int) -> [a]
untransform (ys,k) = take (n-1) (tail (map (ya!) (iterate (pa!) k)))
 where
  n  = length ys
  ya = listArray (0,n-1) ys
  pa = listArray (0,n-1) (map snd (sort (zip ys [0..])))

