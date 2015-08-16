import Data.List
import Data.Array
data Queue a = Queue [a] [a]

qempty :: Queue a
qempty = Queue [] []

qelems :: Queue a -> [a]
qelems (Queue fs bs) = fs ++ reverse bs

qinsert :: Queue a -> a -> Queue a
qinsert (Queue fs bs) b = Queue fs (b:bs)

qremove :: Queue a -> (a, Queue a)
qremove (Queue [] []) = error "Empty Queue"
qremove (Queue [] bs) = qremove (Queue (reverse bs) [])
qremove (Queue (f:fs) bs) = (f, Queue fs bs)



allcp xs = extract (until done step (as,qempty,0,1))
   where
     extract (as,qs,h,k) = qelems as
     done (as,qs,h,k) = (k == n)
     n  = Data.List.length xs
     as = qinsert qempty n
     xa = listArray (0,n-1) xs
     step (as,qs,h,k) | k >= h = (qinsert as a,qinsert as' a,k+a,k+1)
                      | q /= r = (qinsert as m,qinsert qs' m,h,k+1)
		      | q == r = (qinsert as b,qinsert as' b,k+b,k+1)
		       where as'     = snd (qremove as)
		             (q,qs') = qremove qs
		             r       = h-k
		             m       = min q r
		             a       = llcp' 0 k
		             b       = q + llcp' q (q + k)
     llcp' j k | j == n || k == n = 0
               | xa ! j == xa ! k = 1 + llcp' (j + 1) (k + 1)
	       | otherwise = 0
	       
