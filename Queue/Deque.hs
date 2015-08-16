data Deque a = D [a] [a]   

pushFront :: Deque a -> a -> Deque a
pushFront (D xs ys) x = D (x:xs) ys


pushBack :: Deque a -> a -> Deque a
pushBack (D xs ys) y = D xs (y:ys)

popFront :: Deque a -> (a, Deque a)
popFront (D (x:xs) ys) = (x, D xs ys)
popFront (D [] ys)     = popFront $ D lh rh
                         where lh = reverse $ drop h ys
		               rh = take h ys
			       h  = (length ys) `div` 2
			     
popBack :: Deque a -> (a, Deque a)
popBack (D xs (y:ys)) = (y, D xs ys)
popBack (D xs [])     = popBack $ D lh rh            
		         where lh = take h xs
                               rh = reverse $ drop h xs
			       h  = (length xs) `div` 2

emptyDeque :: Deque a
emptyDeque = D [] []
