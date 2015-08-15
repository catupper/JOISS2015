data Queue a = Q [a] [a]   deriving (Show)

insert :: Queue a -> a -> Queue a
insert (Q xs ys) x = Q (x:xs) ys

remove :: Queue a -> (a, Queue a)
remove (Q xs []) = remove $ Q [] (rev xs)
remove (Q xs (y:ys)) = (y, Q xs ys)

rev :: [a] -> [a]
rev = rev' [] 

rev' :: [a] -> [a] -> [a]
rev' xs [] = xs
rev' xs (y:ys) = rev' (y:xs) ys

empty :: Queue a
empty = Q [] []

elems :: Queue a -> [a]
elems (Q xs ys) = xs ++ (rev ys)

