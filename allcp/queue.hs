data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

elems :: Queue a -> [a]
elems (Queue fs bs) = fs ++ reverse bs

insert :: Queue a -> a -> Queue a
insert (Queue fs bs) b = Queue fs (b:bs)

remove :: Queue a -> (a, Queue a)
remove (Queue [] []) = error "Empty Queue"
remove (Queue [] bs) = remove (Queue (reverse bs) [])
remove (Queue (f:fs) bs) = (f, Queue fs bs)

instance (Show a) => Show (Queue a) where
  show q = "Queue(" ++ (unwords $ map show $ elems q) ++ ")"

