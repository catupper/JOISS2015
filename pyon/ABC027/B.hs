main = getLine >> getLine >>= return.map read.words >>= \as -> print $ solve as
     where
       v as = sum as `div` length as
       solve as
         | sum as `mod` length as == 0 = fst $ foldl (next as) (0, 0) as
         | otherwise                   = -1
       next as (s, 0) a = (s,         a - v as)
       next as (s, d) a = (s + 1, d + a - v as)
