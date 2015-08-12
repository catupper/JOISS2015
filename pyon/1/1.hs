minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

main :: IO ()
main = print $ minfree [6, 3, 0, 7, 5, 3, 1, 7, 2]
