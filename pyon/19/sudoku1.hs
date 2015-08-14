import Data.Char (digitToInt)
import Data.List ((\\), transpose)

boxSize = 3
size = boxSize ^ 2 

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk c xs = t : chunk c d where (t, d) = splitAt c xs

rows :: [[a]] -> [[a]]
rows = id

cols :: [[a]] -> [[a]]
cols = transpose

boxs :: [[a]] -> [[a]]
boxs = map concat . concat . map cols . group . map group where group = chunk boxSize

productList :: [[a]] -> [[a]] 
productList [] = [[]]
productList (xs:yss) = [x:ys | x <- xs, ys <- productList yss]

expand :: [[ [Int] ]] -> [ [[Int]] ]
expand choices = productList $ map productList choices

dups :: Eq a => [a] -> Bool
dups [] = False
dups (x:xs) = any (== x) xs || dups xs

valid :: [[Int]] -> Bool
valid grid = not $ any dups (rows grid) ||
                   any dups (cols grid) ||
                   any dups (boxs grid)

pruneRow :: [ [ Int] ] -> [ [Int] ]
pruneRow row = map (remove fixed) row
             where
               fixed = [d | [d] <- row]
               remove del [x] = [x]
               remove del xs = xs \\ del

pruneBy :: ([[ [Int] ]] -> [[ [Int] ]]) -> [[ [Int] ]] -> [[ [Int] ]]
pruneBy f = f . map pruneRow . f

prune :: [[ [Int] ]] -> [[ [Int] ]]
prune = pruneBy cols . pruneBy rows . pruneBy boxs

choice :: Int -> [Int]
choice 0 = [1..size]
choice x = [x]

choices :: [[Int]] -> [[ [Int] ]]
choices = prune . map (map choice)

solve :: [[Int]] -> [ [[Int]] ]
solve = filter valid . expand . choices


convert :: Char -> Int
convert ' ' = 0
convert c = digitToInt c

main :: IO ()
main = getContents >>=
         mapM_ (putStrLn . unlines . map (unwords . map show)) . 
         solve . map (map convert) . lines
{-
main = getContents >>= putStrLn . unlines . map (unlines.map (unwords.map show)) .
                       choices . map (map convert) . lines
-}
