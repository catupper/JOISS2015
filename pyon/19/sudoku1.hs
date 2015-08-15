import Data.Char (digitToInt)
import Data.List ((\\), intersperse, transpose)
import Debug.Trace (trace)

type Grid a = [Row a]
type Row a = [a]
type Col a = [a]
type Box a = [a]
type Choice a = [a]

boxSize = 3
size = boxSize ^ 2 

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk c xs = t : chunk c d where (t, d) = splitAt c xs

single :: [Int] -> Bool
single [_] = True
single _   = False

rows :: Grid a -> [Row a]
rows = id

cols :: Grid a -> [Col a]
cols = transpose

boxs :: Grid a -> [Box a]
boxs = map concat . concat . map cols . group . map group where group = chunk boxSize

productList :: Grid a -> Grid a 
productList [] = [[]]
productList (xs:yss) = [x:ys | x <- xs, ys <- productList yss]

expand1 :: Grid (Choice Int) -> [Grid (Choice Int)]
expand1 rows = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
             where
               (rows1, row : rows2) = break (any minChoice) rows
               (row1, cs : row2)    = break minChoice row
               minChoice           = (==n) . length
               n                    = minimum $ map length $ filter (not . single) $ concat rows

expand :: Grid (Choice Int) -> Choice (Grid Int)
expand choices = productList $ map productList choices

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

valid :: Grid Int -> Bool
valid grid = all nodups (rows grid) && all nodups (cols grid) && all nodups (boxs grid)

complete :: Grid (Choice Int) -> Bool
complete = all $ all $ single

safe :: Grid (Choice Int) -> Bool
safe grid = all ok (rows grid) && all ok (cols grid) && all ok (boxs grid)
          where
            ok = nodups . map head . filter single

pruneRow :: Row (Choice Int) -> Row (Choice Int)
pruneRow row = map remove row
             where
               remove :: [Int] -> [Int]
               remove [x] = [x]
               remove xs = xs \\ (map head $ filter single row)

pruneBy :: ([Row (Choice Int)] -> Grid (Choice Int)) -> Grid (Choice Int) -> Grid (Choice Int)
pruneBy f = f . map pruneRow . f

prune :: Grid (Choice Int) -> Grid (Choice Int)
prune = pruneBy cols . pruneBy rows . pruneBy boxs

choice :: Int -> Choice Int
choice 0 = [1..size]
choice x = [x]

choices :: Grid Int -> Grid (Choice Int)
choices = prune . map (map choice)

search :: Grid (Choice Int) -> Choice (Grid Int)
search grid | not $ safe grid = []
            | complete g      = [map (map head) $ g]
            | otherwise       = concatMap search $ expand1 $ g
            where g = prune grid

solve :: Grid Int -> Choice (Grid Int)
--solve = filter valid . expand . choices
solve = search . choices

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
