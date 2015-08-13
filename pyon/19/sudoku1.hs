import Data.Char (digitToInt)

boxSize = 3
size = boxSize ^ 2 

choice :: Int -> [Int]
choice 0 = [1..size]
choice x = [x]

choices :: [[Int]] -> [[[Int]]]
choices = map $ map choice

solve :: [[Int]] -> [[Int]]
solve = id

convert :: Char -> Int
convert ' ' = 0
convert c = digitToInt c

main :: IO ()
main = getContents >>= putStrLn . unlines . map (unlines.map (unwords.map show)) .
                       choices . map (map convert) . lines

{-
main = getContents >>= putStrLn . unlines . map (unwords . map show) . 
         solve . map (map convert) . lines
-}
