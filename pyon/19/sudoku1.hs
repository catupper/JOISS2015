import Data.Char (digitToInt)

digit :: Int -> Bool
digit = (`elem` [1..9])

blank :: Int -> Bool
blank = (== 0)

solve :: [[Int]] -> [[Int]]
solve = id

convert :: Char -> Int
convert ' ' = 0
convert c = digitToInt c

main :: IO ()
main = getContents >>= putStrLn . unlines . map unwords . map (map show) . 
         solve . map (map convert) . lines
