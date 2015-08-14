import Control.Applicative
import Control.Monad

-----

--be carefull with the type inference

-----

---------
-- get space separated something

getListSp :: Read a => IO [a] 
getListSp = (map read . words) <$> getLine

---------



---------
-- output [a] in space separated form

putListSp :: Show a => [a]  -> IO()
putListSp ls = putStrLn $ show $ unwords $ map show ls

---------


--------
-- read an integer N, get N lines, make a list consist of them

getListLn :: Read a => IO [a]
getListLn = do n <- readLn
               s <- replicateM n getLine
	       return (map read s)
--------



main = do n <- readLn :: IO Int
          a <- getListSp
          print $ solve a

solve :: [Int] -> Int
solve xs | (sum xs) `mod` (length xs) == 0 = solve' xs
         | otherwise = -1

solve' :: [Int] -> Int
solve' xs = length $ filter (needMove xs) [1 .. n-1] 
           where n = length xs
	   
needMove :: [Int] -> Int -> Bool
needMove xs n = ((sum ls) * (length rs)) /= ((sum rs) * (length ls))
                where ls = take n xs
		      rs = drop n xs


