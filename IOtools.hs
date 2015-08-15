import Control.Applicative
import Control.Monad

-----

--be careful with the type inference

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

