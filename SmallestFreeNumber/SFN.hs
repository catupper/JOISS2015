import Control.Applicative
import Control.Monad
import Data.Array
import Data.Array.ST

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




minfree :: [Int] -> Int
minfree = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = runSTArray (do
                 {a <- newArray (0,n) False;
		  sequence [writeArray a x True | x <- xs, x <= n];
		  return a})
               where n = length xs



(\\)    :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (\x -> notElem x vs) us



main = do list <- getListSp
          print $ minfree list


