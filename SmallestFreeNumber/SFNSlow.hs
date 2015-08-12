import Control.Applicative

getIntListSp :: IO [Int]
getIntListSp = (map read . words) <$> getLine



----------------

minfree :: [Int] -> Int
minfree xs = head ([1..] \\ xs)


(\\)    :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (\x -> notElem x vs) us



main = do list <- getIntListSp
          print $ minfree list
       
