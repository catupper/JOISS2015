import Data.List (intercalate)



type Expression = [Term]
type Term = [Factor]
type Factor = [Digit]
type Digit = Int

valExpr :: Expression -> Int
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact

valFact :: Factor -> Int
valFact = foldl1 connect
--valFact = foldl1 (\n d -> 10 * n + d)

connect :: Int -> Int -> Int
--connect a b = a * (10 ^ (floor (logBase 10 b) + 1) + b
connect a b = read $ (show a) ++ (show b)

good :: Int -> Int -> Bool
good = (==)

extend :: Digit -> [Expression] -> [Expression]
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es

glue :: Digit -> Expression -> [Expression]
glue d ((f:fs):ts) = [((d:f):fs  )       :ts,
                      ([d]  :f:fs)       :ts,
		      [[d]       ]:(f:fs):ts ]

expressions :: Int -> [Digit] -> [Expression]
expressions n = filter (good n . valExpr) . foldr extend []

--main = return ()
--main = print $ valExpr [[[1,2]],[[3,4]],[[5],[6]],[[7]],[[8]],[[9]]]
--main = print $ expressions [1,2,3,4,5,6,7,8,9]
main = do l <- getLine
          let [a, i] = map read $ words l
          putStrLn $ unlines $ map showExpr $ expressions a [1..i]

---------Display----------

showExpr :: Expression -> String 
--showExpr [t]    = showTerm t
--showExpr (t:ts) = showTerm t ++ " + " ++ showExpr ts
showExpr = intercalate " + " . map showTerm

showTerm :: Term -> String
--showTerm [f]    = showFctr f
--showTerm (f:fs) = showFctr f ++ "*" ++ showTerm fs
showTerm = intercalate "*" . map showFctr

showFctr :: Factor -> String
showFctr = foldl1 (++) . map show
