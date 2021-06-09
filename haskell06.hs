
ends :: [Int] -> [Int]
ends listInt = (listInt !! 0) : ((listInt !! ((length listInt)-1)): []) 

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : xs

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 (xs) 
  else deduzame2 (xs)

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n, n^2) : (geraTabela (n-1))

contido :: Char -> String -> Bool
contido c [] = False;
contido c (x:xs) = if (c == x) 
    then True 
    else contido c xs

translate :: [(Int,Int)] -> [(Int,Int)]
translate [] = [];
translate ((x,y):xs) = (x+2,y+2) : translate(xs) 

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if((length x) > 5) then 1 + (countLongs xs) else (countLongs xs)

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if((length x) > 5) then x : (onlyLongs xs) else (onlyLongs xs)