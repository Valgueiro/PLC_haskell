sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

double :: [Int] -> [Int]
double [] = [] 
double (a:as) = 2*a:double as

member :: [Int] -> Int -> Bool
member [] a = False
member (a:as) n | n == a = True
                | otherwise = member as n

digits :: String -> String
digits [] = []
digits (a:as) | a>='0' && a<='9' = a:digits as
              | otherwise = digits as

sumPairs :: [(Int,Int)]->[Int]
sumPairs [] = []
sumPairs ((x,y):as) = (x+y):sumPairs as

--Funções polifórmicas length() e ++ (concatenação)