------ Made by: Mateus Valgueiro Teixeira [mvt]

-- Q1
toInt :: Char -> Int
toInt a = fromEnum(a)-fromEnum('0')

digitSum :: [Char] -> Int
digitSum as = countPairs circled
    where
        circled = as ++ [(head (as))]
        countPairs [] = 0
        countPairs [a] = 0
        countPairs (a:b:as) | a == b = (toInt (a)) + countPairs (b:as)
                            | otherwise = countPairs (b:as ) 

-- Q2
permutations :: [Int] -> [[Int]]
permutations [] = []
permutations [a] = [[a]]
permutations (x:xs) = 
    [take i p ++ [x] ++ drop i p| p <- permutations xs, i <- [0..length p]]

-- Q3
type IndexedString = [(Char, Int)]
findSubstrs :: String -> IndexedString -> [Int]
findSubstrs as [] = []
findSubstrs (a:as) ((c,pos):bs) | a == c = findRest as bs pos ++ findSubstrs (a:as) bs
                                | otherwise = findSubstrs (a:as) bs
        where  
            findRest [] [] pos = [pos]
            findRest _ [] pos= [] --not Found
            findRest [] _ pos= [pos]
            findRest (x:xs) ((y, posY):ys) pos | x == y = findRest xs ys pos
                                               | otherwise = []

stringMatching :: String -> String -> [Int]
stringMatching at bt = findSubstrs at btIndexed
        where  
            btIndexed = zip bt [0..]

-- Q4
sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (a:as) = a + sumAll as

sumEquals :: [(Int, Int)] -> Int -> Int -> [Int]
sumEquals as now end | end+1 == now = []
                     | otherwise = [sumAll ([x| (x,nx) <- as, nx == now])] ++ sumEquals as (now+1) end

maxCoef :: [(Int, Int)] -> Int
maxCoef [] = 0
maxCoef ((x,y):as) = max y (maxCoef as) 

poliMult :: [Int] -> [Int] -> [Int]
poliMult as bs = sumEquals allMult 0 (maxCoef allMult)
    where
        indexedAS = zip as [0..]
        indexedBs = zip bs [0..]
        allMult = [(xa * xb, na+nb) | (xa, na) <-indexedAS, (xb, nb) <- indexedBs] 
        

{-- ########################################################## --}
----------- Antações da aula de monitoria [Made by Gabi (monitoria)]


-- -- iguais :: [Char] -> [Char] -> Bool
-- -- iguais [] [] = True
-- -- iguais st [] = False
-- -- iguais (a:as) (b:bs) | (a==b) = iguais as bs
-- --                      | otherwise = False

-- -- func :: [Char] -> [Char] -> Int
-- -- func _ [] = 0
-- -- func (a:as) (b:bs) | a == b && isPrefix (a:as) (b:bs) = 1 + func (a:as) bs
-- --                    | otherwise = func (a:as) bs

-- -- take
-- -- drop

-- -- Questão 2

-- perm :: [Int] -> [[Int]]
-- perm [] = []
-- perm [x] = [[x]]
-- perm (x:xs) =
--  let ps = perm xs in 
--   [take i p ++ [x] ++ drop i p |  p <- ps, i <- [0..length p]]

-- -- Questão 1

-- -- zip
-- -- x = zip "abracadabra" [0..]
-- --zipWith

-- stringMatching :: String -> String -> [Int]
-- stringMatching p t =
--    [i | i <- [0..length t], isPrefix p (drop i t)]

-- isPrefix :: String -> String -> Bool
-- isPrefix [] _ = True
-- isPrefix _ [] = False
-- isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- -- Questão 4

-- coefGrau :: Int -> [Int] -> [Int] -> Int
-- coefGrau n p1 p2 =
--     sum [c1*c2 | (c1,g1) <- p1', (c2,g2) <- p2', g1 + g2 == n]
--         where
--         p1' = zip p1 [0..]
--         p2' = zip p2 [0..]

-- -- quais fafotes de multiplicam pra dar uma constante
-- -- quais fatores se multiplicam pra obter coeficiente de x




