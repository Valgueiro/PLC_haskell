----- Made By: Mateus Valgueiro Teixeira [mvt]

-- Q1:
kSmallest :: [Int] -> Int -> [Int]
kSmallest as z =  onlyFirst (qsortscnd (take z organizedAs))
        where
            qsortfrst [] = []
            qsortfrst ((c,d):ss) = qsortfrst [(x,y) | (x,y)<-ss, x<c] ++ [(c,d)] ++ qsortfrst [(x,y) | (x,y)<-ss, x>=c] 

            qsortscnd [] = []
            qsortscnd ((c,d):ss) = qsortscnd [(x,y) | (x,y)<-ss, y<d] ++ [(c,d)] ++ qsortscnd [(x,y) | (x,y)<-ss, y>=d] 

            organizedAs = qsortfrst (zip as [0..]) -- ordena pelo tamanho

            onlyFirst [] = []
            onlyFirst ((c,d):ss) = [c] ++ onlyFirst ss

-- Q2:
isDivisible :: [Int] -> Int -> Bool
isDivisible [] _ = False
isDivisible pt@(p:ps) a | a == 1 = True
                     | mod a p == 0 = isDivisible pt (div a p)
                     | otherwise = isDivisible ps a


composites :: [Int] -> [Int] -> [Int]
composites ps ns = [x| x <- ns, isDivisible ps x]


-- Q3:
isEqual :: [Int] -> [Int] -> Bool  -- ve se duas listas têm os mesmos algarismos
isEqual [] [] = True
isEqual [] xs = True
isEqual xs [] = False
isEqual (x:xs) ys = length [z| z<-ys, z == x] /= 0 && isEqual xs ys

alreadyCounted :: [Int] -> [[Int]] -> Bool -- checa se alguma lista já existe numa lista de lista baseado em IsEqual
alreadyCounted a bs = length [z| z<-bs, length a == length z && isEqual z a] /= 0

nonRepeated :: [[Int]] -> [[Int]] -- tira as combinações repetidas se baseando nas duas últimas funções
nonRepeated [] = []
nonRepeated [x] = [x]
nonRepeated (x:xs) | alreadyCounted x counted = counted
                   | otherwise = [x] ++ counted
                   where 
                        counted = nonRepeated xs

combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations [a] = [[a]] ++ combinations []
combinations (x:xs) = nonRepeated allCombinations --Retira todas as combinações com repetições
    where
        allAnagrams = [take i p ++ [x] ++ drop i p |  p <- combinations xs, i <- [0..length p]] --todos os anagramas
        allCombinations = combinations [x] ++ combinations xs ++ allAnagrams --pega todas as combinações do termo que eu estou,
                                                                            -- do resto da cadeia, e os anagramas envolvendo os dois
        
