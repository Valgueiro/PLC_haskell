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


{- combinations :: [Int] -> [[Int]] -}