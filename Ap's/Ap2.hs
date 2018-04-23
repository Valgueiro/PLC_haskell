-- Q1
derivate :: (Double -> Double) -> (Double -> Double) -- recebe f e retorna a derivada da função 
derivate f = (\x -> ((f (x+delta)) - (f x))/delta)
    where delta = 0.000001

formula :: (Double -> Double) -> Double -> Double -> Double -> Double
formula f x0 eps xPrev | abs(xNow - xPrev) < eps = xNow
                       | otherwise = formula f xNow eps x0
    where   
        xNow = calc f x0
        calc f x = x - ((f x)/(derivate f x)) 

newton :: (Double -> Double) -> Double -> Double -> Double
newton f x0 eps = formula f x0 eps x0

-- Q2
{- A) Como thrice apenas pega uma função, que neste caso é o map, 
		e aplica ela 3 vezes a certo argumento ele apenas vai esperar
		a função de entrada (f) e o valor de "x" que será usado como entrada 
		para função(x). Com isso, sabemeremos que o map f x será executado 
		3 vezes, e a função f será mapeada para cada elemento de x. 
		Assim: (.) thrice map :: (a->a) -> [a] -> [a]

	B) Não é possivel dizer, pois dará um erro na interpretação. Isso pois,
		após o swap trocar a ordem das duas funções, o thrice estará
		esperando uma função que o tipo de entrada seja diferente do 
		tipo de saída (a -> b), enquanto o map é da forma (a->a).
	
	C) Nesse caso, também não é possivel dizer devido à um erro. Isso pois,
		primeiramente, ela receberá uma Lista e pegará apenas o primeiro elemento,
		devido à função head. Após isso, ela tentará usar este elemento como entrada 
		para a função tail, que retornará um erro para o usuário, pois esta espera 
		novamente uma lista.
-}

-- Q3
getStrsByPos :: Int -> [[String]] -> [String]
getStrsByPos n strs = [w | [(z,w)] <- aws]
	where
		-- Pega cada string da lista de listas e numera pela posição, 
		-- depois filtra, em cada lista, apenas a da posição desejada
		aws = map (filter (\(x,y) -> x==n)) (map (zip [1..]) strs)

countReps :: String -> [String] -> Int
countReps str strs = length [x| x<-strs, x == str] 

removeReps :: Eq a => [a] -> [a]
removeReps [] = []
removeReps (x:xs)   | elem x xs   = removeReps xs
						  | otherwise   = x : removeReps xs
							
zipRepsPos :: Int -> [[String]] -> [(String, Int)]
zipRepsPos n strs = removeReps [(x, countReps x inPos)| x <- inPos]
	where
		inPos = getStrsByPos n strs

getWinner :: [(String, Int)] -> Int -> [String]
getWinner strs length= [x| (x,y) <- strs, y > div (length) 2] 


mine:: Int -> Int -> Int
mine a b = if a<b then a else b

tieBraker :: [String] -> [[String]] -> Int -> [String]
tieBraker winners strs n | (length (biggerIndexes)) == 1 = biggerIndexes --vencedor
								 | (length (biggerIndexes)) == 0 = tieBraker winners strs (n+1)
								 | otherwise = tieBraker biggerIndexes strs (n+1)
		where
			atualRanked = zipRepsPos n strs
			minIndex = foldr1 mine [y| (x,y) <- atualRanked, elem x winners] --pegar o menor dos indexes de quem é winner
			biggerIndexes = [x | (x,y) <- atualRanked, elem x winners, y > minIndex] -- todos os winners que nao tem o menor index


winner :: [[String]] -> [String]
winner strs | (length winners) == 1 = winners
				| (length winners) == 0 = tieBraker (head strs) strs 2
				| otherwise = tieBraker winners strs 2
				
		where
			winners = getWinner (zipRepsPos 1 strs) (length (head strs))
