--Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 1/2017 - 01/05/2017
--
-- Nome: 
--
{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo: isSorted [1,6,8,9,9] ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}
isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [a,b] = a<=b
isSorted (a:b:as) = a<=b && isSorted(b:as)

{- 2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
   cada elemento da lista de entrada é comparado com o seguinte, 
   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
   (nenhuma troca seja mais necessária).
   exemplo, passo a passo: 
       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
                                   ----> [4,3,6,1,8,8]
       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
Implemente a função bSort.
Dica 1: use funções auxiliares, que façam parte do processo;
Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.
-}
bSort :: Ord t => [t] -> [t]
bSort lst | isSorted (iter) = iter
 	  | otherwise = bSort iter 
	where 
           bSortAux [] = []
	   bSortAux [a,b]| a > b = [b,a]
			 | otherwise = [a,b]
	   bSortAux (a:b:as) | a > b = [b] ++ bSortAux (a:as)
			     | otherwise = [a] ++ bSortAux (b:as)

	   iter = bSortAux lst

{- 3) (2.5) explique como funciona e informe qual o resultado da execução das 
   seguintes expressões. Caso estejam erradas explique por que.
a) map (\x -> x + x) [3,5,7,9]
b) filter (\x -> x < 7) [5,7,9,11]
c) foldr1 (*) [-2,0,2,4]
d) foldr (+) 20 [-2,0,2,4]
e) (map (+2) . filter (<7)) [5,7,9,11]
-}

{--
a) [6,10,14,18]
b) [5]
c) 0
d) 24
e) [7]
--}
{- -}
{- 4) (2.5) Dada o tipo de dados Tree t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada, ou seja, os valores em nós ou folhas na sub-àrvore à esquerda são sempre menores ou iguais ao valor do nó, e os da sub-árvore à direita sempre maiores ou iguais.
-}
data Tree t = Node t (Tree t) (Tree t) 
            | Leaf t

--testeOrdenado :: Tree Int
--testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
--testeNaoOrdenado :: Tree Int
--testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))

biggestElem :: Ord t => Tree t -> t
biggestElem (Leaf t) = t
biggestElem (Node n l r) | n >= (biggestElem r) = n
		         | otherwise = biggestElem r

smallestElem :: Ord t => Tree t -> t
smallestElem (Leaf t) = t
smallestElem (Node n l r) | n < (smallestElem l) = n
		          | otherwise = smallestElem l

isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf t) = True
isSortedTree (Node n l r) = (n > biggestElem l) && (n <= smallestElem r) && (isSortedTree l) && (isSortedTree r) 
