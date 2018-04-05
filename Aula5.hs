applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

total :: (Int -> Int) -> Int -> Int
total n 0 = n 0
total n x = n x + total n (x-1)

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 15
vendas 2 = 14

totalVendas :: Int -> Int
totalVendas n = total vendas n

zeroInRange :: (Int -> Int) -> Int -> Bool
zeroInRange f 0 = (f 0 == 0)
zeroInRange f n = zeroInRange f (n-1) || (f n == 0)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n = f n > f (n-1) && isCrescent f (n-1)

fold1 :: (u -> u->u) -> [u] -> u
fold1 f [a] = a
fold1 f (a:as) = f a (fold1 f as)

foldrev1 :: (u -> u->u) -> [u] -> u
foldrev1 f [a] = a
foldrev1 f (a:as) = f (foldrev1 f as) a

{--
Defina as seguintes funções sobre listas
– eleva os itens ao quadrado
• mapping => map (^ 2) [1,2]

– retorna a soma dos quadrados dos itens
• folding => foldl1 (+) (map (^2) [1,2])

– manter na lista todos os itens maiores que zero.
• filtering 
biggerZero :: Int -> Bool
biggerZero n = n > 0

filter biggerZero [0, -1, 2, 1]--}

{--
O que a função
– naosei l = foldr (++) [] (map sing l)
, onde sing a = [a], faz?

R - o Map pega todos os elementos da lista l e transforma
    em uma lista unitária com apenas o valor do elemento.
    Ex: map sing [1,2,3] = [[1],[2],[3]]
    Ja o foldr vai concatenar todos os elementos de volta,
    tornando naosei l = l; --}

maiores :: [[Int]] -> [Int]
maiores l = map onlyBig l
    where
        onlyBig [a] = a
        onlyBig (a:as) | (onlyBig as) < a = a
                       | otherwise = onlyBig as