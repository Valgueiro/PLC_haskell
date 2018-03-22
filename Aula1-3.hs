--- Aula 2
maxi :: Int -> Int -> Int
maxi n m | n >= m = n
         | otherwise = m

fat :: Int -> Int
fat 0 = 1 
fat n = n*fat(n-1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = eq1 && eq2 && eq3
    where eq1 = a==b
          eq2 = b==c
          eq3 = c==d

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = let eq1 = a==b
                     eq2 = b==c 
                    in eq1 && eq2

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 11
vendas 2 = 12
vendas 3 = 14

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = maxi (maxVendas (n-1)) (vendas n)

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = totalVendas (n-1) + vendas n

--- Aula 3
--Integer: precisão arbitrária
--Int: precisão fixa (bounded)

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1) 

paraDireita :: Int -> String -> String
paraDireita 0 str = str
paraDireita n str = (addEspacos n) ++ str 

cabecalho :: String
cabecalho = "Semana   Venda\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = paraDireita 2 (show(0)) ++ paraDireita 8 (show(vendas 0)) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++ paraDireita 2 (show(n)) ++ paraDireita 8 (show(vendas n)) ++ "\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ paraDireita 6 (show(vendas 0)) ++ "\n"

mediaVendas :: Int -> Float
mediaVendas 0 = fromIntegral (vendas 0)
mediaVendas n = fromIntegral (div (totalVendas n) n)

imprimeMedia :: Int -> String
imprimeMedia n = "Media" ++ paraDireita 6 (show(mediaVendas n)) ++ "\n"


imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho ++
                        imprimeSemanas n ++
                        imprimeTotal n ++
                        imprimeMedia n)

------------------------
menor :: Int -> Int -> Int -> Int
menor a b c | a<=b && a<=c = a
            | b<=c && b<=a = b
            | otherwise = c

maior :: Int -> Int -> Int -> Int
maior a b c | a>=b && a>=c = a
            | b>=c && b>=a = b
            | otherwise = c


menorMaior ::  Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (x, y)
        where x = menor a b c
              y = maior a b c

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) = (low, med, big)
    where big = maior a b c
          low = menor a b c
          med | a /= low && a/= big = a
              | b /= low && b/= big = b
              | otherwise = c

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

newPonto:: Float -> Float -> Ponto
newPonto a b = (a,b)

newReta :: Ponto -> Ponto -> Reta
newReta a b = (a, b)

getX :: Ponto -> Float
getX (a, b) =  a

getY :: Ponto -> Float
getY (a, b) =  b

isVertical :: Reta -> Bool
isVertical (pA, pB) = getX pA == getX pB

pontoY :: Float -> Reta -> Float
pontoY x (p1, p2) = ((getY p2 - getY p1)/(getX p2 - getX p1))*(x - getX p1) + getY p1