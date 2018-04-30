--- Q1
{-- A) A função f3 espera como entrada uma função e mais 2 argumentos, dos quais o tipo do primeiro 
--  argumento e da saída devem ser iguais, o que não acontece com a função f4, que recebe também 
--  uma função, mas recebe apenas uma lista de um tipo e retorna um par de duas listas.
--  Para que esta concatenação desse certo, deveriamos utilizar f4 do tipo [a] -> b -> [a] --}

{-- B) Primeiramente, a função (+) servirá como primeiro parâmetro para f2, tornando o tipo
    de f2 (+) :: a -> [a] -> a (lembrando que tipo b = tipo a nesse caso pois (+) :: a -> a -> a).
    Portanto, esta função será recebida como parâmetro para f1
     




--}

f1 :: (a -> b) -> [a] -> [b]
f1 f a = map f a
f2 :: (a -> b -> a) -> a -> [b] -> a
f2 f a b = a

f3 :: (a -> b -> a) -> a -> [b] -> a
f3 f a b = a
f4 :: (a -> bool) -> [a] -> ([a],[a])
f4 f a = (a,a)

--- Q2
data Temperatura = Celsius Float | Fahrenheit Float | Kelvin Float
instance Show Temperatura where
    show (Celsius n)= show(n) ++ " C"
    show (Fahrenheit n) = show(n) ++ " F"
    show (Kelvin n) = show(n) ++ " K"

instance Eq Temperatura where
    (==) (Celsius c) (Fahrenheit f) = c == (5*(f-32)/9)
    (==) (Fahrenheit f) (Celsius c) = Celsius c == Fahrenheit f 
    (==) (Celsius c) (Kelvin k) = c == (k-273)
    (==) (Kelvin k) (Celsius c) = Celsius c == Kelvin k
    (==) (Fahrenheit f) (Kelvin k) = ((f-32)/9) == ((k-273)/5)
    (==) (Kelvin k) (Fahrenheit f) = Fahrenheit f == Kelvin k

instance Ord Temperatura where
    (compare) (Celsius c) (Fahrenheit f) = c `compare` (5*(f-32)/9)
    (compare) (Fahrenheit f) (Celsius c) = (5*(f-32)/9) `compare` c
    (compare) (Celsius c) (Kelvin k) = c `compare` (k-273)
    (compare) (Kelvin k) (Celsius c) = (k-273)  `compare`  c
    (compare) (Fahrenheit f) (Kelvin k) = ((f-32)/9) `compare` ((k-273)/5)
    (compare) (Kelvin k) (Fahrenheit f) = ((k-273)/5) `compare` ((f-32)/9)

minMax :: [Temperatura] -> (Temperatura,Temperatura)
minMax temps = (getMin temps (head temps), getMax temps (head temps))
    where 
        getMin [] res = res
        getMin (a:as) x | a < x = getMin as a
                        | otherwise = getMin as x
        
        getMax [] res = res
        getMax (a:as) x | a > x = getMax as a
                        | otherwise = getMax as x



-- Q3
data LQueue t = LQ [t] deriving (Show)
data RQueue t = Empty | RQ t (RQueue t) deriving (Show)
