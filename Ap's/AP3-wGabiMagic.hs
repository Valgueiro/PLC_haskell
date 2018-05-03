-- Made by the porofesor assistant

{-  1)
    f1 :: (a -> b) -> [a] -> [b]
    f2 :: (a -> b -> a) -> a -> [b] -> a
    f3 :: (a -> b -> a) -> a -> [b] -> a
    f4:: (a -> Bool) -> [a] -> ([a], [a])
    (+) :: (Num a) => a -> a -> a
    (.) :: (b -> c) -> (a -> b) -> a -> c

    Importante:
    - O construtor de tipo (->) é associativo à direita, o que significa que o tipo a -> b -> c deve ser interpretado como a -> (b -> c).
      É interessante notar como este fato está relacionado com aplicação parcial.
    - Aplicação de função é associativa à esquerda, ou seja, f g h = (f g) h.
    - Operadores como +, ., -, * são infixos, ou seja, são escritos entre seus argumentos. No entanto, eles podem ser usados como funções,
      basta colocá-los entre parenteses. Então 3 + 4 pode ser reescrito como (+) 3 4, e f1 . f2 pode ser reescrito como (.) f1 f2.

    a) f3.f4
    Temos os seguintes tipos:
    f3 :: (a3 -> b3 -> a3) -> a3 -> [b3] -> a3
    f4:: (a4 -> Bool) -> [a4] -> ([a4], [a4])
    (.) :: (b -> c) -> (a -> b) -> a -> c

    Aplicando (.) a f3
    (.) f3 = (a -> b) -> a -> c, em que (b -> c) = (a3 -> b3 -> a3) -> (a3 -> ([b3] -> a3)), pois o tipo do primeiro argumento de (.) deve ser o tipo de f3.
    Então   b = (a3 -> b3 -> a3)
            c = (a3 -> ([b3] -> a3)).

    Aplicando (.) f3 a f4:
    (.) f3 f4 = a -> c, em que (a -> b) = (a4 -> Bool) -> ([a4] -> ([a4], [a4])), pois o tipo do primeiro argumento de (.) f3 deve ser o tipo de f4.
    Então   a = (a4 -> Bool)
            b = ([a4] -> ([a4], [a4]))

    Dos passos anteriores, temos:
            b = (a3 -> (b3 -> a3)) = ([a4] -> ([a4], [a4])), logo:  a3 = [a4]
                                                                    (b3 -> a3) = ([a4], [a4]), o que não é possível já que uma função não pode ser igual a uma tupla.
    Portanto há um erro de tipos.

    c) f1.f2 (+)
    Temos os seguintes tipos:
    f1 :: (a1 -> b1) -> ([a1] -> [b1])
    f2 :: (a2 -> b2 -> a2) -> (a2 -> ([b2] -> a2))
    (+) :: (Num a3) => a3 -> a3 -> a3
    (.) :: (b -> c) -> (a -> b) -> a -> c

    A expressão f1 . f2 (+) pode ser reescrita como (.) f1 (f2 (+))

    i) Aplicando f2 a (+):
    f2 (+) :: (a2 -> ([b2] -> a2)), em que (a2 -> b2 -> a2) = (Num a3) => a3 -> a3 -> a3, pois o tipo do primeiro argumento de f2 deve ser o tipo de (+).
    Então   a2 = a3
            b2 = a3
            a2 = a3, em que a3 é um tipo que instancia a classe Num (a3 é um número),
    e o tipo (a2 -> ([b2] -> a2)) pode ser reescrito como Num a3 => (a3 -> ([b3] -> a3)).
    
    ii) Aplicando (.) a f1:
    (.) f1 :: (a -> b) -> a -> c, em que (b -> c) = (a1 -> b1) -> ([a1] -> [b1]), pois o tipo do primeiro argumento de (.) deve ser o tipo de f1.
    Então   b = (a1 -> b1)
            c = ([a1] -> [b1])

    iii) Aplicando (.) f1 a (f2 (+)):
    (.) f1 (f2 (+)) :: a -> c, em que (a -> b) = Num a3 => (a3 -> ([b3] -> a3)), pois o tipo do segundo argumento de (.) deve ser o tipo de (f2 (+)).
    Então   a = a3
            b = ([b3] -> a3).
    Comparando as expressões para b obtidas em iii) e ii), temos:
            b = ([b3] -> a3) = (a1 -> b1), logo:    a1 = [b3]
                                                    b1 = a3.
    Substituindo as igualdades obtidas no tipo de (.) f1 (f2 (+)), que é (a -> c), temos que o tipo da expressão é:
        (a -> c) = a3 -> ([a1] -> [b1]) = a3 -> ([[b3]] -> [a3]).
    O tipo final da expressão é Num a3 => a3 -> ([[b3]] -> [a3]), pois a3 está restrito a um número.
-}

data LQueue t = LQ [t] deriving (Show)
data RQueue t = Empty | RQ t (RQueue t) deriving (Show)

class OprQueue q where
    enqueue :: q t -> t -> q t
    dequeue :: q t -> q t
    isEmpty :: q t -> Bool
    peek :: q t -> t

instance OprQueue LQueue where
    enqueue (LQ xs) x = LQ (xs ++ [x])

    dequeue (LQ (x : xs)) = LQ xs
    dequeue (LQ []) = error "Empty Queue!"

    isEmpty (LQ []) = True
    isEmpty _ = False

    peek (LQ (x : xs)) = x
    peek (LQ []) = error "Empty Queue!"

instance OprQueue RQueue where
    enqueue Empty x = RQ x (Empty)
    enqueue (RQ y q) x = RQ y (enqueue q x)

    dequeue (RQ x q) = q
    dequeue Empty = error "Empty Queue!"

    isEmpty Empty = True
    isEmpty _ = False

    peek (RQ x q) = x
    peek Empty = error "Empty Queue!"
