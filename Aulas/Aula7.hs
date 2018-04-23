
data Estacao = Inverno | Verao | Outono | Primavera

data Temp = Frio | Quente

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " -- " ++ show a

data Shape = Circle Float | Rectangle Float Float

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = 3.14*r*r
area (Rectangle l h) = l*h

data List t = Nil | Cons t (List t)
instance (Show t) => Show (List t) where
    show Nil = "Null"
    show (Cons t (as)) = show(t) ++ " -> " ++ show (as)

data Tree t = NilT | Node t (Tree t) (Tree t)
instance (Show t) => Show (Tree t) where
    show NilT = "Null"
    show (Node t l r) = show (t) ++ "(" ++ show l ++ ") (" ++ show r ++ ")" 

toList :: List t -> [t]
toList Nil = []
toList (Cons t (as)) = [t] ++ (toList as)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

depth :: Tree t -> Int
depth NilT = 0
depth (Node a l r) = 1 + max (depth l) (depth r) 

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node a l r) = [a] ++ collapse l ++ collapse r

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f (NilT) = NilT
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)