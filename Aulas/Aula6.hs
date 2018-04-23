suc :: Int -> Int
suc n = n+1

twice :: (t-> t) -> (t->t)
twice f = f.f

iter :: Int -> (t -> t) -> (t -> t)
iter 1 f = id -- id a = a
iter n f = (iter (n-1) f).f

double :: Int -> Int
double a = 2*a

-- map (\x->x+1) [1,2,3]

-- addNum = (\n->\m-> m+n)
-- add3 = addNum 3

-- let swapeson f = (\u -> \t -> f t u)

multiply :: Int -> Int -> Int
multiply a b = a*b

doubleList :: [Int] -> [Int]
doubleList = map (multiply 2)

-- map (>2) [1,2,4] = [False, Flase, True]
-- (filter (>0) . map (+1)) [-1,1,2]