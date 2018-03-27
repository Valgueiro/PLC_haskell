sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

double :: [Int] -> [Int]
double [] = [] 
double (a:as) = 2*a:double as

member :: [Int] -> Int -> Bool
member [] a = False
member (a:as) n | n == a = True
                | otherwise = member as n

digits :: String -> String
digits [] = []
digits (a:as) | a>='0' && a<='9' = a:digits as
              | otherwise = digits as

sumPairs :: [(Int,Int)]->[Int]
sumPairs [] = []
sumPairs ((x,y):as) = (x+y):sumPairs as

--Funções polifórmicas length() e ++ (concatenação)

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr.Norrell"),
               ("Fernando","Duna")]

getFirst :: (a,b) -> a
getFirst (x,y) = x 

getSecond :: (a,b) -> b
getSecond (x,y) = y

livros :: BancoDados -> Pessoa -> [Livro]
livros [] p = []
livros ((frst,scnd):as) p | p == frst = scnd:livros as p
                          | otherwise = livros as p

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] l = []
emprestimos ((x,y):as) l | l == y = x:emprestimos as l
                         | otherwise = emprestimos as l

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = (length (emprestimos bd l) /= 0) 

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd p l = bd ++ [(p,l)] 

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = [] 
devolver ((pb,lb):as) p l | pb == p && lb == l = devolver as p l
                          | otherwise = (pb,lb):devolver as p l

----------- Com Compreensão

membro :: [Int] -> Int -> Bool
membro as a = length [x| x <- as, x == a] /= 0 

livros' :: BancoDados -> Pessoa -> [Livro]
livros' as p = [y|(x,y)<-as, x == p]

emprestimos' :: BancoDados -> Livro -> [Pessoa]
emprestimos' as l = [x|(x,y)<-as, y==l]

devolver' :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver' bd p l = [x| x <- bd, (p,l)/=x]

qk :: [Int] -> [Int]
qk [] = []
qk (a:as) = qk[x| x<-as, x < a] ++ [a] ++ qk[x|x<-as, x>=a]

-------------- Class Exemples
insert :: [Int] -> Int-> [Int]
insert [] n = [n]
insert xs n = [y| y <- xs, y < n] ++ [n] ++ [z| z <- xs, z >= n] 

-- ins n aas@(a:as)
insSort :: [Int] -> [Int]
insSort [] = []
insSort (x:xs) = insert (insSort xs) x 


