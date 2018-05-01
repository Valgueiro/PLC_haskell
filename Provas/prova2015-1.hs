{-- 2) Um sorteio da Mega-Sena pode ser representado por uma lista de seis números.
Um conjunto de cartões de apostas pode ser representado por uma lista
de listas (cada lista representando um cartão).
Assuma que os número do resultado premiado/sorteado e os números em cada cartão
estão ordenados.
Defina funções para:
(1.0 ponto) a função premiados retorna o numero de cartões premiados com a sena. --}
type Resultado = [Integer]
type Jogos = [[Integer]]

res = [1,2,3,4,5,6]
jogos = [[1,2,3,4,5,6], [1,2,3,4,6,7], [1,2,3,4,5,6]]

qtdEqual :: [Integer] -> [Integer] -> Integer
qtdEqual [] [] = 0
qtdEqual (a:as) (b:bs) | a == b = 1 + qtdEqual as bs
                       | otherwise = qtdEqual as bs
                       
premiados :: Resultado -> Jogos -> Int
premiados _ [] = 0
premiados res (a:as) | qtdEqual res a == 6 = 1 + premiados res as
                     | otherwise = premiados res as
{-- (1.0 ponto) a função acertos retorna a lista com o número de acertos em cada
cartão (do primeiro cartão, do segundo, do terceiro, etc.). --}
acertos :: Resultado -> Jogos -> [Integer]
acertos _ [] = []
acertos res (a:as) = (qtdEqual res a):acertos res as
{-- (1.0 ponto) a função numPremios retorna uma tupla de três inteiros contendo a
quantidade de cartões premiados com 4, 5 ou 6 acertos, respectivamente. --} 
numPremios :: Resultado -> Jogos -> (Int, Int, Int)
numPremios res jogs = (prem 4, prem 5, prem 6)
    where 
        prem = (\x -> length [a | a <- (acertos res jogs), a==x])
{-- 3) Uma linguagem de programação baseada em pilha possui apenas uma pilha (stack)
onde ficam os dados/operandos e todas as instruções são apenas de empilhar,
desempilhar ou fazer operações consumindo (lendo) os dados no topo da pilha e
deixando o resultado final o topo da pilha.
Dados os tipos de dados abaixo e os exemplos, escreva um interpretador que
executa as instruções com o comportamento abaixo: --}
data Instrucao = PUSH Int -- empilha um valor inteiro
 | POP -- desempilha (remove) um valor do topo da pilha
 | ADD -- remove (lê) os dois valores no topo da pilha e deixa a soma deles no topo da pilha
 | SUB -- remove (lê) os dois valores no topo da pilha e deixa a soma deles no topo da pilha
 | DUP -- repete o mesmo valor no topo da pilha (duplica ele)
 deriving (Show)
type Pilha = [Int]
-- (1.0 ponto) evalI avalia uma única instrução em uma dada pilha
evalI :: Instrucao -> Pilha -> Pilha
evalI (PUSH i) stack = i:stack
evalI POP (s:stack) = stack
evalI ADD (s:t:stack) = (s+t):stack
evalI SUB (s:t:stack) = (s-t):stack
evalI DUP (s:stack) = s:s:stack

-- exemplos: eval ADD [1,2,3,4,5] ---> [3,3,4,5] (soma 1+2)
-- exemplos: eval DUP [5,1] ---> [5,5,1] (repete/copia o valor no topo da pilha)
-- exemplos: eval SUB [1,2,3,4] ---> [-1,3,4] -- calcula 1-2=-1
-- exemplos: eval ADD [1,2,3,4,5] ---> [3,3,4,5] (soma 1+2)
-- exemplos: eval PUSH 7 [1,2,3] ---> [7,1,2,3] insere o 7 no topo
-- exemplos: eval POP [8,2,3] ---> [2,3] -- remove 8
{-- (1.0 ponto) evalProg avalia um programa (sequência de instruções) a partir de
uma pilha inicial vazia e retorna o estado final da pilha depois da avaliação --}
evalProg :: [Instrucao] -> Pilha 
evalProg i =  evalProg' i []
        where
            evalProg' [] as = as 
            evalProg' (i:is) as = evalProg' is (evalI i as)
-- exemplos: evalProg [PUSH 3, PUSH 5, DUP, ADD, SUB] ---> [7] (5+5-3)

{-- 4) (2.0 pontos) faça uma função translate que traduz expressões (tipo Expr,
abaixo) para uma sequência (lista) de instruções que, se usadas com o avaliador
da questão 4, avaliam a expressão.
Exemplo: --}
data Expr = Literal Int -- um número
 | Soma Expr Expr -- soma as duas expressões
 | Subtrai Expr Expr -- subtrai a segunda expressão da primeira
 | Dobra Expr -- dobra o valor da expressão
translate :: Expr -> [Instrucao]
translate (Literal a) = [PUSH a]
translate (Soma (expr1) (expr2)) = translate expr2 ++ translate expr1 ++ [ADD] 
translate (Subtrai (expr1) (expr2)) = translate expr2 ++ translate expr1 ++ [SUB]  
translate (Dobra (expr1)) = translate expr1 ++ [DUP]  ++ [ADD] 
-- translate (Soma (Literal 5) (Dobra (Subtrai (Literal 4) (Literal 1)))) 
-- ----> [PUSH 1, PUSH 4, SUB, DUP, ADD, PUSH 5, ADD]
{-- 5) (1.0 ponto) qual a diferença entre avaliação estrita e preguiçosa (lazy)?
Mostre exemplos desta diferença. --}