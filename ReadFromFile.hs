howManyNumbers :: String -> IO()
howManyNumbers x = printEachNum [0..9] 
    where
        qtd s = [y| y<-x, ((fromEnum y)-48)==s]
        printEachNum [a] = print a qtd   
        printEachNum (a:as) = print a qtd >> 
                                printEachNum as
        print a qtd = putStr(show a) >> putStr("   ->   ") >> putStrLn(show(length(qtd a)))
        
                              
main :: IO()
main = putStr "Digite o nome do arquivo:" >>
       getLine >>=
            \st ->
            putStrLn "lendo arquivo" >>
            readFile st >>=
                \x ->
                    howManyNumbers x
