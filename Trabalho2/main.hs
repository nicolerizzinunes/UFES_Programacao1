{- Trabalho 2 - Aluna: Nicole Rizzi Nunes -}

import System.Environment
import System.IO
import Item3
import Item4

main = do 
    arq <- readFile "nome-coord.txt"
    arq1 <- readFile "custos.txt"
    let cidades = lenomes (lines arq)
    let qtd = length cidades
    let custos = lecustos (lines arq1)
    let m = matriz_custos cidades custos qtd
    let ex = excentricidades m qtd
    putStrLn (show ex)
    let d = distribuidora cidades m qtd
    let caminho = item4 d cidades m 

    escreve (fst(cidades!!d)) (minimo ex 999999) caminho
    return ()


-- Funções de leitura:
lecustos [] = []
lecustos (x:xs) = [[(read y :: Double) | y <- (words x)]] ++ (lecustos xs)

lenomes [] = []
lenomes (x:y:xs) = if (x /= "") then (x,coord) : (lenomes xs) else []
 where coord = lecoordenada (words y)

lecoordenada (x:y:z) = ((read x :: Double),(read y :: Double))

imprimedados d cidades = do putStrLn (show (cidades !! d))


--Funções auxiliares para geração da matriz de custos:
matriz_custos cidades custos qtd = [ (linha_custos cidades custos c1 qtd) | c1<-[1..qtd]]

linha_custos cidades custos c1 qtd = [ (calculacusto c1 c2 cidades custos) | c2<-[1..qtd]]

-- Custo entre a cidade c1 indo para c2:
calculacusto c1 c2 cidades custos = ((custos !! (c1-1)) !! (c2-1)) * (distancia (snd dados1) (snd dados2))
 where 
    dados1 = cidades !! (c1-1)
    dados2 = cidades !! (c2-1)

-- Distância entre duas cidades:
distancia (x1,y1) (x2,y2) = sqrt (d1^2 + d2^2)
 where
    d1 = (x2-x1)
    d2 = (y2-y1)

-- Funções de saída:
escreve cidade ex caminho = do
    saida <- openFile "saida.txt" WriteMode
    hPutStrLn saida (cidade)
    hPutStrLn saida (show ex)
    hPutStrLn saida (show (snd caminho))
    hPutStrLn saida (show (fst caminho))
    hClose (saida)
    return ()
