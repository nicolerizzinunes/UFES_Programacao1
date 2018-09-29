module Item3 (excentricidades,distribuidora,minimo,desempate1) where

-- Gerar as excentricidades das cidades e pegar a menor:
excentricidades mat qtd = [(maximo (dijikstra_total x mat) 0) | x<- [0..(qtd-1)]]

-- Dijkstra de uma cidade: 
dijkstra n adj = listfy (aux dist adj)
 where dist = replaceNth n (zip (adj !! n) [0,0..])

listfy [] = []
listfy (x:xs) = fst x : listfy xs

replaceNth n (x:xs)
 | n == 0 = (fst x, 1):xs
 | otherwise = x:replaceNth (n-1) xs

aux dist adj
 | snd min == -1 = dist
 | otherwise = aux newDist adj
 where 
    min = minValido dist
    newDist = attDist dist dist adj (snd min) 0

minValido dist
 | (deletaInvalido (zip dist [0..])) == [] = ((0,0),-1)
 | otherwise = minimum (deletaInvalido (zip dist [0..]))

deletaInvalido [] = []
deletaInvalido (x:xs)
 | (snd $ fst x) == 1 = deletaInvalido xs
 | otherwise = x : deletaInvalido xs

attDist [] v _ n _ = (replaceNth n v)
attDist (d:dist) v adj n m
 | novoCusto < cost = attDist dist (attNth m novoCusto v) adj n (m+1)
 | otherwise = attDist dist v adj n (m+1)
 where 
    novoCusto = ((fst $ v !! n) + ((adj !! n) !! m))
    cost = (fst d)

attNth n novoCusto (x:xs)
 | n == 0 = (novoCusto, snd x):xs
 | otherwise = x:attNth (n-1) novoCusto  xs

-- Dijktra que ser[a chamado na main
dijikstra_total x mat = dijkstra x mat

-- Funções auxiliares gerais pra pegar o min e max de um vetor
maximo [] maior = maior
maximo (x:xs) maior = if (x > maior) then maximo xs x else maximo xs maior

minimo [] menor = menor
minimo (x:xs) menor = if (x < menor) then minimo xs x else minimo xs menor

infinito = 99999999

aparece _ [] _ = []
aparece y (x:xs) pos = if (y == x) then pos:(aparece y xs (pos+1)) else aparece y xs (pos+1)

-- Item 3) Distribuidora:
distribuidora :: [(a, (Double, Double))] -> [[Double]] -> Int -> Int
distribuidora cidades m qtd
 | (length d_possiveis == 1) = (d_possiveis !! 0)
 | otherwise                 = desempate1 d_possiveis cidades
 where
   ex = excentricidades m qtd
   menor = minimo ex infinito
   d_possiveis = aparece menor ex 0

-- Funções para desempate:
desempate1 d_possiveis cidades
 | (length d_possiveis_nova == 1) = d_possiveis_nova !! 0
 | otherwise                      = desempate2 d_possiveis_nova cidades
 where
    vetor_somas = somas d_possiveis cidades 0
    menor_soma = minimo vetor_somas infinito
    d_possiveis_nova = menores_somas menor_soma vetor_somas d_possiveis

menores_somas _ _ [] = []
menores_somas menor_soma (s:somas) (p:ps) = if (menor_soma == s) then p:(menores_somas menor_soma somas ps) else (menores_somas menor_soma somas ps)

-- Desempate por soma das coordenadas
somas d_possiveis cidades aux = if (aux < (length d_possiveis)) then ((fst coord)+(snd coord)) : (somas d_possiveis cidades (aux+1)) else []
 where
    cidade = (cidades !! (d_possiveis !! aux))
    coord = snd cidade

desempate2 d_possiveis cidades = d_possiveis !! (d !! 0)
 where
    primeiras = primeira_coordenada d_possiveis cidades 0
    menor = minimo primeiras infinito
    d = aparece menor primeiras 0

-- Desempate por primeira coordenada
primeira_coordenada d_possiveis cidades aux
 | aux >= (length d_possiveis) = []
 | otherwise = (fst (snd (cidades !! ( d_possiveis !! aux )))) : (primeira_coordenada d_possiveis cidades (aux+1))


