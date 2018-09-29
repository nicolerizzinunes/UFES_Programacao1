module Item4 (item4) where 

infinito = 999999

-- Item 4)

-- Inicialização do vetor
vetorinicial cidade qtd_cidades = (take cidade x) ++ [(0,0)] ++ (drop (cidade+1) x)
 where x = [(-1,99999) | x <- [1..qtd_cidades]] -- (-1 significa cidade não percorrida ainda)

caminho_distribuidora cidade atual v cidades custo_total caminho_total matriz_custos
 | (length caminho_total) == (length cidades)     = volta atual cidade cidades custo_total caminho_total matriz_custos
 | otherwise                                      = caminho_distribuidora cidade caminho_soma novo_v cidades (custo_total+custo_soma) (caminho_total ++[caminho_soma]) matriz_custos
 where
    tupla = calcula_prox atual 0 cidades (infinito,0) v matriz_custos
    custo_soma = fst (fst tupla)
    caminho_soma = snd (fst tupla)
    novo_v = snd tupla

--Função que faz a volta da última cidade pra cidade
volta atual cidade cidades custo_total caminho_total matriz_custos = ((custo_total+custo_soma),(caminho_total))
 where custo_soma = 0 --matriz_custos !! atual !! cidade

calcula_prox atual prox cidades (menor,destino) v matriz_custos
 | prox == atual           = calcula_prox atual (prox+1) cidades (menor,destino) v matriz_custos
 | prox >= (length cidades) = ((menor,destino),((take (destino) v) ++ [(menor,destino)] ++ (drop (destino+1) v))) 
 | (visitado prox v)       = calcula_prox atual (prox+1) cidades (menor,destino) v matriz_custos
 | custo < menor           = calcula_prox atual (prox+1) cidades (custo,prox) v matriz_custos
 | custo == menor          = calcula_prox atual (prox+1) cidades (custo,(desempate1 [destino,prox] cidades)) v matriz_custos
 | otherwise               = calcula_prox atual (prox+1) cidades (menor,destino) v matriz_custos
 where custo = matriz_custos !! atual !! prox

visitado vertice v = ( (fst ( v !! vertice)) /= -1.0)

item4 cidade cidades m = caminho_distribuidora cidade cidade (vetorinicial cidade (length cidades)) cidades 0.0 [cidade] m

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

maximo [] maior = maior
maximo (x:xs) maior = if (x > maior) then maximo xs x else maximo xs maior

minimo [] menor = menor
minimo (x:xs) menor = if (x < menor) then minimo xs x else minimo xs menor

aparece _ [] _ = []
aparece y (x:xs) pos = if (y == x) then pos:(aparece y xs (pos+1)) else aparece y xs (pos+1)
