module Funcoes ( inserecompromisso, cancelacompromisso, reagendamento, disponivel, horarios_disponiveis, insere_breve, insere_min, insere_max) where

-- 1) Inserção de compromisso
inserecompromisso :: (Eq a, Num a, Num a1, Num a2, Num a3, Ord a1, Ord a2, Ord a3) => [(a3, [(a1, [(a2, a)])])] -> (Bool, [(a3, a1)]) -> a3 -> a1 -> a2 -> a -> [(a3, [(a1, [(a2, a)])])]
inserecompromisso [] _ mes dia inicio dur  = [(mes, [(dia, [(inicio,dur)])])]
inserecompromisso (x:xs) inf mes dia inicio dur 
 | (disponivel (x:xs) inf mes dia inicio dur) == False = (x:xs)
 | (fst x) == mes                                      = (mes, insere_comp_mes (snd x) dia inicio dur):xs
 | (fst x) > mes                                       = (mes, [(dia, [(inicio,dur)])]) : (x:xs)
 | otherwise                                           = x : (inserecompromisso xs inf mes dia inicio dur)

insere_comp_mes :: (Ord t, Ord a) => [(a, [(t, t1)])] -> a -> t -> t1 -> [(a, [(t, t1)])]
insere_comp_mes [] dia inicio dur = [(dia, [(inicio,dur)])] 
insere_comp_mes (x:xs) dia inicio dur 
 | (fst x) == dia = (dia, (insere_comp_dia (snd x) inicio dur)):xs
 | (fst x) > dia  = (dia, [(inicio,dur)]):(x:xs)
 | otherwise      = x:(insere_comp_mes xs dia inicio dur)

insere_comp_dia :: Ord a => [(a, t)] -> a -> t -> [(a, t)] 
insere_comp_dia [] inicio dur = [(inicio,dur)]
insere_comp_dia (x:xs) inicio dur
 | (fst x) > inicio = (inicio,dur):(x:xs)
 | otherwise        = x:(insere_comp_dia xs inicio dur)
 
-- 2) Cancelamento de compromisso
cancelacompromisso :: (Eq b, Ord a, Ord a1, Ord a2) => [(a2, [(a1, [(a, b)])])] -> a2 -> a1 -> a -> [(a2, [(a1, [(a, b)])])]
cancelacompromisso [] mes dia inicio = []
cancelacompromisso (x:xs) mes dia inicio
 | (fst x) == mes  = (mes, cancela_comp_mes (snd x) dia inicio):xs
 | (fst x) > mes   = (x:xs)
 | otherwise       = x : (cancelacompromisso xs mes dia inicio)
 
cancela_comp_mes :: (Eq b, Ord a, Ord a1) => [(a1, [(a, b)])] -> a1 -> a -> [(a1, [(a, b)])]
cancela_comp_mes [] _ _ = []
cancela_comp_mes (x:xs) dia inicio 
 | (fst x) == dia = if ((cancela_comp_dia (snd x) inicio) == []) then xs else (dia, (cancela_comp_dia (snd x) inicio)):xs 
 | (fst x) > dia  = (x:xs)
 | otherwise      = x:(cancela_comp_mes xs dia inicio)
 
cancela_comp_dia :: Ord a => [(a, b)] -> a -> [(a, b)]
cancela_comp_dia [] _ = [] 
cancela_comp_dia (x:xs) inicio 
 | (fst x) == inicio  = xs
 | (fst x) > inicio   = (x:xs)
 | otherwise          = x:(cancela_comp_dia xs inicio)
 
-- 3) Reagendamento de compromisso
reagendamento :: (Eq a, Num a, Num a1, Num a2, Num a3, Ord a1, Ord a2, Ord a3) => [(a3, [(a1, [(a2, a)])])] -> (Bool, [(a3, a1)]) -> a3 -> a1 -> a2 -> a3 -> a1 -> a2 -> a -> [(a3, [(a1, [(a2, a)])])]
reagendamento agenda inf mes dia inicio mesN diaN inicioN durN
 | (disp == False) = agenda
 | otherwise       = (inserecompromisso xs inf mesN diaN inicioN durN)
 where 
 xs = (cancelacompromisso agenda mes dia inicio)
 disp = (disponivel xs inf mesN diaN inicioN durN)
 
-- 4) Verificação de disponibilidade
disponivel :: (Eq a, Eq a3, Eq a4, Num a, Num a1, Num a2, Num a3, Num a4, Ord a1, Ord a2) => [(a2, [(a1, [(a4, a3)])])] -> (Bool, [(a2, a1)]) -> a2 -> a1 -> a4 -> a -> Bool
disponivel agenda inf mes dia inicio dur = if (pertencelista ys xs) then True else False
 where 
 xs = horarios_disponiveis agenda inf mes dia
 ys = cria_lista inicio dur {- horários requisitados -}
 
-- 5) Horários disponíveis em um determinado dia 
horarios_disponiveis :: (Eq t, Eq a2, Num a, Num a1, Num t, Num a2, Ord a, Ord a1) => [(a1, [(a, [(t, a2)])])] -> (Bool, [(a1, a)]) -> a1 -> a -> [t]
horarios_disponiveis agenda inf mes dia = if ((dia_valido inf mes dia) == True) then horarios compromissos_do_dia else []
    where compromissos_do_dia = (snd (acessa_dia (snd  (acessa_mes agenda mes)) dia)) --Lista de todos os compromissos do dia

horarios :: (Eq t, Eq a, Num t, Num a) => [(t, a)] -> [t] -- Função que dada a lista de compromissos do dia, gera todos os horários disponíveis
horarios [] = [8,9,10,11,14,15,16,17] 
horarios compromissos_do_dia = [ y | y <- [8,9,10,11,14,15,16,17], (pertencenum y xs) == False]
    where xs = horas_ocupadas compromissos_do_dia 

horas_ocupadas :: (Eq a, Eq a1, Num a, Num a1) => [(a1, a)] -> [a1] -- Função que dada a lista de compromissos do dia, gera todos os horários ocupados
horas_ocupadas [] = [] 
horas_ocupadas (x:compromissos_do_dia) = (cria_lista (fst x) (snd x)) ++ (horas_ocupadas compromissos_do_dia)

cria_lista :: (Eq a, Eq a1, Num a, Num a1) => a1 -> a -> [a1] -- Função auxiliar para gerar os horários ocupados de um determinado dia
cria_lista n 1 = [n] 
cria_lista n qtd = if (n == 11) then [n] ++ cria_lista (14) (qtd-1) else [n] ++ cria_lista (n+1) (qtd-1)

-- 6) Inserção de compromisso o mais breve possível
insere_breve :: (Num a, Num a1, Num a2, Ord a, Ord a1, Ord a2) => [(a1, [(a, [(a2, a2)])])] -> (Bool, [(a1, a)]) -> a1 -> a -> a2 -> [(a1, [(a, [(a2, a2)])])]
insere_breve agenda inf mes dia dur = if (dia > (dias_no_mes mes (fst inf))) then agenda else if (horario /= -1) then inserecompromisso agenda inf mes dia horario dur else insere_breve agenda inf mes (dia+1) dur 
 where 
 hdisp = (horarios_disponiveis agenda inf mes dia)
 horario = (verifica hdisp 8 dur 8 dur)

verifica :: (Num a, Ord a) => [a] -> a -> a -> a -> a -> a --Função auxiliar para verificar qual o primeiro horário do dia que cabe um compromisso. Se não couber em nenhum horário retorna -1.
verifica [] _ _ _ _ = -1
verifica xs inicio dur aux 0 = inicio
verifica xs 12 dur 12 n = verifica xs 14 dur 14 n
verifica xs inicio dur 12 n = verifica xs inicio dur 14 n
verifica xs inicio dur aux n = if (inicio+dur > 18) then -1 else if (pertencenum aux xs) then verifica xs inicio dur (aux+1) (n-1) else verifica xs (aux+1) dur (aux+1) dur

-- 7) Inserção de compromisso no intervalo mínimo:
insere_min :: (Num a1, Num a2, Ord a1, Ord a2) => [(a1, [(a2, [(Int, Int)])])] -> (Bool, [(a1, a2)]) -> a1 -> Int -> [(a1, [(a2, [(Int, Int)])])]
insere_min agenda inf mes dur
 | menor == (0,(0,9)) = agenda --Não há intervalo disponível no mês para colocar o compromisso.
 | otherwise          = inserecompromisso agenda inf mes dia inicio dur
 where 
 menor = procura_menor_intervalo_mes agenda inf mes dur (0,(0,9)) 1 (dias_no_mes mes (fst inf))
 inicio = (fst (snd menor))
 dia = (fst menor)

{-Função auxiliar para procurar o menor intervalo no mês (que caiba o compromisso)-}
procura_menor_intervalo_mes :: (Eq a2, Num a1, Num a2, Num a, Ord a1, Ord a) => [(a1, [(a, [(Int, a2)])])] -> (Bool, [(a1, a)]) -> a1 -> Int -> (a, (Int, Int)) -> a -> a -> (a, (Int, Int))
procura_menor_intervalo_mes agenda inf mes dur menor dia_aux qtddias
 | dia_aux > qtddias                   = menor
 | (snd (snd aux)) == dur              = aux
 | (snd (snd aux)) < (snd (snd menor)) = procura_menor_intervalo_mes agenda inf mes dur aux (dia_aux+1) qtddias
 | otherwise                           = procura_menor_intervalo_mes agenda inf mes dur menor (dia_aux+1) qtddias
 where aux = procura_menor_intervalo_dia agenda inf mes dur dia_aux

procura_menor_intervalo_dia :: (Eq a2, Num a1, Num a, Num a2, Ord a1, Ord a) =>[(a1, [(a, [(Int, a2)])])] -> (Bool, [(a1, a)]) -> a1 -> Int -> a -> (a, (Int, Int))
procura_menor_intervalo_dia agenda inf mes dur dia --função auxiliar com informações do menor intervalo de um dia específico (dia, (inicio, tamanho)) 
 | h == []                           = (dia,(0,9)) -- dia totalmente cheio ou inválido.
 | otherwise                         = (dia, f h (0,9) 1 dur (head h))
 where h = (horarios_disponiveis agenda inf mes dia)

f :: [Int] -> (Int,Int) -> Int -> Int -> Int -> (Int,Int) {-Função auxiliar que dado os horários disponíveis, o menor intervalo atual e o compromisso, 
retorna uma tupla com o inicio do menor intervalo e o tamanho dele. -}
f (x:xs) menor tamanho dur inicio
 | xs == []                                                      = if (tamanho<snd(menor) && tamanho >=dur) then (inicio,tamanho) else menor
 | ((head xs) == (x+1)) ||(x == 11 && (head xs)==14)             = (f xs menor (tamanho+1) dur inicio)
 | tamanho < snd(menor) && tamanho >= dur                        = (f xs (inicio,tamanho) 1 dur inicio)
 | otherwise                                                     = (f xs menor 1 dur (head xs))

-- 8) Inserção de compromisso no intervalo máximo
insere_max :: (Num a1, Num a, Ord a1, Ord a) => [(a1, [(a, [(Int, Int)])])] -> (Bool, [(a1, a)]) -> a1 -> Int -> [(a1, [(a, [(Int, Int)])])]
insere_max agenda inf mes dur
 | (snd (snd maior)) == -1 = inserecompromisso agenda inf mes dia inicio dur
 | (snd (snd maior)) > dur = inserecompromisso agenda inf mes dia inicio dur
 | otherwise               = agenda
 where 
 maior = procura_maior_intervalo_mes agenda inf mes (0,(0,0)) 1 (dias_no_mes mes (fst inf))
 inicio = (fst (snd maior))
 dia = (fst maior)

procura_maior_intervalo_mes agenda inf mes maior dia_aux qtddias -- Função auxiliar para procurar o maior intervalo em um mês
 | dia_aux > qtddias                   = maior
 | (snd (snd aux)) == 8                = aux
 | (snd (snd aux)) > (snd (snd maior)) = procura_maior_intervalo_mes agenda inf mes aux (dia_aux+1) qtddias
 | otherwise                           = procura_maior_intervalo_mes agenda inf mes maior (dia_aux+1) qtddias
 where aux = procura_maior_intervalo_dia agenda inf mes dia_aux
 
procura_maior_intervalo_dia agenda inf mes dia --função auxiliar com informações do maior intervalo de um dia específico (dia, (inicio, tamanho)) 
 | h == []                             = (dia,(0,0))  -- dia totalmente cheio ou inválido
 | h == [8,9,10,11,14,15,16,17]        = (dia,(8,8)) -- dia totalmente livre, maior intervalo possível
 | otherwise                           = (dia, g h (0,0) 1 (head h))
 where h = (horarios_disponiveis agenda inf mes dia)

g :: [Int] -> (Int,Int) -> Int -> Int -> (Int,Int) -- Análoga à f, porém pro intervalo máximo.
g (x:xs) maior tamanho inicio
 | xs == []                                                      = if (tamanho>snd(maior)) then (inicio,tamanho) else maior
 | ((head xs) == (x+1)) ||(x == 11 && (head xs)==14)             = (g xs maior (tamanho+1) inicio)
 | tamanho > snd(maior)                                          = (g xs (inicio,tamanho) 1 inicio)
 | otherwise                                                     = (g xs maior 1 (head xs))

--Funções Auxiliares:
acessa_dia [] dia = (-1,[]) --Não tem compromisso naquele dia
acessa_dia (x:mes) dia = if ((fst x) == dia) then x else acessa_dia mes dia

acessa_mes [] mes = (-1,[]) --Não tem compromisso naquele mes
acessa_mes (x:agenda) mes = if ((fst x) == mes) then x else acessa_mes agenda mes 

dias_no_mes mes bi
 | mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12 = 31
 | mes == 4 || mes == 6 || mes == 9 || mes == 11                                      = 30
 | bi                                                                                 = 29
 | not bi                                                                             = 28
 | otherwise                                                                          = 0

-- dia_valido é a função que se usa para ver se um dia é valido pra marcar coisas ou não 
dia_valido inf mes dia
 | mes < 1 || mes > 12 || dia < 1 || dia > dias  = False
 | otherwise                                     = validade (snd inf) mes dia
 where dias = dias_no_mes mes (fst inf)

validade [] mes dia = True
validade (x:xs) mes dia = if ((fst x) == mes && (snd x) == dia) then False else if ((fst x) >= mes && (snd x) > dia) then True else validade xs mes dia

--Verifica se um numero pertence a lista
pertencenum y [] = False 
pertencenum y (x:xs) = if (y == x) then True else pertencenum y xs

--Verifica se uma lista pertence a outra lista
pertencelista [] _ = True
pertencelista (y:ys) xs = if (pertencenum y xs) then pertencelista ys xs else False