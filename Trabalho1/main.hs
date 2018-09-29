{- Trabalho 1 - Programação 1 - Aluna: Nicole Rizzi Nunes -}
import System.Environment
import System.IO
--import BasicTypes
--import ScheduleManagerTests
import Funcoes

main = do 
    arq <- readFile "agenda.txt"
    let agenda = le_agenda arq
    arq2 <- (readFile "calendario.txt")
    let inf = le_informacoes arq2
    menu agenda inf
    return ()
    
menu :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> IO()
menu agenda inf = do
    putStr "\nEscolha a opção desejada:\n"
    putStr "0 - Sair\n"
    putStr "1 - Recuperar agenda\n"
    putStr "2 - Verificar disponibilidade de horário\n"
    putStr "3 – Verificar disponibilidade no dia\n"
    putStr "4 – Inserir compromisso no horário\n"
    putStr "5 – Inserir compromisso mais breve\n"
    putStr "6 – Inserir compromisso no intervalo mínimo\n"
    putStr "7 – Inserir compromisso no intervalo máximo\n"
    putStr "8 – Cancelar compromisso\n"
    putStr "9 – Reagendar compromisso\n"
    putStr "10 – Gravar agenda\n"
    opcao <- getLine
    putStrLn ""
    chamada opcao agenda inf
    return ()

chamada opcao agenda inf = do
    case opcao of
        "1" -> do
            arq <- readFile "agenda.txt"
            let new_agenda = le_agenda arq
            putStrLn ("Agenda recuperada:  " ++ (show (new_agenda)))
            menu new_agenda inf
            return ()
        "2" -> do
            mes <- getLine
            dia <- getLine
            inicio <- getLine
            duracao <- getLine
            putStrLn ""
            if ( disponivel agenda inf (read mes :: Int) (read dia :: Int) (read inicio :: Int) (read duracao :: Int) ) then putStrLn "Horario disponível" else putStrLn "Horário não disponível"
            menu agenda inf
            return ()
        "3" -> do
            mes <- getLine
            dia <- getLine
            putStrLn ""
            putStrLn ("Horários disponíveis: " ++(show (horarios_disponiveis agenda inf (read mes :: Int) (read dia :: Int))))
            menu agenda inf
            return ()
        "4" -> do
            mes <- getLine
            dia <- getLine
            inicio <- getLine
            duracao <- getLine
            putStrLn ""
            let agenda_atualizada = (inserecompromisso agenda inf (read mes :: Int) (read dia :: Int) (read inicio :: Int) (read duracao :: Int) )
            if (agenda == agenda_atualizada) then putStrLn ("O dia e horário requisitado não está disponível. Agenda não atualizada.") else putStrLn ("Agenda atualizada com sucesso!")
            putStrLn (show agenda_atualizada)
            menu agenda_atualizada inf
            return ()
        "5" -> do
            mes <- getLine
            dia <- getLine
            duracao <- getLine
            putStrLn ""
            let agenda_atualizada = (insere_breve agenda inf (read mes :: Int) (read dia :: Int) (read duracao :: Int))
            if (agenda == agenda_atualizada) then putStrLn ("O compromisso não pôde ser marcado. Agenda não atualizada.") else putStrLn ("Agenda atualizada com sucesso!")
            putStrLn (show agenda_atualizada)
            menu agenda_atualizada inf
            return ()
        "6" -> do
            mes <- getLine
            duracao <- getLine
            putStrLn ""
            let agenda_atualizada = (insere_min agenda inf (read mes :: Int) (read duracao :: Int))
            if (agenda == agenda_atualizada) then putStrLn ("O compromisso não pôde ser marcado. Agenda não atualizada.") else putStrLn ("Agenda atualizada com sucesso!")
            putStrLn (show agenda_atualizada)
            menu agenda_atualizada inf
            return ()
        "7" -> do
            mes <- getLine
            duracao <- getLine
            putStrLn ""
            let agenda_atualizada = (insere_max agenda inf (read mes :: Int) (read duracao :: Int))
            if (agenda == agenda_atualizada) then putStrLn ("O compromisso não pôde ser marcado. Agenda não atualizada.") else putStrLn ("Agenda atualizada com sucesso!")
            putStrLn (show agenda_atualizada)
            menu agenda_atualizada inf
            return ()
        "8" -> do
            mes <- getLine
            dia <- getLine
            inicio <- getLine
            putStrLn ""
            let agenda_atualizada = (cancelacompromisso agenda (read mes :: Int) (read dia :: Int) (read inicio :: Int))
            if (agenda == agenda_atualizada) then putStrLn ("O compromisso que você tentou cancelar não existe. Agenda não atualizada.") else putStrLn ("Agenda atualizada com sucesso!")
            putStrLn (show agenda_atualizada)
            menu agenda_atualizada inf
            return ()
        "9" -> do
            mes <- getLine
            dia <- getLine
            inicio <- getLine
            mesN <- getLine
            diaN <- getLine
            inicioN <- getLine
            durN <- getLine
            putStrLn ""
            let agenda_atualizada = (reagendamento agenda inf (read mes :: Int) (read dia :: Int) (read inicio :: Int) (read mesN :: Int) (read diaN :: Int) (read inicioN :: Int) (read durN :: Int) )
            if (agenda == agenda_atualizada) then putStrLn ("O compromisso não pôde ser reagendado. Agenda não atualizada.") else putStrLn ("Agenda atualizada com sucesso!")
            putStrLn (show (agenda_atualizada))
            menu agenda_atualizada inf
            return ()
        "10" -> do
            grava_agenda agenda
            putStrLn "Agenda atualizada no agenda.txt"
            menu agenda inf
            return ()
        "0" -> do return ()
        _ -> do
            putStr "Opção inválida. Digite Novamente:  "
            opcao <- getLine
            chamada opcao agenda inf
            return ()

-- Ler a agenda:
le_agenda arq = le_meses (lines arq)

le_meses [] = []
le_meses (mes:dias)
 | (snd xs) /= [] = ((read mes::Int), (fst xs)) : (le_meses (snd xs))
 | otherwise      = [((read mes::Int), (fst xs))]
 where xs = le_dias dias ([],[])

le_dias [] t = t
le_dias (dia:xs) t
 | resto == []        = ( (fst t) ++ [( (read dia::Int) , (read compromissos::[(Int,Int)]) )] , [])
 | (head resto) == [] = ( (fst t)++[( (read dia::Int) , (read compromissos::[(Int,Int)]) )], (tail resto) )
 | otherwise          = le_dias resto ( (fst t)++[( (read dia::Int) , (read compromissos::[(Int,Int)]) )] , resto )
 where
    resto = tail xs
    compromissos = head xs

--Escrever a agenda:
grava_agenda agenda = do
    arq <- openFile "agenda.txt" WriteMode
    grava_meses agenda arq
    hClose arq
    return ()

grava_meses (x:xs) arq = do
    hPutStrLn arq (show (fst x))
    grava_dias arq (snd x)
    hPutStr arq "\n"
    if (xs /= []) then grava_meses xs arq else return ()

grava_dias arq (x:xs) = do
    hPutStrLn arq (show (fst x))
    hPutStrLn arq (show (snd x))
    if (xs /= []) then grava_dias arq xs else return ()

--Ler a tupla de informações:
le_informacoes arq = ((read (head linhas) :: Bool), (le_feriados_mes 1 (tail linhas)))
    where linhas = lines arq 

le_feriados_mes _ [] = []
le_feriados_mes 13 _ = []
le_feriados_mes mes (x:xs) = ( le_feriados_dias mes (words x) ) ++ (le_feriados_mes (mes+1) xs)

le_feriados_dias mes [] = []
le_feriados_dias mes (x:xs) = (mes, (read x :: Int)):(le_feriados_dias mes xs)