module Main where

-- Módulos do projeto
import Inventario
import Logica
import Relatorios

-- Módulos de Sistema
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Exception (catch, IOException, evaluate)
import Text.Read (readMaybe)
import System.Exit (exitSuccess) 

-- Constantes para os nomes dos arquivos
invFile :: FilePath
invFile = "Inventario.dat"
logFile :: FilePath
logFile = "Auditoria.log"

-- carrega o inventário
loadInventario :: IO Inventario
loadInventario = (do
    putStrLn $ "Carregando " ++ invFile ++ "..."
    conteudo <- readFile invFile
    
    evaluate (length conteudo)
    
    case readMaybe conteudo of
        Just inv -> do
            putStrLn "Inventário carregado com sucesso."
            return inv
        Nothing -> do
            putStrLn $ "Erro: " ++ invFile ++ " corrompido. Iniciando com inventário vazio."
            return Map.empty
    ) `catch` handleReadError Map.empty

loadLogs :: IO [LogEntry]
loadLogs = (do
    putStrLn $ "Carregando " ++ logFile ++ "..."
    conteudo <- readFile logFile
    
    evaluate (length conteudo)
    
    let logs = map read (lines conteudo)
    putStrLn "Logs carregados com sucesso."
    return logs
    ) `catch` handleReadError []

-- handdle para lidar com erros de leitura 
handleReadError :: a -> IOException -> IO a
handleReadError defaultVal e = do
    putStrLn $ "Aviso: Arquivo não encontrado ou permissão negada. Iniciando com dados vazios."
    return defaultVal

-- Salva o inventário no disco 
saveInventario :: Inventario -> IO ()
saveInventario inv = do
    writeFile invFile (show inv)
    putStrLn "Estado do inventário salvo em Inventario.dat."

-- Salva um log no disco 
saveLog :: LogEntry -> IO ()
saveLog logEntry = do
    appendFile logFile (show logEntry ++ "\n")
    putStrLn "Operação registrada em Auditoria.log."

-- Loop principal do programa
mainLoop :: Inventario -> [LogEntry] -> IO ()
mainLoop inv logs = do
    putStr "\nComando (add, remove, update, report, sair): "
    comando <- getLine
    
    time <- getCurrentTime
    
    processCommand comando time inv logs

-- Processa o comando do usuário
processCommand :: String -> UTCTime -> Inventario -> [LogEntry] -> IO ()
processCommand comando time inv logs = do
    case comando of
        "add" -> do
            putStr "ID do item: "
            id <- getLine
            putStr "Nome do item: "
            nome <- getLine
            putStr "Quantidade inicial: "
            qtdeStr <- getLine
            putStr "Categoria: "
            cat <- getLine

            case readMaybe qtdeStr of
                Nothing -> do
                    putStrLn "Erro: Quantidade deve ser um número."
                    mainLoop inv logs
                Just qtde -> do
                    let resultado = addItem time id nome qtde cat inv
                    handleResultado resultado inv logs

        "remove" -> do
            putStr "ID do item: "
            id <- getLine
            putStr "Quantidade a remover: "
            qtdeStr <- getLine

            case readMaybe qtdeStr of
                Nothing -> do
                    putStrLn "Erro: Quantidade deve ser um número."
                    mainLoop inv logs
                Just qtde -> do
                    let resultado = removerItem time id qtde inv
                    handleResultado resultado inv logs

        "update" -> do
            putStr "ID do item: "
            id <- getLine
            putStr "Nova quantidade total: "
            qtdeStr <- getLine

            case readMaybe qtdeStr of
                Nothing -> do
                    putStrLn "Erro: Quantidade deve ser um número."
                    mainLoop inv logs
                Just qtde -> do
                    let resultado = atualizarQuantidade time id qtde inv
                    handleResultado resultado inv logs
        
        "report" -> do
            -- Chama o menu de relatórios
            handleReport inv logs

        "sair" -> do
            putStrLn "Encerrando..."
            exitSuccess

        _ -> do
            putStrLn "Erro: Comando inválido."
            mainLoop inv logs

handleResultado :: Either String ResultadoOperacao -> Inventario -> [LogEntry] -> IO ()
handleResultado (Left erroMsg) inv logs = do
    -- Caso 1: Falha na lógica
    putStrLn $ "Falha na operação: " ++ erroMsg
    
    time <- getCurrentTime
    let logFalha = LogEntry {
        timestamp = time,
        acao = QueryFail, 
        detalhes = erroMsg,
        status = Falha erroMsg
    }
    
    -- Salva apenas o log
    saveLog logFalha
    
    -- Continua o loop com o inventário antigo
    mainLoop inv (logFalha : logs)

handleResultado (Right (novoInv, logSucesso)) _ logs = do
    -- Caso 2: Sucesso na operação
    putStrLn "Operação bem-sucedida."
    
    -- Salva o novo inventário
    saveInventario novoInv
    -- Salva o log de sucesso
    saveLog logSucesso
    
    mainLoop novoInv (logSucesso : logs)

handleReport :: Inventario -> [LogEntry] -> IO ()
handleReport inv logs = do
    putStrLn "\n--- MÓDULO DE RELATÓRIOS ---"
    putStrLn "1: Ver logs de erro"
    putStrLn "2: Ver histórico por item"
    putStrLn "3: Ver item mais movimentado"
    putStr "Escolha o relatório (ou 'sair' para voltar): "
    
    subCmd <- getLine
    case subCmd of
        "1" -> do
            putStrLn "\n[Relatório: Logs de Erro]"
            let erros = logsDeErro logs
            if null erros
            then putStrLn "(Nenhum erro encontrado nos logs)"
            else mapM_ print erros
            mainLoop inv logs

        "2" -> do
            putStr "Digite o ID do item para ver o histórico: "
            itemID <- getLine
            putStrLn $ "\n[Relatório: Histórico do Item " ++ itemID ++ "]"
            let historico = historicoPorItem itemID logs
            if null historico
            then putStrLn "(Nenhum histórico encontrado para este item)"
            else mapM_ print historico
            mainLoop inv logs

        "3" -> do
            putStrLn "\n[Relatório: Item Mais Movimentado]"
            case itemMaisMovimentado logs of
                Nothing -> putStrLn "(Nenhum dado de movimentação encontrado)"
                Just (item, count) -> 
                    putStrLn $ "Item mais movimentado: " ++ item ++ " (com " ++ show count ++ " operações)"
            mainLoop inv logs

        _ -> do
            putStrLn "Voltando ao menu principal..."
            mainLoop inv logs


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    inv <- loadInventario
    logs <- loadLogs
    
    putStrLn "\nSistema de Gerenciamento de Inventário "
    putStrLn "Comandos: add, remove, update, report, sair"
    
    mainLoop inv logs
