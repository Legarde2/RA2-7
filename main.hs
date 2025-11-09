module Main where

-- Módulos do projeto
import Inventario
import Logica

-- Módulos de Sistema
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import System.Exit (exitSuccess) 

-- Constantes para os nomes dos arquivos
invFile :: FilePath
invFile = "Inventario.dat"
logFile :: FilePath
logFile = "Auditoria.log"

loadInventario :: IO Inventario
loadInventario = (do
    putStrLn $ "Carregando " ++ invFile ++ "..."
    conteudo <- readFile invFile
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
    let logs = map read (lines conteudo)
    putStrLn "Logs carregados com sucesso."
    return logs
    ) `catch` handleReadError []

handleReadError :: a -> IOException -> IO a
handleReadError defaultVal e = do
    putStrLn $ "Aviso: Arquivo não encontrado ou permissão negada. Iniciando com dados vazios."
    return defaultVal

mainLoop :: Inventario -> [LogEntry] -> IO ()
mainLoop inv logs = do
    putStr "\nComando (add, remove, update, report, sair): "
    comando <- getLine
    
    time <- getCurrentTime
    
    processCommand comando time inv logs


processCommand :: String -> UTCTime -> Inventario -> [LogEntry] -> IO ()
processCommand comando time inv logs = do
    case comando of
        "add" -> do
            putStrLn "--- (Lógica 'add' ainda não implementada) ---"
            mainLoop inv logs -- Volta ao loop com o estado antigo

        "remove" -> do
            putStrLn "--- (Lógica 'remove' ainda não implementada) ---"
            mainLoop inv logs

        "update" -> do
            putStrLn "--- (Lógica 'update' ainda não implementada) ---"
            mainLoop inv logs
        
        "report" -> do
            putStrLn "--- (Lógica 'report' ainda não implementada) ---"
            mainLoop inv logs

        "sair" -> do
            putStrLn "Encerrando..."
            exitSuccess

        _ -> do
            putStrLn "Erro: Comando inválido."
            mainLoop inv logs


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    inv <- loadInventario
    logs <- loadLogs
    
    putStrLn "\n--- Sistema de Gerenciamento de Inventário ---"
    putStrLn "Comandos: add, remove, update, report, sair"
    
    mainLoop inv logs
