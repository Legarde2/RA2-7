module Relatorios where

import Inventario (LogEntry(..), StatusLog(..), AcaoLog(..), acao, detalhes, status)
import Data.List (sortOn, groupBy, group, sort)
import Data.Time (UTCTime)

-- Função para filtrar logs de erro
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro todosOsLogs = filter ehFalha todosOsLogs
  where
    ehFalha :: LogEntry -> Bool
    ehFalha log = case status log of
        Falha _ -> True
        Sucesso -> False

-- Função para filtrar histórico de um item específico
historicoPorltem :: String -> [LogEntry] -> [LogEntry]
historicoPorltem itemID logs = filter (pertenceAoItem) logs
  where
    pertenceAoItem :: LogEntry -> Bool
    pertenceAoItem log =
        -- Verifica se o ID do item está nos detalhes do log
        itemID `isInfixOf` detalhes log

-- Função para encontrar o item com mais entradas de log
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado [] = Nothing
itemMaisMovimentado logs =
    let ids = map extrairID logs
        validIDs = filter (/= "N/A") ids
    -- Agrupa os IDs iguais
    in if null validIDs
       then Nothing
       else
        Just $ findMostCommon (group (sort validIDs))
  where
    extrairID :: LogEntry -> String
    extrairID log =
        if acao log == QueryFail
        then "N/A"
        else
            let ws = words (detalhes log)
            in if length ws > 3
               then ws !! 3
               else "N/A"
    
    -- Encontra o grupo mais longo
    findMostCommon :: [[String]] -> (String, Int)
    findMostCommon groups =
        let sortedGroups = sortOn (negate . length) groups
            topGroup = head sortedGroups
        in (head topGroup, length topGroup)

-- Verifica se a primeira string esta contida na segunda
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

-- Verifica se a lista começa com o prefixo
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Retorna todas as caudas de uma lista
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'