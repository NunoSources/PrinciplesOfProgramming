-- Principios de Programação
-- Trabalho 6
-- André da Silva Proença 53370
-- Nuno Fontes 46413

-- Compile: ghc --make Main
-- Run: ./Main
--QuickCheck run:
-- Run: ./Main -t
-- or compile and run once: runhaskell Main.hs 

import System.IO
import Data.List
import Test.QuickCheck
import System.Environment

--A. Agregador
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if args == ["-t"]
    then do
        quickCheck prop_addToTransactionList_reverse_length
        quickCheck prop_addToTransactionList_average
        quickCheck prop_addToTransactionList_maximum
    else do
        input <- getLine
        if input == "exit"
        then return ()
        else readTransactions (take 2 $ words input) (filterGroupByNumbers $ drop 2 $ words input) transactionList 
                 -- Exemplo      ["sum","1"]            ["groupby","2","groupby","3"]


--C. QuickCheck
prop_addToTransactionList_reverse_length :: [Float] -> [[Float]] -> Bool
prop_addToTransactionList_reverse_length [] [] = True
prop_addToTransactionList_reverse_length xs xxs = length tList == (length $ reverse tList)
    where tList = concat $ addToTransactionList xs xxs

prop_addToTransactionList_average :: [Float] -> [[Float]] -> Property
prop_addToTransactionList_average xs xxs = (not $ null tList) ==> tListSum `div` length tList == tListSum `div` length (reverse tList)
    where tList = concat $ addToTransactionList xs xxs
          tListSum = round $ sum tList

prop_addToTransactionList_maximum :: [Float] -> [[Float]] -> Property
prop_addToTransactionList_maximum xs xxs = (not $ null tList) ==> maximum tList == maximum (reverse tList)
    where tList = concat $ addToTransactionList xs xxs  


-- Função que lê as transactions á medida que o utilizador for dando o input
-- Recebe uma lista de metricas, uma lista de groupbys e uma lista de transactions
-- Devolve um IO() com a resposta á metrica dada e aos groupbys
readTransactions :: [String] -> [Int] -> [[Float]] -> IO ()
readTransactions metrica groupby transactionList' = do
    transaction <- getLine
    if transaction == "exit"
    then return ()
    else do
        if metricaComando == "sum"
        then do 
            sumC (processGroupBy $ zip 
             (addToTransactionList (map read $ words transaction) transactionList') 
             (groupBy' (map read $ words transaction) --groupby' devolve a lista de True e False
             (addToTransactionList (map read $ words transaction) transactionList') groupby)) collumn
             {-
             zip [[0,4,1,4],[1,12,1,4],[2,8,1,4],[3,54,2,4],[4,1,2,2]] [True, True, True, False, False]
             = [([0,4,1,4],True),([1,12,1,4],True),([2,8,1,4],True),([3,54,2,4],False),([4,1,2,2],False)]

             processGroupBy [([0,4,1,4],True),([1,12,1,4],True),([2,8,1,4],True),([3,54,2,4],False),([4,1,2,2],False)]
             = [[0,4,1,4],[1,12,1,4],[2,8,1,4]] 

             sumC [[0,4,1,4],[1,12,1,4],[2,8,1,4]] collumn
             -}
            --Apos a sum, vai chamar novamente a funcao readTransactions 
            readTransactions metrica groupby $ addToTransactionList (map read $ words transaction) transactionList'
        else if metricaComando == "average"
        then do
            averageC (processGroupBy $ zip
             (addToTransactionList (map read $ words transaction :: [Float]) transactionList')
             (groupBy' (map read $ words transaction :: [Float]) 
             (addToTransactionList (map read $ words transaction :: [Float]) transactionList') groupby)) collumn
            readTransactions metrica groupby $ 
             addToTransactionList (map read $ words transaction :: [Float]) transactionList'
        else if metricaComando == "maximum"
        then do
            maximumC (processGroupBy $ zip
             (addToTransactionList (map read $ words transaction :: [Float]) transactionList')
             (groupBy' (map read $ words transaction :: [Float]) 
             (addToTransactionList (map read $ words transaction :: [Float]) transactionList') groupby)) collumn
            readTransactions metrica groupby $ 
             addToTransactionList (map read $ words transaction :: [Float]) transactionList'
        else return ()
    where metricaComando = head metrica
          collumn = read $ metrica !! 1 :: Int
          -- Nota: Repetição de código deve-se á variavel transaction não poder estar no scope
          -- quando a passamos diretamente para o where.


-- Função que dada uma string de groupbys e numeros, devolve apenas os numeros do groupby
-- Recebe uma string de groupbys e numeros,
-- Devolve uma lista de inteiros que representam os numeros das colunas a agrupar
-- Exemplo ["groupby","2","groupby","3"] -> [2,3]
filterGroupByNumbers :: [String] -> [Int]
filterGroupByNumbers groupbyList = map read $ filter (/="groupby") groupbyList


-- Lista de transactions dadas pelo utilizador
transactionList :: [[Float]]
transactionList = []

-- Adiciona uma transaction á lista de transactions
-- Recebe a transaction a adicionar e uma lista de transactions
-- Retorna uma lista de transactions com a nova transaction adicionada
addToTransactionList :: [Float] -> [[Float]] -> [[Float]]
addToTransactionList = (:)


-- Função que imprime a soma dos numeros de uma dada coluna
-- Recebe uma lista de transactions e o numero de uma coluna
-- Devolve um IO () com a soma dos numeros de uma dada coluna
sumC :: [[Float]] -> Int -> IO ()
sumC currentTransactionList column = print $
    foldl (\acc transaction -> acc + (transaction !! column)) 0 currentTransactionList


-- Função que imprime a média dos numeros de uma dada coluna
-- Recebe uma lista de transactions e o numero de uma coluna 
-- Devolve um IO () com a média dos numeros de uma dada coluna

averageC :: [[Float]] -> Int -> IO ()
averageC currentTransactionList column = print $
    foldl (\acc transaction -> acc + (transaction !! column)) 0 currentTransactionList / fromIntegral (length currentTransactionList)

-- Função que imprime o valor maximo dos numeros de uma dada coluna
-- Recebe uma lista de transactions e o numero de uma coluna
-- Devolve um IO () o valor maximo dos numeros de uma dada coluna
maximumC :: [[Float]] -> Int -> IO ()
maximumC currentTransactionList column = print $ maximum $ 
    foldl (\acc transaction -> (transaction !! column) : acc) [] currentTransactionList


-- Função que agrupa as transactions de acordo com o número das colunas fornecido.
-- Recebe um tuplo lista de transactions, booleano e adiciona á lista de transactions a 
-- as transactions de cada tuplo se forem True
-- Devolve uma lista de transactions
-- Exemplo: [([0,4,1,4],True),([1,12,1,4],True),([2,8,1,4],True),([3,54,2,4],False),([4,1,2,2],False)] 
--          -> [[0,4,1,4],[1,12,1,4],[2,8,1,4]]
processGroupBy :: (Num a) => [([a], Bool)] -> [[a]]
processGroupBy = foldl (\acc x -> if snd x then fst x : acc else acc) [] 


-- Função que agrupa as transactions de acordo com o número das colunas fornecido.
-- Recebe uma transaction "master" que representa a transaction mais recente introduzida
-- pelo utilizador (que vai ser a base pela qual as outras transactions se vão comparar)
-- Recebe também uma lista de transactions e uma lista de inteiros que são os indexes a agrupar
-- Retorna uma lista de lista booleanos. True significa que a transaction vai ser agrupada.
-- Exemplo [0,4,1,4] -> [[0,4,1,4],[1,12,1,4],[2,8,1,4],[3,54,2,4],[4,1,2,2]] -> [2,3] ===> queremos os indices 2 e 3 das transacoes
--         -> [True, True, True, False, False]
groupBy' :: (Eq a, Num a) => [a] -> [[a]] -> [Int] -> [Bool]
groupBy' transaction transactionList groupbyList = 
    groupByAux sliceFirstTransaction sliceRemainingTransactions
    where sliceFirstTransaction = map (transaction !!) groupbyList
          sliceRemainingTransactions = 
              foldr (\t acc -> map (t !!) groupbyList : acc) [] transactionList


-- Função Auxiliar que verifica se uma transaction é igual á transaction dada
-- Recebe uma transaction com apenas os numeros que correspondem aos indexes, 
-- uma lista de transactions que correspondem aos indexes
-- Devolve uma lista booleanos. True significa que a transaction vai ser agrupada.
-- Exemplo [1,4] -> [[1,4],[1,4],[2,4],[2,2]] -> [True, True, False, False]
groupByAux :: (Eq a, Num a) => [a] -> [[a]] -> [Bool]
groupByAux transaction transactionList =
    foldr (\x acc -> if x == transaction then True : acc else False : acc) [] transactionList
