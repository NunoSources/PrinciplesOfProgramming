--Nuno Fontes 46413

import System.Environment  
import Data.List
import Control.Monad

main :: IO ()
main = do 
      fileName <- getArgs
      contents <- readFile (head fileName)
      putStrLn contents
      putStrLn "Filtering:"
      --lista de palavras a serem filtradas comeca vazia
      let listaPalavras = []
      palavra <- getLine
      --apos recebermos uma palavra, usa a funcao auxiliar para concatenar a palavra dada ah lista
      listaPalavras <- atualizaLista listaPalavras palavra
      filtering contents listaPalavras

--Funcao usada para filtrar uma lista de Strings a partir de uma String dada
filtering :: String -> [String] -> IO ()
filtering contents listaPalavras = do
    let contentsInicial = contents
    --Se o conteudo onde queremos filtrar nao for nulo, entao avancamos
    when(not $ null (lines contents)) $ do
        --palavra atual que iremos usar para filtrar
        let palavraAtual = last listaPalavras
        --Caso a palavra a filtrar nao seja "pop", entramos aqui
        when (not $ palavraAtual == "pop") $ do
            let allLines = lines contents
                {-vai ver se a ultima palavra usado no filtering, estah no contents e 
                se estiver, filtra a lista para uma lista so com esse content que tem 
                a palavra usada no filtering-}
                filteredLines = filter (isInfixOf palavraAtual) allLines
                result = unlines filteredLines   
            putStrLn result
            putStrLn $ "Filtering: " ++ intercalate ", " listaPalavras
            palavra <- getLine
            -- atualiza lista de palavras
            listaPalavras <- atualizaLista listaPalavras palavra
            {-Se a palavra dada for diferente de "pop", entao chamamos novamente a funcao onde estamos, 
            com o result, ou seja, o contents atualizado e com a lista de palavras, caso contrario, 
            chamamos a funcao com o contents inicial e a lista de palavras-}
            if palavra /= "pop" then filtering result listaPalavras else filtering contentsInicial listaPalavras
        --Caso a palavra a filtrar seja "pop", entramos aqui
        when (palavraAtual == "pop") $ do
            {-quando fazemos pop, este vai ser adicionado ah lista de palavras, entao temos de fazer 
            init ah lista de palavras para termos a lista sem o comando pop, depois init novamente para termos 
            na lista de palavras todas as palavras exceto a ultima que eh a palavra retirada apos a execucao do pop, 
            e last da lista de palavras, que nos da a palavra anteriormente escolhida para a filtragem-}
            let comandoAnterior = last $ init (init listaPalavras)
            {-Se a lista de palavras estiver vazia, entao a funcao putStrLn recebe o contentsInicial e retorna uma acao de IO, 
            que imprimirah o contentsInicial, caso contrario, chama a funcao filtering com o conteudo inicial e o comando anterior-}
            if null listaPalavras then putStrLn contentsInicial else filtering contentsInicial [comandoAnterior]

--Funcao usada para atualizar a lista de palavras
atualizaLista :: Monad m => [a] -> a -> m [a]
atualizaLista lista comando = do
    let lista2 = lista ++ [comando]
    return lista2