--Nuno Fontes 46413

--A1
listaDeAlguidaresVazia :: [a]
listaDeAlguidaresVazia = []

--A2
{-Funcao que vai adicionar um elemento a uma lista de alguidares. Recebe um tamanho maximo, 
o elemento a adicionar e uma lista de alguidares. Verifica se a cabeca do alguidar seguinte 
eh maior que o elemento a adicionar e se o elemento eh menor que o last do alguidar atual, 
se for, vamos querer adicionar o elemento ao alguidar anterior, caso contrario, 
passamos xs1 e vamos chamar a funcao recursivamente para comparar o alguidar seguinte-}
adicionaAListaDeAlguidares :: Ord a => Int -> a -> [[a]] -> [[a]]
adicionaAListaDeAlguidares _ x [] = [[x]]
adicionaAListaDeAlguidares maximo elemento [xs] = adicionaElemento maximo elemento xs []
adicionaAListaDeAlguidares maximo elemento (xs1:xs2:xss)
  | head xs2 > elemento && last xs1 > elemento = adicionaElemento maximo elemento xs1 (xs2:xss)
  | otherwise = xs1 : adicionaAListaDeAlguidares maximo elemento (xs2:xss)

{-Funcao auxiliar para adicionar um elemento numa lista de forma ordenada.
Se o tamanho do alguidar que estamos a observar for menor que o tamanho maximo, 
adiciona o elemento a este alguidar, de forma ordenada, caso contrario, 
a funcao vai trocar o ultimo elemento do alguidar com o elemento que queremos adicionar,  
que sera adicionado num alguidar seguinte.
Temos um caso base em que o proximo alguidar estah completamente vazio.
Neste caso, a funcao adiciona ordenadamente o elemento ao alguidar atual, devolve todos os 
elementos deste alguidar exceto o ultimo (init) e vai adicionar este ultimo elemento (last (adicionaOrd elemento xs)) 
ao alguidar vazio-}
adicionaElemento :: Ord a => Int -> a -> [a] -> [[a]] -> [[a]]
adicionaElemento maximo elemento xs (ys:yss)
  | length xs < maximo = adicionaOrd elemento xs : (ys:yss)
  | otherwise = adicionaOrd elemento (init xs) : adicionaElemento maximo (last xs) ys yss
adicionaElemento maximo elemento xs []
  | length xs < maximo = [adicionaOrd elemento xs]
  --Adiciona o elemento a xs e depois devolve o init que eh o xs preenchido e coloca 
  --o ultimo elemento de xs na lista seguinte.
  --maximo = 4 -> init [1,2,3,4,5] = [1,2,3,4] : [5] <- 5 eh adicionado ah lista seguinte
  | otherwise = init (adicionaOrd elemento xs) : adicionaElemento maximo (last (adicionaOrd elemento xs)) [] []

--Funcao auxiliar para adicionar ordenadamente um elemento a uma lista
adicionaOrd :: Ord a => a -> [a] -> [a]
adicionaOrd x [] = [x]
adicionaOrd x (y:ys)
  | x < y = x:y:ys
  | otherwise = y:adicionaOrd x ys

--A3
{-Funcao com comportamento igual ah funcao elem, que vai verificar se um dado elemento 
se encontra numa lista de alguidares.
Usa a funcao de ordem superior 'filter' para, caso encontre um elemento igual a 'elemento', 
crie uma lista com esse elemento. Se a lista criada for diferente de uma lista vazia, 
entao eh porque encontrou um elemento igual a 'elemento' e devolve True.
Faz a operacao do || para fazer o mesmo nos alguidares seguintes.
Se nao criar nenhuma lista com o filter (== elemento) xs, entao significa que nao encontrou nenhum 
alguidar com o elemento igual ao que estamos a comparar e devolve False-}
elemListaDeAlguidares :: Ord a => a -> [[a]] -> Bool
elemListaDeAlguidares _ [] = False
elemListaDeAlguidares elemento (xs:xss) = (filter (== elemento) xs /= []) || elemListaDeAlguidares elemento xss

--A4
{-Funcao que remove um elemento dado de uma lista de alguidares.
Usando a funcao de ordem superior 'filter', vai criar listas onde os elementos 
serao diferentes do elemento dado. Faz filter no alguidar xs e devolve uma alguidar com os elementos diferentes do 
elemento dado, ou seja, remove esse elemento. Faz o mesmo recursivamente para os restantes alguidares-}
removerDaListaDeAlguidares :: Ord a => a -> [[a]] -> [[a]]
removerDaListaDeAlguidares _ [] = []
removerDaListaDeAlguidares elemento (xs:xss) = filter (/= elemento) xs : removerDaListaDeAlguidares elemento xss 

--A5
{-Funcao que usando o fold, recebe o tamanho maximo das sub listas e converte uma dada lista 
numa lista de alguidares.
O acumulador comeca com uma lista vazia, vai efetuar o take maximo na lista xs e 
faz o mesmo com o drop maximo da lista xs, atualizando o valor do acc-}
fromList :: Ord a => Int -> [a] -> [[a]]
fromList _ [] = []
fromList maximo xs = foldl (\acc x -> acc++[take maximo x]) [] (xs : fromList maximo (drop maximo xs))

--A6
{-Funcao de ordem superior que recebe o tamanho maximo dos novos alguidares, uma funcao de um tipo ordenavel, 
noutro tambem ordenavel e uma lista de alguidares, retornando uma nova lista de alguidares com o resultado 
de aplicar a funcao a cada elemento da lista de alguidares antiga.
Efetua o map (map f) para aplicar a funcao a cada elemento da lista de alguidares, concat' para transformar 
esta lista numa lista convencional onde usa o quicksort para ordenar a lista e novamente transformar esta lista 
numa lista de alguidares de tamanho maximo-}
mapListaDeAlguidares :: (Ord a, Ord b) => Int -> (a -> b) -> [[a]] -> [[b]]
mapListaDeAlguidares maximo f = separaAlguidares maximo . quicksort . concat' . map (map f) 


--Funcoes auxiliares

{-Funcao auxiliar para transformar uma lista convencional numa lista de listas, 
com as sub listas a terem um tamanho maximo de valor 'maximo'-}
separaAlguidares :: Int -> [a] -> [[a]]
separaAlguidares _ [] = []
separaAlguidares maximo xs = take maximo xs : separaAlguidares maximo (drop maximo xs)

--Funcao auxiliar para ordenar uma lista
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

--Funcao auxiliar para concatenar uma lista de listas
concat' :: Ord a => [[a]] -> [a]
concat' = foldl (\acc x -> acc++x) [] 

--B1
{-Funcao que recebe o tamanho maximo das sub listas e duas listas de chaves e valores.
Faz zip as listas de chaves e valores e depois separa a lista de tuplos (chave, valor), 
numa lista de alguidares onde cada alguidar tem tamanho maximo = 'maximo'-}
createFastCache :: (Ord a, Ord b) => Int -> [a] -> [b] -> [[(a, b)]]
createFastCache _ [] [] = []
createFastCache maximo xs ys = separaAlguidares maximo (zip xs ys)

--B2
{-Funcao que recebe uma lista de alguidares e um elemento do tipo chave, devolvendo uma lista 
com os varios valores associados a essa chave na lista de alguidares.
A funcao recebe uma lista de listas de tuplos, onde se encontra a chave e o valor associado.
Compara o valor x (chave) do tuplo com key, se for igual, entao adiciona ao acumulador, 
o valor associado a essa chave, caso contrario devolve o acumulador, que comeca vazio.
Vai efetuar estas operacoes na lista de alguidares xss concatenada-}
fastGet :: (Ord a, Ord b) => [[(a, b)]] -> a -> [b]
fastGet [] _ = []
fastGet xss key = foldl (\acc (x,y) -> if x == key then acc++[y] else acc) [] (concat' xss)
