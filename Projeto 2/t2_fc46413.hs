--Nuno Fontes 46413

--A1

{-Concatena o elemento da lista, x, com o elemento seguinte, cabeca da tail da lista, head xs,
  depois chama a funcao recursiva para fazer o mesmo com os restantes elementos-}
paresConsecutivos :: [a] -> [(a, a)]
paresConsecutivos [] = []
paresConsecutivos [_] = []
paresConsecutivos (x:xs) = (x, head xs) : paresConsecutivos xs

--A2

{-Funcao semelhante a funcao paresConsecutivos, mas neste caso vamos subtrair cada elemento ao anterior,
  ou seja, fazemos a diferenca da cabeca da tail da lista, head xs e o elemento anterior da lista, x-}
diferencasConsecutivas :: [Int] -> [Int]
diferencasConsecutivas [] = []
diferencasConsecutivas [_] = []
diferencasConsecutivas (x:xs) = head xs - x : diferencasConsecutivas xs

--B1

{-Funcao que recebe um jogador e uma lista de jogadores, compara o jogador com os jogadores da lista
  e verifica se eh ou nao claramente pior que algum dos jogadores desta lista-}
claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
--No caso de comparar um jogador com uma lista de jogadores vazia, o jogador nao eh claramente pior
claramentePior _ [] = False
--Compara o jogador com o primeiro jogador da lista
claramentePior x (y:ys)
  {-Para isso, vai verificar se o ataque e a defesa do jogador eh inferior ao ataque e defesa do jogador da lista,
    se se confirmar, devolve True-}
  | ataque x < ataque y && defesa x < defesa y = True
  --Caso contrario, vai comparar o jogador com os restantes jogadores da lista, chamando a funcao recursiva
  | otherwise = claramentePior x ys

ataque :: (a,b,c) -> b
ataque (_,b,_) = b

defesa :: (a,b,c) -> c
defesa (_,_,c) = c

--B2

--Funcao que recebe uma lista de jogadores e devolve os jogadores que nao sao claramente piores que nenhum outro jogador
filtroJogadores :: [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores xs = comparaListas xs xs

{-Crio uma funcao comparaListas porque vou precisar de manter os elementos da lista para comparar cada elemento
 de uma lista com os elementos dessa lista, os elementos vao ser mantidos intactos na lista ys-}
comparaListas :: [(String, Int, Int)] -> [(String, Int, Int)] -> [(String, Int, Int)]
--Se a lista a comparar for vazia, retorna me uma lista vazia (nao ha nada a comparar)
comparaListas [] _ = []
--Primeiro vou comparar o primeiro jogador x da lista xs com os jogadores da lista ys
comparaListas (x:xs) ys
  {-Se o jogador a comparar nao for claramente pior do que nenhum outro jogador da lista ys, 
    adiciona esse jogador a lista a retornar e vai chamar a funcao recursiva para comparar os restantes jogadores, um a um, 
    da tail de xs, com a lista ys-}
  | not (claramentePior x ys) = x : comparaListas xs ys
  {-Se o jogador a comparar for claramente pior do que algum outro jogador da lista ys, este jogador nao eh adicionado
    a lista a retornar e chamamos a funcao recursiva para ir comparar o jogador seguinte da tail de xs, com a lista ys-}
  | otherwise = comparaListas xs ys

--C

--Funcao que vai receber uma lista de listas e vai retornar o valor que falta para tornar o sudoku valido
preencherVazio :: [[Int]] -> Int
preencherVazio [] = 0
--Comeca por verificar a primeira lista, xs
preencherVazio (xs:xss)
  --Sabemos que a soma dos elementos de cada linha do sudoku tem de ser igual a 45
  | sum xs == 45 = preencherVazio xss
  {-Se a soma dos elementos de alguma das listas nao for igual 45, sabemos que existe uma casa vazia, 
    devolve assim o numero que substitui o 0, fazendo a diferença entre 45 e a soma dos elementos dessa lista-}
  | otherwise = 45 - sum xs
