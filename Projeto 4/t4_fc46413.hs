--Nuno Fontes 46413

module Eleicoes
( Candidato(..)
, Estado(..)
, Nacao
, criaNacao
, obterEstado
, adicionaVotosEstado
, adicionaVotosNacao
, vencedorEstado
, vencedorEleicao
) where
 
--1
--Tipo que representa um candidato. Assume dois valores possiveis, A ou B
data Candidato = A | B deriving (Eq, Show)

--Representacao de um estado. Definido usando a sintaxe de registos atraves de um construtor chamado Estado
data Estado = Estado { nome :: String
                     , peso :: Int
                     , votosA :: Int
                     , votosB :: Int
                     }

--Tipo que representa uma nacao. Nacao representada a partir de uma lista de estados, usando o sinonimo de tipos
type Nacao = [Estado]

--2
{-Funcao que recebe uma lista de pares (nome, peso) para cada estado e cria uma nacao com esses estados no momento inicial.
Comecando a 0 o numero de votos de cada candidato-}
criaNacao :: [(String, Int)] -> Nacao
criaNacao [] = []
criaNacao (x:xs) = Estado (fst x) (snd x) 0 0 : criaNacao xs

--3
--Funcao que recebe uma nacao e o nome de um estado e devolve o estado com esse nome
obterEstado :: Nacao -> String -> Estado
obterEstado (x:xs) nomeEstado = if nome x == nomeEstado then Estado nomeEstado (peso x) (votosA x) (votosB x) else obterEstado xs nomeEstado

--4
{-Funcao que recebe um estado e um numero de votos para cada um dos candidatos, e atualiza o numero de votos 
de cada um dos candidatos nesse estado, adicionando ao numero de votos atual. Devolve o estado com o numero de votos atualizado-}
adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado estado numVotosA numVotosB = Estado (nome estado) (peso estado) (votosA estado + numVotosA) (votosB estado + numVotosB)

--5
{-Funcao que recebe uma nacao e uma lista de triplos (nome, votosA, votosB), adiciona esses votos aos respetivos estados e 
devolve a nacao com o numero de votos atualizado.
Criei uma funcao auxiliar que vai receber um a um, os estados da nacao e atualiza o numero de votos, usando uma funcao de 
ordem superior. Verifica se o nome do estado que recebe eh igual ao nome que estah no triplo da lista, em caso afirmativo, 
devolve esse estado com valores dos votos atualizados, com o auxilio da funcao criada anteriormente, adicionaVotosEstado.
Caso contrario, o acumulador mantem se com o caso base que eh o estado que recebeu, x, e vai continuar a procurar o estado com esse nome 
no resto da lista de triplos (ys). Se nao encontrar, devolve o estado sem efetuar alteracoes no numero de votos dos candidatos.
A funcao principal (adicionaVotosNacao), chama a funcao atualizaEstado com o primeiro estado da nacao e a lista de triplos e 
faz o mesmo recursivamente com o resto dos estados da nacao e a lista de triplos. No final devolve a nacao com os estados atualizados, 
no caso da lista de triplos nao ser vazia-}
adicionaVotosNacao :: Nacao -> [(String, Int, Int)] -> Nacao
adicionaVotosNacao [] _ = []
adicionaVotosNacao (x:xs) [] = x:xs -- 0 triplos
adicionaVotosNacao (x:xs) ys = atualizaEstado x ys ++ adicionaVotosNacao xs ys -- 1 ou mais triplos

atualizaEstado :: Estado -> [(String, Int, Int)] -> Nacao
atualizaEstado x = foldl (\acc y -> if nome x == fstTriplo y then [adicionaVotosEstado x (sndTriplo y) (thiTriplo y)] else acc) [x]

fstTriplo :: (a, b, c) -> a
fstTriplo (a,_,_) = a

sndTriplo :: (a, b, c) -> b
sndTriplo (_,b,_) = b

thiTriplo :: (a, b, c) -> c
thiTriplo (_,_,c) = c

--6
{-Funcao que recebe um estado, compara o numero de votos de cada candidato num estado e devolve o candidato vencedor nesse estado. 
Em caso de empate, devolve Nothing-}
vencedorEstado :: Estado -> Maybe Candidato
vencedorEstado estado
  | votosA estado == votosB estado = Nothing
  | votosA estado > votosB estado = Just A
  | otherwise = Just B

--7
{-Funcao que recebe uma nacao e devolve o vencedor da eleicao, ou seja, o candidato que alcancou mais representantes em todos os estados.
Criei uma funcao auxiliar para contar os representantes que um candidato recebeu em cada estado. Esta funcao vai receber uma nacao e um candidato, 
compara o vencedor do primeiro estado com o candidato recebido pela funcao, se for o mesmo, devolve o peso deste estado, que corresponde 
ao numero de representantes e vai fazer o mesmo, recursivamente para o resto dos estados da nacao, se o candidato que estamos a comparar for o vencedor 
noutro estado, vai adicionar o peso desse estado ao peso atual. Caso contrario chama a funcao recursivamente.
A funcao principal vai chamar esta funcao auxiliar passando a nacao e o candidato que queremos verificar qual o numero de representantes que obteve 
na nacao. Em caso de empate no numero de representantes na nacao entre os dois candidatos, devolve Nothing. Se o numero de representantes 
do candidato A for maior que o numero de representantes do candidato B, entao o vencedor da nacao eh o candidato A e devolvemos Just A, caso contrario, 
o vencedor da nacao eh o candidato B e devolvemos Just B-}
vencedorEleicao :: Nacao -> Maybe Candidato
vencedorEleicao nacao
  | contaRepresentantes nacao (Just A) == contaRepresentantes nacao (Just B) = Nothing
  | contaRepresentantes nacao (Just A) > contaRepresentantes nacao (Just B) = Just A
  | otherwise = Just B

contaRepresentantes :: Nacao -> Maybe Candidato -> Int
contaRepresentantes [] Nothing = 0
contaRepresentantes [] _ = 0
contaRepresentantes (x:xs) candidato
  | vencedorEstado x == candidato = peso x + contaRepresentantes xs candidato
  | otherwise = contaRepresentantes xs candidato

--8
{-Tornando o tipo de dados Estado instancia da classe Eq. Dois estados sao considerados iguais se tiverem o mesmo peso, 
e se o vencedor nesse estado for o mesmo, ou seja, verificamos se o peso eh o mesmo e se o vencedor nos dois estados eh o mesmo-}
instance Eq Estado where
    (Estado _ peso1 votosA1 votosB1) == (Estado _ peso2 votosA2 votosB2) = 
        peso1 == peso2 && ((votosA1 > votosB1 && votosA2 > votosB2) || (votosA1 < votosB1 && votosA2 < votosB2))

--9
{-Tornando o tipo de dados Estado instancia da classe show. Representacao textual de um estado na forma: 
nome peso votosA votosB-}
instance Show Estado where
    show (Estado nome peso votosA votosB) = nome ++ " " ++ show peso ++ " " ++ show votosA ++ " " ++ show votosB