--Nuno Fontes 46413

--A
frequencias :: String -> [Int]
frequencias xs = [quantos c xs | c <- xs]

quantos :: Char -> String -> Int
quantos c xs = length [x | x <- xs, x == c]

--B
pequenasPalavras :: [String]
pequenasPalavras = [[c,c',c''] | c <- ['a'..'z'], c' <- ['a'..'z'], c'' <- ['a'..'z'],
                    c `elem` "aeiouy" || c' `elem` "aeiouy" || c'' `elem` "aeiouy"]

--C
legendaCampainha :: Int -> Int -> [(String, Int)] -> [String]
legendaCampainha x x' xs = [show andar ++ posicao | andar <- [1..x], xi <- [0..length xs-1], 
                            andar <= snd (xs !! xi), posicao <- [fst (xs !! xi)], andar /= x', x /= 0]
