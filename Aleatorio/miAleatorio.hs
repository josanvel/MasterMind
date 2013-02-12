import System.Random
--Obtener Entero del Random
intRandonR :: (Int,StdGen) -> Int
intRandonR (x,y) = x

--Funcion Aleatoria
miAleatorio :: StdGen -> Int-> Int->Int			
miAleatorio rng min max=let
						x = randomR (min, max) rng 
						in intRandonR x