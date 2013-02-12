--import System.Random
--Numeros en correcta posicion 
posicionCorrecta::[Int]->[Int] ->Int
posicionCorrecta [] [] = 0
posicionCorrecta (xs) (ys) = if [head xs] == [head ys] then 1 + posicionCorrecta (tail xs) (tail ys)
                                        else posicionCorrecta (tail xs) (tail ys)
--Numeros existente en la clave
numeroCorrecto::[Int]->[Int]->Int
numeroCorrecto [] [] = 0
numeroCorrecto (xs) (ys) = length [ (l,m) | l <- xs, m <- ys , l==m]
 
--Descomponer un numero a lista
descomponer::Int -> [Int]
descomponer n 
                |n<10=n:[]
                |otherwise = descomponer (div n 10) ++ [mod n 10]
                    
--Si la clave es Correcta retorna 1
numero :: [Int]->[Int]->Int
numero [] [] = 1
numero (lista1) (lista2) = if lista1 == lista2 
                                then 1
                                else 0 
--Inserta un numero a una lista en la posicion n-esima                 
insertar :: [Int]->Int->Int->Int->[Int]
insertar [] _ _ _ = []
insertar (x:xs) nc y r = if ((r + length [x]) == nc)
                           then   [y]++(insertar(xs) nc y (r+length[x]) )
                           else   [x]++(insertar(xs) nc y (r+length[x]))

insertarNesima :: [Int]->Int->Int->Int
insertarNesima [] _ _ = 0
insertarNesima (x:xs) n r = if (r + length [x]) == n
                           then   x
                           else   (insertarNesima(xs) n (r+length[x])) 

 --Inserta un numero a una lista en la posicion n-esima                 
insertarNumero :: [Int]->Int->[Int]
insertarNumero [] _ = []
insertarNumero (x:xs)  y = if (x == 0)
                           then   [y]++(insertarNumero(xs) y)
                           else   [x]++(insertarNumero(xs) y)
                           
                           
                           
 --Jugar MasterMind                                                       
jugarMM :: [Int]->[Int]->IO()
jugarMM (lista1) (lista2) = do
                                   let  n = posicionCorrecta lista1 lista2  
                                        m = numeroCorrecto lista1 lista2
                                        p = m-n
                                        par = [n,p]
                                         
                                   let  combinacion = [[w,x,y,z]| w<-[1..6], x<-[1..6], y<-[1..6], z<-[1..6]]
                                        aleatorioComb = 555--randomRio(0,1295
                                        listaComb = [0,0,0,0]--combinacion !! aleatorioComb
                                         
                                   if   lista1 == lista2
                                        then putStrLn $ "El COMPUTADOR adivino la clave :"++show(lista1)
                                        else do  putStrLn $ ""++show(lista2)++"  "++show(par) 
                                                 --jugarMM lista1 (listaPosicion lista2 listaComb n)
                                                 putStrLn $ "lista nueva: "++show(listaPosicionC lista2 listaComb n)
                                                 putStrLn $ "lista nueva: "++show(listaNumeroC lista2 (listaPosicionC lista2 listaComb n) p)       
                                           
--Lista de posiciones correcta                                           
listaPosicionC :: [Int]->[Int]->Int->[Int]
listaPosicionC [] [] _ = []
listaPosicionC lista1 lista2 n = do
                                  let     posAleatorio =3--randomRIO(0,3) --[1,4,5,2]
                                          enesimo = lista1 !! posAleatorio
                                          lista =(insertar lista2 (posAleatorio+1) enesimo 0)
                                  if n > 0 
                                         then listaPosicionC lista1 lista (n-1)
                                         else lista
--Lista de numero correcta                                           
listaNumeroC :: [Int]->[Int]->Int->[Int]
listaNumeroC [] [] _ = []
listaNumeroC lista1 lista2 p = do
                                  let     posAleatorio =5--randomRIO(1,6) --[1,4,5,2]
                                          --enesimo = lista1 !! posAleatorio
                                          lista =(insertarNumero lista2 posAleatorio)
                                  if p == 0 
                                         then lista
                                         else listaNumeroC lista1 lista (p-1)                                         
                                         
                                         
--Menu principal                                                      
main::IO()
main = do  
         putStrLn "MASTER MIND"
         putStrLn  "Ingrese la clave secreta : " 
         clave <- readLn
         
         let
                        colors = [1..6]
                        combination = [[w,x,y,z]| w<-colors, x<-colors, y<-colors, z<-colors]
                        lista2 = combination !! 1295
         let            lista1 = descomponer clave--lista de clave secreta
         let            verdad =  numero lista1 lista2
                        --n = posicionCorrecta lista1 lista2  
                       -- m = numeroCorrecto lista1 lista2 
                        
         if verdad == 1 
            then putStrLn "Clave correcta"
            else jugarMM lista1 lista2 
         