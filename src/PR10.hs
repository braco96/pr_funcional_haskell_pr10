-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------                                   INTRODUCCION .
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


-----------------
-----------------EJEMPLO UNO , DADO UN NUMERO Y UNA LISTA DEVOLVEREMOS UNA LISTA CON LOS ELEMENTOS DE LA LISTA MAYORES QUE ESE N.


------------------------------------ 1º Usando la funcion de primer orden filter.

                                              ------------ Utilizamos la funcion de orden superior filter que evalua una condicion respecto a todos los
numerosMasGrande num lista = filter (>num) lista ------------ elementos de la lista, su salida es una lista con los elementos que cumplen esa condicion 
                                              ------------ y se encuentran en la lista.

------------------------------------ 2º Usando la forma recursiva.

mayor x num = x>num ------------Equivalente a poner filter (>num) lista ya que comprueban ambos que un elemento de la lista sea mayor que un numero
 
numerosMasGrande1 :: Int->[Int]->[Int] --------- Signatura de la funcion 
numerosMasGrande1 _ []=[]
numerosMasGrande1 num (x:xs)  ------------ elementos de la funcion
                             | mayor (x) num =x:llamada ---------- Caso 1 , el elemento primero de la lista es mayor que el numero , lo metemos en la lista y llamamos
                                                     ----------- de forma recursiva
                             |otherwise = llamada    ---------- Cualquier otro caso  llamamos de forma recusiva
                    where llamada =numerosMasGrande1 num xs --------Para ahorrar espacio decimos que llamada es la llamada recursiva.




-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------CARACTERISTICAS LENGUAJES FUNCIONALES . 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------





-------------------------------------------------------------------------------------------------------------------------------------------------------------------
---      Demostracion inferencia de tipos : No es necesario declarar los tipos de parámetros ni el resultado al desarrollar una función .
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

ponUltimo elem lista = lista++[elem] -------- Esta funcion coge un elemento de cualquier tipo y lo mete en una lista del mismo tipo al final.

modulo lista = sqrt(sum([x^2|x<-lista])) --------- Esta funcion calcula el modulo de una lista , es decir la raiz de la suma de los cuadrados 
                                         --------- de los elementos en la lista


-------Tras hacer la llamada :t ponUltimo y :t modulo conoceremos los tipos de parámetros y resultado a desarrollar asignado por haskel a cada función . :

------- ponUltimo :: a -> [a] -> [a] , esto nos quiere decir que la funcion ponULtimo toma un tipo a , una lista a y devuelve una lista a

------- modulo :: Floating a => [a] -> a , esto nos quiere decir el tipo a debe ser pertenecer a la clase Floating ,toma una lista y devuelve a



-------------------------------------------------------------------------------------------------------------------------------------------------------------------
---                                          Transparencia referencial :
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


----------------------------------
---Fuertemente tipado:
----------------------------------


----------------------------------
---Abstracción, brevedad y claridad de los códigos :
----------------------------------


----------------------------------
---Polimorfismo de tipos :
----------------------------------


----------------------------------
---Funciones de orden superior : 
----------------------------------


----------------------------------
---Gestión automática de memoria : 
----------------------------------


----------------------------------
---Evaluación perezosa :
----------------------------------


----------------------------------
----------------------------------





-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                                 Clases y tipos :
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                                 Tipos basicos:
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------




-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                                 Tipos compuestos:
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                                      1 Listas :
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                        1.1 Declaracion de listas y ejemplo:
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

l1 =['a','s','a'] ------l1 es [char]=String

l2 = [1,2,3,4] ------l2 es [Integer] una lista de enteros

l3 =[False,True,False] -------l3 es [Bool] una lista de valores booleanos

l4 =[5..10] -----l4 es [Integer] una lista de enteros

---------------------------------- Ejemplos :
---------------------------------- Operador : para unir un elemento al principio de la lista, el tipo debe ser el mismo

l5='c':l1 ------ l5 es [char] de forma que le metemos la letra c primero
          ------ l5 ="casa"

---------------------------------- Operador ++ para concatenar dos listas iguales

l6=l2++l4 ------ l6 es [Integer] y tiene los numeros del 1 al 10

----------------------------------- Funcion ejemplo : suponemos que queremos representar valores binarios como valores booleanos
----------------------------------- True = 1 ; False =0 

resultadoSuma ::Bool->Bool->Bool ---------Esta funcion compara dos valores booleanos y da su suma binaria
resultadoSuma a b  
        | (a == b)   = False ---- Si tenemos true true o false false , la suma binaria es False
        |otherwise = True    ---- Si son distintos la suma es True

suma :: [Bool]->Bool
suma [x] = x                                       ---------- Cuando tenemos un unico elemento , hemos terminado ya que la suma es dicho valor
suma (x:y:xx) = suma ((resultadoSuma x y):xx)      ---------- Si tenemos dos o mas elementos ,calculamos la suma de los dos primeros elmentos y 
                                                   ---------- Lo juntamos con el resto de la lista para irlo sumando.
------------ si probamos suma [True,True,False,True] obetenemos True de forma que T+T=F+F=F+T=T



--------------------------------------------------------------------
----1.2 Ejemplos de funciones predefinidias con listas
--------------------------------------------------------------------

------------Ejemplo 1 : funcion que nos de la cantidad numeros menores que otro que hay una lista .

nMenores n list = length (filter (<n) list) ---------- Filter nos da una lista con los numeros menores que n
                                            ---------- length nos dice cuantos elementos hay en la lista

------------Ejemplo 2 : funcion suma todos los elementos de una lista mayores que n .

sumaMayoresQue n list = sum(filter (>n) list)---------- Para sumar los elmentos mayores que otro numero de una lista , primero filtramos dichos numeros con filter
                                             ---------- y despues los sumamos.

------------Ejemplo 3 : funcion suma un numero a todos los elementos de la lista .

sumaN num lista = map (+num) lista --------Suma un elemento num a todos los elementos de la lista

------------Ejemplo 4 : funcion que nos da el elemento n de la lista.

elementoNesimo num list = head(drop (num-1) list)

------------Ejemplo 5 : funciones primas ,acontinuacion veremos algunas funciones que imitan a otras predefinidas usando otras predefinidas .

last' list = drop ((length list)-1) list ---------- last' hace lo mismo que last , coger el ultimo elemento , para ello ¡ eliminamos todos los elementos menos el ultimo
                                         ---------- lo cual es lo mismo que eliminar  longitud de la lista -1 elementos de la lista.

init' lista = take ((length lista)-1) lista ----------init' hace lo mismo que init tomando todos los elementos de la lista menos el ultimo (misma mecanica last')

tail' lista = drop 1 lista ---------tail' hace lo mismo que la funcion tail , para ello eliminamos el primer elemento

head' lista = take 1 lista---------head' hace lo mismo que la funcion head , para ello cogemos solo el primer elemento


-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                              2 tuplas :
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                  2.1 Declaracion de tuplas y ejemplo:
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

t1=([1,2,3,4,5],'a')  --------la tupla t1 tiene longitud 2 , esta formada por [Integer] y Char

t2=('a',1) -------- la tupla t2 tiene logitud 2 , esta formada por un Char y un Integer

t3 =[t2,('b',2)] --------t3 es una lista de tuplas [(Char,Integer)] t3 = [('a',1),('b',2)]

t4 = ("Luis","Bravo","Collado","x15m069",28229) --------- t4 tiene longitd 5 (String,String,String,String,Integer)
                                                --------- esta tupla corresponde a nombre , apellido1 , apellido2 , matricula, codigo postal.

sacarNombre (a,b,c,_,_) = a++"  "++b++"  "++c ---------Coge una tupla como t4 y nos da el nombre y los apellidos como un String
                                             
                                             --------- sacarNombre t4 tendrá la salida "Luis  Bravo  Collado"

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                            2.2 Ejemplos de funciones predefinidias con tuplas:
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------Ejemplo 1: Suponemos que tenemos una lista de tuplas (Char,Int) y queremos dos funciones , una que nos de el numero resultado de sumar todos los
--------------------------------------Enteros y otra que nos de la cadena de texto resultante a unir todos los char.

textoTupla tupla = [fst(x)|x<-tupla]  ------Como queremos la concatenacion generamos la lista de coger el primer elemento de la tupla para todos los elementos de la lista.
                                    -------- fst coge el primer elemento de la tupla para cada elemento de la lista .

numTupla tupla = sum(([snd(x)|x<-tupla])) ------ Como queremos la suma de todos hacemos lo mismo que en la anterior pero cogiendo el segundo elemento de la tupla y sumamos la lista.

ej =[('c',3),('a',1),('s',20),('a',1)] ------- numTupla ej será 25 y textoTupla ej será "casa"

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------Ejemplo 2 : funcion de orden superior que recibe una condicion para seleccionar los elementos de una lista de enteros.
numFuncion :: ([Int],(Int->Bool))->[Int] -------recibirá una tupla compuesta por una lista de enteros y una funcion, retornará una lista de enteros.
numFuncion (num,f)= [(x)|x<-num,f x]       ------- seleccionamos los elementos de la lista que verifican la condicion

naturales  = flip (curry numFuncion) (>0) ------- curry numFuncion hace que la funcion reciba como parametros una lista y una funcion en lugar de una tupla
                                          ------- flip altera el orden de los parametros de forma que primero recibirá la condicion y naturales heredará lista como unico
                                          ------- parametro

n1=naturales [-1,1,2] -------salida [1,2]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------Ejemplo 3 : Genera cuatro funciones que dada una tupla de 4 elementos nos den el 1º 2º 3º y 4º elemento

fst' (a,_,_,_)=a ----------Nos da el primer elemento

snd' (_,a,_,_)=a ----------Nos da el segundo elemento

trd' (_,_,a,_)=a ----------Nos da el tercer elemento

fth' (_,_,_,a)=a ----------Nos da el cuarto elemento

ej1=("Spain","Madrid",28229,692810101) 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------Ejemplo 4 : Transformar dos funciones predefinidas en otras que reciban una tupla como parametro.

mod' = uncurry (mod) --------Ahora la funcion predefinida mod tomará como parametro una tupla de enteros (Integer,Integer)

div'=uncurry div --------Ahora la funcion predefinida div tomará como parametro una tupla de enteros (Integer,Integer)

-------------------------------------Ejemplo 5: Imaginemos que tenemos un conjunto de personas y queremos asignarles un id por orden de llegada, de forma mas clara
-------------------------------------hay dos listas y queremos generar una lista de tuplas , [a]->[b]->[(a,b)]    a = [String] y b = Int

asignarId :: [String]-> [(String,Int)] --------- El parametro de entrada sera la lista de personas que tenemos y el de salida la tupla con las personas y su id
asignarId list = zip list conjunto     --------- zip genera la tupla asignandole a cada elemento de nuestra lista un elemento de la lista conjunto (contiene los ids)
         where conjunto = [(x)|x<-[1..(length list)]] --------- conjunto es la sucesion de numeros que va desde 1 hasta el numero de personas que hay (longitud lista)


------------Guardamos una lista de las personas para poder probarlo directamente en la consola con el fichero

lspersonas = ["Pepe Lopez","Antonio Moreno","Claudia Fernandez"]

------------ asignarId lspersonas tiene como salida : [("Pepe Lopez",1),("Antonio Moreno",2),("Claudia Fernandez",3)]


-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
----                                                           3 POLIMORFISMO :

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------




-------------------------------------------------------------------------------------------------------------------------------------------------------------------
----                                                 3.1 Funciones polimorficas predefinidas :
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 
-------------Prelude> :t length                                length utiliza el tipo t que es de la clase Foldable y aplicado a a ,nos da un natural
-------------length :: Foldable t => t a -> Int


-------------Prelude> :t map                                   map usa (a->b) para definir la funcion que aplicaremos sobre [a] , como el resultado puede                        
-------------map :: (a -> b) -> [a] -> [b]                     ser o no distinto tenemos la salida [b] de forma que a puede ser o no b , irá implicito en 
-------------                                                  la funcion (a->b). a y b pueden ser cualquier tipo de los anteriores descritos anteriormente


-------------Prelude> :t filter                                filter utiliza una condicion (a->Bool) sobre una lista [a]
-------------filter :: (a -> Bool) -> [a] -> [a]


-------------Prelude> :t curry                                 Sin tener en cuenta el tipo de ningun elemento pasamos de una tupla a tener dos variables
-------------curry :: ((a, b) -> c) -> a -> b -> c             una por cada argumento de una tupla de orden 2


-------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------Ejercicio 1: Hacer la funcion curry' y uncurry' que hagan lo mismo que curry para una tupla de longitud 4.



curry' ::  ((a, b,c,d) -> e) -> (a -> b -> c->d->e)
curry' fun a b c d = fun (a,b,c,d)

uncurry' ::   (a -> b -> c->d->e)->((a, b,c,d) -> e)
uncurry' fun (a,b,c,d)= fun a b c d



genTotal ::(Show a,Show b)=>String->String->a->b->String ---------ej1 tiene numeros pero esta funcion es polimorfica para los dos ultimos parametros
genTotal p c cp tlf= p++espacio++c++espacio++(show cp)++espacio++(show tlf) ---------Fusiona todo en una cadena (a y b pueden ser o no iguales)
            where espacio ="    "


                                 --------------  Reutilizando ej1=("Spain","Madrid",28229,692810101) 
ej0=uncurry' genTotal ej1        --------------  Descurrificando la funcion obtenemos como parametro de entrada una tupla    
                                 --------------  Valor : "Spain    Madrid    28229    692810101" cadena de texto
 

ej1'=("Spain","Madrid","28229",692810101)   ---------Valor : "Spain    Madrid    \"28229\"    692810101 "

------------------------------------------------------------------------------------------------------------------------------------------------------------------- 



-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------Ejercicio2 :Desarroyar una funcion que dado una lista y un elemento del tipo de la lista nos de la lista resultante de elinar ese elemento si está. 


eliminarElementosRecursiva::(Eq a)=>[a]->a->[a]-------------Funcion polimorfica qu quita un elemento de una lista , necesitamos Eq para poder comparar
eliminarElementosRecursiva [] _= []-----------caso base
eliminarElementosRecursiva(x:xs) el 
                            |el==x =resto-----------si es igual no lo metemos
                            |otherwise = x:resto------------si es distinto lo metemos
        where resto = eliminarElementosRecursiva xs el-------------usamos where para almacenar la llamada en resto


eliminarElementos::(Eq a,Show a)=>[a]->a->[a]---------misma arriba pero usando filter
eliminarElementos lista el = filter (/=el) lista ----------metemos todos los de la lista que sean distintos de e1

prueb1=eliminarElementos ("Los caballos suelen escampar en el prado") 'e' ---------Valor :"Los caballos suln scampar n l prado"
prueb2=eliminarElementos [1,2,3,4,5,1,2,1,3] 3 ------------Valor : [1,2,4,5,1,2,1]


-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------                                                  SINONIMOS
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


type ListaCompra = [String] -----------Lista de la compra es una lita de cadenas.

type DiasSemana = [String] ----------- Los dias de la semana tambien.

type AgendaTelefonica=[Integer]-----------Una agenda telefonica es una lista de enteros.



-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------                                                  FUNCIONES  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 

------------------------------------------FUNCIONES DE ORDEN SUPERIOR
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------Ejemplo 1 : Elevar todos los elementos de una lista al cuadrado
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

potenciaOs n lista = map (^n) lista

------------Ejemplo 2 : Sacar una lista con los elementos mayores que n y dividir estos entre n
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

divisionMayorUnidadOs n lista =(map (div n ) (filter (>n) lista) )

------------Ejemplo 3 : Cantidad de elementos menores que n en una lista

nMenores n list = length (filter (<n) list)  


-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 -----------------------------------------Funcion por ajuste de patron :
 -------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------listas por compresion

------------Ejemplo 1 : Elevar todos los elementos de una lista al cuadrado
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

potenciaLc n lista=[x^n|x<-lista]


------------Ejemplo 2 : Sacar una lista con los elementos mayores que n y dividir estos entre n
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


divisionMayorUnidadLc n lista =[(div n x)|x<-lista,x>n]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

 

----------------------------------------if (condicion)then ( ) else( )
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

------------Ejemplo 1 :Elevar todos los elementos de una lista al cuadrado
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


potencia :: (Integral a,Num a)=>a->[a]->[a]
potencia _ []=[]
potencia num (x:xs)=(x^num):potencia num xs

potenciacond :: (Integral a,Num a)=>a->[a]->[a]
potenciacond num lista = if((length lista)>0)then pr:recurs else []
                        where (pr,recurs)= (head(lista)^num,tail(lista))

potenciacondwh :: (Integral a,Num a)=>a->[a]->[a]
potenciacondwh num lista = if((length lista)>0)then pr:recurs else []
                        where (pr,recurs)= (head(lista)^num,tail(lista))


 
------------Ejemplo 2 : Sacar una lista con los elementos mayores que n y dividir estos entre n
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

divisionMayorUnidad :: (Integral a)=>a->[a]->[a]
divisionMayorUnidad _  []=[]
divisionMayorUnidad n (x:xs) =if (x>n)then (div n x):resto else resto
                        where resto= divisionMayorUnidad n xs

-------------------------------------------------------------------------------------------------------------------------------------------------------------------


----------------------------------------barras horizonrales casos introducidos en lugar del if else
   -------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------Ejemplo 1 :Elevar todos los elementos de una lista al cuadrado
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

potenciabarra ::(Integral a,Num a)=>a->[a]->[a]
potenciabarra _ []=[]
potenciabarra num lista
                        |   (length lista>0) = pr:recurs
                        |    otherwise=[]
                        where (pr,recurs)= (head(lista)^num,tail(lista))   
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

------------Ejemplo 2 : Sacar una lista con los elementos mayores que n y dividir estos entre n
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

divisionMayorUnidadbarra :: (Integral a)=>a->[a]->[a]
divisionMayorUnidadbarra _  []=[]
divisionMayorUnidadbarra n (x:xs) 
                        |(x>n) = (div n x):resto 
                        |otherwise = resto
                        where resto =divisionMayorUnidadbarra n xs




-----------------------------------------let para acceder llamada recursiva
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

------------Ejemplo 1 :Elevar todos los elementos de una lista al cuadrado
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
potenciaLET ::(Integral a,Num a)=>a->[a]->[a]
potenciaLET _ []=[]
potenciaLET num lista
                        |   (length lista>0) = pr:recurs
                        |    otherwise=[]
                        where (pr,recurs)= (head(lista)^num,tail(lista))   
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

------------Ejemplo 2 : Sacar una lista con los elementos mayores que n y dividir estos entre n
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

divisionMayorUnidadLET :: (Integral a)=>a->[a]->[a]
divisionMayorUnidadLET _  []=[]
divisionMayorUnidadLET n (x:xs)= let resto=divisionMayorUnidadLET n xs in
                        if (x>n)then (div n x):resto else resto




-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------                                                Ejemplo general







-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------                                                  Tipos algebraicos  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
-------------------------------------------Ejemplo basico :dias de la semana
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
data Dias  = LUNES|MARTES|MIERCOLES|JUEVES|VIERNES|SABADO|DOMINGO deriving (Show,Eq, Ord)

menor dia1 dia2 
               | dia1<dia2 =dia1
               |otherwise = dia2

------------------ *Main> menor LUNES MARTES
------------------ LUNES
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
 

-------------------------------------------Ejercicio moneda :
data Moneda = Cara | Cruz deriving (Eq) ---------Objeto moneda compuesto por cara o cruz como nos interesa compararlo 
                                        ---------Usamos la clase Eq explicada mas abajo

esCara :: Moneda->Bool --------Funcion que nos dice si una moneda esta de cara o cruz
esCara l = l ==Cara

                       --------------Funcion que cuenta las veces que salio cara
contarCaras lista = length(filter (esCara) lista)
 
 -------contarCaras [Cara,Cruz,Cara,Cruz]  VALOR: 2
 
-------------------------------------------MONEDA

-------------------------------------------Ejercicio Dimension :
data Dimension  = R2 (Double,Double) | R3 (Double,Double,Double) deriving Show----------Como esta COMPUESTA POR NATURALES 
                                                                                --------solo nos interesa show
esEspacio :: Dimension->Bool ------Funcion que nos dice si espacio o plano
esEspacio (R2 _) =False
esEspacio (R3 _) =True

sumacuadrado :: Dimension->Double -------Funcion que hace la suma al cuadrado de los elementos para posteriormente 
                                 ---------hacer el modulo
sumacuadrado (R2 a )= (fst a)^2+ (fst a)^2 --------caso plano
sumacuadrado (R3 (a,b,c)) =a^2+b^2+c^2               -------caso espacio
 

moduloVector x = sqrt(sumacuadrado x)-----------FUncion que nos da el modulo de un tipo Dimension.


---------Ejemplos:

r2 =R2(1,2)
r3=R3(1,2,3)
 
------------- *Main> sumacuadrado r2
------------- 2.0
------------- *Main> sumacuadrado r3
------------- 14.0
------------- 
------------- *Main> moduloVector r2
------------- 1.4142135623730951
------------- 
------------- *Main> moduloVector r3
------------- 3.7416573867739413
-------------------------------------------Dimension
 
-------------------------------------------Juego

data Juego = Carta Int| Mano [Juego]deriving (Eq,Show)

----------- Estas funciones transforma Dato en un elemento del tipo maybe
sacarTiros man = map partida man
  
  

partida (Carta val) =  Just val 

partida (Mano game) =  puntuacion((sacarTiros game))
--------------------------------------------

------------------------------------------------------------Esta funcion nos da el valor de nuestra mano , si es mayor que 21 retornará nothing
puntuacion ::[Maybe Int]->Maybe Int
puntuacion lista =if (er(lista)||sum1>21) then Nothing
                  else Just sum1
                  where sum1=suma(lista)
        
------------------------------------------------------------------------------------------------------

-------------------------------- Si el conjunto cumple la condicion la tirada no será valida , esta funcion detecta si lo es
er :: [Maybe Int]->Bool
er [] =False
er ((Just a) :xx) = (0==a)|| a>12||er xx
 
 
------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------                                                  Tablas  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

                                       --------- Generaremos una tabla en la que cada persona tiene un id distinto.
asignarIdTabla :: [String]-> [(String,Int)] --------- El parametro de entrada sera la lista de personas que tenemos y el de salida la tupla con las personas y su id
asignarIdTabla list = zip list conjunto     --------- zip genera la tupla asignandole a cada elemento de nuestra lista un elemento de la lista conjunto (contiene los ids)
         where conjunto = [(x)|x<-[1..(length list)]] --------- conjunto es la sucesion de numeros que va desde 1 hasta el numero de personas que hay (longitud lista)

tablaPeronas =asignarIdTabla (["Pepe","Maria","Juan","Laura","Carla","Jose"]) 
----------- tablaPeronas =[("Pepe",1),("Maria",2),("Juan",3),("Laura",4),("Carla",5),("Jose",6)]


--------- Generaremos una tabla en la que cada persona tiene un nick unico
crearTabla::[a]->[b]->[(a,b)]
crearTabla e1 e2 = zip e1 e2

tablaUsuarios = crearTabla ["sukero33","monty88","fruit67"] ["Sonia Fenandez","Matias Bravo","Chiara Haudenes"]
--------- tablaUsuarios VALOR =[("sukero33","Sonia Fenandez"),("monty88","Matias Bravo"),("fruit67","Chiara Haudenes")]


 
------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------                                                  Arboles  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


data Arbol a = Nodo a [Arbol a] deriving (Eq,Show)


------------------------------------------ Si es valida esta funcion suma todos los dados
suma_:: [Maybe Int]->Int
suma_ []=0
suma_((Just a):resto)=a+suma_ resto

---------------------------------------------------------------------------------------------

 
------------------------------------------LAB1 SEÑORA

maximaaSuma :: Arbol Int -> Int
maximaaSuma a = maximum [sum xs | xs <- ramas a]


------------------------------------------------------CONJUNTO A ARBOL

--------------------------------------------------- Me da un arbol por cada rama
aArbol :: [a]->Arbol a
aArbol [x] = (Nodo x [])
aArbol (x:xs) = (Nodo x [aArbol xs])


------------------------------------------------------ uno la raiz con el conjunto de ramas
generarArbol ::a->[[a]]->Arbol a
generarArbol raiz lista = (Nodo raiz) (map   aArbol lista )

----------------------------------------------------
----------------------------------------------------EJERCICIO ALTURA 
-----------------------------------------Longitud de la altura maxima
alturamas ::Arbol Int   ->[ Int]
alturamas array =  [length(x)|x<-(ramas array)]
  
------------------------------------------Funcion auxiliar para convertir un arbol a un conjunto SEÑORA
ramas :: Arbol a -> [[a]]
ramas (Nodo x []) = [[x]]
ramas (Nodo x as) = [x : xs | a <- as, xs <- ramas a]

 

numeroVertices (Nodo raiz hijos) =  ((foldr (+) 1 (map numeroVertices hijos)))