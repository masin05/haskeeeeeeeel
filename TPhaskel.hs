import Data.Char

--TOKENIZACION

data Token = Num Float | Op Char deriving ( Show , Eq )

tokenizar :: String -> [Token]
tokenizar [] = []
tokenizar (x:xs)
    | isSpace x = tokenizar xs
    | isDigit x = let (x1, x2) = span isDigit (x:xs)
                  in Num (read x1) : tokenizar x2
    | otherwise = Op x : tokenizar xs

--CONSTRUCCIÒN DEL Arbol

data Arbol a = Vacio | Nodo a ( Arbol a ) ( Arbol a ) deriving ( Show , Eq )

class Stack s where
  empty :: s a
  push :: a -> s a -> s a
  top :: s a -> a
  pop :: s a -> s a
  isEmpty :: s a -> Bool

instance Stack [] where
 empty = []
 push = (:)
 top = head
 pop = tail
 isEmpty = null

precedencia :: Token -> Int
precedencia (Op '*') = 2
precedencia (Op '/') = 2
precedencia (Op '+') = 1
precedencia (Op '-') = 1
precedencia _ = 3

-- 1er lista, pila de operadores
-- 2da lista, pila de salida

esNumero :: Token -> Bool
esNumero (Num _) = True
esNumero _       = False


shuntingYard2 :: [Token] -> [Token] -> [Arbol Token] -> Arbol Token
shuntingYard2 [] [] [salida] = salida
shuntingYard2 [] (op:ops) (der:izq:resto) = shuntingYard2 [] ops (Nodo op izq der : resto)
shuntingYard2 (x:xs) ops salida | esNumero x = shuntingYard2 xs ops (Nodo x Vacio Vacio : salida) -- se crea una hoja
                                | x == (Op '(') = shuntingYard2 xs (x : ops) salida  -- se agrega el ( a la pila de ops
                                | x == (Op ')') = let (x1, x2) = span (\op -> op /= Op '(') ops -- cuando encuentra un ) comienza a poner operadores en salida hasta el ( y lo descarta
                                                      nuevosArboles = foldl (\(der:izq:resto) op -> Nodo op izq der : resto) salida x1
                                                  in shuntingYard2 xs (pop x2) nuevosArboles
                                | not (isEmpty ops) && precedencia (x) <= precedencia (top ops) = let (der : izq : resto) = salida 
                                                                                                  in shuntingYard2 (x:xs) (pop ops) (Nodo (top ops) izq der : resto)
                                | otherwise = shuntingYard2 xs (x:ops) salida -- si tiene mayor precedencia, se guarda en ops

shuntingYard :: String -> Arbol Token
shuntingYard xs = shuntingYard2 (tokenizar xs) [] [] -- las listas en blanco son para el shuntingYard2

--EJERCICIO 2--

-- 1ero identificador, 2do prioridad, 3ero expresion
data Job = J Int Int ( Arbol Token ) deriving ( Show , Eq )

instance Ord Job where
    (<=) :: Job -> Job -> Bool
    (J _ p1 _) <= (J _ p2 _) = p1 <= p2

data Heap a = Empty | Node a (Heap a) (Heap a)

minElement :: Heap a -> a
minElement Empty = undefined
minElement (Node x _ _) = x

size :: Heap a -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r

insert :: Ord a => a -> Heap a -> Heap a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
 | x < y && size l <= size r = Node x (insert y l) r
 | x < y && size l > size r = Node x l (insert y r)
 | x >= y && size l <= size r = Node y (insert x l) r
 | x >= y && size l > size r = Node y l (insert x r)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = Empty
deleteMin (Node _ Empty r) = r
deleteMin (Node _ l Empty) = l 
deleteMin (Node d (Node x ll lr) (Node y rl rr))
 | x <= y = Node x (deleteMin (Node d ll lr)) (Node y rl rr)
 | otherwise = Node y (Node x ll lr) (deleteMin (Node d rl rr))

class PriorityQueue pq where
 pqEmpty :: Ord a => pq a
 pqEnqueue :: Ord a => a -> pq a -> pq a
 pqFront :: Ord a => pq a -> a
 pqDequeue :: Ord a => pq a -> pq a
 pqIsEmpty :: Ord a => pq a -> Bool --averiguar si hace falta sacar el Ord a o no

instance PriorityQueue Heap where
 pqEmpty = Empty
 pqEnqueue = insert 
 pqFront = minElement
 pqDequeue = deleteMin
 pqIsEmpty Empty = True
 pqIsEmpty _ = False 

heapToList :: Heap a -> [a]
heapToList Empty = []
heapToList (Node x izq der) = x : (heapToList izq ++ heapToList der)

cambiarprioridad :: Int -> Int -> Heap Job -> Heap Job
cambiarprioridad idABuscar pNuevo heap = let listaJobs = heapToList heap
                                             listaModificada = map actualizar listaJobs
                                             actualizar (J id p arbol) 
                                              | id == idABuscar = (J id pNuevo arbol)
                                              | otherwise       = (J id p arbol)
                                         in foldr pqEnqueue Empty listaModificada

evaluarArbol :: Arbol Token -> Float
evaluarArbol (Nodo (Num x) Vacio Vacio) = x
evaluarArbol (Nodo (Op operador) izq der) | operador == '+' = evaluarArbol izq + evaluarArbol der
                                          | operador == '-' = evaluarArbol izq - evaluarArbol der
                                          | operador == '*' = evaluarArbol izq * evaluarArbol der
                                          | otherwise       = evaluarArbol izq / evaluarArbol der

procesarQueue :: ( PriorityQueue pq ) => pq Job -> [ Float ]
procesarQueue q | pqEmpty q = []
                | otherwise = let pqFront q = jobActual
                                  J _ _ arbol = jobActual
                                  resultado = evaluarArbol arbol
                                  cola = pqDequeue q
                              in resultado : procesarQueue cola
