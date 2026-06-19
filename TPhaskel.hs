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
precedencia _ = 0

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

class Queue q where
 qEmpty :: q a
 qEnqueue :: a -> q a -> q a
 qFront :: q a -> a
 qDequeue :: q a -> q a
 qIsEmpty :: q a -> Bool

instance Queue [] where
 qEmpty = []
 qEnqueue = (:)
 qFront = last
 qDequeue = init
 qIsEmpty = null

class PriorityQueue pq where
 pqEmpty :: Ord a => pq a
 pqEnqueue :: Ord a => a -> pq a -> pq a
 pqFront :: Ord a => pq a -> a
 pqDequeue :: Ord a => pq a -> pq a
 pqIsEmpty :: Ord a => pq a -> Bool

data Heap a = Empty | Node a (Heap a) (Heap a)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h2 = h2
merge h1 Empty = h1
merge (Node x1 izq1 der1) (Node x2 izq2 der2)
    | x1 <= x2  = Node x1 (merge der1 (Node x2 izq2 der2)) izq1  --dando vuelta der1  e izq 1Lo que estás haciendo es dar vuelta el árbol
    | otherwise = Node x2 (merge der2 (Node x1 izq1 der1)) izq2  --en cada paso de la recursión por lo que se balancea

instance PriorityQueue Heap where
 pqEmpty = Empty
 pqEnqueue = 
