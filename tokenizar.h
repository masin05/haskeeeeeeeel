import Data.Char

--TOKENIZACIÒN

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

precedencia :: Char -> Int
precedencia '*' = 2
precedencia '/' = 2
precedencia '+' = 1
precedencia '-' = 1
precedencia _ = 0

class Queue q where
 empty :: q a
 enqueue :: a → q a → q a
 front :: q a → a
 dequeue :: q a → q a
 isEmpty :: q a → Bool

instance Queue [] where
2 empty = []
3 enqueue = (:)
4 front = last
5 dequeue = init
6 isEmpty = null

shuntingYard :: String -> Arbol Token
shuntingYard xs = algoritmo 



where 
algoritmo = tokenizar xs
