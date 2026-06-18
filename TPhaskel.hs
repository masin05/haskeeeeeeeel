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
precedencia Op '*' = 2
precedencia Op '/' = 2
precedencia Op '+' = 1
precedencia Op '-' = 1
precedencia _ = 0

-- 1er empty pila de operadores
-- 

indetificar :: [Token] -> Stack Token -> [Token] -> [Token]
identificar [] [] salida = salida
identificar [] ops salida = identificar [] ops salida
identificar (x:xs) ops salida | 
                              | 
                              | 
                              | otherwise =

shuntingYard :: String -> Arbol Token
shuntingYard xs = identificar (tokenizar xs) empty empty
    where 
        identificar [] [] ArbolFinal = ArbolFinal

