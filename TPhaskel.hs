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

data Pila a = PTop a (Pila a) | PEmpty deriving (Show, Eq)

instance Stack Pila where
  empty :: Pila a
  empty = PEmpty

  push :: a -> Pila a -> Pila a
  push = PTop

  top :: Pila a -> a
  top (PTop x _) = x

  pop :: Pila a -> Pila a
  pop (PTop _ p) = p

  isEmpty :: Pila a -> Bool
  isEmpty PEmpty = True
  isEmpty _ = False

precedencia :: Char -> Int
precedencia '*' = 2
precedencia '/' = 2
precedencia '+' = 1
precedencia '-' = 1
precedencia _ = 0

-- 1er empty pila de operadores
-- 

indetificar :: [Token] -> Stack Pila -> [Token]
identificar (x:xs) = 

shuntingYard :: String -> Arbol Token
shuntingYard xs = identificar (tokenizar xs) empty empty
    where 
        identificar [] [] ArbolFinal = ArbolFinal

