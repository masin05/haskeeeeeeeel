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
 sEmpty :: s a
 push :: a -> s a -> s a
 top :: s a -> a
 pop :: s a -> s a
 sIsEmpty :: s a -> Bool

instance Stack [] where
 sEmpty = []
 push = (:)
 top = head
 pop = tail
 sIsEmpty = null

precedencia :: Char -> Int
precedencia '*' = 2
precedencia '/' = 2
precedencia '+' = 1
precedencia '-' = 1
precedencia _ = 0

shuntingYard :: String -> Arbol Token
shuntingYard xs = identificar (tokenizar xs)
    where
