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


shuntingYard2 :: [Token] -> [Token] -> [Token] -> [Token]
shuntingYard2 [] [] salida = salida
shuntingYard2 [] (op:ops) salida = shuntingYard2 [] ops (salida ++ [op])
shuntingYard2 (x:xs) ops salida | esNumero x = shuntingYard2 xs ops (salida ++ [x])
                              | x == (Op '(') = shuntingYard2 xs (x : ops) salida
                              | x == (Op ')') = let (x1, x2) = span (\op -> op /= Op '(') ops
                                                in  shuntingYard2 xs (pop x2) (salida ++ x1)
                              | not (isEmpty ops) && precedencia (x) <= precedencia (top ops) = shuntingYard2 (x:xs) (pop ops) (salida ++ [top ops])
                              | otherwise = shuntingYard2 xs (x : ops) salida

shuntingYard :: String -> Arbol Token
shuntingYard xs = shuntingYard2 (tokenizar xs)
