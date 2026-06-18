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


shuntingYard :: [Token] -> [Token] -> [Token] -> Arbol Token
shuntingYard [] [] salida = salida
shuntingYard [] (op:ops) salida = shuntingYard [] ops (salida ++ [op])
shuntingYard (x:xs) ops salida | esNumero x = shuntingYard xs ops (salida ++ [x])
                              | x == (Op '(') = shuntingYard xs (x : ops) salida
                              | x == (Op ')') = let (x1, x2) = span (!='(') ops
                                                in  shuntingYard (x:xs) (pop x2) (salida ++ x1)
                              | precedencia (x) <= precedencia (top ops) = shuntingYard xs (x : pop ops) (salida ++ [top ops])
                              | otherwise = shuntingYard xs (x : ops) salida
