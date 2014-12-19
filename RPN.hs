module Main where

data Operation = Plus | Minus | Mult | Div
               deriving (Eq, Show)

data RPNElem = Num Int
             | Op Operation
             deriving (Eq, Show)

type RPNProg = [RPNElem]

parseRpnProg :: String -> Maybe RPNProg
parseRpnProg s = go [] s
  where
    go stk ('+':xs) = go (Op Plus  : stk) xs
    go stk ('-':xs) = go (Op Minus : stk) xs
    go stk ('*':xs) = go (Op Mult  : stk) xs
    go stk ('/':xs) = go (Op Div   : stk) xs
    go stk (' ':xs) = go stk xs
    go stk (x:xs)   = case reads (x:xs) of
                           [(int,rest)] -> go (Num int : stk) rest
                           _            -> Nothing
    go stk []       = Just $ reverse stk

evalRpnProg :: RPNProg -> Maybe Int
evalRpnProg p = go [] p
  where
    go stk       (Num i   :ps) = go (i:stk)   ps
    go (i:j:stk) (Op Plus :ps) = go ((i+j):stk) ps
    go (i:j:stk) (Op Minus:ps) = go ((i-j):stk) ps
    go (i:j:stk) (Op Mult :ps) = go ((i*j):stk) ps
    go (i:j:stk) (Op Div  :ps) = go ((i `div` j):stk) ps
    go _         ((Op _)  :_)  = Nothing
    go (s:_)   []              = Just s
    go []        []            = Nothing


main :: IO ()
main = putStrLn $ show $ parseRpnProg "1 2 + 3 4 + +"
