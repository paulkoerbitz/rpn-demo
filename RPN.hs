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

main :: IO ()
main = putStrLn $ show $ parseRpnProg "1 2 + 3 4 + +"
