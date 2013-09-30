{-# LANGUAGE  RankNTypes #-}

module Parser.PolishListOfSuccess where

infixl 3 <|>
infixl 4 <*>,<*,<$,<$>

type Parser a = String -> [(a, String)]
(<|>) :: Parser a -> Parser a -> Parser a
(<*>) :: Parser (b -> a) -> Parser b -> Parser a
pSucceed :: a -> Parser a
--pFail :: Parser a
pSym :: Char -> Parser Char
pSucceed v inp = [(v, inp)]
--pFail = []

(p <|> q) inp = p inp ++ q inp
(p <*> q) inp = [(f a, rr) | (f , r) <- p inp, (a, rr) <- q r]

pSym a inp = case inp of
                 (s:ss) ->  if a == s 
                     then [(s, ss)]
                     else []
                 [] -> []
                
                
f <$> q = pSucceed f <*> q
p <* q = const <$> p <*> q
f <$ q = const f <$> q


pPlus = (+) <$> pInt <* pSym '+' <*> pInt
pInt = 0 <$ pSym '0' <|> (1 <$ pSym '1') <|> (2 <$ pSym '2')
         <|> (3 <$ pSym '3') <|> (4 <$ pSym '4') <|> (5 <$ pSym '5')
         <|> (6 <$ pSym '6')
        <|> (7 <$ pSym '7')
        <|> (8 <$ pSym '8')
        <|> (9 <$ pSym '9')


--Parser.PolishListOfSuccess> main
--[(3,"")]
--Parser.PolishListOfSuccess> Ok, modules loaded: Parser.PolishListOfSuccess.
--Parser.PolishListOfSuccess> pPlus "1+"
--[]
--Parser.PolishListOfSuccess> pPlus "1+2+3"
--[(3,"+3")]

main =  pPlus "1+2"

