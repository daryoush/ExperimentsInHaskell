{-# LANGUAGE  RankNTypes, GADTs, MultiParamTypeClasses, FunctionalDependencies, TypeOperators, 
ScopedTypeVariables #-}
module Parser.Polish where

infixl 3 <|>
infixl 4 <*>,<*,<$,<$>


data Steps a s  = Val a s
        | forall b. App (Steps (b -> a) (Steps b s))
        | Shift (Steps a s)
        | Done (Steps a s)
        | Fail
        
--instance Show a => Show (Steps a s) where
--        show (Val a s) = "Val " ++ show a
--        show (App  x) = "App " ++ "FUNCTION"
--        show (Shift s) = "Shift " ++ show s
--        show (Done s) = "Done " ++ show s
--        show (Fail) = "Fail"
--        
        
        
evalSteps :: Steps a s -> (a, s)
evalSteps (Val a s) = (a, s)
evalSteps (App s) = let (f , s1) = evalSteps s 
                        (a, s2) = evalSteps s1   
                    in (f a, s2)
                        
evalSteps (Shift v) = evalSteps v
evalSteps (Done v) = evalSteps v
evalSteps Fail = error "wrong input"


-- Instead of modeling the parser model the computations that would make up a paser.
-- As we have modeled the computation, we need to combine the computations into the 
-- parser computation the grammer. 
-- When the computation  is evaluated (in parse function) the parsing  (String transformation) 
-- happens.

newtype Par a = P (forall w. HasProgress w => (String -> w) -> String -> Steps a w)
unP (P p) = p

parse :: forall t. Par t -> String -> (t, String)



pSucceed :: a -> Par a
pFail :: Par a
pSucceed a = P (Val a.)
pFail = P (\_ _ -> Fail)
P p <*> P q = P ((App.) . p . q)
P p <|> P q = P (\k input -> p k input `best` (q k input))

f <$> q = pSucceed f <*> q
p <* q = const <$> p <*> q
f <$ q = const f <$> q

pReturn :: forall a. a -> Par a
pReturn v = pSucceed v

opt :: Par  a -> a -> Par a
p `opt` v = p <|> pReturn v

pMany :: forall t. Par t -> Par [t]
pMany p = (:) <$> p <*> (pMany p <|> pEnd)   -- (pSucceed [])   --`opt` [ ]

-- by putting a 'best' function between two steps we either get the best of the two as a step or
-- we call getprogress to explore each of the steps by one step and then compare them
-- so if we do    s1 'best' s2 'best' s3 'best' s4     as we would do in a OR composition
-- when we do evalstep the step is    s1 best (s2 best (s3 best s4))
-- now the get progress either drills down to a done, fail or shift.  

best :: forall t t1.
                   HasProgress t1 =>
                   Steps t t1 -> Steps t t1 -> Steps t t1
best (Fail)  p = p
best q  Fail = q
best (Done _)  (Done _) = error "ambiguous grammar"
best (Done a)  q = Done a
best p  (Done a) = Done a
best (Shift v)  (Shift w) = Shift (v `best` w)
best p q = getProgress id p  `best`  getProgress id q

-- WONT COMPILE getProgress f (Val a s) =  getProgress (f . Val a ) s
-- WONT COMPILEgetProgress f (App s) =  getProgress (f . App ) s
--getProgress f (Done p) = Done (f p)
--getProgress f (Shift s) = Shift (f s)
--getProgress f (Fail ) = Fail


class HasProgress st where
        getProgress :: (st -> Steps x y) -> st -> Steps x y

instance HasProgress s => HasProgress (Steps a s) where
        getProgress f (Val a s) =  getProgress (f . Val a ) s
        getProgress f (App s) =  getProgress (f . App ) s
        getProgress f (Done p) = Done (f p)
        getProgress f (Shift s) = Shift (f s)
        getProgress f (Fail ) = Fail

-- I guess this is the base class instance, so if there are no specific instances, the 
-- implementation here would be used.
instance HasProgress () where
        getProgress f () = Fail
        
pSym :: Char -> Par Char
pSym a = P (\k input ->  case input of
         (s : ss) -> if a == s
                then (Shift . Val s . k) ss
                else Fail
         [ ] -> Fail)
        
parse p input = let (result, rest) =  justEval p input
                    in (result, fst . evalSteps $ rest)

justEval :: forall a. Par a -> String -> (a, Steps String ())
justEval p  input = evalSteps (justRunp p input )

justRunp :: forall t. Par t -> String -> Steps t (Steps String ())
justRunp (P p) input = p (\rest -> Done (Val rest ()))
                                          input                                       
pPlus :: Par Integer
pPlus = (+) <$> pInt <* pSym '+' <*> pInt
pInt = 0 <$ pSym '0' <|> (1 <$ pSym '1') <|> (2 <$ pSym '2')
         <|> (3 <$ pSym '3') <|> (4 <$ pSym '4') <|> (5 <$ pSym '5')
         <|> (6 <$ pSym '6')
        <|> (7 <$ pSym '7')
        <|> (8 <$ pSym '8')
        <|> (9 <$ pSym '9')

pEnd :: forall a. Par [a]
pEnd = pSucceed []


pSum = id <$> ((+) <$> pPlus <* pSym '+' <*> pPlus )<|> pPlus 

-- Parser.Polish> main3
--App FUNCTION
-- Parser.Polish> main2
--(3,Done Val "")
--Parser.Polish> main1
--(3,"+3")
-- Parser.Polish> main
--(3,"")
main = parse pPlus "1+2"
main1 = parse pPlus "1+2+3"
main2 = justEval pPlus "1+2"
main3 = justRunp pPlus "1+2"
main4 = parse pSum "1+2"
main5 = parse pSum "1+2+3"



-- | Our first two parsers are simple; one recognises a single 'a' character and the other one a single 'b'. Since we will use them later we 
--   convert the recognised character into `String` so they can be easily combined.
pa  ::Par String 
pa  = lift <$> pSym 'a'
pb  :: Par String 
pb = lift <$> pSym 'b'
pc  :: Par String 
pc = lift <$> pSym 'c'
lift a = [a]

(<++>) :: Par String -> Par String -> Par String
p <++> q = (++) <$> p <*> q
pa2 =   pa <++> pa
pa3 =   pa <++> pa2
pab = pa <++> pb

pax = pMany pa
