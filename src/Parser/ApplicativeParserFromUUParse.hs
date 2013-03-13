{-# LANGUAGE RankNTypes, GADTs, MultiParamTypeClasses, FunctionalDependencies, TypeOperators, ScopedTypeVariables #-}
module Parser.ApplicativeParserFromUUParser where

import Control.Applicative
import Debug.Trace
import qualified Data.ListLike as LL


trace' :: String -> b -> b
--trace' m v = {- trace m -}  v
trace' m v = trace m  v  

-- | The data type `Steps` is the core data type around which the parsers are constructed.
--   It describes a tree structure of streams containing (in an interleaved way) both the online result of 
--the parsing process,
--   and progress information. Recognising an input token should correspond to a certain amount of @`Progress`@, 
--   which tells how much of the input state was consumed. 
--   The @`Progress`@ is used to implement the breadth-first search process, in which alternatives are
--   examined in a more-or-less synchronised way. The meaning of the various @`Step`@ constructors is as follows:
--
--   [`Step`] A token was succesfully recognised, and as a result the input was 'advanced' by the distance 
--  @`Progress`@
--
--   [`Apply`] The type of value represented by the `Steps` changes by applying the function parameter.
--
--   [`Fail`] A correcting step has to be made to the input; the first parameter contains information about what
-- was expected in the input, 
--   and the second parameter describes the various corrected alternatives, each with an associated `Cost`

-- The original implementation used List-Of-Successes using the list computation as a means to compose computation.
-- In the new one we are using continuation function (first argument of the PF type_  to compose the computation. 
-- The resulting step is step over what ever future computation returns   Step ( FUTURE STEP).   Outer step is from
-- the the top level fuction, the inner steps are from the future.  At the end of the 
-- future steps is either a Fail or 
--  QUESTION:   What is end of the final success?  undefined?

type Progress = Int
type Strings  = [String]
type Cost     = Int

data  Steps   a  where
      Step   ::                 Progress       ->  Steps a                             -> Steps   a
      Apply  ::  forall a b.    (b -> a)       ->  Steps   b                           -> Steps   a
      Fail   ::                 Strings                                                -> Steps   a



-- | `norm` makes sure that the head of the seqeunce contains progress information. 
--   It does so by pushing information about the result (i.e. the `Apply` steps) backwards.
--
norm ::  Steps a ->  Steps   a
norm     (Apply f (Step   p    l  ))   =   Step  p (Apply f l)
norm     (Apply _ (Fail   ss    ))   =   Fail ss 
norm     (Apply f (Apply  g    l  ))   =   norm (Apply (f.g) l)
norm     steps                         =   steps


-- | The function @best@ compares two streams
best :: Steps a -> Steps a -> Steps a
x `best` y =   norm x `best'` norm y

best' :: Steps   b -> Steps   b -> Steps   b
Fail  sl       `best'`  Fail  sr          =   Fail (sl ++ sr) 
Fail  _        `best'`  r                 =   r   -- <----------------------------- to be refined
l              `best'`  Fail  _           =   l
Step  n   l    `best'`  Step  m  r
    | n == m                              =   Step n (l  `best` r)     
    | n < m                               =   Step n (l  `best`  Step (m - n)  r)
    | n > m                               =   Step m (Step (n - m)  l  `best` r)
_                       `best'`  _       =   error "missing alternative in best'" 




-- We have two operations we perform on the stack: pushing a value, 
--and popping two values, applying the one to the other and pushing the result back.
-- Pushing i taking a value and creating a pair

-- REALLY INTERESTING THAT APPLY WOULD ONLY WORK WHEN TOP OF THE STACK IS FUNCTION!
-- TYPE SIGNATURE SAYS THE 'a' in Steps a is a pair (b->a, (b,r))   
-- SO APPLY ONLY WORKS WHEN THE COMPILER CAN GURANTEE THE the two top memebers of the stack

apply       :: Steps (b -> a, (b, r)) -> Steps (a, r)
apply       =  Apply (\(b2a, (b, r)) -> (b2a b, r))   -- removed the let my doing a pattern matching on args

push        :: v -> Steps   r -> Steps   (v, r)
push v      =  Apply (\ r -> (v, r))

apply2fst   :: (b -> a) -> Steps (b, r) -> Steps (a, r)
apply2fst f = Apply (\ (b, r) -> (f b, r)) 

noAlts :: Steps a
noAlts      =  Fail []

-- PF is polymorphic in st and a types.  st for state and a for witness
-- The first argument is the continuation.  An instance of this type must  take step, figure out
-- the evidence (a), then pass the step to the continuation and then do post processing
-- combining the resut of the continuation with eveidence for its own result.
newtype PF st a = PF (forall r . (st -> Steps r)  -> st -> Steps(a, r) )  --   future parser


-- | The function @`parse`@ shows the prototypical way of running a parser on
-- some specific input.
-- By default we use the future parser, since this gives us access to partal
-- result; future parsers are expected to run in less space.
parse :: (Eof t) => PF t a -> t -> a
parse  (PF pf)  = fst . eval . pf  (\ rest -> if eof rest 
                                              then  Step 0 $ Step 0 $ Step 0 $ Step 0 $ Step 0 $ error "ambiguous parser?"
                                              else error "pEnd missing?")



-- | @`eval`@ removes the progress information from a sequence of steps, and constructs the value embedded in it.
--   If you are really desparate to see how your parsers are making progress (e.g. when you have written an ambiguous 
-- parser, and you cannot find the cause of
--   the exponential blow-up of your parsing process), you may switch on the trace in the 
--function @`eval`@ (you will need to edit the library source code).
-- 
eval :: Steps   a      ->  a
eval (Step  n    l)     =   trace' ("Step " ++ show n ++ "\n") (eval l)
eval (Fail   ss   )     =   trace' ("expecting: " ++ show ss)  undefined  -- what to do here?
eval (Apply  f   l)     =   f (eval l)



traverse :: Int -> Steps a -> Int -> Int  -> Int 
traverse 0  _            v c  =  trace' ("traverse " ++ show' 0 v c ++ " choosing" ++ show v ++ "\n") v
traverse n (Step _   l)  v c  =  trace' ("traverse Step   " ++ show' n v c ++ "\n") (traverse (n -  1 ) l (v - n) c)
traverse n (Apply _  l)  v c  =  trace' ("traverse Apply  " ++ show n ++ "\n")      (traverse n         l  v      c)
traverse n (Fail m ) v c  =  trace' ("traverse Fail   " ++ show m ++ show' n v c ++ "\n") 
                                 (foldr (\ (w,l) c' -> if v + w < c' then traverse (n -  1 ) l (v+w) c'
                                                       else c') c (map ($m) undefined)  -- ??????
                                 )

show' :: (Show a, Show b, Show c) => a -> b -> c -> String
show' n v c = "n: " ++ show n ++ " v: " ++ show v ++ " c: " ++ show c


-- HOW TO MAKE SENSE OF THE FMap code with the "."
--  apply2fst f :: Steps (b, r) -> Steps (a,r)
--   p k :: st -> Steps (a, r)
--    \  k  ->  apply2fst f . p k  ====    \  k  ->  (apply2fst f ). (p k)  ==== ( \  k s ->  apply2fst f (p k s))
-- because (.) :: (b -> c) -> (a -> b) -> a -> c        -- Defined in `GHC.Base'
--          infixr 9 . 
-- with . you get a function that needs the "s" to be passed to the 2nd arg first then
-- result combined.    since we don't have the s in the lambda we would need the "." to bring the s in the  p k.
-- A fixity declaration can be given for any infix operator or constructor (including those made from ordinary identifiers, 
--such as `elem`). This declaration specifies a precedence level from 0 to 9 (with 9 being the strongest; 
-- normal application is assumed to have a precedence level of 10), and left-, right-, or non-associativity.
-- See http://en.wikipedia.org/wiki/Operator_associativity for more on operator fixity
-- NOTE We have infixr, infixl, infix (NON_ASSOCIATIVE)
instance Functor (PF st) where
  fmap f (PF p) = PF  ( \  k  ->  apply2fst f . p k ) -- pure f <*> pf
                           

-- p.q   
-- (<*>) :: f (a -> b) -> f a -> f b
-- p :: (st -> Steps X) -> st -> Steps (a -> b, X)
-- q :: (st -> Steps r) -> st -> Steps (a, r)

-- notie that functions like (XXXX .) means thar the result takes a function and value
-- (a->b) -> a -> b.    That is like the PF type where the first argument is functiin and
-- 2nd is the state.


-- in p.q you have to look at p and q as a -> b type function
-- a is first argument b is the resulting function that takes state
--  p::   (st -> Steps X) -> (st -> Steps (a->b, X))
--              b         ->     c
--  q::  (st -> Steps x) ->  (st -> Steps (a, X))
--              a         ->      b

--   So in p.q    p's "x"is (a,X) of that is returned from q

justCompose :: forall t t1 t2 r.
                          PF t t1 -> PF t t2 -> (t -> Steps r) -> t -> Steps (t1, (t2, r))
justCompose (PF p) (PF q) = p.q

justApply :: forall a b a1 r.
                        (a -> Steps (b -> a1, (b, r))) -> a -> Steps (a1, r)
justApply = (apply .)

justApplyApplyToCompose :: forall a b a1 r.
                                      PF a (b -> a1)
                                      -> PF a b -> (a -> Steps r) -> a -> Steps (a1, r)
justApplyApplyToCompose  p q = justApply . justCompose p q

instance   Applicative (PF state) where
  PF p  <*> ~(PF q)   =  PF ((apply .) . (p .q)) 
  pure a              = PF ((push a).) 


instance   Alternative (PF  state) where 
  PF p  <|> PF q =  PF  (\  k inp  -> p k inp `best` q k inp)
  empty                =  PF ( \  k inp  ->  noAlts) 



 -- | The class `Eof` contains a function `eof` which is used to check whether we have reached the 
 -- end of the input and `deletAtEnd` 
--   should discard any unconsumed input at the end of a successful parse.
class Eof state where
       eof          ::  state   -> Bool
       deleteAtEnd  ::  state   -> Maybe (Cost, state)

-- | The input state may maintain a location which can be used in generating error messages. 
--   Since we do not want to fix our input to be just a @String@ we provide an interface
--   which can be used to advance this location by passing  information about the part recognised. 
-- This function is typically
--   called in the `splitState` functions.

class Show loc => loc `IsLocationUpdatedBy` str where
    advance :: loc -- ^ The current position
            -> str -- ^ The part which has been removed from the input
            -> loc

-- | The class `StoresErrors` is used by the function `pErrors` which retrieves the generated 
--  correction steps since the last time it was called.
--

-- NOTE:  THe state -> error means that for this class there can't be two different implementation
-- of a given state type to different error types.  So compiler wont need both types to figure out
-- which instance to use.

class state `StoresErrors`  error | state -> error where
  -- | `getErrors` retrieves the correcting steps made since the last time the function was called. The result can, 
  --    by using it in a monad, be used to control how to proceed with the parsing process.
  getErrors :: state -> ([error], state)


-- NOTE:  THe state -> pos means that for this class there can't be two different implementation
-- of a given state type to different pos types.  So compiler wont need both types to figure out
-- which instance to use.

class state `HasPosition`  pos | state -> pos where
  -- | `getPos` retrieves the correcting steps made since the last time the function was called. The result can, 
  --   by using it as the left hand side of a monadic bind, be used to control how to proceed with the parsing process.
  getPos  ::  state -> pos
  
  
  -- ** The type @`Nat`@ for describing the minimal number of tokens consumed
-- | The data type @`Nat`@ is used to represent the minimal length of a parser.
--   Care should be taken in order to not evaluate the right hand side of the binary function @`nat-add`@ more 
-- than necesssary.

data Nat = Zero  Nat -- the length of the non-zero part of the parser is remembered)
         | Succ Nat
         | Infinite
         | Unspecified
         | Hole
         deriving  Show
  
  
  

                          
-----------------------------------------------------
---
---        DERIVED INSTANCES
-----
----
----
-----------------------------------------------------


-- *  `Error`
-- |The data type `Error` describes the various kinds of errors which can be generated by the instances in this module
data Error  pos =    Inserted String pos        Strings  
                     -- ^  @String@ was inserted at @pos@-ition, where we expected  @Strings@
                   | Deleted  String pos        Strings
                     -- ^  @String@ was deleted at @pos@-ition, where we expected  @Strings@
                   | Replaced String String pos Strings
                     -- ^ for future use
                   | DeletedAtEnd String
                     -- ^ the unconsumed part of the input was deleted

instance (Show pos) => Show (Error  pos) where 
 show (Inserted s pos expecting)       = "--    Inserted  " ++  s ++  show_expecting  pos expecting 
 show (Deleted  t pos expecting)       = "--    Deleted   " ++  t ++  show_expecting  pos expecting
 show (Replaced old new pos expecting) = "--    Replaced  " ++ old ++ " by "++ new ++  show_expecting  pos expecting
 show (DeletedAtEnd t)                 = "--    The token " ++ t ++ " was not consumed by the parsing process."



show_expecting :: Show pos => pos -> [String] -> String
show_expecting pos [a]    = " at position " ++ show pos ++ " expecting " ++ a
show_expecting pos (a:as) = " at position " ++ show pos ++ 
                            " expecting one of [" ++ a ++ concat (map (", " ++) as) ++ "]"
show_expecting pos []     = " expecting nothing"



-- * The Stream data type
-- | The data type `Str` holds the input data to be parsed, the current location, the error messages generated 
--   and whether it is ok to delete elements from the input. Since an insert/delete action is 
--   the same as a delete/insert action we try to avoid the first one. 
--   So: no deletes after an insert.

data Str a s loc = Str { -- | the unconsumed part of the input
                         input    :: s,             
                         -- | the accumulated error messages
                         msgs     :: [Error loc],
                         -- | the current input position  
                         pos      :: loc,           
                         -- | we want to avoid deletions after insertions
                         deleteOk :: !Bool         
                       }




  
  -- |  MOFIED____ REMOVED TWO UNUSED ARGS
  -- FUNCTION THAT DOES THE SPLIT STATE IS PASSED TO PSymExt.  This function lifts the function to PF type
  -- QUESTION DO I NEED FOR ALL?
-- PF (forall r . (st -> Steps r)  -> st -> Steps(a, r) )  --   future parserne
-- k :: st -> Steps r
-- t :: (token -> state -> Steps a) 
-- What its trying to do
--pSyma=Pf (λk st → case splitState a st of
--                  Just (t , ss ) → if a ‘eqSymTok‘ t
--                         then Step (push t (k ss))
--                         else Fail 
--                  Nothing → Fail
--         )

-- splitstate takes the state, gets its token then calls its first first argument with the token
-- and new state to build the step 
-- pSymExt lifts the split state to PF by taken the step and constructing the PF type 
pSymExt ::  (forall a. (token -> state  -> Steps a) -> state -> Steps a)  -> PF state token
pSymExt splitState   = PF  ( \ k -> splitState  (\ t -> push t . k)  )
   
   

            
------ | `pSatisfy`  describes and elementary parsing step. Its first parameter check whether the head element of 
-- the input can be recognised, 
------    and the second parameter how to proceed in case an element recognised by this parser is absent, 
------    and parsing may proceed by pretending such an element was present in the input anayway.
-- NOTE that this method require ScopedTypeVariables flags to be turned on, otherwise it wont compile
pSatisfy :: forall loc state a .((Show a,  loc `IsLocationUpdatedBy` a, 
                        LL.ListLike state a) => (a -> Bool) -> PF (Str  a state loc) a)
pSatisfy p   = pSymExt splitState 
-- NOTE I REMOVED THE TWO UNUSED ARGS,   DEFINE how to split the state, and let pSymExt lift it into PF type
  where   splitState :: forall r. (Show a,  loc `IsLocationUpdatedBy` a, LL.ListLike state a) => 
                        ((a ->  (Str  a state loc)  -> Steps r) ->  (Str  a state loc) -> Steps r)
          splitState  k (Str  tts   messages position _)     --- OK to del can be removed
            = show_attempt ("Try Predicate: TBD.... at position " ++ show position ++ "\n") (
                 if   LL.null tts 
                 then Fail ["failed!!!"] 
                 else let t       = LL.head tts
                          ts      = LL.tail tts
                      in if p t
                         then  show_symbol ("Accepting symbol: " ++ show t ++ " at position: " ++ show position ++"\n") 
                              (Step 1 (k t (Str ts messages (advance position t) True)))
                        else  Fail ["failed!!!"]
              )  

       
{-# INLINE show_attempt #-}
show_attempt :: forall t t1. t -> t1 -> t1
show_attempt m v =  {- trace m -}  v
     
{-# INLINE show_symbol #-}
show_symbol :: String -> b -> b
show_symbol m v =   {- trace m -}  v
-- show_symbol m v =     trace m   v          

-- | `pSym` recognises a specific element. Furthermore it can be specified what element.
--  Information about `Insertion` is derived from the parameter.
--   is to be inserted in case such an element is not at the head of the input.
pSym ::   (Eq a,Show a, IsLocationUpdatedBy loc a, LL.ListLike state a) => a ->  PF (Str a state loc) a    
pSym  t =  pSatisfy (==t) 



instance (Show a, LL.ListLike s a) => Eof (Str a s loc) where
       eof (Str  i        _    _    _    )              = LL.null i
       deleteAtEnd (Str s msgs pos ok )     | LL.null s = Nothing
                                            | otherwise = Just (5, Str (LL.tail s) 
                                                 (msgs ++ [DeletedAtEnd (show (LL.head s))]) pos ok)


--- Demo support types and functions

data LineColPos = LineColPos !Int !Int !Int  deriving Show
instance IsLocationUpdatedBy LineColPos Char where
   advance (LineColPos line pos abs) c = case c of
                               '\n' ->  LineColPos (line+1) 0                           (abs + 1) 
                               '\t' ->  LineColPos line     (pos + 8 - (pos-1) `mod` 8) (abs + 1)
                               _    ->  LineColPos line     (pos + 1)                   (abs + 1)



instance  StoresErrors (Str a s loc) (Error loc) where
       getErrors   (Str  inp  msgs pos ok    )     = (msgs, Str inp [] pos ok)


-- |  `createStr` initialises the input stream with the input data and the initial position. 
--There are no error messages yet.
createStr :: LL.ListLike s a => loc -> s -> Str a s loc
createStr beginpos ls = Str ls [] beginpos True

------------------------------
---   DEMO
-----------------------------

type Parser a = PF (Str Char String LineColPos) a

-- | The fuction @`run`@ runs the parser and shows both the result, and the correcting steps which 
-- were taken during the parsing process.
run :: Show t =>  Parser t -> String -> IO ()
run p inp = do  let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                putStrLn ("--  Result: " ++ show a)
                if null errors then  return ()
                               else  do putStr ("--  Correcting steps: \n")
                                        show_errors errors
                putStrLn "-- "
             where show_errors :: (Show a) => [a] -> IO ()
                   show_errors = sequence_ . (map (putStrLn . show))


-- | The function `pEnd` should be called at the end of the parsing process. It deletes any unconsumed input, 
-- turning it into error messages.

pEnd    :: (StoresErrors st error, Eof st) => PF st [error]
pEnd    = PF (  \ k   inp -> let deleterest inp =  case deleteAtEnd inp of
                                                  Nothing -> let (finalerrors, finalstate) = getErrors inp
                                                             in push finalerrors (k finalstate)
                                                  Just (i, inp') -> Fail ["fail in the end"] 
                                            in deleterest inp)



-- | Our first two parsers are simple; one recognises a single 'a' character and the other one a single 'b'. 
-- Since we will use them later we 
--   convert the recognised character into `String` so they can be easily combined.
pa  ::Parser String 
pa  = lift <$> pSym 'a'

pb  :: Parser String 
pb = lift <$> pSym 'b'
pc  :: Parser String 
pc = lift <$> pSym 'c'


(<++>) :: Parser String -> Parser String -> Parser String
p <++> q = (++) <$> p <*> q
pa2 :: Parser String
pa2 =   pa <++> pa
pa3 :: Parser String
pa3 =   pa <++> pa2
lift :: forall t. t -> [t]
lift a = [a]


main::IO()
main = run pa3 "aa"
