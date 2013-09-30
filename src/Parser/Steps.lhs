> {-# LANGUAGE  RankNTypes, GADTs, MultiParamTypeClasses, FunctionalDependencies, TypeOperators, ScopedTypeVariables #-}
> module Parser.Steps where


Think of design in terms of Constructor, Combinators of single types, morphisems (composite 
computation) and observers.

Think of design as Expression Evaluation (or rewriteing) 

In case of parser note that the parsing grammer is Haskell code that is like if pattern then...
THe parse is about taking grammer and data and producing results
Maps/Fold...   Operate on a data stream without regard to the content.   Parser is about
working on the stream if the stream satisfy a syntax.

Exlain the role of Steps in going form parsing to 


Imports 

> import Control.Applicative
> import Debug.Trace
> import qualified Data.ListLike as LL

Steps are a data type that is used to encapsulate a computation process.   A query plan.

A parsing grammer, or a query needs to be "compile-d into steps

Apply is using forall a b because:

        it doesn't care about type and it doesn't want the type of function to appear in the data
        it allws for hetrogenous application of Apply.  (I guess otherwise a single Apply use in
        code would limit the a and b type.   --- VERIFY THIS 

> type Progress = Int
> type Strings  = [String]

> data  Steps   a  where
>      Step   :: Progress                 ->  Steps a    -> Steps   a
>      Apply  :: forall a b. (b -> a)     ->  Steps b    -> Steps   a
>      Fail   :: Strings                  -> Steps   a


`norm` makes sure that the head of the seqeunce contains progress information. 
It does so by pushing information about the result (i.e. the `Apply` steps) backwards.

> norm ::  Steps a ->  Steps   a
> norm = undefined

norm     (Apply _ f (Step   p    l  ))   =   Step  p (Apply "xx" f l)
norm     (Apply _ _ (Fail   ss    ))   =   Fail ss 
norm     (Apply _ f (Apply _  g    l  ))   =   norm (Apply "yy"  (f.g) l)
norm     steps                         =   steps


The function @best@  normalizes then compares two streams

> best :: Steps a -> Steps a -> Steps a
> x `best` y =   norm x `best'` norm y

> best' :: Steps   b -> Steps   b -> Steps   b
> best' = undefined

Fail  sl       `best'`  Fail  sr          =   Fail (sl ++ sr) 
Fail  _        `best'`  r                 =   r   -- <----------------------------- to be refined
l              `best'`  Fail  _           =   l
Step  n   l    `best'`  Step  m  r
    | n == m                              =   Step n (l  `best` r)     
    | n < m                               =   Step n (l  `best`  Step (m - n)  r)
    | n > m                               =   Step m (Step (n - m)  l  `best` r)
_                       `best'`  _       =   error "missing alternative in best'" 




We have two operations we perform on the stack: pushing a value, 
and popping two values, applying the one to the other and pushing the result back.
Pushing is taking a value and creating a pair

Note this function takes a "Steps a"  of parameter type (b->a, (b,r)) to Steps of parameter
(a,r)


> apply       :: Steps (b -> a, (b, r)) -> Steps (a, r)
> apply = undefined

apply       =  trace "apply function" Apply "aa" (\(b2a, (b, r)) -> (b2a b, r))   -- removed the let my doing a pattern matching on args

> push        :: v -> Steps   r -> Steps   (v, r)
> push = undefined

push v      =  trace "push function:" Apply "pp"  (\ r -> (v, r))

> apply2fst   :: (b -> a) -> Steps (b, r) -> Steps (a, r)
> apply2fst = undefined

apply2fst f = trace "apply to fst" Apply "pp2fst" (\ (b, r) -> (f b, r)) 

> noAlts :: Steps a
> noAlts      =  Fail []


Some experiments with Steps type


DO SOME STEPS EXAMPLES
Create a step and display it
then try normalizing it
compare two steps
try apply, push and apply2fst in the steps




Enabling Debuging Statments

> trace' :: String -> b -> b
> --trace' m v = {- trace m -}  v
> trace' m v = trace m  v 
        