> {-# LANGUAGE RankNTypes #-}

> module FinallyTagless.CPS2 where

> import Data.Map as Map
> import Control.Monad.Cont

Basic Idea

We still use search trees to represent stochastic expressions, but we no longer implement the 
stochastic language using the operations pv_unit and pv_bind in the search-tree monad. 
We use the continuation monad instead: we represent a stochastic expression of type ’a not as 
a tree of type ’a pV but in CPS, as a function from the continuation 
type ’a -> bool pV to the answer type bool pV. 

NOTE:  The SearchTree example is similar to the Tunring failure to list of success.  (I think!)
The change here is similar to what happens to the Applicative Parser when we got from list of
successes to the continuation.


  
Data types are the Same as the SearchTree example:

type 'a vc = V of 'a | C of (unit -> 'a pV)
and 'a pV = (prob * 'a vc) list

> type Probability = Rational
> data VC a = V a | C (() -> PV a) 
> newtype PV a = PV [(Probability, VC a)] deriving Show

> instance (Show a) => Show (VC a) where 
>       show (V a) = "(" ++ (show a) ++ ")"
>       show (C _) = "COMPUTATION"

> instance Monad PV where  
>       return a = PV [(1, V a)]
>       -- PV a -> (a -> PV b) -> PV B
>       PV m >>= f = PV (Prelude.map mapf m) where 
>                    mapf (p, V x) = (p, C (\_  ->  f x))
>                    mapf (p, C t) = (p, C (\_ -> (t () >>= f)))

> explore:: (Ord a) => Maybe Int -> PV a -> PV a
> explore (Just maxdepth)(PV choices)  = 
>       let  loop p depth down [] answers = answers
>            -- ans is a map of values and their probabilty (summed up)
>            loop p depth down ((pt, V v):rest) (ans, susp) =  
>               loop p depth down rest (Map.insertWith' (+) v  (p * pt) ans, susp)
>            -- if max depth is zero then ignore the depth, otherwise just go to the depth specified
>            loop p (Just depth)  down@(Just d) ((pt, C t):rest) answers = 
>               let (PV t_res) = t () 
>                   -- decide if the new computation should be explored
>                   down' = if (depth == 0 || depth < maxdepth) then Just True else Nothing  in 
>                   -- add the result of the current computation, with the result of rest
>                   loop p (Just depth) down rest 
>                       (loop (p*pt) (Just (depth+1)) down' t_res answers)
>            -- case when the computation should not be explored anymore (down = Nothing)
>            loop p depth down  ((pt, c):rest) (ans, susp) = 
>               loop p depth down rest (ans, ((p*pt), c) : susp)
>            (values,susp) = loop 1.0 (Just 0) (Just True) choices (Map.empty,[])
>            valuesFolded = Map.foldWithKey (\k x ks -> (x, V k):ks) [] values
>            in PV (valuesFolded ++ susp)




-----------------------------------------------------------
--  CPS Specific parts
-------------------------------------------------------------


module CPS = struct

   type ’a pm = (’a -> bool pV) -> bool pV
   type (’a,’b) arr = ’a -> (’b -> bool pV) -> bool pV
   let b x = fun k -> k x
   let dist ch = fun k ->
       List.map (function (p,v) -> (p, C (fun () -> k v))) ch
   let neg e = fun k -> e (fun v -> k (not v))
   let con e1 e2 = fun k -> e1 (fun v1 ->
                    if v1 then e2 k else b false k)
   let dis e1 e2 = fun k -> e1 (fun v1 ->
                    if v1 then (b true k) else e2 k)
   let if_ et e1 e2 = fun k -> et (fun t ->
                       if t then e1 () k else e2 () k)
   let lam e = fun k -> k (fun x -> e (fun k -> k x))
   let app e1 e2 = fun k -> e1 (fun f -> e2 (fun x -> f x k))
   let reify0 m = m pv_unit
  end
  
> type PM a = Cont (PV Bool) a -- (a -> PV Bool) -> PV Bool     -- PV is either a boolean value (V a) or Computation
>                                           -- C ( ...)  returning a boolean value
> type Arr a b = a -> (b -> PV Bool) -> PV Bool



> b :: Bool -> PM Bool
> b x = return x

> dist :: [(Probability, a)] -> PM a
> dist  ch = cont $ \k ->  PV $ Prelude.map (\(p, v) -> (p, C (\_ -> k v))) ch

> neg :: PM Bool -> PM Bool
> neg e =  e >>= (\x -> return (not x)) --   \k ->  e (\v -> k (not v))

> con :: PM Bool -> PM Bool -> PM Bool
> con e1 e2 = e1 >>= (\a -> if a then e2 else b False) --    \k -> e1 (\v1 -> if v1 then (e2 k) else (b False k)) 
> dis :: PM Bool -> PM Bool -> PM Bool
> dis e1 e2 =e1 >>= (\a -> if a then b True else e2)
> if_ :: PM Bool -> PM a  ->  PM a-> PM a  -- removed the ().  Correct?
> if_ et e1 e2 = et >>= ( \a -> if a then e1 else e2)
> lam :: (PM a -> PM b) -> PM (Arr a b) 
> lam e = undefined -- \k -> k (\x -> e (\k2 -> k2 x))
> app :: PM (Arr a b)   -> (PM a -> PM b)
> app e1 e2= undefined -- \k -> e1 (\f -> e2 (\x -> f x k))

> reify0 :: forall t a. ((a -> PV a) -> t) -> t
> reify0 m = m (\a -> PV [(1, V a)])

let flip p = dist [(p, true); (1.-.p, false)]
let let_ e f = app (lam f) e
let grass_model () =
let_ (flip 0.3) (fun rain ->
let_ (flip 0.5) (fun sprinkler ->
let_ (dis (con (flip 0.9) rain)
(dis (con (flip 0.8) sprinkler)
(flip 0.1))) (fun grass_is_wet ->
if_ grass_is_wet (fun () -> rain) (fun () -> dist []))))

> flp :: Probability -> PM Bool
> flp p = dist [(p, True), (1-p, False)]
> let_ :: forall a b. PM a -> (PM a -> PM b) -> PM b
> let_ e f = app (lam f) e

I removed the "()" for the as the grass model argument

> grass_model  =
>       let_ (flp 0.3) 
>             (\rain ->      -- "\rain -> .... " is a computation that returns PM a
>                let_ (flp 0.5)
>                      (\sprinkler ->
>                               let_ (dis (con (flp 0.9) rain)
>                                    (dis (con (flp 0.8) sprinkler)
>                                    (flp 0.1)))  -- logical expression of the PMs 
>                                       (\grass_is_wet ->
>                                           if_ grass_is_wet  rain  (dist []))))

> simpleModel :: PM Bool
> simpleModel = let_ (flp 0.3) f

notice that the function input is a PM Bool that is a list of (prob, V bool).  The list is then used
in >>= operation to map over values in the list.

> f :: PM Bool -> PM Bool
> f = (\x -> if_ x x (dist []))
> main = undefined -- explore (Just 1) $  reify0 grass_model 