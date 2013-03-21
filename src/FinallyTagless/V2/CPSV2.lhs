> {-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}

> module FinallyTagless.V2.CPSV2 where

> import  FinallyTagless.V2.Base
> import Data.Map as Map
> import FinallyTagless.V2.PV
> import FinallyTagless.V2.ProbExplore
> import FinallyTagless.V2.ProbLanguage

Basic Idea

We still use search trees to represent stochastic expressions, but we no longer implement the 
stochastic language using the operations pv_unit and pv_bind in the search-tree monad. 
We use the continuation monad instead: we represent a stochastic expression of type ’a not as 
a tree of type ’a pV but in CPS, as a function from the continuation 
type ’a -> bool pV to the answer type bool pV. 

NOTE:  The SearchTree example is similar to the Tunring failure to list of success.  (I think!)
The change here is similar to whatappens to the Applicative Parser when we got from list of
successes to the continuation.

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
  
> newtype PM a = PM ((a -> PV Bool) -> PV Bool )    -- PV is either a boolean value (V a) or Computation
> 
> --newtype Arr a b = Arr(a -> (b -> PV Bool) -> PV Bool)

> fff x = PM (\k -> k x)

> {-|
> instance ProbLangauge PM  where
>       b x = PM (\k -> k x)
>       dist  ch = \k ->  PV $ Prelude.map (\(p, v) -> (p, C (\_ -> k v))) ch
>       neg e = \k ->  e (\v -> k (not v))
>       con e1 e2 = \k -> e1 (\v1 -> if v1 then (e2 k) else (b False k)) 
>       dis e1 e2 = \k -> e1 (\v1 -> if v1 then (b True k) else (e2 k)) 
>       if_ et e1 e2 = \k -> et (\t -> 
>               if t then (e1 k) else (e2 k))
>       lam e = \k -> k (\x -> e (\k2 -> k2 x))
>       app e1 e2= \k -> e1 (\f -> e2 (\x -> f x k))
> -}

> {-|
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
> main = explore (Just 1) $  reify0 grass_model 
> -}