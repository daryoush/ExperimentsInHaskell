> {-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

> module FinallyTagless.V2.SearchTreeV2 where

> import Control.Monad
> import Data.Map as Map
> import FinallyTagless.V2.PV
> import FinallyTagless.V2.ProbExplore
> import FinallyTagless.V2.ProbLanguage
> import  FinallyTagless.V2.Base

let explore (maxdepth : int option) (choices : 'a pV) : 'a pV =
let rec loop p depth down choices ((ans,susp) as answers) =
match choices with
| [] -> answers
| (pt, V v)::rest ->
loop p depth down rest
(PMap.insert_with (+.) v (pt *. p) ans, susp)
| (pt, C t)::rest when down ->
let down' =
match maxdepth with Some x -> depth < x | None -> true
in loop p depth down rest
(loop (pt *. p) (depth + 1) down' (t ()) answers)
| (pt, c)::rest ->
loop p depth down rest (ans, (pt *. p,c)::susp) in
let (ans,susp) = loop 1.0 0 true choices (PMap.empty,[])
in PMap.foldi (fun v p a -> (p, V v)::a) ans susp

Examples of equivalent PVs

[(0.6, V true); (0.4, V false)]
  [(0.4, V true); (0.4, V false); (0.2, V true)]
[(0.4, V true); (1.0, C (fun () ->
                           [(0.4, V false); (0.2, V true)]))]
  [(0.4, V true); (0.8, C (fun () ->
                           [(0.5, V false); (0.25, V true)]))]
  [(0.4, V true); (0.8, C (fun () ->
                           [(0.5, V false); (0.25, V true);
                            (0.25, C (fun () -> []))]))]

module SearchTree = struct
type 'a pm = 'a pV
type ('a,'b) arr = 'a -> 'b pV


(> newtype Arr a b = (a -> PV b)                       
                            

                         
module type ProbSig = sig
type 'a pm
type ('a,'b) arr
val b : bool -> bool pm
val dist : (prob * 'a) list -> 'a pm
val neg : bool pm -> bool pm
val con : bool pm -> bool pm -> bool pm
val dis : bool pm -> bool pm -> bool pm
val if_ ; bool pm -> (unit -> 'a pm) -> (unit -> 'a pm) -> 'a pm
val lam : ('a pm -> 'b pm) -> ('a,'b) arr pm
val app : ('a,'b) arr pm -> ('a pm -> 'b pm)
end


let b = pv_unit
let dist ch = List.map (fun (p,v) -> (p, V v)) ch
let neg e = pv_bind e (fun x -> pv_unit (not x))
let con e1 e2 = pv_bind e1 (fun v1 ->
if v1 then e2 else (pv_unit false))
let dis e1 e2 = pv_bind e1 (fun v1 ->
if v1 then (pv_unit true) else e2)
let if_ et e1 e2 = pv_bind et (fun t ->
if t then e1 () else e2 ())
let lam e = pv_unit (fun x -> e (pv_unit x))
let app e1 e2 = pv_bind e1 (pv_bind e2)
end

> instance  ProbLangauge PV  where
>       b = return 
>       dist  ch = PV $  Prelude.map (\(p, v) -> (p, V v)) ch
>       neg e = e >>= (\x -> return $ not x)   -- this wraps the value or computation in a computation
>       con e1 e2 = e1 >>= (\v1 -> if v1 then e2 else return False)
>       dis e1 e2 = e1 >>= (\v1 -> if v1 then return True else e2)
>       if_ et e1 e2 = et >>= (\t -> if t then e1 else e2)
>       lam e = return (\x -> e (return x)) 
>       app e1 e2= e1 >>= (e2 >>=) 


let flip p = dist [(p, true); (1.-.p, false)]
let let_ e f = app (lam f) e
let grass_model () =
let_ (flip 0.3) (fun rain ->
let_ (flip 0.5) (fun sprinkler ->
let_ (dis (con (flip 0.9) rain)
(dis (con (flip 0.8) sprinkler)
(flip 0.1))) (fun grass_is_wet ->
if_ grass_is_wet (fun () -> rain) (fun () -> dist []))))

> flp :: Probability -> PV Bool
> flp p = dist [(p, True), (1-p, False)]
> let_ :: forall a b. PV a -> (PV a -> PV b) -> PV b
> let_ e f = app (lam f) e

I removed the "()" for the as the grass model argument

> grass_model ::  PV Bool
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

> simpleModel :: PV Bool
> simpleModel = let_ (flp 0.3) f

notice that the function input is a PM Bool that is a list of (prob, V bool).  The list is then used
in >>= operation to map over values in the list.

> f :: PV Bool -> PV Bool
> f = (\x -> if_ x x (dist []))
> main = explore (Just 0) $ grass_model 
