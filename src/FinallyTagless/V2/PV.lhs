> module FinallyTagless.V2.PV where

> import  FinallyTagless.V2.Base

type 'a vc = V of 'a | C of (unit -> 'a pV)
and 'a pV = (prob * 'a vc) list

> data VC a = V a | C (() -> PV a) 
> newtype PV a = PV [(Probability, VC a)] deriving Show

> instance (Show a) => Show (VC a) where 
>       show (V a) = "(" ++ (show a) ++ ")"
>       show (C t) = "COMPUTATION"


let pv_unit (x : 'a) : 'a pV = [(1.0, V x)]
let rec pv_bind (m : 'a pV) (f : 'a -> 'b pV) : 'b pV =
List.map (function
| (p, V x) -> (p, C (fun () -> f x))
| (p, C t) -> (p, C (fun () -> pv_bind (t ()) f))) m


In the PV moand >>= composition wraps the computation or value of "f" into a "C" type (function) of VC 

> instance Monad PV where  
>       return a = PV [(1, V a)]
>       -- PV a -> (a -> PV b) -> PV B
>       PV m >>= f = PV (Prelude.map mapf m) where 
>                    mapf (p, V x) = (p, C (\_  ->  f x))
>                    mapf (p, C t) = (p, C (\_ -> (t () >>= f)))