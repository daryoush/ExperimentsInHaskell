> {-# LANGUAGE ScopedTypeVariables #-}

> module FinallyTagless.SearchTree where

> import Control.Monad
> import Data.Map as Map

type 'a vc = V of 'a | C of (unit -> 'a pV)
and 'a pV = (prob * 'a vc) list

> type Probability = Rational
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


module SearchTree = struct
type 'a pm = 'a pV
type ('a,'b) arr = 'a -> 'b pV
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



> type PM a = PV a
> type Arr a b = a -> PV b

> b :: Bool -> PM Bool
> b = return 

> dist :: [(Probability, a)] -> PM a
> dist  ch = PV $ Prelude.map (\(p, v) -> (p, V v)) ch

> neg :: PM Bool -> PM Bool
> neg e = e >>= (\x -> return $ not x)   -- this wraps the value or computation in a computation

> con :: PM Bool -> PM Bool -> PM Bool
> con e1 e2 = e1 >>= (\v1 -> if v1 then e2 else return False)
> dis :: PM Bool -> PM Bool -> PM Bool
> dis e1 e2 = e1 >>= (\v1 -> if v1 then return True else e2)
> if_ :: PM Bool -> PM a  ->  PM a-> PM a  -- removed the ().  Correct?
> if_ et e1 e2 = et >>= (\t -> if t then e1 else e2)
> lam :: (PM a -> PM b) -> PM (Arr a b) 
> lam e = return (\x -> e (return x)) 
> app :: PM (Arr a b)   -> (PM a -> PM b)
> app e1 e2= e1 >>= (e2 >>=) 


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

> grass_model ::  PM Bool
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


> main = explore (Just 0) $ grass_model 
