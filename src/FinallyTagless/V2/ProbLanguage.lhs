
> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

> module FinallyTagless.V2.ProbLanguage where

> import  FinallyTagless.V2.Base

                         
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




> class ProbLangauge pl  where 
>       b :: Bool -> pl h Bool
>       dist :: [(Probability, a)] ->  pl h Bool 
>       neg :: pl h Bool -> pl h  Bool
>       con :: pl h Bool -> pl h Bool -> pl h Bool
>       dis :: pl h Bool -> pl h Bool -> pl h Bool
>       if_ :: pl h Bool -> pl h a  ->  pl h a-> pl h a  -- removed the ().  Correct?
>       lam :: pl (a,h) b  -> pl h (a->b)
>       app :: pl h (a->b) -> pl h a -> repr h b
> --      lam :: (pl a -> pl b) -> pl (a -> b)
> --      app :: pl (a -> b)   -> (pl a -> pl b)
