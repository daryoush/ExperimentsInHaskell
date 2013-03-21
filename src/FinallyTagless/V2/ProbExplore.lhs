> module FinallyTagless.V2.ProbExplore where

> import Data.Map as Map
> import FinallyTagless.V2.PV

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