
module Primes.Prime where 

import qualified Data.Map as Map
  
sieveAsLet::(Num b, Ord b) => [b] -> Map.Map b [b]
sieveAsLet lst = sieve' lst Map.empty where
        sieve' [] t = t
        sieve' (x:xs) t =  let  
                reinsert table prime = Map.insertWith (++) (x+prime) [prime] table
                in case Map.lookup x t of
                Just a -> sieve' xs (foldl reinsert (Map.delete x t) a)
                Nothing -> sieve' xs (Map.insert (x*x) [x] t)

-- key is the square, the number that has been reached.  note if there are multiple ways
-- to get to a number then the value which is the list of primes would have multiple numbers
-- just a is where the the number being looked at x is the same as previous x*x
-- foldl is only working on list not map
 
 
sieve::(Num b, Ord b) => [b] -> Map.Map b [b]
sieve lst = sieve' lst Map.empty where
        sieve' [] t = t
        sieve' (x:xs) t =    
                case Map.lookup x t of
                -- Prelude.foldl :: (a -> b -> a) -> a -> [b] -> a
                Just a -> sieve' xs (foldl reinsert (Map.delete x t) a)
                Nothing -> sieve' xs (Map.insert (x*x) [x] t)
                where
                  reinsert table prime = Map.insertWith (++) (x+prime) [prime] table
                                
sieveTest :: (Num b, Ord b) => [b] -> Map.Map b [b]
sieveTest lst = sieve' lst Map.empty where
        sieve' [] t = t
        sieve' (x:xs) t =    
                case Map.lookup x t of
                -- Prelude.foldl :: (a -> b -> a) -> a -> [b] -> a
                -- after sieve [1..9] table looks like xx here:
                -- let xx= fromList [(1,[1]),(10,[2]),(12,[3]),(25,[5]),(49,[7])]
                -- Prelude.foldl (reinsertF 10) (Map.delete 10 xx) [2]
                -- returns the result of sieve [1..10]
                -- fromList [(1,[1]),(12,[2,3]),(25,[5]),(49,[7])]
                -- HOW TYPES WORK
                -- :t Prelude.foldl
                -- Prelude.foldl :: (a -> b -> a) -> a -> [b] -> a
                -- :t reinsertF 10
                -- reinsertF 10 :: (Num a, Ord a) => Map a [a] -> a -> Map a [a]
                -- so in foldl's ( a->b->a ) "a"  is a Map a [a]
                -- b is a list of current primes that have been removed form the list
                -- in the above case it is [2] since that 10 is the key for [2] now
                -- it is  a strange fold because all it seems to be doing is map transofrmation
                 
                Just a -> sieve' xs (foldl (reinsertF x) (Map.delete x t) a)
                -- If I don't do the fold, it wont work because the "a" here is a [2] but the 
                -- reinsertF needs "2"
                -- WONT WORK Just a -> sieve' xs ((reinsertF x) (Map.delete x t) a)
                -- LOOKS AS IF THE REINSERTF should be modified to accept a list and append it to the key
                Nothing -> sieve' xs (Map.insert (x*x) [x] t)
             
reinsertF :: (Num a, Ord a) => a -> Map.Map a [a] -> a -> Map.Map a [a]                
reinsertF x table prime = Map.insertWith (++) (x+prime) [prime] table                                                      
                                                      
                                        