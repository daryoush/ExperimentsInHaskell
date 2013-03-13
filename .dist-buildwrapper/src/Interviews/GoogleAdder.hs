module Interviews.GoogleAdder where


import Debug.Trace
import Control.Monad.Trans.State
import Control.Monad

rev [] = []
rev (x:xs) = (rev xs) ++ [x]


addTwoLists::[Int] -> [Int] -> Int -> [Int] -> [Int]
--addTwoLists x y c r | trace ("x:" ++ show(x) ++ " y: " ++ (show y) ++ " c = " ++ show(c) ++ " r:" ++ (show r) ) False = undefined 
addTwoLists [] [] 0 r = r
addTwoLists [] [] 1 r = 1:r
addTwoLists x [] c r =  addTwoLists x [0] c r
addTwoLists [] x c r = addTwoLists [0] x c r
addTwoLists (x:xs) (y:ys) c r = let sum = x + y + c
                                    (carry, toadd) = sum `quotRem` 10
                                    in addTwoLists xs ys carry (toadd : r)
                                    
main  = addTwoLists (reverse [9,9,9,9]) (reverse [1,1]) 0 []


--  MONADIC STYLE

type Adder = State Int

myadd::Int -> Int -> Adder Int
myadd a b =  
                do
                        c <- get  
                        let (newCarry, result) = (a+b+c) `quotRem` 10         
                        put newCarry
                        return result


-- Modify the standard zipWith to zip the list to the longest list (use default values for the 
-- missing elements in the shorter list
zipWithToLongestList :: (a->b->c) -> a-> [a] -> b->[b]->[c]  -- add default values to fill to a and b
zipWithToLongestList f defA (a:as) defB (b:bs) = f a b : zipWithToLongestList f defA as defB bs
zipWithToLongestList f defA (a:as) defB [] = f a defB : zipWithToLongestList f defA as  defB []
zipWithToLongestList f defA [] defB (b:bs) = f defA b : zipWithToLongestList f defA []  defB bs
zipWithToLongestList _   _     _   _  _ = []


main2 = let (sum, carry ) = runState (sequence (zipWithToLongestList myadd 0 (reverse [9,9,9,9])  0 (reverse [1,1]))) 0 in
        carry : sum  -- append the carry to the result
        

        
-- APPLICATVE STYLE
-- Using Applicative functor we can map over the list with side effects (carry)
