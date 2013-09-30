> module Interviews.IntListAddto10  where


Problem:   We have a list of Inetgers and we like to find the first two integers that add to 10.
e need the integers and their offset within the array.

> {-# OPTIONS_HADDOCK  ignore-exports #-}
> {-# LANGUAGE  FlexibleInstances,
>              TypeSynonymInstances,
>              MultiParamTypeClasses,
>              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
>              CPP  #-}



Solutuon using the list functions

> input = [1000, 999..1]

> inputWithIndex =  zip input [1..]


Lets see how Foldl' can help.  From Foldl' documentation
        We somehow have to tell the system that the inner redex should be reduced before 
        the outer. Fortunately this is possible with the seq function:
        
        seq :: a -> b -> b
        seq is a primitive system function that when applied to x and y will first reduce x then return y. The idea is that y references x so that when y is reduced x will not be a big unreduced chain anymore.
        
        Now lets fill in the pieces:
        
         foldl' f z []     = z
         foldl' f z (x:xs) = let z' = z `f` x 
                             in seq z' $ foldl' f z' xs
         
         sum3 = foldl' (+) 0
         
         try3 = sum3 veryBigList
        If we now evaluate try3 we get the correct answer and we get it very quickly:
        
        500000500000
        Lets see what happens:
        
        try3 -->
        sum3 veryBigList -->
        foldl' (+) 0 veryBigList -->
         
        foldl' (+) 0 [1..1000000] -->
        foldl' (+) 1 [2..1000000] -->
        foldl' (+) 3 [3..1000000] -->
        foldl' (+) 6 [4..1000000] -->
        foldl' (+) 10 [5..1000000] -->
        -- ...
        -- ... You see that the stack doesn't overflow
        -- ...
        foldl' (+) 499999500000 [1000000] -->
        foldl' (+) 500000500000 [] -->
        500000500000


If instead of (+) I have 
[Int] is the ist of numbers that I have seen so far and are yet to find matches in
[(int, int)] are list pairs that I have found so far
Int is the next number in the list to find match on

> findNumberInList::([Int] , [(Int, Int)]) -> Int -> ([Int], [(Int, Int)])
> findNumberInList (nums, acc) newNum = 
>       let     matches = filter (\x -> x == 10 - newNum)  nums -- list of number that matches the e
>               addToAcc [] = acc
>               addToAcc x  = acc ++ zip  matches  (repeat newNum)
>       in      (nums ++ [newNum], addToAcc matches)
                                        

> testdataWithUndefined = [9,1, 6,4, undefined]

> testFold_1 = head $ snd $foldl findNumberInList ([],[]) input
> testFold_2 = head $ snd $foldl findNumberInList ([],[]) testdataWithUndefined


--
--
--> import Data.Char
--> import Text.ParserCombinators.UU
--> import Text.ParserCombinators.UU.Utils
--> import Text.ParserCombinators.UU.Derived
--> import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
--> import System.IO
--> import GHC.IO.Handle.Types
--> import qualified Data.ListLike as LL
--
---- import Control.Monad
--
--> type Parser a = P (Str Char String LineColPos) a
--
--
--S