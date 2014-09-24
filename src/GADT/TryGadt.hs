{-# LANGUAGE GADTs #-}
module GADT.TryGadt where


data Expr a where
       I   :: Int  -> Expr Int
       B   :: Bool -> Expr Bool

       

iList = [I 10, I 20]
 
ibMixlist = [I 10, B True]  -- WONT Compile
