{-# LANGUAGE RankNTypes, KindSignatures #-}

module MonadTransformers.TransformerExperiment where


import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.List

x :: StateT Integer (ListT Identity) [Char]
x =  do 
        s <- get
        put (s + 1)
        return "aaa"

y =  do 
        s <- get
        put (s + 1)
        return "bbb"

-- Could also have done StateT Integer [] String
z :: StateT Integer (ListT Identity) String
z = do
        a <- x    -- from StateT type that is a monad get its value that is [a] where a is 
                  -- State Integer String
        b <- y
        return (a ++ b)

runZ = runIdentity $ runListT $ runStateT z 1

xx =  do 
        s <- get
        put (s + 1)
        return "aaa"

yy =  do 
        s <- get
        put (s + 1)
        return "bbb"      
        
zz :: [State Integer String]
zz = [xx] ++ [yy]

runZZ = runState (sequence zz) 1


-- *MonadT--ransformers.TransformerExperiment> main
-- in StateT: [("aaabbb",3)]
-- in State: (["aaa","bbb"],3)
main = do
        putStrLn $ "in StateT: " ++ show(runZ)
        putStrLn $ "in State: " ++ show(runZZ)