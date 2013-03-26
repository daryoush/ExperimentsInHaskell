{-# LANGUAGE RankNTypes, KindSignatures #-}



--
--instance MonadCont (Cont r) where
--    callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a)) c

--  c is the continuation for the whole (f (.....))
--  within f, c is also  wrapped into a function that looks like continuation (it would expect)
--  there for f can apply the conitnuation as many times as it wishes.
--  without continuation, f can only do a return, where as now it can do as many calls as it pleases.
-- see http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-Cont-Class.html#v:callCC
--    
    
--
--From http://www.vex.net/~trebla/haskell/cont-monad.xhtml
--
--
-- 1  body = do
-- 2    ...
-- 3    ko <- read outside
-- 4    callCC $ \ki -> do
-- 5      write inside ki    -- inside stores line 7   -- FUTURE IS LINE 7 
-- 6      ko ()              -- jump to line 15        -- NOTE a RETURN FRM THIS BLOCK IS ALSO TO LINE 7
-- 7    ...
-- 8    body
--
-- 9  caller = do
--10    ...
--11    ki <- read inside
--12    callCC $ \ko -> do
--13      write outside ko   -- outside stores line 15
--14      ki ()              -- jump to line 7
--15    ...
--16    caller

{-| Yield-style generators. -}
module Continuations.CallCCSample.Yield where

import Control.Monad.Cont
import Data.IORef

{-| 
Type of return values of generators. @More@ means the generator yields. @End@
means the generator finishes. See below for examples.
-}
data Dot a b = More a | End b deriving Show

{-|
Create a yield-style generator from a body. Example:

> g <- mkgen (\yield c0 -> do
>                c1 <- yield a0
>                c2 <- yield a1
>                return b
>            )

Then we can use:

> dot0 <- g c0  -- dot0 = More a0
> dot1 <- g c1  -- dot1 = More a1
> dot2 <- g c2  -- dot2 = End b

Further calls to @g@ return @End b@ too.

Although usually @m=n@, i.e., @g@ and the body are in the same monad as the
@mkgen@ call, technically they can be different. The @mkgen@ call
is in m with MonadIO for allocating IORef. The body and @g@ are in n with
MonadIO and MonadCont for using IORef and callCC.
-}
mkgen :: (MonadIO m, MonadIO n, MonadCont n) =>
         ((a -> n c) -> c -> n b) -> m (c -> n (Dot a b))
         -- in First argument is function where c is the initial value, (a->nc) is the continuation taht generates
         -- next value, nb is the final result from the function as a IO, cont monad.   
mkgen bdy = do
  inside <- liftIO (newIORef undefined)   --  IO Ref is a A mutable variable in the IO monad
  outside <- liftIO (newIORef undefined)
  -- yield :: a -> n c 
  let yield y = do    --This is definition of the func.  The callCC will be evauated when the yeild is called.
        ko <- liftIO (readIORef outside)
        callCC (\ki -> do
                   liftIO (writeIORef inside ki)
                   ko (More y)
               )
      -- next :: c -> n (Dot a b) 
      next x = do
        ki <- liftIO (readIORef inside)  -- computation that either yields the next value or end
        callCC (\ko -> do
                   liftIO (writeIORef outside ko)
                   ki x   
               )
      -- start:: (MonadIO n, MonadCont n) => c -> n b 
      start x = do
        e <- bdy yield x  -- so long as the callcc works it stays in yield, otherwise it goes to the 
                           -- next part where the End is returned
        liftIO (putStrLn ".... after body yeild x in the start")
        liftIO (writeIORef inside (\_ -> return (End e)))  --  Write a new value into an IORef
        ko <- liftIO (readIORef outside)
        ko (End e)
        undefined
  liftIO (writeIORef inside start)   -- start is the first function that next will catyp
  return next
  
  
  
  
---------------------------------------------------------
--
--
--    USAGE
--
--------------------------------------------------------------------


-- A function that take a continutation int -> m bool
--          meaning it would at some point generate an int internally and pass it to the continuation
--          to get a m bool back.
-- In put value of a bool (that is used to generate the int for the continuation
-- return value will be m [char]   so the function changes the result back from the continuation to 
-- string.

-- the initial input b starts off the loop, but from then on the bool returns from the cont determines
-- the next step of the function.

-- Note this function is called in callcc context.  when it returns the callcc returns and then it
-- can update its values


body :: forall (m :: * -> *).
                   MonadIO m =>
                   (Int -> m Bool) -> Bool -> m [Char]
body yield b = bodyloop b 0 where
  bodyloop False n = do    -- to stop the loop returns the "x" character to the caller
    liftIO (putStrLn "body receives False")
    return (replicate n 'x')
  bodyloop True n = do    --  the yield the value, to get the next bool.  inc value and loop with new bool 
    liftIO (putStrLn "body receives True")
    bb <- yield n  
    bodyloop bb $! (n+1)

cmain :: ContT r IO ()
cmain = do
  g <- mkgen body   -- construct an instance of cont from a function. see type signature
                    --  (MonadIO m, MonadIO n, MonadCont n) =>
                    --          ((a -> n c) -> c -> n b) -> m (c -> n (Dot a b))
                    --  mkgen will be using teh callcc which means the rest of the computation
                    -- in this case  "cmainloop True"  with be passes to the function as continuation
                    -- Also note that cmain itself will need a contination to write its results too.
                    -- that continuation will be   (() -> IO r)
                    -- how does liftIO work here?  LiftIO lets the monadic computation at the contT
                    -- level to write to the IO monad that is inside the ContT
                    -- Why is there a () on the type signature
                    -- why is return () in the function when it sees the End?
  let cmainloop b = do
        d <- g b
        case d of
          End s -> do liftIO (putStrLn ("caller receives " ++ s))
                      return ()
          More n -> do liftIO (putStrLn ("caller receives " ++ show n))
                       cmainloop (n < 5)   -- go to five items then set the boolean to false, so the
                                           -- body gets a false.
  cmainloop True


-- *Continuations.Sample.Yield> main
--body receives True
--caller receives 0
--body receives True
--caller receives 1
--body receives True
--caller receives 2
--body receives True
--caller receives 3
--body receives True
--caller receives 4
--body receives True
--caller receives 5
--body receives False
--caller receives xxxxxx
-- *Continuations.Sample.Yield> 

main = runContT cmain return