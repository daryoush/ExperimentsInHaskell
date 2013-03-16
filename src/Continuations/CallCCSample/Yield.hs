

--
--From http://www.vex.net/~trebla/haskell/cont-monad.xhtml
--
--
-- 1  body = do
-- 2    ...
-- 3    ko <- read outside
-- 4    callCC $ \ki -> do
-- 5      write inside ki    -- inside stores line 7
-- 6      ko ()              -- jump to line 15
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
         -- next value, nb is the final result from the function as a monad.   
mkgen body = do
  inside <- liftIO (newIORef undefined)   --  IO Ref is a A mutable variable in the IO monad
  outside <- liftIO (newIORef undefined)
  let yield y = do
        ko <- liftIO (readIORef outside)
        callCC (\ki -> do
                   liftIO (writeIORef inside ki)
                   ko (More y)
               )
      next x = do
        ki <- liftIO (readIORef inside)
        callCC (\ko -> do
                   liftIO (writeIORef outside ko)
                   ki x
               )
      start x = do
        e <- body yield x
        liftIO (writeIORef inside (\_ -> return (End e)))  --  Write a new value into an IORef
        ko <- liftIO (readIORef outside)
        ko (End e)
        undefined
  liftIO (writeIORef inside start)
  return next
  
  
  
  
  
---------------------------------------------------------
--
--
--    USAGE
--
--------------------------------------------------------------------



body :: forall (m :: * -> *).
                   MonadIO m =>
                   (Int -> m Bool) -> Bool -> m [Char]
body yield b = bodyloop b 0 where
  bodyloop False n = do
    liftIO (putStrLn "body receives False")
    return (replicate n 'x')
  bodyloop True n = do
    liftIO (putStrLn "body receives True")
    b <- yield n
    bodyloop b $! (n+1)

cmain :: ContT r IO ()
cmain = do
  g <- mkgen body
  let cmainloop b = do
        d <- g b
        case d of
          End s -> do liftIO (putStrLn ("caller receives " ++ s))
                      return ()
          More n -> do liftIO (putStrLn ("caller receives " ++ show n))
                       cmainloop (n < 5)
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