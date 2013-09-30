{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Continuations.MyInstances where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.Monoid


-- Attempting to derive the continuation instances to see if I understand them.



-- Base types

newtype Cont r a = Cont {

    {- | Runs a CPS computation, returns its result after applying
    the final continuation to it.
    Parameters:

    * a continuation computation (@Cont@).

    * the final continuation, which produces the final result (often @id@).
    -}
    runCont :: (a -> r) -> r
}


newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }


class (Monad m) => MonadCont m where
    {- | @callCC@ (call-with-current-continuation)
    calls a function with the current continuation as its argument.
    Provides an escape continuation mechanism for use with Continuation monads.
    Escape continuations allow to abort the current computation and return
    a value immediately.
    They achieve a similar effect to 'Control.Monad.Error.throwError'
    and 'Control.Monad.Error.catchError'
    within an 'Control.Monad.Error.Error' monad.
    Advantage of this function over calling @return@ is that it makes
    the continuation explicit,
    allowing more flexibility and better control
    (see examples in "Control.Monad.Cont").

    The standard idiom used with @callCC@ is to provide a lambda-expression
    to name the continuation. Then calling the named continuation anywhere
    within its scope will escape from the computation,
    even if it is many layers deep within nested computations.
    -}
    callCC :: ((a -> m b) -> m a) -> m a
    


--fmap :: (a -> b) -> f a -> f b  

--(>>=) :: forall a b. m a -> (a -> m b) -> m b
--return :: a -> m a


--ask :: m rSource
--local :: (r -> r) -> m a -> m a

--get :: m sSource
--put :: s -> m ()

--lift :: Monad m => m a -> t m a
--liftIO :: IO a -> m a

-- Instances to fill in




instance Functor (Cont r) where
    fmap f m =  Cont (\c -> m `runCont`  (c.f))
    --Cont (\c ->  runCont m (\a ->  c (f a)))   --- NOTE (\a -> c (f a)) can be written as c.f


instance Monad (Cont r) where
    return a = Cont (\c ->  c a)   --  ($) :: (a -> b) -> a -> b  so ($ a)  is (a->b) -> b
    m >>= k  = Cont (\c -> runCont m (\a -> runCont  (k a) c)) --- WOW I got this myself :)


-- F's view of the world is that there is a function as its argument that would
-- abort computation and pass values to the continuation.

instance MonadCont (Cont r) where
    callCC f = Cont (  \c -> runCont ( f (\a -> Cont (\_ -> c a )) ) c )
    
 {- |
The continuation monad transformer.
Can be used to add continuation handling to other monads.
-}


instance (Monad m) => Functor (ContT r m) where
    fmap f m = undefined

instance (Monad m) => Monad (ContT r m) where
    return a = undefined
    m >>= k  = undefined

instance (Monad m) => MonadCont (ContT r m) where
    callCC f = undefined
    
    
  
    
-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance MonadTrans (ContT r) where
    lift m = undefined

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = undefined

-- Needs -fallow-undecidable-instances
instance (MonadReader r' m) => MonadReader r' (ContT r m) where
    ask       = undefined
    local f m = undefined
    


--
-- > type CS r s = ContT r (StateT s Identity)
-- >
-- > runCS :: forall s a a1.
-- >                    ContT a (StateT s Identity) a1
-- >                    -> s -> (a1 -> StateT s Identity a) -> (a, s)
-- > runCS x st contin = runIdentity $ runStateT (runContT x contin) st
--



-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ContT r m) where
    get = undefined
    put = undefined    
    
    
    
----------  Other Transformers Acting like MonadCont

-- StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- Just cut and paste the state monad here because MonadCont would need it
instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str
    
    
-- A Statefull computation that its a in  s -> (s,a) is continuation type (ContT)
-- You have a situation like:
-- 
-- > type SC r s = StateT s (ContT r Identity)
-- > runSC :: forall a s a1.
-- >                    ((a1, s) -> Identity a) -> s -> StateT s (ContT a Identity) a1 -> a
-- > runSC finalCont initState  m = runIdentity $ runContT  (runStateT  m initState) finalCont
--
-- Idea of callcc is to "push" the continuation to the MonadCont monad embeded 
-- within the outside computation (state, writer, error)


instance (MonadCont m) => MonadCont (StateT s m) where
    callCC =   undefined    
    
 
-- ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k  = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT $ \_ -> fail msg
    
    
instance (MonadCont m) => MonadCont (ReaderT r m) where
    callCC f = undefined
    
    
-- WriterT

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg


instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
    callCC f = undefined   
    
    