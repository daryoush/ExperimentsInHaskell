> {-# LANGUAGE RankNTypes, KindSignatures #-}
> module MonadTransformers.TypeSignature where

A module to experiments with Monad Type transformer as type constructors to build up 
types of various froms of computation.

From  the runXXX function one can see the type of the computation that one gets when constructs
a XXX type using the monad transformers.

> import Control.Monad.Cont
> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Writer
> import Control.Monad.Identity


How to determine the type of the transformer:

In the run method the runXX works from inside out in the type signauer.
The last runXX (the first in the runXX $ ......)   give the final result  as "XX a" with the "a" being
the  returned type of the "......" function.

> type CSE r s e  = ContT r (StateT s (ErrorT e Identity))



> runCSE :: forall e s a a1.
>                       ContT a (StateT s (ErrorT e Identity)) a1  -- CSE Type
>                       -> s  
>                       -> (a1 -> StateT s (ErrorT e Identity) a)  -- Continuation
>                       -> Either e (a, s)
> runCSE x st  contin= runIdentity $ runErrorT $ runStateT (runContT  x contin) st

> type SEC r s e  =  StateT s (ErrorT e ( ContT r Identity))



> runSEC :: forall a e s a1.
>                     StateT s (ErrorT e (ContT a Identity)) a1 -- SEC type
>                     -> s 
>                     -> (Either e (a1, s) -> Identity a)  -- continuation
>                     -> a
> runSEC x st contin = runIdentity $ runContT (runErrorT (runStateT x st)) contin


> type SCE r s e  =  StateT s ( ContT r (ErrorT e Identity))



> runSCE :: forall e a s a1.
>                     StateT s (ContT a (ErrorT e Identity)) a1
>                     -> s 
>                     -> ((a1, s) -> ErrorT e Identity a) 
>                     -> Either e a
> runSCE x st contin = runIdentity $ runErrorT (runContT  (runStateT x st) contin)

> type  EW e w = WriterT w (ErrorT e Identity) 


> runEW :: forall e w a.
>                    WriterT w (ErrorT e Identity) a -> Either e (a, w)
> runEW x  = runIdentity $ runErrorT $ runWriterT x


A combination that gives you the log and possible error values

> type  WEX e w x = ErrorT w (WriterT e x) 

> type  WE e w = WEX e w Identity


> runWE :: forall w e a.
>                    ErrorT e (WriterT w Identity) a -> (Either e a, w)
> runWE x  = runIdentity $  runWriterT $ runErrorT x


> type  WEIO e w = WEX e w IO
> runWEIO :: forall w (m :: * -> *) e a.
>                      ErrorT e (WriterT w m) a -> m (Either e a, w)
> runWEIO x  =  runWriterT $ runErrorT x

