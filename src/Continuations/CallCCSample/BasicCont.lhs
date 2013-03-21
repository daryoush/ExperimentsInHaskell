> module BasicCont where

> import Control.Monad.Cont


The following wont compile unless we change it to ContT r IO String
 cmain :: ContT r IO () 
 cmain = do
       return "aaaaa"


> cmain :: ContT r IO String
> cmain = do
>       liftIO $ putStrLn "In the cmain"
>       return "aaaaa"


What kind of continuation function would you need to run the cmain

:t runContT cmain
runContT cmain :: (String -> IO r) -> IO r

Note the the final result type is based on the continuation function

> main1 = runContT cmain return

Alternatively we can return the length of string so r be an int

> main2 = runContT cmain (\s -> return $ length s)
