module Continuations.DelimContInHaskell where
 
 
import Control.Monad.CC
import Control.Monad 
 
data Iterator r a = I a (CC r (Iterator r a)) | Done

current :: Iterator r a -> Maybe a
current (I a _) = Just a
current Done    = Nothing
 
next :: Iterator r a -> CC r (Iterator r a)
next (I _ m) = m
next Done    = return Done
 
iterator :: ((a -> CC r ()) -> CC r ()) -> CC r (Iterator r a)
iterator loop = reset $ \p ->
                  loop (\a ->
                     shift p $ \k ->
                         return $ I a (k $ return ())) >> return Done
 
test = do i <- iterator $ forM_ [1..5]
          go [] i
  where
  go l Done = return l
  go l i    = do let (Just a) = current i
                     l' = replicate a a ++ l
                 i' <- next i
                 go l' i'