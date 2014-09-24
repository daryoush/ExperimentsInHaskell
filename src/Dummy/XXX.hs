module Dummy.XXX where

import Control.Monad.Instances

y ::  Integer -> Integer -> [Integer]
y = \a -> (\r -> [r+a])

x :: Integer -> Integer
x = (+3)

xxx = x >>= y


z = do 
      a <- x
      y a
        
main = print (z 10)   -- note that 10 as r is passes to both x and y. while result of x is also 
                      -- passed to y as argument.
                      
-- *Dummy.XXX > main
-- [23]