{-# LANGUAGE FlexibleInstances #-}
module NumAndUnit.StackOverFlowExample where

--Basic idea is that if you have information in front of the number that should be used
--to construct your type say:
--
--10 xx yyy zzz
--
--it looks as if xx, yyy, and zzz are applied to number.
--
--You want to define an instance of num of a  type xx->yyy->zzz  the type
--is constructed by fromInsterger that takes a number and returns a function.  YOu can 
--also write it with pattern matching to parse out the xx,yyyy, and zzz is
--
--fromInteger n (XXX YYY ZZZ)  = YOUR TYPE OF COMPUTATION

— i think in Haskell, an int literal is converted to 

—-fromIntger xxx
—-

-- Code is from http://stackoverflow.com/questions/22109333/how-can-i-write-human-language-units-as-postfixes-in-haskell-like-3-seconds/22157873#22157873

newtype TimeUnit = TimeUnit Integer
  deriving (Eq, Show)

instance Num (TimeUnit -> TimeUnit) where
  fromInteger n = \(TimeUnit scale) -> TimeUnit (n * scale)

seconds, minutes, hours, days :: TimeUnit

seconds = TimeUnit 1000000
minutes = 60 seconds
hours   = 60 minutes
days    = 24 hours

soon :: TimeUnit
soon = 2 Hours