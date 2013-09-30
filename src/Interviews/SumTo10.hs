module Interviews.SumTo10 where
import Control.Monad

data NN = NN { offset:: Int, number :: Int} deriving Show

findNumber ::  Int -> [NN] -> [NN]
findNumber n   = filter ( \(NN _ x) ->  x == n)

inputWithOffset = zipWith ( \a b -> NN a b) [1..]

testdata = inputWithOffset [100..200]



-- take a list of numbers and fold the findNumber n in each part of the list.

