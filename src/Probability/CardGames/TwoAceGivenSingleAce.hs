module Probability.CardGames.TwoAceGivenSingleAce where

import Debug.Trace (trace)
import Probability.CardGames.Cards


import Control.Monad

--  Problem:
--   Given a pair of cards from a full deck.  What is the probability of two aces if
--    a) one of the cards is known to be an Ace
--    b) one of the cards is known to be an Ace of Spade

drawDoubleCards :: CardOptions
drawDoubleCards = chooseNCards 2 allCards           


pairsWithSingleAce :: CardOptions
pairsWithSingleAce = chooseSingleAce drawDoubleCards

pairsWithAceSpade ::CardOptions
pairsWithAceSpade = chooseAceSpade drawDoubleCards

pDblAceGivenSingleAce :: Double
pDblAceGivenSingleAce = let src = pairsWithSingleAce
                            target =  chooseDoubleAce src                                
                            in getNaiveProbInPerCent  target src
                                  
                           
pDblAceGivenAceSpace :: Double
pDblAceGivenAceSpace = let src = pairsWithAceSpade
                           target = chooseDoubleAce src
                           in getNaiveProbInPerCent target src                                

getNaiveProbInPerCent::[a] -> [a] -> Double
getNaiveProbInPerCent events space = trace
  ("trace.... event(size: " ++
     show (length events) ++
       ") to space (size: " ++ show (length space) ++ ")")
  100
  * fromInteger (toInteger (length events))
  / fromInteger (toInteger (length space))

main::IO()
main = putStrLn $ "Prob of Double Ace Given An Ace = " ++ show pDblAceGivenSingleAce ++ "%" ++
                "\nProb of Double Ace Given An Ace of Space = " ++ show pDblAceGivenAceSpace ++ "%"
               
               
----------------------
--  Monadic Style
----------------------

--dblGivenSingleAce :: CardOptions
dblGivenSingleAce = do
                --eventSpace <- []
                --outcomes <- []
                hand <- chooseNCards 2 allCards
                guard $ singleAce hand
--                eventSpace ++ hand
--                return eventSpace
--                guard $ doubleAce hand
--                outcomes ++ hand
--                return (outcomes, eventSpace)
                