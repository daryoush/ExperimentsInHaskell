module PrettyPrinter.Utils where

import Debug.Trace

-- Experiment with toSeq and function composition

oneAnd, twoAnd, three :: String -> String
oneAnd = ("one,"++)
twoAnd = ("two,"++)
three = ("three"++)

oneAndTwoAndThree = toSeq (oneAnd . twoAnd . three)

toSeq s | trace ("to list: "  ++ show (s [])) False = undefined
toSeq s  = s []


newLine w True s i = ((' ':), s-1)  -- (:) is the list contrsuctor, going from char to String  
newLine w False s i = ((('\n':replicate i ' ')++), w -i)  -- note we build new line and indent from
                                                        -- char then use the string in ++

prune s p | trace ("prune width " ++ show (s) ++ " remaining list: " ++ show p ) False = undefined                                                        
prune _ []  = True
prune s (p:ps) = (p <= s) && prune (s - p) ps
                                                   