module Misc.ContextFullInApplicative where

--Code is from 
--http://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/
--
--
--Here’s the key insight: normally, grammars are defined as finite objects: a finite set of 
--terminals, a finite set of nonterminals, and a finite set of productions. However, 
--Haskell’s general recursion means that we can write down a "grammar" with an infinite set of 
--production rules. This is what lets us get away with parsing context-sensitive languages with 
--Applicative: we just make a different production rule for every possible input!
--
--First, some imports. Notice that I do not import Control.Monad.

import Text.Parsec
import Text.Parsec.String
import Control.Arrow ((&&&))
import Control.Applicative hiding ((<|>))
import Data.List (group)

--The usual guard function is for MonadPlus, but we can make something equivalent for Alternative.

guard' :: Alternative f => Bool -> f ()
guard' True  = pure ()
guard' False = empty

--
--And now for the meat of the example. parseArbitrary takes an arbitrary predicate on Strings built 
-- from lowercase letters and turns it into a parser. The created parser will accept Strings for 
--which the predicate evaluates to True (returning ()) and fail for any other string.

parseArbitrary :: (String -> Bool) -> Parser ()
parseArbitrary p =
--
--If we encounter eof, we simply ensure that the predicate holds of the empty string.
--
      (eof <* guard' (p [])) 
--
--Otherwise, we choose between 26 alternatives based on the next character in the input. If the 
--character c is encountered, we make a recursive call to parseArbitrary (p . (c:)). The remainder 
--of the input must satisfy (p . (c:)), that is, it must consist of some String s such that (c:s) 
--satisfies the predicate p.
--

  <|> foldr (<|>) parserZero 
        (map (\c -> char c *>
                    parseArbitrary (p . (c:))
             ) 
             ['a'..'z']
        )
--
--For any given predicate p, you can think of parseArbitrary p as an infinite tree with a 26-way 
--branch at each node. Each node "remembers" the path taken to reach it from the root of the tree, 
--in the form of prepend functions composed with the original predicate. We have constructed an 
--infinite grammar: each node in the tree corresponds to a production, one for every 
--possible input prefix.
--
--Let’s try it out. Here’s a function which only accepts Strings of the form "aaabbbccc",
-- with an equal number of a’s, b’s, and c’s. This is a well-known example of a language which 
--is not context-free (easily shown using the pumping lemma for context-free languages).
--
f :: String -> Bool
f s 
  | [('a',na), ('b',nb), ('c',nc)] 
-- FROM ORIG    <- map (head &&& length). group $ s   --- ((map (head &&& length)) . group) s
                                          --- THAT IS SAME AS map (head &&& length) (group s)
--Misc.ContextFullInApplicative> :t head &&& length
--head &&& length :: [c] -> (c, Int)   -- from a list (or String) get first char and length
--Misc.ContextFullInApplicative> :t group
--group :: Eq a => [a] -> [[a]]
--Misc.ContextFullInApplicative>   
                                       
--      <- map (head &&& length) (group s)  
--   Yet easer to understand, but more verbose?
      <- map (\x -> (head x, length x)) $ group s    

    = na == nb && nb == nc

  | otherwise = False
--
--Now we make f into a parser and test it on some example inputs:
--
p = parseArbitrary f

main = do
  parseTest p "aaa"
  parseTest p "aaabbbcc"
  parseTest p "aaaabcccc"
  parseTest p "aaaaabbbbbccccc"