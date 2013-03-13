module Probability.CardGames.Cards where




data Suit = Clubs
            | Diamonds
            | Hearts
            | Spades
              deriving (Eq,Enum,Show,Bounded)

data Value = Ace
             | Two
             | Three
             | Four
             | Five
             | Six
             | Seven
             | Eight
             | Nine
             | Ten
             | Jack
             | Queen
             | King
             deriving (Eq,Enum,Show,Bounded)

data Card = Card Value Suit
              deriving (Eq,Show)

type CardOptions = [[Card]]


allCards :: [Card]
allCards = [Card x y | x <- [Ace .. King],  y <- [Clubs .. Spades]]
  
  
  
chooseN:: Int -> [a] -> [[a]]
--chooseN 0 _     = []    -- make sense?
--chooseN _ []    = []    -- make sense?
--chooseN 1 lst   = [[x] | x <- lst]   -- put every element of the list into a selection 
--chooseN n (x:xs)= [ first ++rest  | first <- [[x]]
--                                    ,rest <- chooseN (n-1) xs] 
--                  ++ chooseN n xs   -- choose from the rest of the list 
-- 

-- Alternative implementation 
-- from http://www.haskell.org/pipermail/beginners/2011-November/008991.html
chooseN _ []    = []    -- make sense?
chooseN 1 lst   = map (:[]) lst  -- put every element of the list into a selection 
chooseN n (x:xs) = (map (x:) $ chooseN (n-1) xs) ++ chooseN n xs                 
                  
chooseNCards:: Int -> [Card] -> CardOptions
chooseNCards = chooseN


singleAce :: [Card] -> Bool
singleAce  []                   = False
singleAce ((Card v _):xs)       = v == Ace || singleAce xs

doubleAce :: [Card] -> Bool
doubleAce []                    = False
doubleAce (x:[])                = False
doubleAce ((Card v1 _):(Card v2 _):xs) = (v1 == Ace && v2 == Ace ) ||
                                         (v1 == Ace && singleAce xs) ||
                                         (v2 == Ace && singleAce xs) ||
                                         doubleAce xs
                           
singleAceOfSuit :: Suit -> [Card] -> Bool
singleAceOfSuit _ [] = False
singleAceOfSuit ss ((Card v s):xs)  = (v == Ace && s == ss)  || singleAceOfSuit ss xs

chooseSingleAce = filter singleAce
chooseDoubleAce = filter doubleAce
chooseAceSpade  = filter $ singleAceOfSuit Spades
