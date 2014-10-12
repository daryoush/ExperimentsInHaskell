{-# LANGUAGE FlexibleInstances #-}

--FlexibleInstances so we can make the complex function call of the formatter into an instance

module PrettyPrinter.Formatter2 (
        )where

import PrettyPrinter.PPInterface hiding (main) 
import PrettyPrinter.Utils

type Fit = Bool
type Space = Int
type Len = Int
type Indent = Int
type Result = String -> String
type Width = Int 
type F2 = (Width -> Fit -> Space ->  Indent -> (Space, [Len]->[Len], Result))


-- width as constant for now!

instance PrettyPrinter F2 where
  noDoc  _ _ s  _        = (s, id, id)
  text   t  _ _ s  _      = (s-length t, (length t:), (t++))
  line w f s  i          = (s', (1:), r) where
                                (r, s') = newLine w f s i 
  (d1 <> d2) w f s  i    = (sr, hl.hr, rl.rr) where 
                                   (sl, hl, rl) = d1 w f s  i
                                   (sr, hr, rr) = d2 w f sl i
                        
  gr d w f s  i         = v where
                                v@(_, hd, _) = d w (prune s (toSeq hd)) s  i
                                -- Craziness!!!  hd is on both side of the equality
  nest j d w f s  i      = d w f s  (i+j)

--- TODO::    EXPLAIN how the  hd works in the group

toF2Doc :: Tree -> F2
toF2Doc t = showTree t  
                
main w = let  
            (_, _, v) = (toF2Doc tree)   w False w  0
       in toSeq v

 
--  Notice that line is the only place where the "fit" information is needed.  It needs to determine 
-- if a line should be space or /n.   This information is evaluated lazily within a group and
-- is shared by member of the group.
-- In the tree example the first line is encounterd at
-- a12[b123[c1234, <----  First choice
-- it needs to see if c1234 and d1234 group can be done in a line after a12 and b123 or not
-- then      
-- a12[b123[c1234, d1234],  <-----   2nd choice
-- It needs to find if the b123 e123 f123 and their children can fit on a line 
-- then at 
-- a12[b123[c1234, d1234], e123, f123[g1234,  <---  3rd choice
-- need to determine if the g1234 h1234 i1234 can fit on the line either on the same line as
-- previous or a new ine
-- *PrettyPrinter.Formatter2> main 5
--"to list: [4,1,5,1,1,5,1]
--prune width 1 remaining list: [4,1,5,1,1,5,1]
--to list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 5 remaining list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 2 remaining list: [1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 1 remaining list: [4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--to list: [4,1,5,1,1,5,1,1,5,1]
--prune width 1 remaining list: [4,1,5,1,1,5,1,1,5,1]
--to list: "a12[b123[c1234,\n         d1234],\n    e123,\n    f123[g1234,\n         h1234,\n         i1234]]"
--a12[b123[c1234,\n         d1234],\n    e123,\n    f123[g1234,\n         h1234,\n         i1234]]"
---  At width of 5 every line is /n


-- *PrettyPrinter.Formatter2> main 20
--"to list: [4,1,5,1,1,5,1]
--prune width 16 remaining list: [4,1,5,1,1,5,1]
--prune width 12 remaining list: [1,5,1,1,5,1]
--prune width 11 remaining list: [5,1,1,5,1]
--prune width 6 remaining list: [1,1,5,1]
--prune width 5 remaining list: [1,5,1]
--prune width 4 remaining list: [5,1]
--to list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 20 remaining list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 17 remaining list: [1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 16 remaining list: [4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 12 remaining list: [1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 11 remaining list: [5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 6 remaining list: [1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 5 remaining list: [1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 4 remaining list: [5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--to list: [4,1,5,1,1,5,1,1,5,1]
--prune width 16 remaining list: [4,1,5,1,1,5,1,1,5,1]
--prune width 12 remaining list: [1,5,1,1,5,1,1,5,1]
--prune width 11 remaining list: [5,1,1,5,1,1,5,1]
--prune width 6 remaining list: [1,1,5,1,1,5,1]
--prune width 5 remaining list: [1,5,1,1,5,1]
--prune width 4 remaining list: [5,1,1,5,1]
--to list: "a12[b123[c1234,\n         d1234],\n    e123,\n    f123[g1234,\n         h1234,\n         i1234]]"
--a12[b123[c1234,\n         d1234],\n    e123,\n    f123[g1234,\n         h1234,\n         i1234]]"

-- at width 20 still every line is /n

-- *PrettyPrinter.Formatter2> main 30
--"to list: [4,1,5,1,1,5,1]
--prune width 26 remaining list: [4,1,5,1,1,5,1]
--prune width 22 remaining list: [1,5,1,1,5,1]
--prune width 21 remaining list: [5,1,1,5,1]
--prune width 16 remaining list: [1,1,5,1]
--prune width 15 remaining list: [1,5,1]
--prune width 14 remaining list: [5,1]
--prune width 9 remaining list: [1]
--prune width 8 remaining list: []
--to list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 30 remaining list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 27 remaining list: [1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 26 remaining list: [4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 22 remaining list: [1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 21 remaining list: [5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 16 remaining list: [1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 15 remaining list: [1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 14 remaining list: [5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 9 remaining list: [1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 8 remaining list: [1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 7 remaining list: [1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 6 remaining list: [4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 2 remaining list: [1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 1 remaining list: [1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 0 remaining list: [4,1,5,1,1,5,1,1,5,1,1]
--to list: [4,1,5,1,1,5,1,1,5,1]
--prune width 26 remaining list: [4,1,5,1,1,5,1,1,5,1]
--prune width 22 remaining list: [1,5,1,1,5,1,1,5,1]
--prune width 21 remaining list: [5,1,1,5,1,1,5,1]
--prune width 16 remaining list: [1,1,5,1,1,5,1]
--prune width 15 remaining list: [1,5,1,1,5,1]
--prune width 14 remaining list: [5,1,1,5,1]
--prune width 9 remaining list: [1,1,5,1]
--prune width 8 remaining list: [1,5,1]
--prune width 7 remaining list: [5,1]
--prune width 2 remaining list: [1]
--prune width 1 remaining list: []
--to list: "a12[b123[c1234, d1234],\n    e123,\n    f123[g1234, h1234, i1234]]"
--a12[b123[c1234, d1234],\n    e123,\n    f123[g1234, h1234, i1234]]"

-- at 30 there is consolidation of the lower nodes


-- * PrettyPrinter.Formatter2> main 55
--"to list: [4,1,5,1,1,5,1]
--prune width 51 remaining list: [4,1,5,1,1,5,1]
--prune width 47 remaining list: [1,5,1,1,5,1]
--prune width 46 remaining list: [5,1,1,5,1]
--prune width 41 remaining list: [1,1,5,1]
--prune width 40 remaining list: [1,5,1]
--prune width 39 remaining list: [5,1]
--prune width 34 remaining list: [1]
--prune width 33 remaining list: []
--to list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 55 remaining list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 52 remaining list: [1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 51 remaining list: [4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 47 remaining list: [1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 46 remaining list: [5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 41 remaining list: [1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 40 remaining list: [1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 39 remaining list: [5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 34 remaining list: [1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 33 remaining list: [1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 32 remaining list: [1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 31 remaining list: [4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 27 remaining list: [1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 26 remaining list: [1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 25 remaining list: [4,1,5,1,1,5,1,1,5,1,1]
--prune width 21 remaining list: [1,5,1,1,5,1,1,5,1,1]
--prune width 20 remaining list: [5,1,1,5,1,1,5,1,1]
--prune width 15 remaining list: [1,1,5,1,1,5,1,1]
--prune width 14 remaining list: [1,5,1,1,5,1,1]
--prune width 13 remaining list: [5,1,1,5,1,1]
--prune width 8 remaining list: [1,1,5,1,1]
--prune width 7 remaining list: [1,5,1,1]
--prune width 6 remaining list: [5,1,1]
--prune width 1 remaining list: [1,1]
--prune width 0 remaining list: [1]
--to list: [4,1,5,1,1,5,1,1,5,1]
--prune width 51 remaining list: [4,1,5,1,1,5,1,1,5,1]
--prune width 47 remaining list: [1,5,1,1,5,1,1,5,1]
--prune width 46 remaining list: [5,1,1,5,1,1,5,1]
--prune width 41 remaining list: [1,1,5,1,1,5,1]
--prune width 40 remaining list: [1,5,1,1,5,1]
--prune width 39 remaining list: [5,1,1,5,1]
--prune width 34 remaining list: [1,1,5,1]
--prune width 33 remaining list: [1,5,1]
--prune width 32 remaining list: [5,1]
--prune width 27 remaining list: [1]
--prune width 26 remaining list: []
--to list: "a12[b123[c1234, d1234],\n    e123,\n    f123[g1234, h1234, i1234]]"
--a12[b123[c1234, d1234],\n    e123,\n    f123[g1234, h1234, i1234]]"

--  At width of 55 still the top level needs to be broken up because all of the groups doesn't
-- fit in one line


-- *PrettyPrinter.Formatter2> main 56
--"to list: [4,1,5,1,1,5,1]
--prune width 52 remaining list: [4,1,5,1,1,5,1]
--prune width 48 remaining list: [1,5,1,1,5,1]
--prune width 47 remaining list: [5,1,1,5,1]
--prune width 42 remaining list: [1,1,5,1]
--prune width 41 remaining list: [1,5,1]
--prune width 40 remaining list: [5,1]
--prune width 35 remaining list: [1]
--prune width 34 remaining list: []
--to list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 56 remaining list: [3,1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 53 remaining list: [1,4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 52 remaining list: [4,1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 48 remaining list: [1,5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 47 remaining list: [5,1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 42 remaining list: [1,1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 41 remaining list: [1,5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 40 remaining list: [5,1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 35 remaining list: [1,1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 34 remaining list: [1,1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 33 remaining list: [1,4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 32 remaining list: [4,1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 28 remaining list: [1,1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 27 remaining list: [1,4,1,5,1,1,5,1,1,5,1,1]
--prune width 26 remaining list: [4,1,5,1,1,5,1,1,5,1,1]
--prune width 22 remaining list: [1,5,1,1,5,1,1,5,1,1]
--prune width 21 remaining list: [5,1,1,5,1,1,5,1,1]
--prune width 16 remaining list: [1,1,5,1,1,5,1,1]
--prune width 15 remaining list: [1,5,1,1,5,1,1]
--prune width 14 remaining list: [5,1,1,5,1,1]
--prune width 9 remaining list: [1,1,5,1,1]
--prune width 8 remaining list: [1,5,1,1]
--prune width 7 remaining list: [5,1,1]
--prune width 2 remaining list: [1,1]
--prune width 1 remaining list: [1]
--prune width 0 remaining list: []
--to list: [4,1,5,1,1,5,1,1,5,1]
--prune width 26 remaining list: [4,1,5,1,1,5,1,1,5,1]
--prune width 22 remaining list: [1,5,1,1,5,1,1,5,1]
--prune width 21 remaining list: [5,1,1,5,1,1,5,1]
--prune width 16 remaining list: [1,1,5,1,1,5,1]
--prune width 15 remaining list: [1,5,1,1,5,1]
--prune width 14 remaining list: [5,1,1,5,1]
--prune width 9 remaining list: [1,1,5,1]
--prune width 8 remaining list: [1,5,1]
--prune width 7 remaining list: [5,1]
--prune width 2 remaining list: [1]
--prune width 1 remaining list: []
--to list: "a12[b123[c1234, d1234], e123, f123[g1234, h1234, i1234]]"
--a12[b123[c1234, d1234], e123, f123[g1234, h1234, i1234]]"

-- At 56 we got the whole group on to one line.  
