{-# LANGUAGE FlexibleInstances #-}

--FlexibleInstances so we can make the complex function call of the formatter into an instance

module PrettyPrinter.Formatter1 (
        )where

import PrettyPrinter.PPInterface hiding (main) 
import PrettyPrinter.Utils

type Fit = Bool
type Space = Int
type Hp = Int
type Indent = Int
type Result = String -> String
type Width = Int
type F1 = (Width -> Fit -> Space -> Hp -> Indent -> (Space, Hp, Result))



instance PrettyPrinter F1 where
  noDoc   _ _ s h _        = (s, h, id)
  text  t  _  _ s h _      = (s-length t, h+length t, (t++))
  line  w f s h i          = (s', h+1, r) where
                                (r, s') = newLine w f s i 
  (d1 <> d2) w f s h i    = (sr, hr, rl.rr) where 
                                   (sl, hl, rl) = d1 w f s h i
                                   (sr, hr, rr) = d2 w f sl hl i
                        
  gr  d w f s h i         = v where
                                mep = h + s
                                v@(sd, hd, rd) = d w (hd <= mep) s h i
                                -- Craziness!!!  hd is on both side of the equality
  nest j d  w f s h i      = d  w f s h (i+j)

--- TODO::    EXPLAIN how the  hd works in the group


toF1Doc :: Tree -> F1
toF1Doc t = showTree t  
                
main w = let (_, _, v) = (toF1Doc tree) w False w 0 0
       in toSeq v
