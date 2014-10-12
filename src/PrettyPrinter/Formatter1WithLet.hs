{-# LANGUAGE FlexibleInstances #-}

--FlexibleInstances so we can make the complex function call of the formatter into an instance

--  Using let instead of where to see how it is differs.
module PrettyPrinter.Formatter1WithLet  (
        )where



import PrettyPrinter.PPInterface hiding (main) 
import PrettyPrinter.Utils



type Fit = Bool
type Space = Int
type Hp = Int
type Indent = Int
type Result = String -> String
type Width = Int
type F1WithLet = (Width -> Fit -> Space -> Hp -> Indent -> (Space, Hp, Result))



instance PrettyPrinter F1WithLet where
  noDoc          = \_ _ s h _ -> (s, h, id)
  text  t        = \_ _ s h _ -> (s-length t, h+length t, (t++))
  line           = \w f s h i -> let  (r, s') = newLine w f s i in (s', h+1, r)
                                 
  d1 <> d2       = \w f s h i -> 
                          let (sl, hl, rl) = d1 w f s h i
                              (sr, hr, rr) = d2 w f sl hl i
                          in (sr, hr, rl.rr)  
                                   
                        
  gr  d          = \w f s h i -> 
                           let
                                mep = h + s
                                v@(sd, hd, rd) = d w (hd <= mep) s h i
                                -- Craziness!!!  hd is on both side of the equality
                          in v
  nest j d       = \w f s h i -> d w f s h (i+j)

--- TODO::    EXPLAIN how the  hd works in the group


toF1Doc :: Tree -> F1WithLet
toF1Doc t = showTree t  
                
main w = let   (_, _, v) = (toF1Doc tree) w  False w 0 0
       in toSeq v

       


--ex = Tree "aaa" [Tree "bbbb" [Tree "ccc" [], Tree "dd" []]
--                , Tree "eee" []
--                , Tree "ffff" [ Tree "gg" [], Tree "hhhh" [], Tree "ii" []]
--                ]
--                
-- if we were to do the Deep embeding we would have got
--
-- Group (Text "aaa" :<> Nest 3
--   ((Text "[" :<> Nest 1 
--   (((Group (Text "bbbbb" :<> Nest 5 
--      ((Text "[" :<> Nest 1 
--      (((Group (Text "ccc" :<> Nest 3 NoDoc)
--      :<> Text ",") :<> Line) :<> 
--      Group (Text "dd" :<> Nest 2 NoDoc))) 
--      :<> Text "]")) 
--   :<> Text ",") :<> Line) :<> 
--   (((Group (Text "eee" :<> Nest 3 NoDoc) 
--   :<> Text ",") :<> Line) :<> 
--   Group (Text "ffff" :<> Nest 4 
--       ((Text "[" :<> Nest 1 
--       (((Group (Text "gg" :<> Nest 2 NoDoc) 
--       :<> Text ",") :<> Line) :<> 
--       (((Group (Text "hhh" :<> Nest 3 NoDoc)
--       :<> Text ",") :<> Line) :<> 
--       Group (Text "ii" :<> Nest 2 NoDoc))))
--       :<> Text "]")))))
--   :<> Text "]"))

-- *PrettyPrinter.Formatter1WithLet> main
-- "aaa[bbbbb[ccc,\n          dd],\n    eee,\n    ffff[gg, hhh, ii]]"      