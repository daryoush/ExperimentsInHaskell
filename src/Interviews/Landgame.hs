module Interviews.Landgame where

import Control.Monad
import Data.List

type  Point = (Int, Int)
type Rect = (Point, Point)



findLargestEmptyArea s t = head.sortBy compareArea $ findEmptyAreas s t
                           where 
                             compareArea (r1, a1) (r2, a2) = compare a2 a1
                             findEmptyAreas s t= fmap recAndArea  $ findRects s t
                             recAndArea r@((xl, yl), (xh, yh)) = (r, (xh-xl) * (yh -yl)) 
                             findRects sq trees = foldM applyTree sq trees
                             applyTree r@((xl, yl), (xh, yh) ) (xt, yt) 
                                 | xt < xl ||  yt < yl || xt > xh || yt > yh = [r]
                                 | otherwise = above ++ left ++ right ++  bottom
                                 where 
                                   above = if yt < yh then [((xl, yt + 1), (xh, yh))] else []
                                   left = if xt > xl then [((xl, yl), (xt -1, yh))] else []
                                   right = if xt < xh then [((xt+1, yl), (xh, yh))] else []
                                   bottom = if  yt > yl then [((xl,yl), (xh, yt -1))] else []

main = print $  findLargestEmptyArea ((0,0), (10,10)) [(1,1), (2,3), (10,10), (11, 11)]