module BreadthFirst where

-- This code is was in my notes from Utrecht Functional Programming Summer School 
-- The code  uses tie-knot concept to lable the tree in breadth first order.
-- Code is by  Doaitse Swierstra  I made some changes to it (fixing bugs, I hope)

data Tree = Leaf | Node Tree Int Tree deriving Show

lable (Node ln _ rn) ((h:r):rest) = (Node lr h rr, r:r2) where
                                        (lr, r1) = lable ln rest
                                        (rr, r2) = lable rn r1

lable _ _  = (Leaf, [])

lt t = let (r, unused) = lable t ([1..]:unused)
       in (r, unused)

test1 = lt Leaf
test2 = lt  $ Node Leaf 0 Leaf  -- (Node (Node(Leaf 0 Leaf)) 0 Leaf)
test3 = lt $ Node (Node Leaf 0 Leaf) 0 Leaf
test4 = lt $ Node (Node Leaf 0 Leaf) 0 (Node Leaf 0 Leaf)

test5 = lt $ Node (Node (Node Leaf 0 Leaf) 0 (Node Leaf 0 Leaf)) 0 (Node Leaf 0 Leaf)

test6 = lt (Node (Node (Node (Node Leaf 0 Leaf) 0  (Node Leaf 0 Leaf)) 0 (Node Leaf 0 Leaf)) 0 Leaf)

test7 = lt $ Node (Node (Node Leaf 0 Leaf) 0  (Node Leaf 0 Leaf) 0 Leaf
