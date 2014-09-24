module Circuits.FlipFlop where


nand x y = not $! (x   &&   y) 

dff d clk q1 = let a = nand d clk
                   b = nand a clk
                   q = nand (not q1)  a
                   qn= nand q1 b
                   in (d, clk, q, qn)
                      