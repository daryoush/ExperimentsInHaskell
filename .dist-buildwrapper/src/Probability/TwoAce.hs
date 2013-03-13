module Probability.TwoAce where


x=[1..13]
suits = ["H", "S", "D", "C"]
cards = [(s, n) | n <- x, s <- suits] 


twocards  (x:xs) = let firstpair = [(x, y) | y <- xs]
                       rest = twocards xs
                   in firstpair ++ rest
twocards (x:[]) = []   
twocards _      = []      

allTwocards = twocards cards           

atleastoneace = [hand | hand@((s1, n1),(s2, n2)) <- allTwocards, (n1 == 1 || n2 == 1) ]

bothace = [hand | hand@((s1, n1),(s2, n2)) <- allTwocards, (n1 == 1 && n2 == 1) ]


test = twocards cards

main::IO()
main =  do
                putStrLn "Hello"
                ; putStrLn "World"
