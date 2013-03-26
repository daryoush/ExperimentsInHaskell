> module BlockMaze.Solution where

> import Data.List.Split



Problem: 

http://thenextweb.com/google/2013/03/20/google-engineer-discovers-that-many-11th-graders-in-vietnam-could-pass-the-google-interview-process/


Given a data file describing a maze with diagonal walls, count the number of enclosed areas, and measure the size of the largest one.

So the goal is to write  a function that if applied to the datafile gives us 
     (number of enclosed areas, size of the largest one)
     
How can we break down the problem?

EXPAIN PROCESS HERE.
ALGERBRA OF THE LINES AND CONNECTION  -- MAY BE CHAIN  BORDER
LONGEST LINE 


We need to build lines from a given node
extend the line as far as possible
detect if line is in a loop (stop the process of extending the line)
determine if a line enciricles an area 
calculate the area of the encricled areas.

What is calclulation that can take points and keep extending it to its longest path?    Mutiple folds?   

Select starting points for each do a non deterministic fold until end criteria is met
so there is a filterM to determine when a continuation should continue?

ones : list of 1s on each line.  Each line is a line number and list of offsets of 1st

> type Line = (Int, [Int])
> type Point = (Integer, Integer)

> ones s = zip [0..] $ Prelude.map (indexes '1') (splitOn  "\n" s) -- get 1s on each row with line number

> indexes c line = 
>        let 
>                --loop l r i | trace ("list " ++ show(l) ++ " res: " ++ (show r) ++ " index: " ++ (show i) ) False = undefined 
>                loop [] res _= res
>                loop (x:xs) res i  | x == c  = loop xs (res++[i]) (i+1)
>                                   | otherwise = loop xs res (i+1)
>
>                in loop line [] 0



From points on two adjacent lines figure out the connections segments


-- ((1,[0,5,8,12,16,24]),(2,[1,4,6,7,11,13,17,23]))

> segments (_, (_,[])) = []
> segments ((l1,l1xs), (l2,l2xs)) = [((l1,l1x), (l2,l2x)) | l1x <- l1xs, l2x <-l2xs, l1x == l2x +1 || l1x== l2x - 1]


connections are the direct connections between two points on two different lines.  In this problem connectins are only between
adjacent lines.  Each connection has a unique id (connections are not directional, with id we can determine equality quicky

> data Conn = C { cid::Int, e1::Point, e2::Point} deriving Show

> instance Eq Conn where
>       (==) = \a b -> cid a == cid b  -- two connections are equal if they have same id

> testData = "10010\n" ++
>            "01101\n" ++
>            "00100\n" ++
>            "01010\n" ++
>            "00100"   

> mazeSolution s = 
>       let ones  = zip [0..] $ Prelude.map (indexes '1') (splitOn  "\n" s) -- get 1s on each row with line number
>           adjLines = zip ones (tail ones) -- zip each row with its neighbor
>           segs = adjLines >>= segments  -- all segments between two adj rows
>           cons =  zipWith (\id (p1, p2)  -> (C id p1 p2))  [1..] segs 
>           in do
>               print s  
>               print $ ones
>               print segs
>               print cons
>              


> main = mazeSolution testData