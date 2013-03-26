module BlockMaze.Area where


-- THIS IS JUST SCRATCH CODE TO TRY OUT SOME IDEAS

import System.IO
import Data.List.Split
import Debug.Trace
import Data.Map as Map
import Control.Monad

type Point = (Integer, Integer)

data Line =  L {start::Point, curEnd::Point, points::[Point]} deriving Show


main = do 
        rawMapData <- readFile "src/BlockMaze/data2.txt"    
        let results = zip [0..] $ Prelude.map (indexes '1') (splitOn  "\n" rawMapData) -- get 1s on each row with line number
            adjLines = zip results (tail results) -- all rows
            connections = adjLines >>= segments  -- all segments between two adj rows
            connectionsAsMayKeyValue = connections >>= (\(x,y) -> [ ( x, [y]),  (y,[x]) ]) -- add both direction
            connectionMap = Map.fromListWith (++)  connectionsAsMayKeyValue
            -- TODO FOLD THE FUNCTION TO BUILD LIST OF LINES FROM EACH SEGMENT
            ls =  foldM foldmf []  $ toList connectionMap
            in   print rawMapData --  results -- ls  -- $ toList connectionMap
            
foldmf :: [Line] -> ((Integer, Integer), [(Integer, Integer)]) -> IO [Line]
foldmf x y | trace ( "x: " ++ (show x) ++ " y: " ++ (show y) ) False = undefined
foldmf [] (x,ys) = return $ Prelude.map (\z -> L x z [z, x]) ys
foldmf ls (x,ys) =  return $ ls >>= (addSegmentToLine x ys)

-- foldM f a1 [x1, x2, ..., xm]
-- ==main

--
--       do
--         a2 <- f a1 x1
--         a3 <- f a2 x2main
--         ...
--         f am xm
 
 
-- xs are of the form :   ((1,5),[(2,6),(2,4)])


--dd = Nde ((1,2), [(2,3), (3,4)])


--foldFunc start lines  [] = lines        
--foldFunc start []  ends = Prelude.map (\x -> L start x [x]) ends
--foldFunc start lines  ends = lines >>= (addSegmentToLine start ends)   -- FOR NOW IF THE END POINTS ARE THE SAME IT WOULD EXTEND THE LINE and GENERATE NEW LINES, LATER CHECK TO MAKE SURE LINE IS NOT TERMINATED AND END POOINT IS NOT ALREADY ON THE LIST
          
       
addSegmentToLine  x ys ls@(L s e p) | (x==e && e /= (-1,-1))  = Prelude.map (\y -> if y `elem` p then (L s (-1,-1) ((-1,-1):y:p)) else (L s y (y:p)) ) ys
                                    | otherwise = [ls]
                                 
lineContainsPoint p   = elem p.points

addEndPoint (L s _ ps) p =  L s p (p:ps)
 
-- ((1,[0,5,8,12,16,24]),(2,[1,4,6,7,11,13,17,23]))

segments (_, (_,[])) = []
segments ((i,xs), (j,ys)) = [((i,x), (j,y)) | x <- xs, y <-ys, x == y +1 || x== y - 1]


indexes c line = 
        let 
                --loop l r i | trace ("list " ++ show(l) ++ " res: " ++ (show r) ++ " index: " ++ (show i) ) False = undefined 
                loop [] res _= res
                loop (x:xs) res i  | x == c  = loop xs (res++[i]) (i+1)
                                   | otherwise = loop xs res (i+1)
                in loop line [] 0
        