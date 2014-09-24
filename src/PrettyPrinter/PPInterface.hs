module PrettyPrinter.PPInterface where

import Data.List

        
class PrettyPrinter f where
  noDoc :: f
  text  :: String -> f
  line  :: f
  (<>)  :: f -> f -> f
  gr :: f -> f
  nest  :: Int -> f -> f
  
  
-- A simple case of data capturing the pretty print instance
data Doc = NoDoc                -- Unit element for '<>'
        | Text String           -- Piece of text to be included in Document
        | Line                  -- either a line break or ' '
        | Doc :<> Doc          -- concatnation of two documnets
        | Group Doc             -- Group indicates that either all Lines in the Doc are 
                                -- printed as line-break or ' '
        | Nest Int Doc          -- indenet all lines by Int spaces
        deriving Show
        
  
instance PrettyPrinter Doc where
  noDoc = NoDoc
  text  = Text
  line  = Line
  (<>)    =  (:<>)
  gr = Group
  nest  = Nest
  
  
data Tree = Node String [Tree]

showTree (Node s ts) = gr (text s <>
                               nest (length s) (showBracket ts))

showBracket [] = noDoc
showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees [t] = showTree t
showTrees (t:ts) = showTree t <> text "," <> line <> showTrees ts


tree = Node "aaa" [ Node "bbbbb" [ Node "ccc" [],
                                   Node "dd" []
                    ],
                    Node "eee" [],
                    Node "ffff" [ Node "gg" [],
                                  Node "hhh" [],
                                  Node "ii" []
                    ]
       ]
  
  
--  From Swierstra Paper, the intersperse doesn't work here, instead use the 
-- above code from Philip Wadler paper
-- http://www.cs.ox.ac.uk/publications/books/fop/dist/fop/chapters/11/Tree.hs
--data Tree = Tree String [Tree]
--
--showTree :: PrettyPrinter f => Tree -> f
--showTree (Tree s ts) =  undefined -- gr ( text s <> nest (length s) (showBracket ts))
-- 
--showBracket :: PrettyPrinter f => [Tree] -> f          
--showBracket [] =  noDoc
--showBracket ts =  text "[" <> nest 1 (showTrees ts) <> text "]"
-- 
--showTrees :: PrettyPrinter f => [Tree] -> f               
--showTrees t  =  foldr (<>) noDoc (intersperse (text "," <> line) t)
--
--
--ex = Tree "aaa" [Tree "bbbb" [Tree "ccc" [], Tree "dd" []]
--                , Tree "eee" []
--                , Tree "ffff" [ Tree "gg" [], Tree "hhhh" [], Tree "ii" []]
--                ]
--                
              
toNoFormatDoc :: Tree -> Doc
toNoFormatDoc t = showTree t


main = toNoFormatDoc tree
