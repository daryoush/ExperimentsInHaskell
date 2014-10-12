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
  
  
  
-- DSL View 
-- Tree is expressed using langauge of Nodes and children
-- ShowTree translates the langauge of Tree to langauge of PrettyPrinting
-- Doc is a data representation of the PrettyPrinting langauge for a possible DeepEmbedding DSL 
-- Evaluation
-- As is shown In other modules in this package, a Shallow Embeding of the pretty printing is 
-- also possible. 

data Tree = Node String [Tree]

showTree (Node s ts) = gr (text s <>
                               nest (length s) (showBracket ts))

showBracket [] = noDoc
showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees [t] = showTree t
showTrees (t:ts) = showTree t <> text "," <> line <> showTrees ts


tree = Node "a12" [ Node "b123" [ Node "c1234" [],
                                   Node "d1234" []
                    ],
                    Node "e123" [],
                    Node "f123" [ Node "g1234" [],
                                  Node "h1234" [],
                                  Node "i1234" []
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
--ex = Tree "a12" [Tree "b123" [Tree "c1234" [], Tree "d1234" []]
--                , Tree "e123" []
--                , Tree "f123" [ Tree "g234" [], Tree "h1234" [], Tree "i1234" []]
--                ]
--                
              
toNoFormatDoc :: Tree -> Doc
toNoFormatDoc t = showTree t


main = toNoFormatDoc tree


-- *PrettyPrinter.PPInterface> main
-- Group (Text "aaa" :<> Nest 3 ((Text "[" :<> Nest 1 (((Group (Text "bbbbb" :<> Nest 5 ((Text "[" :<> Nest 1 (((Group (Text "ccc" :<> Nest 3 NoDoc) :<> Text ",") :<> Line) :<> Group (Text "dd" :<> Nest 2 NoDoc))) :<> Text "]")) :<> Text ",") :<> Line) :<> (((Group (Text "eee" :<> Nest 3 NoDoc) :<> Text ",") :<> Line) :<> Group (Text "ffff" :<> Nest 4 ((Text "[" :<> Nest 1 (((Group (Text "gg" :<> Nest 2 NoDoc) :<> Text ",") :<> Line) :<> (((Group (Text "hhh" :<> Nest 3 NoDoc) :<> Text ",") :<> Line) :<> Group (Text "ii" :<> Nest 2 NoDoc)))) :<> Text "]"))))) :<> Text "]"))


--  Group (Text "aaa" :<> Nest 3 ((Text "[" :<> Nest 1 (((Group (Text "bbbbb" :<> Nest 5 ((Text "[" :<> Nest 1 (((Group (Text "ccc" :<> Nest 3 NoDoc) :<> Text ",") :<> Line) :<> Group (Text "dd" :<> Nest 2 NoDoc))) :<> Text "]")) :<> Text ",") :<> Line) :<> (((Group (Text "eee" :<> Nest 3 NoDoc) :<> Text ",") :<> Line) :<> Group (Text "ffff" :<> Nest 4 ((Text "[" :<> Nest 1 (((Group (Text "gg" :<> Nest 2 NoDoc) :<> Text ",") :<> Line) :<> (((Group (Text "hhh" :<> Nest 3 NoDoc) :<> Text ",") :<> Line) :<> Group (Text "ii" :<> Nest 2 NoDoc)))) :<> Text "]"))))) :<> Text "]"))
