module PrettyPrinter.Formatter1 (
        )where


import PrettyPrinter.PPInterface hiding (main) 

type Fit = Bool
type Space = Int
type Hp = Int
type Indent = Int
type Result = String -> String

newtype Formatter1 = F1 ( Fit -> Space -> Hp -> Indent -> (Space, Hp, Result))



instance PrettyPrinter Formatter1 where
  noDoc = undefined
  text  = undefined
  line  = undefined
  (<>)    =  undefined
  gr = undefined
  nest  = undefined

main = show ("Hello")
