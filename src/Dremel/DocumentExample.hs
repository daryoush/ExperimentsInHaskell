module Dremel.DocumentExample where


import Data.Maybe

-- This module is trying to experiment with the exmaple instance of message defined in the 
-- Dremel paper.   Follow on work will try to implement computationsthat would genereate the functions
-- that are handcode for the message example here. 

-- Example type is 
-- message Document {
--    required int64 DocId;
--    optional group Links {
--      repeated int64 Backward;
--      repeated int64 Forward;
--    }
--    required group Name {
--      repeated group Language {
--              required Strng Code;
--              optional String Country;
--      }
--      optional String URL;
--    }
-- }
--    

-- Conversation rules:
--   group x -> newtype
--   required -> no tag   (note may be later define a type that has default value)
--   repeated x -> [x]  -- empty list if no repetition
--   optional x -> Maybe x
--   message -> tuple of all top level fields
--   primitive type x name = type name = x



type Document = ( DocId, Maybe Links, [Name])
type DocId = Int
type Links = ([Backward], [Forward])
type Langauge =  (Code,Maybe Country)
type Name =  ([Langauge],URL)
type Backward = Int
type Forward = Int
type URL = Maybe String
type Code = String
type Country = String


r1 :: Document
r1 = (10
        , Just ([], [20,40,60])
        , [ 
             ([ ("en-us", Just "us"), ("en", Nothing)], Just "http://A")
             , ([], Just "http://B")
             , ([("en-gb", Just "gb")], Nothing)
          ]
     )
        
r2 :: Document
r2 = (20
        , Just ([10, 30], [80])
        , [([], Just "http://C")]
      )

