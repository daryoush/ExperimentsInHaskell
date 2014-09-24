
{-# OPTIONS_HADDOCK  ignore-exports #-}
{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
              CPP  #-}

-- | This module contains a lot of examples of the typical use of our parser combinator library. 
--   We strongly encourage you to take a look at the source code.
--   At the end you find a @`main`@ function which demonstrates the main characteristics. 
--   Only the @`run`@ function is exported since it may come in handy elsewhere.

module Parser.Coindemo  where
import Data.Char
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import System.IO
import GHC.IO.Handle.Types
import qualified Data.ListLike as LL
import Data.Monoid


-- * The Stream data type
-- | The data type `Str` holds the input data to be parsed, the current location, the error messages generated 

type Parser a = P (Str Int (Sum Int) Int) a


instance   IsLocationUpdatedBy Int Int where
--    advance :: loc -- ^ The current position
--            -> str -- ^ The part which has been removed from the input
--            -> loc
     advance = undefined
 
instance LL.FoldableLL (Sum Int) Int where         
            
instance LL.ListLike (Sum Int) Int where
      
p1  ::Parser [Int] 
p1  = lift <$> pSym 1

lift a = [a]