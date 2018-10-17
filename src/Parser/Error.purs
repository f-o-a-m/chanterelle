module Parser.Error where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Lazy as L
import Data.List.Lazy.Types as NELL
import Data.List.Types as NEL
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple)

class ParserError e where
  fromString :: String -> e

instance parserErrorString :: ParserError String where
  fromString = id

instance parserErrorList :: ParserError e => ParserError (List e) where
  fromString = applicativeParserError

instance parserErrorLList :: ParserError e => ParserError (L.List e) where
  fromString = applicativeParserError

instance parserErrorArray :: ParserError e => ParserError (Array e) where
  fromString = applicativeParserError

instance parserErrorNEList :: ParserError e => ParserError (NEL.NonEmptyList e) where
  fromString = applicativeParserError

instance parserErrorNELList :: ParserError e => ParserError (NELL.NonEmptyList e) where
  fromString = applicativeParserError

instance parserErrorNEArray :: ParserError e => ParserError (NEA.NonEmptyArray e) where
  fromString = applicativeParserError

instance parserErrorTuple :: (Monoid a, ParserError b) => ParserError (Tuple a b) where
  fromString = applicativeParserError

instance parserErrorEither :: ParserError b => ParserError (Either a b) where
  fromString = applicativeParserError

instance parserErrorFunction :: ParserError b => ParserError ((->) a b) where
  fromString = applicativeParserError

instance parserErrorExcept :: ParserError b => ParserError (ExceptT e Identity b) where
  fromString = applicativeParserError

instance parserErrorUnit :: ParserError Unit where
  fromString _ = unit

applicativeParserError :: ∀ f e. Applicative f => ParserError e => String -> f e
applicativeParserError = pure <<< fromString
