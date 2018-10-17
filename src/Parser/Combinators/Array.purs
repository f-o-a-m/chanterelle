module Parser.Combinators.Array where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (invalid, unV)
import Global (isFinite, isNaN, readFloat)
import Parser (Parser(..))
import Parser.Combinators ((|=))
import Parser.Combinators as C
import Parser.Error (class ParserError, fromString)

uncons :: ∀ eff e a. Semigroup e => ParserError e => Parser eff e (Array a) a
uncons = C.uncons Array.uncons

elementBy :: ∀ eff e a. Semigroup e => ParserError e => (a -> a -> Boolean) -> a -> Parser eff e (Array a) Unit
elementBy eq a = void (uncons |= eq a)

element :: ∀ eff e a. Semigroup e => ParserError e => Eq a => a -> Parser eff e (Array a) Unit
element = elementBy eq

untilBy :: ∀ eff e a. Semigroup e => ParserError e => (Array a -> Array a -> Boolean) -> (a -> Boolean) -> Parser eff e (Array a) Unit
untilBy eq pred = void (manyBy eq (uncons |= not <<< pred))

until :: ∀ eff e a. Semigroup e => ParserError e => Eq a => (a -> Boolean) -> Parser eff e (Array a) Unit
until = untilBy eq

empty :: ∀ eff e a. Semigroup e => ParserError e => Parser eff e (Array a) Unit
empty =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    pure case str of
      [] -> pure unit
      _ -> invalid (fromString "Expected empty array")

-- | Given an equality test on streams, parse many occurrences of a given parser.
-- | If the parser fails to consume the stream, the accumulated list will be returned.
-- | Consumption is just a check if the old stream is equivalent to the
-- | current stream, using the given equivalence function.
manyBy :: ∀ eff e s a. Semigroup e => (s -> s -> Boolean) -> Parser eff e s a -> Parser eff e s (Array a)
manyBy eq (Parser p) =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    let
      go acc str' = do
        ea <- p ref
        str'' <- liftEff $ Ref.readRef ref
        if not (str' `eq` str'')
          then unV (\_ -> pure $ pure acc) (\a -> go (Array.cons a acc) str'') ea
          else pure $ pure acc
    go [] str

someBy :: ∀ eff e s a. Semigroup e => (s -> s -> Boolean) -> Parser eff e s a -> Parser eff e s (NonEmptyArray a)
someBy pred p = NEArray.cons' <$> p <*> manyBy pred p

many :: ∀ eff e s a. Semigroup e => Eq s => Parser eff e s a -> Parser eff e s (Array a)
many = manyBy eq

some :: ∀ eff e s a. Semigroup e => Eq s => Parser eff e s a -> Parser eff e s (NonEmptyArray a)
some = someBy eq

int :: ∀ eff e. Semigroup e => ParserError e => Parser eff e (Array String) Int
int =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    case Array.uncons str of
      Just {head, tail} -> do
        liftEff $ Ref.writeRef ref tail
        pure case Int.fromString head of
          Just n -> pure n
          _ -> invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

number :: ∀ eff e. Semigroup e => ParserError e => Parser eff e (Array String) Number
number =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    case Array.uncons str of
      Just {head, tail} -> do
        liftEff $ Ref.writeRef ref tail
        let
          n = readFloat head
        pure if isNaN n || not (isFinite n)
          then pure n
          else invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

pair :: ∀ eff e a. Semigroup e => ParserError e => Parser eff e (Array a) (Tuple a a)
pair = Tuple <$> uncons <*> uncons
