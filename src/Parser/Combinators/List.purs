module Parser.Combinators.List where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (invalid, unV)
import Global (isFinite, isNaN, readFloat)
import Parser (Parser(..))
import Parser.Combinators ((|=))
import Parser.Combinators as C
import Parser.Error (class ParserError, fromString)

uncons :: ∀ eff e a. Semigroup e => ParserError e => Parser eff e (List a) a
uncons = C.uncons List.uncons

elementBy :: ∀ eff e a. Semigroup e => ParserError e => (a -> a -> Boolean) -> a -> Parser eff e (List a) Unit
elementBy eq a = void (uncons |= eq a)

element :: ∀ eff e a. Semigroup e => ParserError e => Eq a => a -> Parser eff e (List a) Unit
element = elementBy eq

untilBy :: ∀ eff e a. Semigroup e => ParserError e => (List a -> List a -> Boolean) -> (a -> Boolean) -> Parser eff e (List a) Unit
untilBy eq pred = void (manyBy eq (uncons |= not <<< pred))

until :: ∀ eff e a. Semigroup e => ParserError e => Eq a => (a -> Boolean) -> Parser eff e (List a) Unit
until = untilBy eq

empty :: ∀ eff e a. Semigroup e => ParserError e => Parser eff e (List a) Unit
empty =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    pure case str of
      List.Nil -> pure unit
      _ -> invalid (fromString "Expected empty array")

-- | Given an equality test on streams, parse many occurrences of a given parser.
-- | If the parser fails to consume the stream, the accumulated list will be returned.
-- | Consumption is just a check if the old stream is equivalent to the
-- | current stream, using the given equivalence function.
manyBy :: ∀ eff e s a. Semigroup e => (s -> s -> Boolean) -> Parser eff e s a -> Parser eff e s (List a)
manyBy eq (Parser p) =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    let
      go acc str' = do
        ea <- p ref
        str'' <- liftEff $ Ref.readRef ref
        if not (str' `eq` str'')
          then unV (\_ -> pure $ pure acc) (\a -> go (List.Cons a acc) str'') ea
          else pure $ pure acc
    go List.Nil str

someBy :: ∀ eff e s a. Semigroup e => (s -> s -> Boolean) -> Parser eff e s a -> Parser eff e s (NonEmptyList a)
someBy pred p = cons' <$> p <*> manyBy pred p
  where
  cons' x xs = NonEmptyList (x :| xs)

many :: ∀ eff e s a. Semigroup e => Eq s => Parser eff e s a -> Parser eff e s (List a)
many = manyBy eq

some :: ∀ eff e s a. Semigroup e => Eq s => Parser eff e s a -> Parser eff e s (NonEmptyList a)
some = someBy eq

int :: ∀ eff e. Semigroup e => ParserError e => Parser eff e (List String) Int
int =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    case List.uncons str of
      Just {head, tail} -> do
        liftEff $ Ref.writeRef ref tail
        pure case Int.fromString head of
          Just n -> pure n
          _ -> invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

number :: ∀ eff e. Semigroup e => ParserError e => Parser eff e (List String) Number
number =
  Parser \ref -> do
    str <- liftEff $ Ref.readRef ref
    case List.uncons str of
      Just {head, tail} -> do
        liftEff $ Ref.writeRef ref tail
        let
          n = readFloat head
        pure if isNaN n || not (isFinite n)
          then pure n
          else invalid (fromString "Could not parse string as an int")
      _ -> pure (invalid (fromString "Empty stream"))

pair :: ∀ eff e a. Semigroup e => ParserError e => Parser eff e (List a) (Tuple a a)
pair = Tuple <$> uncons <*> uncons
