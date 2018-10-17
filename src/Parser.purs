module Parser
  ( Parser(..)
  , parse
  , exec
  , withError
  , withState
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Plus (class Plus)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (V, unV, invalid)
import Parser.Error (class ParserError, fromString)

newtype Parser eff e s a =
  Parser
    (Ref s -> ContT Unit (Eff (ref :: REF | eff)) (V e a))

derive instance newtypeParser :: Newtype (Parser eff e s a) _

instance functorParser :: Semigroup e => Functor (Parser eff e s) where
  map = liftA1

instance applyParser :: Semigroup e => Apply (Parser eff e s) where
  apply (Parser pab) (Parser pa) =
    Parser \ref -> do
      ab <- pab ref
      a <- pa ref
      pure $ ab <*> a

instance applicativeParser :: Semigroup e => Applicative (Parser eff e s) where
  pure x = Parser \_ -> ContT (_ $ pure x)

-- | The `Alt` instance runs two parsers in parallel. If the first succeeds, its result is
-- | used and the second's is discarded. If the first fails, the second one is used.
-- | If the first hasn't completed by the time the second one has, the second waits until
-- | it knows how to act.
instance altParser :: Semigroup e => Alt (Parser eff e s) where
  alt (Parser pa) (Parser pb) =
    Parser \ref ->
      ContT \cb -> do
        -- Copy the stream so we're not concurrently mutating the same stream
        stream <- Ref.readRef ref
        ref' <- Ref.newRef stream

        -- Set the first parser's success state to "indeterminate"
        successRef <- Ref.newRef Nothing

        runContT (pa ref) \ea ->
          unV
            (\_ -> do
              Ref.writeRef successRef (Just false)
            )
            (\_ -> do
              Ref.writeRef successRef (Just true)
              cb ea
            )
            ea

        runContT (pb ref') \eb ->
          let
            go = do
              done <- Ref.readRef successRef
              case done of
                Just false -> cb eb
                Just true -> mempty
                _ -> go
          in go

instance plusParser :: (Semigroup e, ParserError e) => Plus (Parser eff e s) where
  empty = Parser \_ -> ContT (_ $ invalid (fromString "empty called"))

instance alternativeParser :: (Semigroup e, ParserError e) => Alternative (Parser eff e s)

instance semigroupParser :: (Semigroup e, Semigroup a) => Semigroup (Parser eff e s a) where
  append = lift2 append

instance monoidParser :: (Semigroup e, Monoid a) => Monoid (Parser eff e s a) where
  mempty = pure mempty

-- | Run a parser given an initial stream and a continuation.
parse :: ∀ eff e s a. Parser eff e s a -> s -> (Either e a -> Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit
parse p stream cb = exec p stream \ea _ -> cb ea

exec :: ∀ eff e s a. Parser eff e s a -> s -> (Either e a -> s -> Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit
exec (Parser p) stream cb = do
  streamRef <- Ref.newRef stream
  runContT (p streamRef) \ea -> do
    str <- Ref.readRef streamRef
    cb (unV Left Right ea) str

withError :: ∀ eff e e' s a. (e -> e') -> Parser eff e s a -> Parser eff e' s a
withError f (Parser pa) = Parser (map (lmap f) <<< pa)

withState :: ∀ eff e s a. (s -> s) -> Parser eff e s a -> Parser eff e s a
withState f (Parser p) =
  Parser \ref -> do
    liftEff $ Ref.modifyRef ref f
    p ref
