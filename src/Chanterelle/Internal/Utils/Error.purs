module Chanterelle.Internal.Utils.Error where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Except.Trans (ExceptT, runExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)

catchingAff
  :: forall m a
   . MonadAff m
  => MonadThrow String m
  => Aff a
  -> m a
catchingAff = withExceptM' show <<< liftAff <<< try

except'
  :: forall m e a
   . MonadThrow e m
  => Either e a
  -> m a
except' = either throwError pure

withExcept'
  :: forall m e e' a
   . MonadThrow e' m
  => (e -> e')
  -> Either e a
  -> m a
withExcept' f = except' <<< lmap f

exceptM'
  :: forall m e a
   . MonadThrow e m
  => m (Either e a)
  -> m a
exceptM' m = m >>= except'

withExceptM'
  :: forall m e e' a
   . MonadThrow e' m
  => (e -> e')
  -> m (Either e a)
  -> m a
withExceptM' f m = map (lmap f) m >>= except'

withExceptT'
  :: forall m e e' a
   . MonadThrow e' m
  => (e -> e')
  -> ExceptT e m a
  -> m a
withExceptT' e m = runExceptT (withExceptT e m) >>= except'

exceptNoteA'
  :: forall m e a
   . MonadThrow e m
  => m (Maybe a)
  -> e
  -> m a
exceptNoteA' a e = a >>= flip exceptNoteM' e

infixl 9 exceptNoteA' as !?

exceptNoteM'
  :: forall m e a
   . MonadThrow e m
  => Maybe a
  -> e
  -> m a
exceptNoteM' a e = maybe (throwError e) pure a

infixl 9 exceptNoteM' as ??

eitherM
  :: forall m e a
   . Monad m
  => (e -> m a)
  -> ExceptT e m a
  -> m a
eitherM f m = runExceptT m >>= either f pure

eitherM_
  :: forall m e a b
   . Monad m
  => (e -> m b)
  -> ExceptT e m a
  -> m Unit
eitherM_ f m = eitherM (void <<< f) (void m)
