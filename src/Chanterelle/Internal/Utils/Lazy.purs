module Chanterelle.Internal.Utils.Lazy where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Data.Array (snoc, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

firstSuccess :: forall m a e b
              . MonadAff m
             => Array a
             -> (a -> m (Either e b))
             -> m (Either { failures :: Array (Tuple a e) } { result :: b, input :: a,  failures :: Array (Tuple a e) })
firstSuccess arr runner = go arr []
  where go arr' failures = case uncons arr' of
          Nothing -> pure $ Left { failures }
          Just { head, tail } -> runner head >>= case _ of
            Left e -> go tail (snoc failures (Tuple head e))
            Right result -> pure $ Right { result, input: head, failures }