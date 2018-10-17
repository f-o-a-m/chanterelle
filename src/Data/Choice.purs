module Data.Choice where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Semigroup.Foldable (class Foldable1, fold1, foldMap1)

data Choice a
  = Finally a
  | Or a (Choice a)

infixr 6 Or as <<|>>

derive instance functorChoice :: Functor Choice

instance foldableChoice :: Foldable Choice where
  foldr a b c = foldrDefault a b c
  foldl a b c = foldlDefault a b c
  foldMap f =
    case _ of
      Finally a -> f a
      a <<|>> that -> f a <> foldMap f that

instance foldable1Choice :: Foldable1 Choice where
  fold1 =
    case _ of
      Finally a -> a
      a <<|>> that -> a <> fold1 that
  foldMap1 f =
    case _ of
      Finally a -> f a
      a <<|>> that -> f a <> foldMap1 f that
