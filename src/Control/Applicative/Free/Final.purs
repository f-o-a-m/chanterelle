module Control.Applicative.Free.Final where

import Prelude

import Data.Const (Const(..))
import Data.Monoid (class Monoid)
import Data.Newtype (un)

newtype Ap f a = Ap (∀ g. Applicative g => (f ~> g) -> g a)

instance functorAp :: Functor (Ap f) where
  map f (Ap a) = Ap \nt -> map f (a nt)

instance applyAp :: Apply (Ap f) where
  apply (Ap f) (Ap a) = Ap \nt -> f nt <*> a nt

instance applicativeAp :: Applicative (Ap f) where
  pure x = Ap \_ -> pure x

runAp :: ∀ f g. Applicative g => (f ~> g) -> Ap f ~> g
runAp nt (Ap a) = a nt

runAp_ :: ∀ m f b. Monoid m => (∀ a. f a -> m) -> Ap f b -> m
runAp_ f = un Const <<< runAp (Const <<< f)

liftAp :: ∀ f. f ~> Ap f
liftAp fa = Ap \nt -> nt fa

hoistAp :: ∀ f g. (f ~> g) -> Ap f ~> Ap g
hoistAp nt (Ap f) = Ap \nt' -> f (nt' <<< nt)

retractAp :: ∀ f. Applicative f => Ap f ~> f
retractAp (Ap nt) = nt id
