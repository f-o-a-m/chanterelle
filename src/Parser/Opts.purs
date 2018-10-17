module Parser.Opts where

import Prelude

import Control.Alt ((<|>))
import Control.Applicative.Free.Final (Ap, liftAp, retractAp, runAp_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref as Ref
import Data.Array (elem)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (length, minimumBy)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, un)
import Data.Semigroup.Foldable (foldMap1, for1_)
import Node.Process (PROCESS, argv)
import Data.Choice (Choice)
import Parser (Parser, exec)
import Parser.Combinators (stream, (<?>))
import Parser.Combinators.Array as Array
import Parser.Error (class ParserError)
import Partial.Unsafe (unsafeCrashWith)

newtype Error = Error String

instance parserError :: ParserError Error where
  fromString s = Error s

newtype Opt eff a
  = Opt
  { name :: Array String
  , description :: Array String
  , parser :: Parser eff (List Error) (Array String) a
  }

derive instance newtypeOpt :: Newtype (Opt eff a) _

derive instance functorOpt :: Functor (Opt eff)

instance applyOpt :: Apply (Opt eff) where
  apply (Opt f) (Opt a) =
    Opt a
      { name = f.name <> a.name
      , description = f.description <> a.description
      , parser = f.parser <*> a.parser
      }

instance applicativeOpt :: Applicative (Opt eff) where
  pure x = Opt {name: [], description: [], parser: pure x}

type Option eff = Ap (Opt eff)

type OptInfo = {name :: String, description :: String}

string :: ∀ eff. OptInfo -> Option eff String
string a = mkOpt a parser
  where
  parser = Array.element a.name *> Array.uncons

int :: ∀ eff. OptInfo -> Option eff Int
int a = mkOpt a p
  where
  p = Array.until (eq a.name) *> Array.int

command :: ∀ eff. OptInfo -> Option eff String
command a = mkOpt a (parser <?> pure (Error $ "Couldn't match command " <> a.name))
  where
  parser = Array.element a.name *> pure a.name

flag :: ∀ eff. OptInfo -> Option eff Boolean
flag a = mkOpt a' parser
  where
  a' = a {name = "--" <> a.name}
  parser = (elem a'.name) <$> stream

flag' :: ∀ eff. OptInfo -> Option eff Boolean
flag' a = mkOpt a' parser
  where
  a' = a {name = "--" <> a.name}
  parser = true <$ Array.element a'.name <|> pure false

analyze :: ∀ eff b m. Monoid m => (∀ a. Opt eff a -> m) -> Choice (Option eff b) -> m
analyze f = foldMap1 (runAp_ f)

mkOpt :: ∀ eff a. OptInfo -> Parser eff (List Error) (Array String) a -> Option eff a
mkOpt {name, description} parser = liftAp (Opt {name: [name], description: [description], parser})

runOpts :: ∀ eff a. Choice (Option (process :: PROCESS | eff) a) -> (Either (List Error) a -> Eff (ref :: Ref.REF, process :: PROCESS | eff) Unit) -> Eff (ref :: Ref.REF, process :: PROCESS | eff) Unit
runOpts opts cb = do
  let completionNumber = length opts
  resultRef <- Ref.newRef {errors: [], successes: [], completed: 0}
  args <- A.drop 2 <$> argv

  for1_ opts \option -> do
    let
      opt = retractAp option
      optRec = un Opt opt
    exec optRec.parser args
      \result remainder -> case result of
        Left errs ->
          Ref.modifyRef resultRef
            \res -> res {errors = [{errs, remainder}] <> res.errors, completed = 1 + res.completed}
        Right succ ->
          Ref.modifyRef resultRef
            \res -> res {successes = [{succ, remainder}] <> res.successes, completed = 1 + res.completed}

  let
    go = do
      {errors, successes, completed} <- Ref.readRef resultRef
      if completionNumber == completed -- All our parsers have run
        then
          let
            winnerGetter :: ∀ b r. Array {remainder :: Array b | r} -> Maybe {remainder :: Array b | r}
            winnerGetter = minimumBy (comparing (A.length <<< _.remainder))
            successWinner = winnerGetter successes
            failureWinner = winnerGetter errors
          in
            case successWinner of
              Just {succ} ->
                cb (Right succ)
              _ ->
                case failureWinner of
                  Just {errs} -> cb (Left errs)
                  _ -> unsafeCrashWith "The impossible! No error winner, no success winner."
        else go

  go
