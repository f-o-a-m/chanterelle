module Main where

import Prelude

import Deploy (deployContracts)
import Chanterelle.Deploy (deploy)
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ void $ deploy nodeUrl 60 deployContracts
  where
  nodeUrl = "http://localhost:8545"
