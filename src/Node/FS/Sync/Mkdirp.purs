module Node.FS.Sync.Mkdirp (mkdirp) where

import Prelude
import Control.Monad.Eff (Eff)
import Node.FS (FS)


foreign import mkdirp
  ∷ ∀ eff
  . String
  → Eff (fs :: FS | eff) Unit