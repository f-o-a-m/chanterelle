-- | too lazy to fork https://github.com/joshuahhh/purescript-mkdirp and PR to update it
-- | so I just vendored it instead LOL
module Node.FS.Sync.Mkdirp (mkdirp) where

import Prelude
import Control.Monad.Eff (Eff)
import Node.FS (FS)


foreign import mkdirp
  ∷ ∀ eff
  . String
  → Eff (fs :: FS | eff) Unit