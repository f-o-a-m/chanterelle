module Chanterelle.Genesis 
    ( runGenesisGenerator
    ) where

import Prelude (Unit, bind, show, void, ($), (<<<), (<>), (>>=))
import Chanterelle.Project (loadProject)
import Chanterelle.Internal.Genesis (generateGenesis)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (GenesisGenerationError(MalformedProjectErrorG), logGenesisGenerationError)
import Chanterelle.Internal.Utils (jsonStringifyWithSpaces)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Error.Class (try)
import Data.Argonaut as A
import Data.Either (Either(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process (PROCESS)
import Node.Process as P

runGenesisGenerator :: forall e. FilePath -> FilePath -> Eff (console :: CONSOLE, fs :: FS, now :: NOW, process :: PROCESS | e) Unit 
runGenesisGenerator genesisIn genesisOut = do
    root <- liftEff P.cwd
    void <<< launchAff $
      (try $ loadProject root) >>= case _ of
        Left err -> liftAff <<< logGenesisGenerationError $ MalformedProjectErrorG (message err)
        Right project -> (liftAff $ generateGenesis project genesisIn) >>= case _ of
            Right gb -> do
                let strungGb = jsonStringifyWithSpaces 4 (A.encodeJson gb)
                try (FS.writeTextFile UTF8 genesisOut strungGb) >>= case _ of
                    Left err -> log Error $ "Couldn't write genesis block to " <> show genesisOut <> ": " <> show err
                    Right _  -> log Info $ "Successfully wrote generated genesis block to " <> show genesisOut
            Left err -> liftAff $ logGenesisGenerationError err