module Chanterelle.Genesis
    ( runGenesisGenerator
    ) where

import Chanterelle.Internal.Genesis (generateGenesis)
import Chanterelle.Internal.Logging (LogLevel(..), log, logGenesisGenerationError)
import Chanterelle.Internal.Types.Genesis (GenesisGenerationError(MalformedProjectErrorG))
import Chanterelle.Internal.Utils.Json (jsonStringifyWithSpaces)
import Chanterelle.Project (loadProject)
import Control.Monad.Aff (error, launchAff, throwError)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Error.Class (try)
import Data.Argonaut as A
import Data.Either (Either(..))
import Network.Ethereum.Web3 (ETH)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process (PROCESS)
import Node.Process as P
import Prelude (Unit, bind, show, void, ($), (<<<), (<>), (>>=))

runGenesisGenerator
  :: forall e.
     FilePath
  -> FilePath
  -> Eff (console :: CONSOLE, fs :: FS, process :: PROCESS, eth :: ETH, exception :: EXCEPTION | e) Unit
runGenesisGenerator genesisIn genesisOut = do
    root <- liftEff P.cwd
    void <<< launchAff $
      (try $ loadProject root) >>= case _ of
        Left err -> liftAff $ do
          _ <- logGenesisGenerationError $ MalformedProjectErrorG (message err)
          throwError $ error "LoadProject Error"
        Right project -> liftAff $
          generateGenesis project genesisIn >>= case _ of
            Right gb -> do
                let strungGb = jsonStringifyWithSpaces 4 (A.encodeJson gb)
                try (FS.writeTextFile UTF8 genesisOut strungGb) >>= case _ of
                    Left err -> do
                      _ <- log Error $ "Couldn't write genesis block to " <> show genesisOut <> ": " <> show err
                      throwError $ error "WriteGenesis Error"
                    Right _  -> log Info $ "Successfully wrote generated genesis block to " <> show genesisOut
            Left err -> do
              _ <- logGenesisGenerationError err
              throwError $ error "GenerateGenesis Error"
