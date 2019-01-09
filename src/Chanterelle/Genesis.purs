module Chanterelle.Genesis
    ( runGenesisGenerator
    ) where

import Chanterelle.Internal.Genesis (generateGenesis)
import Chanterelle.Internal.Logging (LogLevel(..), log, logGenesisGenerationError)
import Chanterelle.Internal.Types.Genesis (GenesisGenerationError(MalformedProjectErrorG, NothingToDo))
import Chanterelle.Internal.Utils.Json (jsonStringifyWithSpaces)
import Chanterelle.Project (loadProject)
import Effect.Aff (error, launchAff, throwError)
import Effect.Aff.Class (liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Control.Monad.Error.Class (try)
import Data.Argonaut as A
import Data.Either (Either(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process as P
import Prelude (Unit, bind, show, void, ($), (<<<), (<>), (>>=), pure, unit)

runGenesisGenerator :: FilePath -> FilePath -> Effect Unit
runGenesisGenerator genesisIn genesisOut = do
    root <- liftEffect P.cwd
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
            Left (NothingToDo str) -> do
              _ <- log Info $ "Nothing for genesis generator to do : " <> str
              pure unit
            Left err -> do
              _ <- logGenesisGenerationError err
              throwError $ error "GenerateGenesis Error"
