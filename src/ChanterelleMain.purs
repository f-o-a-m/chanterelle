module ChanterelleMain where

import Prelude

import Chanterelle (Args'(..), ArgsCLI, Command(..), CommonOpts(..), DeployOptions(..), DirPath, GenesisOptions(..), SelectCLI(..), SelectPS(..), chanterelle, traverseDeployOptions)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployM)
import Control.Apply (lift2)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process (cwd)
import Options.Applicative (Parser, ParserInfo, argument, command, customExecParser, help, helpDoc, helper, hsubparser, info, infoOption, int, long, metavar, option, prefs, progDesc, short, showHelpOnEmpty, str, strOption, value, (<**>))
import Text.PrettyPrint.Leijen (indent, text, (</>))

foreign import version_ :: String
version :: forall a. Parser (a -> a)
version = infoOption version_
  (  long "version"
  <> help "Print version information" )

parser :: DirPath -> Parser ArgsCLI
parser cwd' = ado
  opts <- commonOpts cwd'
  cmds <- hsubparser
            ( command "build"
              (info (pure Build)
                    (progDesc "Build project"))
           <> command "compile"
              (info (pure Compile)
                    (progDesc "Compile project"))
           <> command "codegen"
              (info (pure Codegen)
                    (progDesc "generate purescript"))
           <> command "genesis"
              (info (Genesis <$> genesisParser)
                    (progDesc "generate purescript"))
           <> command "deploy"
              (info (Deploy <$> deployParser)
                    (progDesc "run deploy script")) )
  in Args' opts cmds

genesisParser :: Parser GenesisOptions
genesisParser = ado
  input <- strOption
            ( long "input"
           <> metavar "INPUT"
           <> value "dist"
           <> help "path to some json file containing GENESIS_INPUT" )
  output <- strOption
            ( long "output"
           <> metavar "OUTPUT"
           <> value "dist" 
           <> help "path to some json file containing GENESIS_OUTPUT")
  in GenesisOptions {input, output}

deployParser :: Parser (DeployOptions SelectCLI)
deployParser = ado
  nodeURL <- strOption
            ( long "node-url"
           <> metavar "URL"
           <> value "http://localhost:8545"
           <> help "node URL" )
  timeout <- option int
            ( long "timeout"
           <> metavar "SECOND"
           <> value 60
           <> help "timeout in seconds")
  script <- SelectCLI <$> argument str
            ( metavar "FILE"
           <> helpDoc (Just $ text "path to compiled output of a module with signature: " </> indent 2 (text "{ deploy :: DeployM Unit }")))
  in DeployOptions {nodeURL, timeout, script}


commonOpts :: DirPath -> Parser CommonOpts
commonOpts cwd' = map CommonOpts $ {optVerbosity:_, rootPath:_}
  <$> strOption
      ( short 'v'
     <> long "verbosity"
     <> metavar "LEVEL"
     <> help "The level of logging"
     <> value "info" )
  <*> strOption
      ( short 'r'
     <> long "project-root"
     <> metavar "ROOT"
     <> help "Override the default project root"
     <> value cwd' )


pinfo :: DirPath -> ParserInfo ArgsCLI
pinfo cwd' = info (parser cwd' <**> (lift2 (>>>) version helper))
  ( progDesc "A more functional truffle" )

main :: Effect Unit
main = launchAff_ do
  ourCwd <- liftEffect $ cwd
  args <- liftEffect $ customExecParser (prefs showHelpOnEmpty) (pinfo ourCwd)
  res <- runExceptT $ flip traverseDeployOptions args \(DeployOptions {nodeURL, timeout, script: SelectCLI scriptPath}) -> do
    script <- ExceptT $ try (liftEffect $ loadDeployMFromScriptPath scriptPath)
    pure $ DeployOptions {nodeURL, timeout, script: SelectPS script}
  case res of
    Left err -> do
      log Error $ "Couldn't load deploy script" <> show err
    Right args' -> chanterelle args'

foreign import loadDeployMFromScriptPath :: String -> Effect (DeployM Unit)


