module Chanterelle.Internal.Genesis where

import Chanterelle.Internal.Compile (compileModuleWithoutWriting, decodeContract, makeSolcInput, resolveContractMainModule)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types.Bytecode (Bytecode(..))
import Chanterelle.Internal.Types.Compile (CompileError(..), OutputContract(..), runCompileMExceptT)
import Chanterelle.Internal.Types.Genesis (GenesisAlloc(..), GenesisBlock(..), GenesisGenerationError(..), insertGenesisAllocs, lookupGenesisAllocs)
import Chanterelle.Internal.Types.Project (ChanterelleModule(..), ChanterelleModuleType(..), ChanterelleProject(..), ChanterelleProjectSpec(..), InjectableLibraryCode(..), Libraries(..), Library(..), Network(..), Networks(..), isFixedLibrary, resolveNetworkRefs)
import Chanterelle.Internal.Utils.Lazy (firstSuccess)
import Chanterelle.Internal.Utils.Web3 (resolveCodeForContract)
import Control.Error.Util (note)
import Control.Monad.Error.Class (try, throwError)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.State.Trans (execStateT, get, put)
import Data.Argonaut as A
import Data.Array ((!!), catMaybes, replicate, null, all)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, elem)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, takeRight)
import Data.Traversable (for, for_, sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomRange)
import Network.Ethereum.Web3 (Address, HexString, embed, mkAddress, mkHexString, unAddress, unHex)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Prelude (class Functor, class Show, bind, discard, flip, otherwise, pure, show, unit, ($), (&&), (<$>), (<<<), (<=), (<>), (==), (>>=))

substituteLibraryAddress :: HexString -> Address -> Either String HexString
substituteLibraryAddress hsBytecode target = ret
    where -- length of the asm PUSH20 <ownaddr>; ADDRESS; EQ
          -- which is part of the library's nonpayability guard
          -- so 27 bytes of opcodes *2 cause it's hex
          minBytecodeLength = 46

          -- evm opcodes
          op_push20'     = "73"
          op_push20 addr = op_push20' <> (unHex $ unAddress addr)
          op_addr        = "30"
          op_eq          = "14"

          -- work against the raw hex string
          bytecode = unHex hsBytecode

          -- split at minBytecodeLength since we have to edit before that
          bcsplit = let s = S.splitAt minBytecodeLength bytecode in { preamble: s.before, code: s.after }

          -- is the first instruction a PUSH20?
          firstByteIsPush20 = S.take 2 bcsplit.preamble == op_push20'

          -- are the last instructions in preamble ADDRESS; EQ?
          preambleEndsInAddrEq = takeRight 4 bcsplit.preamble == (op_addr <> op_eq)

          -- if yes to both then it's a library
          firstBytesAreLibPreamble = firstByteIsPush20 && preambleEndsInAddrEq

          newPreamble = (op_push20 target) <> op_addr <> op_eq

          ret | S.length bytecode <= minBytecodeLength = Left "Bytecode too short to be a library"
              | firstBytesAreLibPreamble == false      = Left "Bytecode does not look like a library"
              | otherwise                              = note "Couldn't make a valid HexString" $ mkHexString (newPreamble <> bcsplit.code)

-- this might actually be useless in retrospect
generateAddress :: forall m f
                 . MonadEffect m
                => Foldable f
                => f Address
                -> m Address
generateAddress blacklist = do
  addr' <- randomAddress
  case mkHexAddress addr' of
    Nothing -> do
      log Debug $ "Generated address " <> show addr' <> " which isn't actually a valid address"
      generateAddress blacklist
    Just addr -> if addr `elem` blacklist
                   then do
                     log Debug $ "Generated address " <> show addr <> " which is blacklisted, retrying"
                     generateAddress blacklist
                   else do
                     log Debug $ "Successfully generated address " <> show addr
                     pure addr
  where randomHexDigit = toHex <<< floor <$> liftEffect (randomRange (toNumber 0) (toNumber 16)) -- 0 inclusive, 16 exclusive, aka 0-F
        hexDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']
        randomAddress = fromCharArray <$> (sequence $ replicate 40 randomHexDigit)
        toHex n = fromMaybe '0' $ hexDigits !! n
        mkHexAddress s = mkHexString s >>= mkAddress

generateGenesis :: forall m
                 . MonadAff m
                => ChanterelleProject
                -> FilePath
                -> m (Either GenesisGenerationError GenesisBlock)
generateGenesis cp@(ChanterelleProject project) genesisIn = liftAff <<< runExceptT $
  if nothingToDo
    then throwError $ NothingToDo ntdReason
    else do
      genesis <- loadGenesisIn
      mInjects <- for libs $ case _ of
        FixedLibrary { name } -> throwError $ CouldntInjectLibrary name "is a fixed library without any source code"
        FixedLibraryWithNetwork { name, address, networks } -> do
            let (ChanterelleProjectSpec spec) = project.spec
                (Networks networksToUse) = resolveNetworkRefs networks spec.networks

            if null networksToUse
                then throwError $ CouldntResolveLibraryNoNetworks name
                else pure unit
            reses <- firstSuccess networksToUse $ \network -> runExceptT $ do
                code <- ExceptT $ resolveCodeForContract network address
                except $ substituteLibraryAddress code address
            case reses of
                Left { failures } -> throwError $ CouldntResolveLibrary name failures
                Right { result, failures, input } -> do
                  let (Network successNet) = input
                  log Warn $ "Failures encountered when resolving library " <> show name <> ": "
                  for_ failures $ \(Tuple (Network failedNet) err) -> log Warn $ "    via " <> show failedNet.name <> ": " <> err
                  log Info $ "Successfully resolved library " <> show name <> " via network " <> show successNet.name
                  pure $ Just { name, address, injectedBytecode: result }
        InjectableLibrary { name, address, code } -> case code of
            InjectableWithBytecode bc -> do
                injectedBytecode <- withExceptT (CouldntInjectLibraryAddress name) <<< except $ substituteLibraryAddress bc address
                pure $ Just { name, address, injectedBytecode }
            InjectableWithSourceCode r f -> do
                let libProject = cp
                hexBytecode <- withExceptT (CouldntCompileLibrary name) <<< flip runCompileMExceptT libProject $ do
                    let mfi@(ChanterelleModule mfi') = libModuleForInput { name, root: r, filePath: f }
                    input <-  makeSolcInput name f
                    output <- compileModuleWithoutWriting mfi input
                    decoded <- decodeContract name output
                    OutputContract { deployedBytecode } <- resolveContractMainModule f decoded mfi'.solContractName
                    case deployedBytecode of
                        BCLinked x -> pure x.bytecode
                        BCUnlinked _ -> throwError $ UnexpectedSolcOutput "Source code compiled to unlinked bytecode"
                injectedBytecode <- withExceptT (CouldntInjectLibraryAddress name) <<< except $ substituteLibraryAddress hexBytecode address
                pure $ Just { name, address, injectedBytecode }
        DeployableLibrary { name } -> do
          log Warn $ "Not injecting library " <> name <> " into genesis as it appears to be deployable."
          pure Nothing
      let injects = catMaybes mInjects
      injectedGenesis <- flip execStateT genesis <<< for injects $ \{ name, address, injectedBytecode } -> do
        (GenesisBlock currGen) <- get
        newAllocs <- case lookupGenesisAllocs address currGen.allocs of
            Nothing -> pure $ insertGenesisAllocs address (GenesisAlloc { code: Just injectedBytecode, storage: Nothing, balance: embed 0 }) currGen.allocs
            Just (GenesisAlloc existingAlloc) -> if existingAlloc.code == Just injectedBytecode
                                   then pure currGen.allocs
                                   else throwError $ CouldntInjectLibraryAddress name $ "Genesis block already contains an entry for address " <> show address <> " with different bytecode"
        put $ GenesisBlock currGen { allocs = newAllocs }
      pure injectedGenesis

  where (ChanterelleProjectSpec spec) = project.spec
        (Libraries libs)              = spec.libraries

        libModuleForInput {name, root, filePath } = 
            let { root, dir, base, ext, name } = Path.parse filePath
             in ChanterelleModule { moduleName: name, solContractName: name, moduleType: LibraryModule, solPath: filePath, jsonPath: "", pursPath: "" }

        { nothingToDo, ntdReason } = if null libs
                                          then { nothingToDo: true, ntdReason: "No libraries specified in project" }
                                          else if all isFixedLibrary libs
                                                 then { nothingToDo: true, ntdReason: "All libraries are fixed libraries" }
                                                 else { nothingToDo: false, ntdReason: "There's stuff to do!" }
        
        wrapLoadFailure :: forall e m' a. Show e => Functor m' => m' (Either e a) -> ExceptT GenesisGenerationError m' a
        wrapLoadFailure = withExceptT (CouldntLoadGenesisBlock genesisIn <<< show) <<< ExceptT
        
        loadGenesisIn = do
            genTxt <- wrapLoadFailure (try $ FS.readTextFile UTF8 genesisIn)
            wrapLoadFailure (pure $ A.jsonParser genTxt >>= A.decodeJson)
