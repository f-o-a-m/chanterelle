module Compile.Types where

import Prelude
import Data.Argonaut as A
import Data.Argonaut ((:=), (~>), (.?))
import Node.Path (FilePath)

newtype Dependency = Dependency String
derive instance eqDependency  :: Eq Dependency
instance encodeJsonDependency :: A.EncodeJson Dependency where
  encodeJson (Dependency d) = A.encodeJson d
instance decodeJsonDependency :: A.DecodeJson Dependency where
  decodeJson d = Dependency <$> A.decodeJson d

newtype ChanterelleProject =
  ChanterelleProject { name                :: String
                     , version             :: String
                     , sourceDir           :: FilePath
                     , sources             :: Array String
                     , dependencies        :: Array Dependency
                     , solcOutputSelection :: Array String
                     }
derive instance eqChanterelleProject  :: Eq ChanterelleProject
instance encodeJsonChanterelleProject :: A.EncodeJson ChanterelleProject where
  encodeJson (ChanterelleProject project) =  "name"                := A.encodeJson project.name
                                          ~> "version"             := A.encodeJson project.version
                                          ~> "sourceDir"           := A.encodeJson project.sourceDir
                                          ~> "sources"             := A.encodeJson project.sources
                                          ~> "dependencies"        := A.encodeJson project.dependencies
                                          ~> "solcOutputSelection" := A.encodeJson project.solcOutputSelection
                                          ~> A.jsonEmptyObject
instance decodeJsonChanterelleProject :: A.DecodeJson ChanterelleProject where
  decodeJson j = do
    obj                 <- A.decodeJson j
    name                <- obj .? "name"
    version             <- obj .? "version"
    sourceDir           <- obj .? "sourceDir"
    sources             <- obj .? "sources"
    dependencies        <- obj .? "dependencies"
    solcOutputSelection <- obj .? "solcOutputSelection"
    pure $ ChanterelleProject { name, version, sourceDir, sources, dependencies, solcOutputSelection }
