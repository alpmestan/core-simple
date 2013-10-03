{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Core.Simple where

import Control.Applicative
import Data.Aeson
import Data.Text.Lazy            (Text)
import Data.Time.Clock
import GHC.Generics
import System.Exit
import System.FilePath
import System.Process

import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T

{- | 
-}
data CoreOutputOpt = DSuppressIdInfo           -- ^ Remove id information
                   | DSuppressCoercions        -- ^ Omit the coercions
                   | DSuppressTypeApplications -- ^ Omit the type applications
                   | DSuppressUniques          -- ^ Don't use unique names
                   | DSuppressModulePrefixes   -- ^ Don't show module prefixes before an identifier

instance Show CoreOutputOpt where
    show DSuppressIdInfo           = "-dsuppress-idinfo"
    show DSuppressCoercions        = "-dsuppress-coercions"
    show DSuppressTypeApplications = "-dsuppress-type-applications"
    show DSuppressUniques          = "-dsuppress-uniques"
    show DSuppressModulePrefixes   = "-dsuppress-module-prefixes"
    
instance FromJSON CoreOutputOpt where
    parseJSON (String s) = case s of
        "-dsuppress-idinfo"            -> return DSuppressIdInfo
        "-dsuppress-coercions"         -> return DSuppressCoercions
        "-dsuppress-type-applications" -> return DSuppressTypeApplications
        "-dsuppress-uniques"           -> return DSuppressUniques
        "-dsuppress-module-prefixes"   -> return DSuppressModulePrefixes
        
instance ToJSON CoreOutputOpt where
    toJSON DSuppressIdInfo           = String "-dsuppress-idinfo"
    toJSON DSuppressCoercions        = String "-dsuppress-coercions"
    toJSON DSuppressTypeApplications = String "-dsuppress-type-applications"
    toJSON DSuppressUniques          = String "-dsuppress-uniques"
    toJSON DSuppressModulePrefixes   = String "-dsuppress-module-prefixes"

-- | It's equal to [ DSuppressIdInfo, DSuppressCoercions, DSuppressTypeApplications
--   , DSuppressUniques, DSuppressModulePrefixes ]
defaultCoreOutputOpts :: [CoreOutputOpt]
defaultCoreOutputOpts = [ DSuppressIdInfo
                        , DSuppressCoercions
                        , DSuppressTypeApplications
                        , DSuppressUniques
                        , DSuppressModulePrefixes ]

formatCoreOutputOpts :: [CoreOutputOpt] -> String
formatCoreOutputOpts = unwords . map show

-- | ghc -O0, -O1 and -O2
data OptLevel = O0 -- ^ ghc -O0 ...
              | O1 -- ^ ghc -O1 ...
              | O2 -- ^ ghc -O2 ...
  deriving Generic
  
instance Show OptLevel where
    show O0 = "-O0"
    show O1 = "-O1"
    show O2 = "-O2"
    
instance FromJSON OptLevel where
    parseJSON (String s) = case s of
        "-O0" -> return O0
        "-O1" -> return O1
        "-O2" -> return O2
        
instance ToJSON OptLevel where
    toJSON O0 = String "-O0"
    toJSON O1 = String "-O1"
    toJSON O2 = String "-O2"

-- | ghc version, e.g \"7.6.3\"
type GhcVersion = Text

-- | Synonym for 'Text'
type ModuleName = Text

-- | Synonym for 'Text'
type GhcCoreError = Text

-- | Synonym for 'Text'
type GeneratedCore = Text

-- | Synonym for 'Text'
type HaskellCode = Text

-- | Argument to be passed to the main function, 'ghcCoreFor'
data GhcArgs = GhcArgs
    { ghcVersion     :: GhcVersion -- ^ GHC version to use, e.g \"7.6.3\"
    , optLevel       :: OptLevel   -- ^ 'O0', 'O1' or 'O2'
    , coreOutputOpts :: [CoreOutputOpt] -- ^ Control the output, see 'CoreOutputOpt'. See 'defaultCoreOutputOpts'
    , haskellCode    :: HaskellCode -- ^ Text of your Haskell code
    } deriving Generic

instance FromJSON GhcArgs where
    parseJSON (Object o) = GhcArgs 
                       <$> o .: "ghcVersion"
                       <*> o .: "optLevel"
                       <*> o .: "coreOutputOpts"
                       <*> o .: "haskellCode"
    
instance ToJSON GhcArgs where
    toJSON (GhcArgs ghcVersion optLevel coreOutputOpts haskellCode) =
        object [ "ghcVersion"     .= ghcVersion
               , "optLevel"       .= optLevel
               , "coreOutputOpts" .= coreOutputOpts
               , "haskellCode"    .= haskellCode
               ]

data GhcCoreResult = GhcCoreResult 
    { execTime       :: Text -- ^ execution time
    , generatedCore  :: Text -- ^ generated core
    }
    
instance FromJSON GhcCoreResult where
    parseJSON (Object o) = GhcCoreResult
                       <$> o .: "execTime"
                       <*> o .: "generatedCore"

instance ToJSON GhcCoreResult where
    toJSON (GhcCoreResult execTime core) =
        object [ "execTime"      .= execTime
               , "generatedCore" .= core
               ]

-- | Runs the given GHC version by calling out to ghc-<'ghcVersion'>, which must be
--   in the PATH environment variable
ghcCoreFor :: GhcArgs -- ^ args to GHC, see 'GhcArgs'
           -> FilePath
           -> ModuleName
           -> IO (Either GhcCoreError GhcCoreResult) -- ^ Text of the error or Core result
ghcCoreFor (GhcArgs ghcVer optLevel coreOutputOpts haskellCode) hsFilesDir modName = do
    beginning <- getCurrentTime
    T.writeFile hsFilePath haskellModule
    (exitStatus, out, err) <- readProcessWithExitCode ghc args ""
    end <- length out `seq` getCurrentTime
    case exitStatus of
        ExitSuccess      -> return . Right $ 
            GhcCoreResult (T.pack . show $ diffUTCTime end beginning) 
                          (cleanUp (T.pack out))
                          
        ExitFailure code -> return . Left  $ errorMsg code out err
        
    where errorMsg code out err = T.pack $ "GHC failed to compile your code, exit code: " 
                                        ++ show code 
                                        ++ "\n" 
                                        ++ out ++ err
                                        
          args = words $ " "           -- just up to the object file
                      ++ show optLevel -- O0, O1, O2
                      ++ " " 
                      ++ hsFilePath    -- absolute (?) file path to where 
                                       -- we should write the code
                      ++ " -ddump-simpl "
                      ++ formatCoreOutputOpts coreOutputOpts
                           
          ghc  = "ghc-" ++ T.unpack ghcVer
          hsFilePath = hsFilesDir </> hsFileName
          hsFileName = T.unpack modName <.> "hs"
          haskellModule = "module " `T.append` modName `T.append` " where \n"
                                    `T.append` haskellCode
          
          cleanUp = T.init . T.init . T.init . T.init -- removes 4 `\n' at the end
                  . T.unlines . drop 5 . T.lines      -- removes first 5 useless lines