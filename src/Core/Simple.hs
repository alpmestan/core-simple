{-# LANGUAGE OverloadedStrings #-}

module Core.Simple ( ghcCoreFor
                   , OptLevel(..)
                   , GHCVersion
                   , ModuleName
                   , GHCCoreError
                   , GeneratedCore
                   , HaskellCode
                   , CoreOutputOpt(..)
                   , defaultCoreOutputOpts ) where

import Data.Text.Lazy            (Text)
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
  deriving Eq

instance Show CoreOutputOpt where
    show DSuppressIdInfo           = "-dsuppress-idinfo"
    show DSuppressCoercions        = "-dsuppress-coercions"
    show DSuppressTypeApplications = "-dsuppress-type-applications"
    show DSuppressUniques          = "-dsuppress-uniques"
    show DSuppressModulePrefixes   = "-dsuppress-module-prefixes"

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

instance Show OptLevel where
    show O0 = "-O0"
    show O1 = "-O1"
    show O2 = "-O2"

-- | ghc version, e.g \"7.6.3\"
type GHCVersion = Text

-- | Synonym for 'Text'
type ModuleName = Text

-- | Synonym for 'Text'
type GHCCoreError = Text

-- | Synonym for 'Text'
type GeneratedCore = Text

-- | Synonym for 'Text'
type HaskellCode = Text

-- | Runs the given GHC version by calling out to ghc-<ghcVer>, which must be
--   in the PATH environment variable
ghcCoreFor :: GHCVersion      -- ^ GHC version to use, e.g \"7.6.3\"
           -> OptLevel        -- ^ 'O0', 'O1' or 'O2'
           -> [CoreOutputOpt] -- ^ Control the output, see 'CoreOutputOpt'. See 'defaultCoreOutputOpts'
           -> HaskellCode     -- ^ Text of your Haskell code
           -> ModuleName      -- ^ Choose a name for the module
           -> FilePath        -- ^ Directory to write the haskell code in
           -> IO (Either GHCCoreError GeneratedCore) -- ^ Text of the error or Core code
ghcCoreFor ghcVer optLevel coreOutputOpts haskellCode modName hsFilesDir = do
    T.writeFile hsFilePath haskellModule
    (exitStatus, out, err) <- readProcessWithExitCode ghc args ""
    case exitStatus of
        ExitSuccess      -> return . Right $ cleanUp (T.pack out)
        ExitFailure code -> return . Left  $ errorMsg code out err
        
    where errorMsg code out err = T.pack $ "GHC failed to compile your code, exit code: " 
                                        ++ show code 
                                        ++ "\n" 
                                        ++ out ++ err
                                        
          args = words $ " "         -- just up to the object file
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