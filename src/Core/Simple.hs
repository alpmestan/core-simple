{-# LANGUAGE OverloadedStrings #-}

module Core.Simple ( ghcCoreFor
                   , OptLevel
                   , GHCVersion
                   , ModuleName
                   , GHCCoreError
                   , GeneratedCore
                   , HaskellCode   ) where

import Data.Text.Lazy            (Text)
import System.Exit
import System.FilePath
import System.Process

import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T

-- | ghc -O0, -O1 and -O2
data OptLevel = O0 | O1 | O2

instance Show OptLevel where
    show O0 = "-O0"
    show O1 = "-O1"
    show O2 = "-O2"

-- | ghc version, e.g "7.6.3"
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
--   It writes the given haskell code in a file in the given directory (last argument)
--   using the 'ModuleName' argument for the name: modName.hs
--   and inserts "module <modName> where" at the beginning of the haskell code
--   The optLevel switch lets you build with either -O0, -O1 or -O2.
ghcCoreFor :: GHCVersion -> HaskellCode -> OptLevel -> ModuleName -> FilePath -> IO (Either GHCCoreError GeneratedCore)
ghcCoreFor ghcVer haskellCode optLevel modName hsFilesDir = do
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
                      ++ " -ddump-simpl -dsuppress-idinfo -dsuppress-coercions \
                         \ -dsuppress-type-applications -dsuppress-uniques \
                         \ -dsuppress-module-prefixes"
                           
          ghc  = "ghc-" ++ T.unpack ghcVer
          hsFilePath = hsFilesDir </> hsFileName
          hsFileName = T.unpack modName <.> "hs"
          haskellModule = "module " `T.append` modName `T.append` " where \n"
                                    `T.append` haskellCode
          
          cleanUp = T.init . T.init . T.init . T.init -- removes 4 `\n' at the end
                  . T.unlines . drop 5 . T.lines -- removes first 5 useless lines