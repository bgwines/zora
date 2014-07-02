module Paths_Zora (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,1,9], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/brett/.cabal/bin"
libdir     = "/Users/brett/.cabal/lib/Zora-1.1.9/ghc-7.6.3"
datadir    = "/Users/brett/.cabal/share/Zora-1.1.9"
libexecdir = "/Users/brett/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Zora_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Zora_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Zora_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Zora_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
