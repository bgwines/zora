module Paths_Zora (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,1,21], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/brett/.cabal/bin"
libdir     = "/Users/brett/.cabal/lib/x86_64-osx-ghc-7.8.3/Zora-1.1.21"
datadir    = "/Users/brett/.cabal/share/x86_64-osx-ghc-7.8.3/Zora-1.1.21"
libexecdir = "/Users/brett/.cabal/libexec"
sysconfdir = "/Users/brett/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Zora_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Zora_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Zora_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Zora_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Zora_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
