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
version = Version {versionBranch = [1,1,18], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/brett/Library/Haskell/bin"
libdir     = "/Users/brett/Library/Haskell/ghc-7.8.3-x86_64/lib/Zora-1.1.18"
datadir    = "/Users/brett/Library/Haskell/share/ghc-7.8.3-x86_64/Zora-1.1.18"
libexecdir = "/Users/brett/Library/Haskell/libexec"
sysconfdir = "/Users/brett/Library/Haskell/etc"

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
