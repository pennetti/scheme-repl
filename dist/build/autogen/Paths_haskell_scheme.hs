module Paths_haskell_scheme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/gandalf/.cabal/bin"
libdir     = "/home/gandalf/.cabal/lib/haskell-scheme-0.1.0.0/ghc-7.4.2"
datadir    = "/home/gandalf/.cabal/share/haskell-scheme-0.1.0.0"
libexecdir = "/home/gandalf/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_scheme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_scheme_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
