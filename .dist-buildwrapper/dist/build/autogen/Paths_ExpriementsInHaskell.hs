module Paths_ExpriementsInHaskell (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/ijet/Library/Haskell/ghc-7.4.2/lib/ExpriementsInHaskell-0.1/bin"
libdir     = "/Users/ijet/Library/Haskell/ghc-7.4.2/lib/ExpriementsInHaskell-0.1/lib"
datadir    = "/Users/ijet/Library/Haskell/ghc-7.4.2/lib/ExpriementsInHaskell-0.1/share"
libexecdir = "/Users/ijet/Library/Haskell/ghc-7.4.2/lib/ExpriementsInHaskell-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ExpriementsInHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ExpriementsInHaskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ExpriementsInHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ExpriementsInHaskell_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
