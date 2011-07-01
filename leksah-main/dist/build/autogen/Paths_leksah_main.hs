module Paths_leksah_main (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jutaro/.cabal/bin"
libdir     = "/home/jutaro/.cabal/lib/leksah-main-1.0.0/ghc-7.0.3"
datadir    = "/home/jutaro/.cabal/share/leksah-main-1.0.0"
libexecdir = "/home/jutaro/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "leksah_main_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "leksah_main_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "leksah_main_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "leksah_main_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
