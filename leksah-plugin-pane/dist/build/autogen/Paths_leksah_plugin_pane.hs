module Paths_leksah_plugin_pane (
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
libdir     = "/home/jutaro/.cabal/lib/leksah-plugin-pane-1.0.0/ghc-7.0.3"
datadir    = "/home/jutaro/.cabal/share/leksah-plugin-pane-1.0.0"
libexecdir = "/home/jutaro/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "leksah_plugin_pane_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "leksah_plugin_pane_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "leksah_plugin_pane_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "leksah_plugin_pane_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
