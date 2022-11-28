{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_open_games_hs (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/s1855283/Desktop/EPF/open-games-hs/.stack-work/install/x86_64-osx/1f81e4e2e55d3c0706d5eef999621727320e00c9c016eb5fb0631b7e289cc503/8.8.4/bin"
libdir     = "/Users/s1855283/Desktop/EPF/open-games-hs/.stack-work/install/x86_64-osx/1f81e4e2e55d3c0706d5eef999621727320e00c9c016eb5fb0631b7e289cc503/8.8.4/lib/x86_64-osx-ghc-8.8.4/open-games-hs-0.1.0.0-45t1Au8oeXaBVmXHSw3WqJ-open-games-exe"
dynlibdir  = "/Users/s1855283/Desktop/EPF/open-games-hs/.stack-work/install/x86_64-osx/1f81e4e2e55d3c0706d5eef999621727320e00c9c016eb5fb0631b7e289cc503/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/s1855283/Desktop/EPF/open-games-hs/.stack-work/install/x86_64-osx/1f81e4e2e55d3c0706d5eef999621727320e00c9c016eb5fb0631b7e289cc503/8.8.4/share/x86_64-osx-ghc-8.8.4/open-games-hs-0.1.0.0"
libexecdir = "/Users/s1855283/Desktop/EPF/open-games-hs/.stack-work/install/x86_64-osx/1f81e4e2e55d3c0706d5eef999621727320e00c9c016eb5fb0631b7e289cc503/8.8.4/libexec/x86_64-osx-ghc-8.8.4/open-games-hs-0.1.0.0"
sysconfdir = "/Users/s1855283/Desktop/EPF/open-games-hs/.stack-work/install/x86_64-osx/1f81e4e2e55d3c0706d5eef999621727320e00c9c016eb5fb0631b7e289cc503/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "open_games_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "open_games_hs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "open_games_hs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "open_games_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "open_games_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "open_games_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
