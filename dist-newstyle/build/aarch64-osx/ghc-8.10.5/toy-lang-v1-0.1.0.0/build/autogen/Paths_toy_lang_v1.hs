{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_toy_lang_v1 (
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

bindir     = "/Users/nliittsc/.cabal/bin"
libdir     = "/Users/nliittsc/.cabal/lib/aarch64-osx-ghc-8.10.5/toy-lang-v1-0.1.0.0-inplace"
dynlibdir  = "/Users/nliittsc/.cabal/lib/aarch64-osx-ghc-8.10.5"
datadir    = "/Users/nliittsc/.cabal/share/aarch64-osx-ghc-8.10.5/toy-lang-v1-0.1.0.0"
libexecdir = "/Users/nliittsc/.cabal/libexec/aarch64-osx-ghc-8.10.5/toy-lang-v1-0.1.0.0"
sysconfdir = "/Users/nliittsc/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "toy_lang_v1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "toy_lang_v1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "toy_lang_v1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "toy_lang_v1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "toy_lang_v1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "toy_lang_v1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
