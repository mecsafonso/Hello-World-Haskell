{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_proyecto_vacio (
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

bindir     = "/home/mecs/Hello-World-Haskell/Hullabalooza/.stack-work/install/x86_64-linux-tinfo6/e358769447a2a3ee0ce09940962ac687980080e2149c7bd25dbb3e5ba8a5f0fe/8.8.3/bin"
libdir     = "/home/mecs/Hello-World-Haskell/Hullabalooza/.stack-work/install/x86_64-linux-tinfo6/e358769447a2a3ee0ce09940962ac687980080e2149c7bd25dbb3e5ba8a5f0fe/8.8.3/lib/x86_64-linux-ghc-8.8.3/proyecto-vacio-0.1.0.0-B55rRcTfCpC9xgSAy4q1Oy"
dynlibdir  = "/home/mecs/Hello-World-Haskell/Hullabalooza/.stack-work/install/x86_64-linux-tinfo6/e358769447a2a3ee0ce09940962ac687980080e2149c7bd25dbb3e5ba8a5f0fe/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/mecs/Hello-World-Haskell/Hullabalooza/.stack-work/install/x86_64-linux-tinfo6/e358769447a2a3ee0ce09940962ac687980080e2149c7bd25dbb3e5ba8a5f0fe/8.8.3/share/x86_64-linux-ghc-8.8.3/proyecto-vacio-0.1.0.0"
libexecdir = "/home/mecs/Hello-World-Haskell/Hullabalooza/.stack-work/install/x86_64-linux-tinfo6/e358769447a2a3ee0ce09940962ac687980080e2149c7bd25dbb3e5ba8a5f0fe/8.8.3/libexec/x86_64-linux-ghc-8.8.3/proyecto-vacio-0.1.0.0"
sysconfdir = "/home/mecs/Hello-World-Haskell/Hullabalooza/.stack-work/install/x86_64-linux-tinfo6/e358769447a2a3ee0ce09940962ac687980080e2149c7bd25dbb3e5ba8a5f0fe/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "proyecto_vacio_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "proyecto_vacio_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "proyecto_vacio_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "proyecto_vacio_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proyecto_vacio_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proyecto_vacio_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
