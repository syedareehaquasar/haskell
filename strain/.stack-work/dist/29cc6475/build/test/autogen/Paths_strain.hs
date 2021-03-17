{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_strain (
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
version = Version [0,1,0,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\DELL\\Exercism\\haskell\\strain\\.stack-work\\install\\e498b7ba\\bin"
libdir     = "C:\\Users\\DELL\\Exercism\\haskell\\strain\\.stack-work\\install\\e498b7ba\\lib\\x86_64-windows-ghc-8.8.4\\strain-0.1.0.3-1UOwCmUKiA9HNluBmDWOwi-test"
dynlibdir  = "C:\\Users\\DELL\\Exercism\\haskell\\strain\\.stack-work\\install\\e498b7ba\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\DELL\\Exercism\\haskell\\strain\\.stack-work\\install\\e498b7ba\\share\\x86_64-windows-ghc-8.8.4\\strain-0.1.0.3"
libexecdir = "C:\\Users\\DELL\\Exercism\\haskell\\strain\\.stack-work\\install\\e498b7ba\\libexec\\x86_64-windows-ghc-8.8.4\\strain-0.1.0.3"
sysconfdir = "C:\\Users\\DELL\\Exercism\\haskell\\strain\\.stack-work\\install\\e498b7ba\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "strain_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "strain_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "strain_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "strain_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "strain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "strain_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
