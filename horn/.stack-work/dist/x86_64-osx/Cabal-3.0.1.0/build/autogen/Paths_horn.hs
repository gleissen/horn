{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_horn (
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

bindir     = "/Users/gleissen/teaching/verification/horn/horn/.stack-work/install/x86_64-osx/262f561c641e036f399b86f10908f4bfdb9c6bf73240386325fc2e76c709c046/8.8.3/bin"
libdir     = "/Users/gleissen/teaching/verification/horn/horn/.stack-work/install/x86_64-osx/262f561c641e036f399b86f10908f4bfdb9c6bf73240386325fc2e76c709c046/8.8.3/lib/x86_64-osx-ghc-8.8.3/horn-0.1.0.0-AhuwpgKKA6WF4yPeZ923GP"
dynlibdir  = "/Users/gleissen/teaching/verification/horn/horn/.stack-work/install/x86_64-osx/262f561c641e036f399b86f10908f4bfdb9c6bf73240386325fc2e76c709c046/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/gleissen/teaching/verification/horn/horn/.stack-work/install/x86_64-osx/262f561c641e036f399b86f10908f4bfdb9c6bf73240386325fc2e76c709c046/8.8.3/share/x86_64-osx-ghc-8.8.3/horn-0.1.0.0"
libexecdir = "/Users/gleissen/teaching/verification/horn/horn/.stack-work/install/x86_64-osx/262f561c641e036f399b86f10908f4bfdb9c6bf73240386325fc2e76c709c046/8.8.3/libexec/x86_64-osx-ghc-8.8.3/horn-0.1.0.0"
sysconfdir = "/Users/gleissen/teaching/verification/horn/horn/.stack-work/install/x86_64-osx/262f561c641e036f399b86f10908f4bfdb9c6bf73240386325fc2e76c709c046/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "horn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "horn_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "horn_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "horn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "horn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "horn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
