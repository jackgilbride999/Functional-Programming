{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_fib (
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

bindir     = "C:\\Users\\jackg\\College\\Functional-Programming\\Topics-In-Functional\\Weekly-Exercises\\Week3\\.stack-work\\install\\380abdb7\\bin"
libdir     = "C:\\Users\\jackg\\College\\Functional-Programming\\Topics-In-Functional\\Weekly-Exercises\\Week3\\.stack-work\\install\\380abdb7\\lib\\x86_64-windows-ghc-8.8.4\\fib-0.1.0.0-BZUGYkTDfooEOPjKZAzss-week3"
dynlibdir  = "C:\\Users\\jackg\\College\\Functional-Programming\\Topics-In-Functional\\Weekly-Exercises\\Week3\\.stack-work\\install\\380abdb7\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\jackg\\College\\Functional-Programming\\Topics-In-Functional\\Weekly-Exercises\\Week3\\.stack-work\\install\\380abdb7\\share\\x86_64-windows-ghc-8.8.4\\fib-0.1.0.0"
libexecdir = "C:\\Users\\jackg\\College\\Functional-Programming\\Topics-In-Functional\\Weekly-Exercises\\Week3\\.stack-work\\install\\380abdb7\\libexec\\x86_64-windows-ghc-8.8.4\\fib-0.1.0.0"
sysconfdir = "C:\\Users\\jackg\\College\\Functional-Programming\\Topics-In-Functional\\Weekly-Exercises\\Week3\\.stack-work\\install\\380abdb7\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fib_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fib_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fib_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
