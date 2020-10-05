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

bindir     = "/home/jackgilbride/College/Functional-Programming/Topics-In-Functional/Weekly-Exercises/Week1/.stack-work/install/x86_64-linux-tinfo6/a1315e10e9c25fd86f52be9dc996c7aeaa5d2fba6b0a2cbb5ab9162530d43e30/8.8.4/bin"
libdir     = "/home/jackgilbride/College/Functional-Programming/Topics-In-Functional/Weekly-Exercises/Week1/.stack-work/install/x86_64-linux-tinfo6/a1315e10e9c25fd86f52be9dc996c7aeaa5d2fba6b0a2cbb5ab9162530d43e30/8.8.4/lib/x86_64-linux-ghc-8.8.4/fib-0.1.0.0-K1tFjz8t0OFKQzJXZW126g-ex2"
dynlibdir  = "/home/jackgilbride/College/Functional-Programming/Topics-In-Functional/Weekly-Exercises/Week1/.stack-work/install/x86_64-linux-tinfo6/a1315e10e9c25fd86f52be9dc996c7aeaa5d2fba6b0a2cbb5ab9162530d43e30/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/jackgilbride/College/Functional-Programming/Topics-In-Functional/Weekly-Exercises/Week1/.stack-work/install/x86_64-linux-tinfo6/a1315e10e9c25fd86f52be9dc996c7aeaa5d2fba6b0a2cbb5ab9162530d43e30/8.8.4/share/x86_64-linux-ghc-8.8.4/fib-0.1.0.0"
libexecdir = "/home/jackgilbride/College/Functional-Programming/Topics-In-Functional/Weekly-Exercises/Week1/.stack-work/install/x86_64-linux-tinfo6/a1315e10e9c25fd86f52be9dc996c7aeaa5d2fba6b0a2cbb5ab9162530d43e30/8.8.4/libexec/x86_64-linux-ghc-8.8.4/fib-0.1.0.0"
sysconfdir = "/home/jackgilbride/College/Functional-Programming/Topics-In-Functional/Weekly-Exercises/Week1/.stack-work/install/x86_64-linux-tinfo6/a1315e10e9c25fd86f52be9dc996c7aeaa5d2fba6b0a2cbb5ab9162530d43e30/8.8.4/etc"

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
  return (dir ++ "/" ++ name)
