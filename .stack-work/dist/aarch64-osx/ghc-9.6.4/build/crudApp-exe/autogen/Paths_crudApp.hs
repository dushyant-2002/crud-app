{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_crudApp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/dushyant.singh/Desktop/CrudApp/crud/crudApp/.stack-work/install/aarch64-osx/e9287c177f6a33e3af0c607df1245b2e1e8ae1266b96a7a9d143b6563e0a9d2e/9.6.4/bin"
libdir     = "/Users/dushyant.singh/Desktop/CrudApp/crud/crudApp/.stack-work/install/aarch64-osx/e9287c177f6a33e3af0c607df1245b2e1e8ae1266b96a7a9d143b6563e0a9d2e/9.6.4/lib/aarch64-osx-ghc-9.6.4/crudApp-0.1.0.0-ELzSxcUWydL3bNKAS6MQ0I-crudApp-exe"
dynlibdir  = "/Users/dushyant.singh/Desktop/CrudApp/crud/crudApp/.stack-work/install/aarch64-osx/e9287c177f6a33e3af0c607df1245b2e1e8ae1266b96a7a9d143b6563e0a9d2e/9.6.4/lib/aarch64-osx-ghc-9.6.4"
datadir    = "/Users/dushyant.singh/Desktop/CrudApp/crud/crudApp/.stack-work/install/aarch64-osx/e9287c177f6a33e3af0c607df1245b2e1e8ae1266b96a7a9d143b6563e0a9d2e/9.6.4/share/aarch64-osx-ghc-9.6.4/crudApp-0.1.0.0"
libexecdir = "/Users/dushyant.singh/Desktop/CrudApp/crud/crudApp/.stack-work/install/aarch64-osx/e9287c177f6a33e3af0c607df1245b2e1e8ae1266b96a7a9d143b6563e0a9d2e/9.6.4/libexec/aarch64-osx-ghc-9.6.4/crudApp-0.1.0.0"
sysconfdir = "/Users/dushyant.singh/Desktop/CrudApp/crud/crudApp/.stack-work/install/aarch64-osx/e9287c177f6a33e3af0c607df1245b2e1e8ae1266b96a7a9d143b6563e0a9d2e/9.6.4/etc"

getBinDir     = catchIO (getEnv "crudApp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "crudApp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "crudApp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "crudApp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "crudApp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "crudApp_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
