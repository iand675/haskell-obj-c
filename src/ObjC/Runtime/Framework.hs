{-# LANGUAGE ForeignFunctionInterface #-}

-- | Dynamic framework loading via @dlopen@.
--
-- Use this to load macOS frameworks at runtime that are not linked
-- at compile time, making their Objective-C classes available to
-- @objc_getClass@ and the rest of the runtime.
module ObjC.Runtime.Framework
  ( loadFramework
  , loadFrameworkAt
  ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, withCString, peekCString)

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

-- | RTLD_LAZY = 0x1 on macOS
foreign import ccall unsafe "dlopen"
  c_dlopen :: CString -> CInt -> IO (Ptr ())

foreign import ccall unsafe "dlerror"
  c_dlerror :: IO CString

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

rtldLazy :: CInt
rtldLazy = 0x1

-- | Load a macOS framework by name. Looks for it in
-- @\/System\/Library\/Frameworks\/\<name\>.framework\/\<name\>@.
--
-- Throws an 'IOError' if loading fails.
--
-- After loading, all Objective-C classes defined by the framework become
-- available to @objc_getClass@ and friends.
loadFramework :: String -> IO ()
loadFramework name =
  loadFrameworkAt ("/System/Library/Frameworks/" ++ name ++ ".framework/" ++ name)

-- | Load a framework (or any dynamic library) by full path.
--
-- Throws an 'IOError' if loading fails.
loadFrameworkAt :: String -> IO ()
loadFrameworkAt path = do
  handle <- withCString path $ \cpath -> c_dlopen cpath rtldLazy
  if handle == nullPtr
    then do
      err <- c_dlerror
      if err == nullPtr
        then ioError (userError ("dlopen failed for: " ++ path))
        else do
          errStr <- peekCString err
          ioError (userError ("dlopen failed for " ++ path ++ ": " ++ errStr))
    else pure ()
