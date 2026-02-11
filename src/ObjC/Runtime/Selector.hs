{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C selector functions from @\<objc\/runtime.h\>@.
module ObjC.Runtime.Selector
  ( -- * Selector operations
    sel_registerName
  , sel_getName
  , sel_isEqual

    -- * Convenience
  , mkSelector
  , selName
  ) where

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CBool(..))
import System.IO.Unsafe (unsafePerformIO)
import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "sel_registerName"
  c_sel_registerName :: CString -> IO Selector

foreign import ccall unsafe "sel_getName"
  c_sel_getName :: Selector -> IO CString

foreign import ccall unsafe "sel_isEqual"
  c_sel_isEqual :: Selector -> Selector -> IO ObjCBool

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Register (or look up) a selector by name. Maps a C string to a @Selector@.
sel_registerName :: CString -> IO Selector
sel_registerName = c_sel_registerName

-- | Get the name of a selector as a C string.
sel_getName :: Selector -> IO CString
sel_getName = c_sel_getName

-- | Test whether two selectors are equal.
sel_isEqual :: Selector -> Selector -> IO Bool
sel_isEqual a b = fromObjCBool <$> c_sel_isEqual a b

-- ---------------------------------------------------------------------------
-- Convenience
-- ---------------------------------------------------------------------------

-- | Create a selector from a Haskell 'String'.
--
-- This is pure because @sel_registerName@ is idempotent and thread-safe:
-- it always returns the same @Selector@ pointer for a given name.
mkSelector :: String -> Selector
mkSelector name = unsafePerformIO (withCString name c_sel_registerName)
{-# NOINLINE mkSelector #-}

-- | Get the name of a selector as a Haskell 'String'.
selName :: Selector -> IO String
selName sel = c_sel_getName sel >>= peekCString
