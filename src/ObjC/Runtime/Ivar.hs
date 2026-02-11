{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C instance variable introspection functions from
-- @\<objc\/runtime.h\>@.
module ObjC.Runtime.Ivar
  ( ivar_getName
  , ivar_getTypeEncoding
  , ivar_getOffset
  ) where

import Foreign.C.Types (CPtrdiff(..))
import Foreign.C.String (CString)

import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "ivar_getName"
  c_ivar_getName :: Ivar -> IO CString

foreign import ccall unsafe "ivar_getTypeEncoding"
  c_ivar_getTypeEncoding :: Ivar -> IO CString

foreign import ccall unsafe "ivar_getOffset"
  c_ivar_getOffset :: Ivar -> IO CPtrdiff

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Get the name of an instance variable.
ivar_getName :: Ivar -> IO CString
ivar_getName = c_ivar_getName

-- | Get the type encoding of an instance variable.
ivar_getTypeEncoding :: Ivar -> IO CString
ivar_getTypeEncoding = c_ivar_getTypeEncoding

-- | Get the byte offset of an instance variable from the start of the object.
ivar_getOffset :: Ivar -> IO CPtrdiff
ivar_getOffset = c_ivar_getOffset
