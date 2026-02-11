{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C associated object functions from
-- @\<objc\/runtime.h\>@.
module ObjC.Runtime.Association
  ( objc_setAssociatedObject
  , objc_getAssociatedObject
  , objc_removeAssociatedObjects
  ) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUIntPtr(..))
import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "objc_setAssociatedObject"
  c_objc_setAssociatedObject :: RawId -> Ptr () -> RawId -> ObjCAssociationPolicy -> IO ()

foreign import ccall unsafe "objc_getAssociatedObject"
  c_objc_getAssociatedObject :: RawId -> Ptr () -> IO RawId

foreign import ccall unsafe "objc_removeAssociatedObjects"
  c_objc_removeAssociatedObjects :: RawId -> IO ()

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Set an associated value on an object for a given key and policy.
-- The key is typically the address of a static variable.
objc_setAssociatedObject :: RawId -> Ptr () -> RawId -> ObjCAssociationPolicy -> IO ()
objc_setAssociatedObject = c_objc_setAssociatedObject

-- | Get the associated value for a given key.
objc_getAssociatedObject :: RawId -> Ptr () -> IO RawId
objc_getAssociatedObject = c_objc_getAssociatedObject

-- | Remove all associated objects from an object. This is mainly for
-- use by the runtime; you typically should not call this.
objc_removeAssociatedObjects :: RawId -> IO ()
objc_removeAssociatedObjects = c_objc_removeAssociatedObjects
