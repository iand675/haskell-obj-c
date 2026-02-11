{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings for dynamically creating new Objective-C classes at runtime.
module ObjC.Runtime.ClassBuilder
  ( objc_allocateClassPair
  , objc_registerClassPair
  , objc_disposeClassPair
  ) where

import Foreign.C.Types (CSize(..))
import Foreign.C.String (CString)
import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "objc_allocateClassPair"
  c_objc_allocateClassPair :: Class -> CString -> CSize -> IO Class

foreign import ccall unsafe "objc_registerClassPair"
  c_objc_registerClassPair :: Class -> IO ()

foreign import ccall unsafe "objc_disposeClassPair"
  c_objc_disposeClassPair :: Class -> IO ()

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Allocate a new class/metaclass pair. @superclass@ is the superclass,
-- @name@ is the class name, and @extraBytes@ is usually 0.
-- Between this call and 'objc_registerClassPair', you may add ivars
-- and methods.
objc_allocateClassPair :: Class -> CString -> CSize -> IO Class
objc_allocateClassPair = c_objc_allocateClassPair

-- | Register a class pair previously created with 'objc_allocateClassPair'.
-- After registration, the class is ready to use.
objc_registerClassPair :: Class -> IO ()
objc_registerClassPair = c_objc_registerClassPair

-- | Destroy a class pair created with 'objc_allocateClassPair'.
-- Do not call this on classes that have existing instances.
objc_disposeClassPair :: Class -> IO ()
objc_disposeClassPair = c_objc_disposeClassPair
