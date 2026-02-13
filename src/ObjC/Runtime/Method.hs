{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C method introspection functions from
-- @\<objc\/runtime.h\>@.
module ObjC.Runtime.Method
  ( -- * Method operations
    method_getName
  , method_getImplementation
  , method_getTypeEncoding
  , method_getNumberOfArguments
  , method_copyReturnType
  , method_copyArgumentType
  , method_getReturnType
  , method_getArgumentType
  , method_setImplementation
  , method_exchangeImplementations
  , method_getDescription
  ) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUInt(..), CSize(..))
import Foreign.C.String (CString)

import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "method_getName"
  c_method_getName :: Method -> IO (Ptr ObjCSel)

foreign import ccall unsafe "method_getImplementation"
  c_method_getImplementation :: Method -> IO IMP

foreign import ccall unsafe "method_getTypeEncoding"
  c_method_getTypeEncoding :: Method -> IO CString

foreign import ccall unsafe "method_getNumberOfArguments"
  c_method_getNumberOfArguments :: Method -> IO CUInt

foreign import ccall unsafe "method_copyReturnType"
  c_method_copyReturnType :: Method -> IO CString

foreign import ccall unsafe "method_copyArgumentType"
  c_method_copyArgumentType :: Method -> CUInt -> IO CString

foreign import ccall unsafe "method_getReturnType"
  c_method_getReturnType :: Method -> CString -> CSize -> IO ()

foreign import ccall unsafe "method_getArgumentType"
  c_method_getArgumentType :: Method -> CUInt -> CString -> CSize -> IO ()

foreign import ccall unsafe "method_setImplementation"
  c_method_setImplementation :: Method -> IMP -> IO IMP

foreign import ccall unsafe "method_exchangeImplementations"
  c_method_exchangeImplementations :: Method -> Method -> IO ()

foreign import ccall unsafe "method_getDescription"
  c_method_getDescription :: Method -> IO (Ptr ObjCMethodDescription)

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Get the selector of a method.
method_getName :: Method -> IO Sel
method_getName m = Selector <$> c_method_getName m

-- | Get the implementation (function pointer) of a method.
method_getImplementation :: Method -> IO IMP
method_getImplementation = c_method_getImplementation

-- | Get the type encoding string of a method.
method_getTypeEncoding :: Method -> IO CString
method_getTypeEncoding = c_method_getTypeEncoding

-- | Get the number of arguments a method accepts (including self and _cmd).
method_getNumberOfArguments :: Method -> IO CUInt
method_getNumberOfArguments = c_method_getNumberOfArguments

-- | Copy the return type string. The caller must 'free' the result.
method_copyReturnType :: Method -> IO CString
method_copyReturnType = c_method_copyReturnType

-- | Copy the argument type string at the given index. The caller must
-- 'free' the result.
method_copyArgumentType :: Method -> CUInt -> IO CString
method_copyArgumentType = c_method_copyArgumentType

-- | Get the return type into a caller-supplied buffer.
method_getReturnType :: Method -> CString -> CSize -> IO ()
method_getReturnType = c_method_getReturnType

-- | Get the argument type at an index into a caller-supplied buffer.
method_getArgumentType :: Method -> CUInt -> CString -> CSize -> IO ()
method_getArgumentType = c_method_getArgumentType

-- | Set the implementation of a method. Returns the old implementation.
method_setImplementation :: Method -> IMP -> IO IMP
method_setImplementation = c_method_setImplementation

-- | Exchange the implementations of two methods (method swizzling).
method_exchangeImplementations :: Method -> Method -> IO ()
method_exchangeImplementations = c_method_exchangeImplementations

-- | Get a pointer to the method description struct. The pointer is owned
-- by the runtime; do not free it.
method_getDescription :: Method -> IO (Ptr ObjCMethodDescription)
method_getDescription = c_method_getDescription
