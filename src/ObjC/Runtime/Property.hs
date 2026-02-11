{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C property introspection functions from
-- @\<objc\/runtime.h\>@.
module ObjC.Runtime.Property
  ( property_getName
  , property_getAttributes
  , property_copyAttributeList
  , property_copyAttributeValue
  ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CUInt(..))
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)

import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "property_getName"
  c_property_getName :: ObjCProperty -> IO CString

foreign import ccall unsafe "property_getAttributes"
  c_property_getAttributes :: ObjCProperty -> IO CString

foreign import ccall unsafe "property_copyAttributeList"
  c_property_copyAttributeList :: ObjCProperty -> Ptr CUInt -> IO (Ptr ObjCPropertyAttribute)

foreign import ccall unsafe "property_copyAttributeValue"
  c_property_copyAttributeValue :: ObjCProperty -> CString -> IO CString

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Get the name of a property.
property_getName :: ObjCProperty -> IO CString
property_getName = c_property_getName

-- | Get the attribute string of a property.
property_getAttributes :: ObjCProperty -> IO CString
property_getAttributes = c_property_getAttributes

-- | Copy the list of property attributes. Returns a Haskell list; the
-- C array is freed automatically.
property_copyAttributeList :: ObjCProperty -> IO [ObjCPropertyAttribute]
property_copyAttributeList prop =
  alloca $ \countPtr -> do
    arr <- c_property_copyAttributeList prop countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Copy the value for a named property attribute. The returned C string
-- must be freed by the caller with 'free'.
property_copyAttributeValue :: ObjCProperty -> CString -> IO CString
property_copyAttributeValue = c_property_copyAttributeValue
