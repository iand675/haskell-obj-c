{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C protocol functions from @\<objc\/runtime.h\>@.
module ObjC.Runtime.Protocol
  ( -- * Obtaining protocols
    objc_getProtocol
  , objc_copyProtocolList

    -- * Creating protocols
  , objc_allocateProtocol
  , objc_registerProtocol

    -- * Modifying protocols
  , protocol_addMethodDescription
  , protocol_addProtocol
  , protocol_addProperty

    -- * Querying protocols
  , protocol_getName
  , protocol_isEqual
  , protocol_conformsToProtocol
  , protocol_copyMethodDescriptionList
  , protocol_getMethodDescription
  , protocol_copyPropertyList
  , protocol_getProperty
  , protocol_copyProtocolList
  ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CBool(..), CUInt(..))
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)

import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "objc_getProtocol"
  c_objc_getProtocol :: CString -> IO Protocol

foreign import ccall unsafe "objc_copyProtocolList"
  c_objc_copyProtocolList :: Ptr CUInt -> IO (Ptr Protocol)

foreign import ccall unsafe "objc_allocateProtocol"
  c_objc_allocateProtocol :: CString -> IO Protocol

foreign import ccall unsafe "objc_registerProtocol"
  c_objc_registerProtocol :: Protocol -> IO ()

foreign import ccall unsafe "protocol_addMethodDescription"
  c_protocol_addMethodDescription :: Protocol -> Selector -> CString -> ObjCBool -> ObjCBool -> IO ()

foreign import ccall unsafe "protocol_addProtocol"
  c_protocol_addProtocol :: Protocol -> Protocol -> IO ()

foreign import ccall unsafe "protocol_addProperty"
  c_protocol_addProperty :: Protocol -> CString -> Ptr ObjCPropertyAttribute -> CUInt -> ObjCBool -> ObjCBool -> IO ()

foreign import ccall unsafe "protocol_getName"
  c_protocol_getName :: Protocol -> IO CString

foreign import ccall unsafe "protocol_isEqual"
  c_protocol_isEqual :: Protocol -> Protocol -> IO ObjCBool

foreign import ccall unsafe "protocol_conformsToProtocol"
  c_protocol_conformsToProtocol :: Protocol -> Protocol -> IO ObjCBool

foreign import ccall unsafe "protocol_copyMethodDescriptionList"
  c_protocol_copyMethodDescriptionList :: Protocol -> ObjCBool -> ObjCBool -> Ptr CUInt -> IO (Ptr ObjCMethodDescription)

-- Note: protocol_getMethodDescription returns a struct by value.
-- We use a C wrapper or alloca to handle this. For simplicity, we
-- provide a Haskell-level wrapper that calls the raw function via
-- a helper approach. The raw C function returns the struct in registers
-- on ARM64 (it fits in 2 registers), but GHC FFI can't handle struct
-- return by value. We use a ptr-based approach instead.
--
-- On ARM64/macOS, small structs (<=16 bytes) are returned in registers.
-- ObjCMethodDescription is exactly 16 bytes (2 pointers), so we can
-- treat it as two pointer-sized returns. However, GHC's FFI doesn't
-- support this. We'll skip the raw FFI for this one function and
-- provide an alternative implementation.

foreign import ccall unsafe "protocol_copyPropertyList"
  c_protocol_copyPropertyList :: Protocol -> Ptr CUInt -> IO (Ptr ObjCProperty)

foreign import ccall unsafe "protocol_getProperty"
  c_protocol_getProperty :: Protocol -> CString -> ObjCBool -> ObjCBool -> IO ObjCProperty

foreign import ccall unsafe "protocol_copyProtocolList"
  c_protocol_copyProtocolList :: Protocol -> Ptr CUInt -> IO (Ptr Protocol)

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Look up a protocol by name. Returns a null Protocol if not found.
objc_getProtocol :: CString -> IO Protocol
objc_getProtocol = c_objc_getProtocol

-- | Copy the list of all registered protocols.
objc_copyProtocolList :: IO [Protocol]
objc_copyProtocolList =
  alloca $ \countPtr -> do
    arr <- c_objc_copyProtocolList countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Allocate a new protocol (not yet registered).
objc_allocateProtocol :: CString -> IO Protocol
objc_allocateProtocol = c_objc_allocateProtocol

-- | Register a previously allocated protocol, making it available to the
-- runtime.
objc_registerProtocol :: Protocol -> IO ()
objc_registerProtocol = c_objc_registerProtocol

-- | Add a method description to a protocol.
-- @isRequired@: whether the method is required.
-- @isInstanceMethod@: whether it is an instance method (vs class method).
protocol_addMethodDescription :: Protocol -> Selector -> CString -> Bool -> Bool -> IO ()
protocol_addMethodDescription proto sel types isReq isInst =
  c_protocol_addMethodDescription proto sel types (toObjCBool isReq) (toObjCBool isInst)

-- | Add a protocol that this protocol conforms to.
protocol_addProtocol :: Protocol -> Protocol -> IO ()
protocol_addProtocol = c_protocol_addProtocol

-- | Add a property to a protocol.
protocol_addProperty :: Protocol -> CString -> Ptr ObjCPropertyAttribute -> CUInt -> Bool -> Bool -> IO ()
protocol_addProperty proto name attrs count isReq isInst =
  c_protocol_addProperty proto name attrs count (toObjCBool isReq) (toObjCBool isInst)

-- | Get the name of a protocol.
protocol_getName :: Protocol -> IO CString
protocol_getName = c_protocol_getName

-- | Test whether two protocols are equal.
protocol_isEqual :: Protocol -> Protocol -> IO Bool
protocol_isEqual a b = fromObjCBool <$> c_protocol_isEqual a b

-- | Test whether a protocol conforms to another protocol.
protocol_conformsToProtocol :: Protocol -> Protocol -> IO Bool
protocol_conformsToProtocol a b = fromObjCBool <$> c_protocol_conformsToProtocol a b

-- | Copy the list of method descriptions in a protocol.
protocol_copyMethodDescriptionList :: Protocol -> Bool -> Bool -> IO [ObjCMethodDescription]
protocol_copyMethodDescriptionList proto isReq isInst =
  alloca $ \countPtr -> do
    arr <- c_protocol_copyMethodDescriptionList proto (toObjCBool isReq) (toObjCBool isInst) countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Get a single method description from a protocol.
-- Note: this function is not available via direct FFI because it returns
-- a struct by value. Use 'protocol_copyMethodDescriptionList' and filter
-- instead.
protocol_getMethodDescription :: Protocol -> Selector -> Bool -> Bool -> IO (Maybe ObjCMethodDescription)
protocol_getMethodDescription proto sel isReq isInst = do
  descs <- protocol_copyMethodDescriptionList proto isReq isInst
  pure (findDesc descs)
  where
    findDesc [] = Nothing
    findDesc (d:ds)
      | methodDescName d == sel = Just d
      | otherwise = findDesc ds

-- | Copy the list of properties declared by a protocol.
protocol_copyPropertyList :: Protocol -> IO [ObjCProperty]
protocol_copyPropertyList proto =
  alloca $ \countPtr -> do
    arr <- c_protocol_copyPropertyList proto countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Get a property from a protocol by name.
protocol_getProperty :: Protocol -> CString -> Bool -> Bool -> IO ObjCProperty
protocol_getProperty proto name isReq isInst =
  c_protocol_getProperty proto name (toObjCBool isReq) (toObjCBool isInst)

-- | Copy the list of protocols adopted by a protocol.
protocol_copyProtocolList :: Protocol -> IO [Protocol]
protocol_copyProtocolList proto =
  alloca $ \countPtr -> do
    arr <- c_protocol_copyProtocolList proto countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result
