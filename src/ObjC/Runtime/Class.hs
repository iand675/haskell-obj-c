{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C class and class-lookup functions from
-- @\<objc\/runtime.h\>@.
module ObjC.Runtime.Class
  ( -- * Obtaining class definitions
    objc_getClass
  , objc_getMetaClass
  , objc_lookUpClass
  , objc_getRequiredClass
  , objc_getClassList
  , objc_copyClassList

    -- * Working with classes
  , class_getName
  , class_getSuperclass
  , class_isMetaClass
  , class_getInstanceSize
  , class_getVersion
  , class_setVersion
  , class_respondsToSelector

    -- * Instance variables
  , class_getInstanceVariable
  , class_getClassVariable
  , class_copyIvarList

    -- * Methods
  , class_getInstanceMethod
  , class_getClassMethod
  , class_copyMethodList
  , class_getMethodImplementation
  , class_addMethod
  , class_replaceMethod

    -- * Protocols
  , class_conformsToProtocol
  , class_copyProtocolList
  , class_addProtocol

    -- * Properties
  , class_getProperty
  , class_copyPropertyList
  , class_addProperty
  , class_replaceProperty

    -- * Ivars (adding)
  , class_addIvar

    -- * Ivar layout
  , class_getIvarLayout
  , class_setIvarLayout
  , class_getWeakIvarLayout
  , class_setWeakIvarLayout

    -- * Instance creation
  , class_createInstance

    -- * Convenience
  , getClass
  , getRequiredClass
  , className
  ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CBool(..), CInt(..), CSize(..), CUInt(..))
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (alloca)

import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI — Obtaining class definitions
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "objc_getClass"
  c_objc_getClass :: CString -> IO Class

foreign import ccall unsafe "objc_getMetaClass"
  c_objc_getMetaClass :: CString -> IO Class

foreign import ccall unsafe "objc_lookUpClass"
  c_objc_lookUpClass :: CString -> IO Class

foreign import ccall unsafe "objc_getRequiredClass"
  c_objc_getRequiredClass :: CString -> IO Class

foreign import ccall unsafe "objc_getClassList"
  c_objc_getClassList :: Ptr Class -> CInt -> IO CInt

foreign import ccall unsafe "objc_copyClassList"
  c_objc_copyClassList :: Ptr CUInt -> IO (Ptr Class)

-- ---------------------------------------------------------------------------
-- Raw FFI — Working with classes
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_getName"
  c_class_getName :: Class -> IO CString

foreign import ccall unsafe "class_getSuperclass"
  c_class_getSuperclass :: Class -> IO Class

foreign import ccall unsafe "class_isMetaClass"
  c_class_isMetaClass :: Class -> IO ObjCBool

foreign import ccall unsafe "class_getInstanceSize"
  c_class_getInstanceSize :: Class -> IO CSize

foreign import ccall unsafe "class_getVersion"
  c_class_getVersion :: Class -> IO CInt

foreign import ccall unsafe "class_setVersion"
  c_class_setVersion :: Class -> CInt -> IO ()

foreign import ccall unsafe "class_respondsToSelector"
  c_class_respondsToSelector :: Class -> Ptr ObjCSel -> IO ObjCBool

-- ---------------------------------------------------------------------------
-- Raw FFI — Instance variables
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_getInstanceVariable"
  c_class_getInstanceVariable :: Class -> CString -> IO Ivar

foreign import ccall unsafe "class_getClassVariable"
  c_class_getClassVariable :: Class -> CString -> IO Ivar

foreign import ccall unsafe "class_copyIvarList"
  c_class_copyIvarList :: Class -> Ptr CUInt -> IO (Ptr Ivar)

-- ---------------------------------------------------------------------------
-- Raw FFI — Methods
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_getInstanceMethod"
  c_class_getInstanceMethod :: Class -> Ptr ObjCSel -> IO Method

foreign import ccall unsafe "class_getClassMethod"
  c_class_getClassMethod :: Class -> Ptr ObjCSel -> IO Method

foreign import ccall unsafe "class_copyMethodList"
  c_class_copyMethodList :: Class -> Ptr CUInt -> IO (Ptr Method)

foreign import ccall unsafe "class_getMethodImplementation"
  c_class_getMethodImplementation :: Class -> Ptr ObjCSel -> IO IMP

foreign import ccall unsafe "class_addMethod"
  c_class_addMethod :: Class -> Ptr ObjCSel -> IMP -> CString -> IO ObjCBool

foreign import ccall unsafe "class_replaceMethod"
  c_class_replaceMethod :: Class -> Ptr ObjCSel -> IMP -> CString -> IO IMP

-- ---------------------------------------------------------------------------
-- Raw FFI — Protocols
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_conformsToProtocol"
  c_class_conformsToProtocol :: Class -> Protocol -> IO ObjCBool

foreign import ccall unsafe "class_copyProtocolList"
  c_class_copyProtocolList :: Class -> Ptr CUInt -> IO (Ptr Protocol)

foreign import ccall unsafe "class_addProtocol"
  c_class_addProtocol :: Class -> Protocol -> IO ObjCBool

-- ---------------------------------------------------------------------------
-- Raw FFI — Properties
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_getProperty"
  c_class_getProperty :: Class -> CString -> IO ObjCProperty

foreign import ccall unsafe "class_copyPropertyList"
  c_class_copyPropertyList :: Class -> Ptr CUInt -> IO (Ptr ObjCProperty)

foreign import ccall unsafe "class_addProperty"
  c_class_addProperty :: Class -> CString -> Ptr ObjCPropertyAttribute -> CUInt -> IO ObjCBool

foreign import ccall unsafe "class_replaceProperty"
  c_class_replaceProperty :: Class -> CString -> Ptr ObjCPropertyAttribute -> CUInt -> IO ()

-- ---------------------------------------------------------------------------
-- Raw FFI — Ivars (adding)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_addIvar"
  c_class_addIvar :: Class -> CString -> CSize -> Word8 -> CString -> IO ObjCBool

-- ---------------------------------------------------------------------------
-- Raw FFI — Ivar layout
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_getIvarLayout"
  c_class_getIvarLayout :: Class -> IO (Ptr Word8)

foreign import ccall unsafe "class_setIvarLayout"
  c_class_setIvarLayout :: Class -> Ptr Word8 -> IO ()

foreign import ccall unsafe "class_getWeakIvarLayout"
  c_class_getWeakIvarLayout :: Class -> IO (Ptr Word8)

foreign import ccall unsafe "class_setWeakIvarLayout"
  c_class_setWeakIvarLayout :: Class -> Ptr Word8 -> IO ()

-- ---------------------------------------------------------------------------
-- Raw FFI — Instance creation
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "class_createInstance"
  c_class_createInstance :: Class -> CSize -> IO RawId

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Look up a class by name. Returns 'nilClass' if not found.
objc_getClass :: CString -> IO Class
objc_getClass = c_objc_getClass

-- | Look up a metaclass by name.
objc_getMetaClass :: CString -> IO Class
objc_getMetaClass = c_objc_getMetaClass

-- | Look up a class by name. Unlike 'objc_getClass', does not call the
-- class handler callback if the class is not found.
objc_lookUpClass :: CString -> IO Class
objc_lookUpClass = c_objc_lookUpClass

-- | Like 'objc_getClass' but aborts the process if the class is not found.
objc_getRequiredClass :: CString -> IO Class
objc_getRequiredClass = c_objc_getRequiredClass

-- | Get the total number of registered classes and optionally fill a buffer.
objc_getClassList :: Ptr Class -> CInt -> IO CInt
objc_getClassList = c_objc_getClassList

-- | Copy the list of all registered classes. The returned array must be
-- freed with 'free'. The count is written to the output parameter.
objc_copyClassList :: Ptr CUInt -> IO (Ptr Class)
objc_copyClassList = c_objc_copyClassList

-- | Get the name of a class as a C string.
class_getName :: Class -> IO CString
class_getName = c_class_getName

-- | Get the superclass of a class. Returns 'nilClass' for root classes.
class_getSuperclass :: Class -> IO Class
class_getSuperclass = c_class_getSuperclass

-- | Test whether a class is a metaclass.
class_isMetaClass :: Class -> IO Bool
class_isMetaClass cls = fromObjCBool <$> c_class_isMetaClass cls

-- | Get the size in bytes of instances of a class.
class_getInstanceSize :: Class -> IO CSize
class_getInstanceSize = c_class_getInstanceSize

-- | Get the version of a class definition.
class_getVersion :: Class -> IO CInt
class_getVersion = c_class_getVersion

-- | Set the version of a class definition.
class_setVersion :: Class -> CInt -> IO ()
class_setVersion = c_class_setVersion

-- | Test whether instances of a class respond to a selector.
class_respondsToSelector :: Class -> Selector args ret -> IO Bool
class_respondsToSelector cls (Selector sel) = fromObjCBool <$> c_class_respondsToSelector cls sel

class_getInstanceVariable :: Class -> CString -> IO Ivar
class_getInstanceVariable = c_class_getInstanceVariable

class_getClassVariable :: Class -> CString -> IO Ivar
class_getClassVariable = c_class_getClassVariable

-- | Copy the list of instance variables declared by a class.
-- Returns the ivars and their count. The caller does NOT need to free
-- the returned list (it is copied into a Haskell list).
class_copyIvarList :: Class -> IO [Ivar]
class_copyIvarList cls =
  alloca $ \countPtr -> do
    arr <- c_class_copyIvarList cls countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

class_getInstanceMethod :: Class -> Selector args ret -> IO Method
class_getInstanceMethod cls (Selector sel) = c_class_getInstanceMethod cls sel

class_getClassMethod :: Class -> Selector args ret -> IO Method
class_getClassMethod cls (Selector sel) = c_class_getClassMethod cls sel

-- | Copy the list of instance methods implemented directly by a class.
class_copyMethodList :: Class -> IO [Method]
class_copyMethodList cls =
  alloca $ \countPtr -> do
    arr <- c_class_copyMethodList cls countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Get the IMP for a given selector on a class.
class_getMethodImplementation :: Class -> Selector args ret -> IO IMP
class_getMethodImplementation cls (Selector sel) = c_class_getMethodImplementation cls sel

-- | Add a method to a class. Returns 'True' if successful.
class_addMethod :: Class -> Selector args ret -> IMP -> CString -> IO Bool
class_addMethod cls (Selector sel) imp types = fromObjCBool <$> c_class_addMethod cls sel imp types

-- | Replace the implementation of a method. Returns the old IMP.
class_replaceMethod :: Class -> Selector args ret -> IMP -> CString -> IO IMP
class_replaceMethod cls (Selector sel) imp types = c_class_replaceMethod cls sel imp types

-- | Test whether a class conforms to a protocol.
class_conformsToProtocol :: Class -> Protocol -> IO Bool
class_conformsToProtocol cls proto = fromObjCBool <$> c_class_conformsToProtocol cls proto

-- | Copy the list of protocols adopted by a class.
class_copyProtocolList :: Class -> IO [Protocol]
class_copyProtocolList cls =
  alloca $ \countPtr -> do
    arr <- c_class_copyProtocolList cls countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Add a protocol to a class. Returns 'True' if successful.
class_addProtocol :: Class -> Protocol -> IO Bool
class_addProtocol cls proto = fromObjCBool <$> c_class_addProtocol cls proto

-- | Get a property by name.
class_getProperty :: Class -> CString -> IO ObjCProperty
class_getProperty = c_class_getProperty

-- | Copy the list of properties declared by a class.
class_copyPropertyList :: Class -> IO [ObjCProperty]
class_copyPropertyList cls =
  alloca $ \countPtr -> do
    arr <- c_class_copyPropertyList cls countPtr
    if arr == nullPtr
      then pure []
      else do
        count <- fromIntegral <$> peek countPtr
        result <- peekArray count arr
        free arr
        pure result

-- | Add a property to a class.
class_addProperty :: Class -> CString -> Ptr ObjCPropertyAttribute -> CUInt -> IO Bool
class_addProperty cls name attrs count = fromObjCBool <$> c_class_addProperty cls name attrs count

-- | Replace a property on a class.
class_replaceProperty :: Class -> CString -> Ptr ObjCPropertyAttribute -> CUInt -> IO ()
class_replaceProperty = c_class_replaceProperty

-- | Add an instance variable to a class being constructed (between
-- 'objc_allocateClassPair' and 'objc_registerClassPair').
class_addIvar :: Class -> CString -> CSize -> Word8 -> CString -> IO Bool
class_addIvar cls name size alignment types =
  fromObjCBool <$> c_class_addIvar cls name size alignment types

class_getIvarLayout :: Class -> IO (Ptr Word8)
class_getIvarLayout = c_class_getIvarLayout

class_setIvarLayout :: Class -> Ptr Word8 -> IO ()
class_setIvarLayout = c_class_setIvarLayout

class_getWeakIvarLayout :: Class -> IO (Ptr Word8)
class_getWeakIvarLayout = c_class_getWeakIvarLayout

class_setWeakIvarLayout :: Class -> Ptr Word8 -> IO ()
class_setWeakIvarLayout = c_class_setWeakIvarLayout

-- | Create a new instance of a class, allocating memory in the default
-- malloc zone. @extraBytes@ are for indexed ivars.
class_createInstance :: Class -> CSize -> IO RawId
class_createInstance = c_class_createInstance

-- ---------------------------------------------------------------------------
-- Convenience
-- ---------------------------------------------------------------------------

-- | Look up a class by Haskell 'String'. Returns 'Nothing' if not registered.
getClass :: String -> IO (Maybe Class)
getClass name = do
  cls <- withCString name c_objc_getClass
  pure (if cls == nilClass then Nothing else Just cls)

-- | Look up a class by Haskell 'String'. Throws an 'error' if the class
-- is not registered with the Objective-C runtime.
getRequiredClass :: String -> IO Class
getRequiredClass name = do
  cls <- withCString name c_objc_getClass
  if cls == nilClass
    then error ("getRequiredClass: class not found: " ++ name)
    else pure cls

-- | Get the name of a class as a Haskell 'String'.
className :: Class -> IO String
className cls = c_class_getName cls >>= peekCString
