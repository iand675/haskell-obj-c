{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bindings to Objective-C object instance functions from
-- @\<objc\/runtime.h\>@, plus convenience helpers for the fundamental
-- @+alloc@ / @-init@ lifecycle.
module ObjC.Runtime.Object
  ( -- * Object lifecycle
    alloc
  , objcInit
  , new

    -- * Low-level object operations (raw 'RawId')
  , object_copy
  , object_dispose
  , object_getClass
  , object_setClass
  , object_isClass
  , object_getClassName
  , object_getIvar
  , object_setIvar
  , object_setIvarWithStrongDefault
  , object_getInstanceVariable
  , object_setInstanceVariable
  ) where

import Data.Proxy (Proxy(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CBool(..), CSize(..))
import Foreign.C.String (CString)
import Foreign.LibFFI (retPtr, retVoid)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)

-- ---------------------------------------------------------------------------
-- Raw FFI (all take/return RawId — unmanaged pointers)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "object_copy"
  c_object_copy :: RawId -> CSize -> IO RawId

foreign import ccall unsafe "object_dispose"
  c_object_dispose :: RawId -> IO RawId

foreign import ccall unsafe "object_getClass"
  c_object_getClass :: RawId -> IO Class

foreign import ccall unsafe "object_setClass"
  c_object_setClass :: RawId -> Class -> IO Class

foreign import ccall unsafe "object_isClass"
  c_object_isClass :: RawId -> IO ObjCBool

foreign import ccall unsafe "object_getClassName"
  c_object_getClassName :: RawId -> IO CString

foreign import ccall unsafe "object_getIvar"
  c_object_getIvar :: RawId -> Ivar -> IO RawId

foreign import ccall unsafe "object_setIvar"
  c_object_setIvar :: RawId -> Ivar -> RawId -> IO ()

foreign import ccall unsafe "object_setIvarWithStrongDefault"
  c_object_setIvarWithStrongDefault :: RawId -> Ivar -> RawId -> IO ()

foreign import ccall unsafe "object_getInstanceVariable"
  c_object_getInstanceVariable :: RawId -> CString -> Ptr (Ptr ()) -> IO Ivar

foreign import ccall unsafe "object_setInstanceVariable"
  c_object_setInstanceVariable :: RawId -> CString -> Ptr () -> IO Ivar

-- ---------------------------------------------------------------------------
-- Haskell wrappers (operate on RawId)
-- ---------------------------------------------------------------------------

-- | Return a copy of an object. @size@ is the size of the object.
object_copy :: RawId -> CSize -> IO RawId
object_copy = c_object_copy

-- | Free the memory occupied by an object. Returns nil.
object_dispose :: RawId -> IO RawId
object_dispose = c_object_dispose

-- | Return the class of an object.
object_getClass :: RawId -> IO Class
object_getClass = c_object_getClass

-- | Set the class of an object. Returns the old class.
object_setClass :: RawId -> Class -> IO Class
object_setClass = c_object_setClass

-- | Return whether an object is a class or metaclass.
object_isClass :: RawId -> IO Bool
object_isClass obj = fromObjCBool <$> c_object_isClass obj

-- | Return the class name of an object as a C string.
object_getClassName :: RawId -> IO CString
object_getClassName = c_object_getClassName

-- | Read the value of an instance variable.
object_getIvar :: RawId -> Ivar -> IO RawId
object_getIvar = c_object_getIvar

-- | Set the value of an instance variable (unsafe_unretained semantics
-- for unknown memory management).
object_setIvar :: RawId -> Ivar -> RawId -> IO ()
object_setIvar = c_object_setIvar

-- | Set the value of an instance variable (strong semantics for unknown
-- memory management).
object_setIvarWithStrongDefault :: RawId -> Ivar -> RawId -> IO ()
object_setIvarWithStrongDefault = c_object_setIvarWithStrongDefault

-- | Get an instance variable by name. The value is written to the output
-- pointer. Returns the 'Ivar' descriptor.
object_getInstanceVariable :: RawId -> CString -> Ptr (Ptr ()) -> IO Ivar
object_getInstanceVariable = c_object_getInstanceVariable

-- | Set an instance variable by name.
object_setInstanceVariable :: RawId -> CString -> Ptr () -> IO Ivar
object_setInstanceVariable = c_object_setInstanceVariable

-- ---------------------------------------------------------------------------
-- Object lifecycle (+alloc / -init) — managed Id a
-- ---------------------------------------------------------------------------

-- | Send @+[Class alloc]@ and return an unmanaged, typed 'Id'.
--
-- The returned object has __no finalizer__ — it must be passed to an
-- @init@ method (which returns a properly managed 'Id').
--
-- The class is determined by the return type via 'staticClass'. Use with
-- @TypeApplications@ when the return type is ambiguous:
--
-- @
-- obj <- alloc \@NSWindow
-- @
alloc :: forall a. IsObjCObject (Id a) => IO (Id a)
alloc = do
  cls <- staticClass (Proxy @(Id a))
  raw <- sendClassMsg cls (mkSelector "alloc") (retPtr retVoid) []
  unmanagedObject (castPtr raw)

-- | Send @-[obj init]@ — the plain @NSObject@ initializer.
--
-- Returns a properly managed 'Id' with a release finalizer (the init
-- result is a +1 owned reference).
--
-- The input object (typically from 'alloc') may differ from the output
-- due to class clusters; always use the returned value.
objcInit :: IsObjCObject (Id a) => Id a -> IO (Id a)
objcInit obj = do
  raw <- sendMsg obj (mkSelector "init") (retPtr retVoid) []
  ownedObject (castPtr raw)

-- | @\[\[Class alloc\] init\]@ — allocate and default-initialize.
--
-- Equivalent to ObjC's @+new@. The class is inferred from the return type:
--
-- @
-- menuItem <- new \@NSMenuItem
-- @
new :: forall a. IsObjCObject (Id a) => IO (Id a)
new = alloc >>= objcInit
