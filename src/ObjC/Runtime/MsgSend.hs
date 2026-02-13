{-# LANGUAGE ForeignFunctionInterface #-}

-- | Dynamic Objective-C message sending via @libffi@.
--
-- On ARM64, @objc_msgSend@ is a trampoline whose calling convention must
-- match the target method exactly. We use @libffi@ to construct the
-- correct call frame at runtime, given the argument and return types.
--
-- Usage example (sending @length@ to an NSString):
--
-- @
-- len <- sendMsg nsString lengthSel (retCULong) []
-- @
module ObjC.Runtime.MsgSend
  ( -- * Core message sending (polymorphic receiver)
    sendMsg
  , sendMsgFpret
  , sendMsgStret

    -- * Raw message sending (takes 'RawId')
  , sendRawMsg
  , sendRawMsgFpret
  , sendRawMsgStret

    -- * Class message sending
  , sendClassMsg
  , sendClassMsgFpret
  , sendClassMsgStret

    -- * Super message sending
  , sendSuperMsg

    -- * Low-level access to objc_msgSend function pointers
  , objcMsgSendPtr
  , objcMsgSendSuperPtr
  , objcMsgSendStretPtr
  , objcMsgSendFpretPtr
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtrToFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke)
import Foreign.LibFFI (Arg, RetType, callFFI, argPtr)
import System.IO.Unsafe (unsafePerformIO)

import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- FFI imports for the C stubs that return objc_msgSend addresses
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "hs_objc_msgSend_ptr"
  c_hs_objc_msgSend_ptr :: IO (Ptr ())

foreign import ccall unsafe "hs_objc_msgSendSuper_ptr"
  c_hs_objc_msgSendSuper_ptr :: IO (Ptr ())

foreign import ccall unsafe "hs_objc_msgSend_stret_ptr"
  c_hs_objc_msgSend_stret_ptr :: IO (Ptr ())

foreign import ccall unsafe "hs_objc_msgSend_fpret_ptr"
  c_hs_objc_msgSend_fpret_ptr :: IO (Ptr ())

-- ---------------------------------------------------------------------------
-- Cached function pointers (evaluated once via unsafePerformIO)
-- ---------------------------------------------------------------------------

-- | Function pointer to @objc_msgSend@.
objcMsgSendPtr :: FunPtr a
objcMsgSendPtr = unsafePerformIO (castPtrToFunPtr <$> c_hs_objc_msgSend_ptr)
{-# NOINLINE objcMsgSendPtr #-}

-- | Function pointer to @objc_msgSendSuper@.
objcMsgSendSuperPtr :: FunPtr a
objcMsgSendSuperPtr = unsafePerformIO (castPtrToFunPtr <$> c_hs_objc_msgSendSuper_ptr)
{-# NOINLINE objcMsgSendSuperPtr #-}

-- | Function pointer to @objc_msgSend_stret@ (or @objc_msgSend@ on ARM64).
objcMsgSendStretPtr :: FunPtr a
objcMsgSendStretPtr = unsafePerformIO (castPtrToFunPtr <$> c_hs_objc_msgSend_stret_ptr)
{-# NOINLINE objcMsgSendStretPtr #-}

-- | Function pointer to @objc_msgSend_fpret@ (or @objc_msgSend@ on ARM64).
objcMsgSendFpretPtr :: FunPtr a
objcMsgSendFpretPtr = unsafePerformIO (castPtrToFunPtr <$> c_hs_objc_msgSend_fpret_ptr)
{-# NOINLINE objcMsgSendFpretPtr #-}

-- ---------------------------------------------------------------------------
-- Polymorphic message sending (receiver is any IsObjCObject)
-- ---------------------------------------------------------------------------

-- | Send a message to any 'IsObjCObject' receiver.
--
-- Uses 'withObjCPtr' to keep the receiver alive for the duration of the call.
sendMsg :: IsObjCObject recv => recv -> Selector args ret -> RetType a -> [Arg] -> IO a
sendMsg recv (Selector sel) retType args =
  withObjCPtr recv $ \ptr ->
    callFFI objcMsgSendPtr retType (argPtr ptr : argPtr sel : args)

-- | Send a message that returns a floating-point value.
-- On x86_64, this uses @objc_msgSend_fpret@. On ARM64, identical to 'sendMsg'.
sendMsgFpret :: IsObjCObject recv => recv -> Selector args ret -> RetType a -> [Arg] -> IO a
sendMsgFpret recv (Selector sel) retType args =
  withObjCPtr recv $ \ptr ->
    callFFI objcMsgSendFpretPtr retType (argPtr ptr : argPtr sel : args)

-- | Send a message that returns a struct.
-- On x86_64, this uses @objc_msgSend_stret@. On ARM64, identical to 'sendMsg'.
sendMsgStret :: IsObjCObject recv => recv -> Selector args ret -> RetType a -> [Arg] -> IO a
sendMsgStret recv (Selector sel) retType args =
  withObjCPtr recv $ \ptr ->
    callFFI objcMsgSendStretPtr retType (argPtr ptr : argPtr sel : args)

-- ---------------------------------------------------------------------------
-- Raw message sending (takes RawId directly)
-- ---------------------------------------------------------------------------

-- | Send a message using a raw 'RawId' receiver. For low-level FFI use.
sendRawMsg :: RawId -> Selector args ret -> RetType a -> [Arg] -> IO a
sendRawMsg (RawId obj) (Selector sel) retType args =
  callFFI objcMsgSendPtr retType (argPtr obj : argPtr sel : args)

-- | Raw variant of 'sendMsgFpret'.
sendRawMsgFpret :: RawId -> Selector args ret -> RetType a -> [Arg] -> IO a
sendRawMsgFpret (RawId obj) (Selector sel) retType args =
  callFFI objcMsgSendFpretPtr retType (argPtr obj : argPtr sel : args)

-- | Raw variant of 'sendMsgStret'.
sendRawMsgStret :: RawId -> Selector args ret -> RetType a -> [Arg] -> IO a
sendRawMsgStret (RawId obj) (Selector sel) retType args =
  callFFI objcMsgSendStretPtr retType (argPtr obj : argPtr sel : args)

-- ---------------------------------------------------------------------------
-- Class message sending (unchanged — Class is already a raw pointer)
-- ---------------------------------------------------------------------------

-- | Send a message to an Objective-C class (class method).
sendClassMsg :: Class -> Selector args ret -> RetType a -> [Arg] -> IO a
sendClassMsg (Class cls) (Selector sel) retType args =
  callFFI objcMsgSendPtr retType (argPtr cls : argPtr sel : args)

-- | Class method variant of 'sendMsgFpret'.
sendClassMsgFpret :: Class -> Selector args ret -> RetType a -> [Arg] -> IO a
sendClassMsgFpret (Class cls) (Selector sel) retType args =
  callFFI objcMsgSendFpretPtr retType (argPtr cls : argPtr sel : args)

-- | Class method variant of 'sendMsgStret'.
sendClassMsgStret :: Class -> Selector args ret -> RetType a -> [Arg] -> IO a
sendClassMsgStret (Class cls) (Selector sel) retType args =
  callFFI objcMsgSendStretPtr retType (argPtr cls : argPtr sel : args)

-- ---------------------------------------------------------------------------
-- Super message sending
-- ---------------------------------------------------------------------------

-- | Send a message to the superclass implementation.
--
-- You must construct an 'ObjCSuper' struct and pass a pointer to it as
-- the first argument. The selector is the second argument.
sendSuperMsg :: ObjCSuper -> Selector args ret -> RetType a -> [Arg] -> IO a
sendSuperMsg super_ (Selector sel) retType args =
  alloca $ \superPtr -> do
    poke superPtr super_
    callFFI objcMsgSendSuperPtr retType (argPtr superPtr : argPtr sel : args)
