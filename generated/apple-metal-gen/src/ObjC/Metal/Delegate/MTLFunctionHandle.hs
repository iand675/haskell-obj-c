{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLFunctionHandle@.
--
-- Usage:
--
-- @
-- delegate <- newMTLFunctionHandle defaultMTLFunctionHandleOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLFunctionHandle
  ( MTLFunctionHandleOverrides(..)
  , defaultMTLFunctionHandleOverrides
  , newMTLFunctionHandle
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol MTLFunctionHandle@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLFunctionHandleOverrides = MTLFunctionHandleOverrides
  { _name :: !(Maybe (IO RawId))
  , _device :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLFunctionHandleOverrides :: MTLFunctionHandleOverrides
defaultMTLFunctionHandleOverrides = MTLFunctionHandleOverrides
  { _name = Nothing
  , _device = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlFunctionHandleDelegateClass #-}
mtlFunctionHandleDelegateClass :: Class
mtlFunctionHandleDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLFunctionHandle" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_name = unSelector (mkSelector "name")
      sel_device = unSelector (mkSelector "device")
  -- name
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionHandleOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_0

  -- device
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionHandleOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionHandleOverrides
    if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newMTLFunctionHandle :: MTLFunctionHandleOverrides -> IO RawId
newMTLFunctionHandle overrides = do
  inst <- class_createInstance mtlFunctionHandleDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
