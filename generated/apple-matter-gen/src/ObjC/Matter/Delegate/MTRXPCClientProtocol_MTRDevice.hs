{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRXPCClientProtocol_MTRDevice@.
--
-- Usage:
--
-- @
-- delegate <- newMTRXPCClientProtocol_MTRDevice defaultMTRXPCClientProtocol_MTRDeviceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRXPCClientProtocol_MTRDevice
  ( MTRXPCClientProtocol_MTRDeviceOverrides(..)
  , defaultMTRXPCClientProtocol_MTRDeviceOverrides
  , newMTRXPCClientProtocol_MTRDevice
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

-- | Overrides record for @\@protocol MTRXPCClientProtocol_MTRDevice@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRXPCClientProtocol_MTRDeviceOverrides = MTRXPCClientProtocol_MTRDeviceOverrides
  { _device_receivedAttributeReport :: !(Maybe (RawId -> RawId -> IO ()))
  , _device_receivedEventReport :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceBecameActive :: !(Maybe (RawId -> IO ()))
  , _deviceCachePrimed :: !(Maybe (RawId -> IO ()))
  , _deviceConfigurationChanged :: !(Maybe (RawId -> IO ()))
  , _device_internalStateUpdated :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRXPCClientProtocol_MTRDeviceOverrides :: MTRXPCClientProtocol_MTRDeviceOverrides
defaultMTRXPCClientProtocol_MTRDeviceOverrides = MTRXPCClientProtocol_MTRDeviceOverrides
  { _device_receivedAttributeReport = Nothing
  , _device_receivedEventReport = Nothing
  , _deviceBecameActive = Nothing
  , _deviceCachePrimed = Nothing
  , _deviceConfigurationChanged = Nothing
  , _device_internalStateUpdated = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrxpcClientProtocol_MTRDeviceDelegateClass #-}
mtrxpcClientProtocol_MTRDeviceDelegateClass :: Class
mtrxpcClientProtocol_MTRDeviceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRXPCClientProtocol_MTRDevice" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_device_receivedAttributeReport = unSelector (mkSelector "device:receivedAttributeReport:")
      sel_device_receivedEventReport = unSelector (mkSelector "device:receivedEventReport:")
      sel_deviceBecameActive = unSelector (mkSelector "deviceBecameActive:")
      sel_deviceCachePrimed = unSelector (mkSelector "deviceCachePrimed:")
      sel_deviceConfigurationChanged = unSelector (mkSelector "deviceConfigurationChanged:")
      sel_device_internalStateUpdated = unSelector (mkSelector "device:internalStateUpdated:")
  -- device:receivedAttributeReport:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    case _device_receivedAttributeReport rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "device:receivedAttributeReport:" "v@:@@" stub_0

  -- device:receivedEventReport:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    case _device_receivedEventReport rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "device:receivedEventReport:" "v@:@@" stub_1

  -- deviceBecameActive:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    case _deviceBecameActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceBecameActive:" "v@:@" stub_2

  -- deviceCachePrimed:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    case _deviceCachePrimed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceCachePrimed:" "v@:@" stub_3

  -- deviceConfigurationChanged:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    case _deviceConfigurationChanged rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceConfigurationChanged:" "v@:@" stub_4

  -- device:internalStateUpdated:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    case _device_internalStateUpdated rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "device:internalStateUpdated:" "v@:@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCClientProtocol_MTRDeviceOverrides
    if queriedSel == sel_device_receivedAttributeReport then pure (maybe 0 (const 1) (_device_receivedAttributeReport rec_))
    else if queriedSel == sel_device_receivedEventReport then pure (maybe 0 (const 1) (_device_receivedEventReport rec_))
    else if queriedSel == sel_deviceBecameActive then pure (maybe 0 (const 1) (_deviceBecameActive rec_))
    else if queriedSel == sel_deviceCachePrimed then pure (maybe 0 (const 1) (_deviceCachePrimed rec_))
    else if queriedSel == sel_deviceConfigurationChanged then pure (maybe 0 (const 1) (_deviceConfigurationChanged rec_))
    else if queriedSel == sel_device_internalStateUpdated then pure (maybe 0 (const 1) (_device_internalStateUpdated rec_))
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
newMTRXPCClientProtocol_MTRDevice :: MTRXPCClientProtocol_MTRDeviceOverrides -> IO RawId
newMTRXPCClientProtocol_MTRDevice overrides = do
  inst <- class_createInstance mtrxpcClientProtocol_MTRDeviceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
