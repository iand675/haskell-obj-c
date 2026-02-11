{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CBPeripheralManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCBPeripheralManagerDelegate defaultCBPeripheralManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreBluetooth.Delegate.CBPeripheralManagerDelegate
  ( CBPeripheralManagerDelegateOverrides(..)
  , defaultCBPeripheralManagerDelegateOverrides
  , newCBPeripheralManagerDelegate
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

-- | Overrides record for @\@protocol CBPeripheralManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CBPeripheralManagerDelegateOverrides = CBPeripheralManagerDelegateOverrides
  { _peripheralManagerDidUpdateState :: !(Maybe (RawId -> IO ()))
  , _peripheralManager_willRestoreState :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheralManagerDidStartAdvertising_error :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheralManager_didAddService_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheralManager_central_didSubscribeToCharacteristic :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheralManager_central_didUnsubscribeFromCharacteristic :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheralManager_didReceiveReadRequest :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheralManager_didReceiveWriteRequests :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheralManagerIsReadyToUpdateSubscribers :: !(Maybe (RawId -> IO ()))
  , _peripheralManager_didOpenL2CAPChannel_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCBPeripheralManagerDelegateOverrides :: CBPeripheralManagerDelegateOverrides
defaultCBPeripheralManagerDelegateOverrides = CBPeripheralManagerDelegateOverrides
  { _peripheralManagerDidUpdateState = Nothing
  , _peripheralManager_willRestoreState = Nothing
  , _peripheralManagerDidStartAdvertising_error = Nothing
  , _peripheralManager_didAddService_error = Nothing
  , _peripheralManager_central_didSubscribeToCharacteristic = Nothing
  , _peripheralManager_central_didUnsubscribeFromCharacteristic = Nothing
  , _peripheralManager_didReceiveReadRequest = Nothing
  , _peripheralManager_didReceiveWriteRequests = Nothing
  , _peripheralManagerIsReadyToUpdateSubscribers = Nothing
  , _peripheralManager_didOpenL2CAPChannel_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cbPeripheralManagerDelegateDelegateClass #-}
cbPeripheralManagerDelegateDelegateClass :: Class
cbPeripheralManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCBPeripheralManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_peripheralManagerDidUpdateState = unSelector (mkSelector "peripheralManagerDidUpdateState:")
      sel_peripheralManager_willRestoreState = unSelector (mkSelector "peripheralManager:willRestoreState:")
      sel_peripheralManagerDidStartAdvertising_error = unSelector (mkSelector "peripheralManagerDidStartAdvertising:error:")
      sel_peripheralManager_didAddService_error = unSelector (mkSelector "peripheralManager:didAddService:error:")
      sel_peripheralManager_central_didSubscribeToCharacteristic = unSelector (mkSelector "peripheralManager:central:didSubscribeToCharacteristic:")
      sel_peripheralManager_central_didUnsubscribeFromCharacteristic = unSelector (mkSelector "peripheralManager:central:didUnsubscribeFromCharacteristic:")
      sel_peripheralManager_didReceiveReadRequest = unSelector (mkSelector "peripheralManager:didReceiveReadRequest:")
      sel_peripheralManager_didReceiveWriteRequests = unSelector (mkSelector "peripheralManager:didReceiveWriteRequests:")
      sel_peripheralManagerIsReadyToUpdateSubscribers = unSelector (mkSelector "peripheralManagerIsReadyToUpdateSubscribers:")
      sel_peripheralManager_didOpenL2CAPChannel_error = unSelector (mkSelector "peripheralManager:didOpenL2CAPChannel:error:")
  -- peripheralManagerDidUpdateState:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManagerDidUpdateState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "peripheralManagerDidUpdateState:" "v@:@" stub_0

  -- peripheralManager:willRestoreState:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_willRestoreState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheralManager:willRestoreState:" "v@:@@" stub_1

  -- peripheralManagerDidStartAdvertising:error:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManagerDidStartAdvertising_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheralManagerDidStartAdvertising:error:" "v@:@@" stub_2

  -- peripheralManager:didAddService:error:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_didAddService_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheralManager:didAddService:error:" "v@:@@@" stub_3

  -- peripheralManager:central:didSubscribeToCharacteristic:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_central_didSubscribeToCharacteristic rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheralManager:central:didSubscribeToCharacteristic:" "v@:@@@" stub_4

  -- peripheralManager:central:didUnsubscribeFromCharacteristic:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_central_didUnsubscribeFromCharacteristic rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheralManager:central:didUnsubscribeFromCharacteristic:" "v@:@@@" stub_5

  -- peripheralManager:didReceiveReadRequest:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_didReceiveReadRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheralManager:didReceiveReadRequest:" "v@:@@" stub_6

  -- peripheralManager:didReceiveWriteRequests:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_didReceiveWriteRequests rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheralManager:didReceiveWriteRequests:" "v@:@@" stub_7

  -- peripheralManagerIsReadyToUpdateSubscribers:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManagerIsReadyToUpdateSubscribers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "peripheralManagerIsReadyToUpdateSubscribers:" "v@:@" stub_8

  -- peripheralManager:didOpenL2CAPChannel:error:
  stub_9 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    case _peripheralManager_didOpenL2CAPChannel_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheralManager:didOpenL2CAPChannel:error:" "v@:@@@" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralManagerDelegateOverrides
    if queriedSel == sel_peripheralManagerDidUpdateState then pure (maybe 0 (const 1) (_peripheralManagerDidUpdateState rec_))
    else if queriedSel == sel_peripheralManager_willRestoreState then pure (maybe 0 (const 1) (_peripheralManager_willRestoreState rec_))
    else if queriedSel == sel_peripheralManagerDidStartAdvertising_error then pure (maybe 0 (const 1) (_peripheralManagerDidStartAdvertising_error rec_))
    else if queriedSel == sel_peripheralManager_didAddService_error then pure (maybe 0 (const 1) (_peripheralManager_didAddService_error rec_))
    else if queriedSel == sel_peripheralManager_central_didSubscribeToCharacteristic then pure (maybe 0 (const 1) (_peripheralManager_central_didSubscribeToCharacteristic rec_))
    else if queriedSel == sel_peripheralManager_central_didUnsubscribeFromCharacteristic then pure (maybe 0 (const 1) (_peripheralManager_central_didUnsubscribeFromCharacteristic rec_))
    else if queriedSel == sel_peripheralManager_didReceiveReadRequest then pure (maybe 0 (const 1) (_peripheralManager_didReceiveReadRequest rec_))
    else if queriedSel == sel_peripheralManager_didReceiveWriteRequests then pure (maybe 0 (const 1) (_peripheralManager_didReceiveWriteRequests rec_))
    else if queriedSel == sel_peripheralManagerIsReadyToUpdateSubscribers then pure (maybe 0 (const 1) (_peripheralManagerIsReadyToUpdateSubscribers rec_))
    else if queriedSel == sel_peripheralManager_didOpenL2CAPChannel_error then pure (maybe 0 (const 1) (_peripheralManager_didOpenL2CAPChannel_error rec_))
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
newCBPeripheralManagerDelegate :: CBPeripheralManagerDelegateOverrides -> IO RawId
newCBPeripheralManagerDelegate overrides = do
  inst <- class_createInstance cbPeripheralManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
