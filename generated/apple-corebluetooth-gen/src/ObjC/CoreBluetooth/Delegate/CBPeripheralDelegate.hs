{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CBPeripheralDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCBPeripheralDelegate defaultCBPeripheralDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreBluetooth.Delegate.CBPeripheralDelegate
  ( CBPeripheralDelegateOverrides(..)
  , defaultCBPeripheralDelegateOverrides
  , newCBPeripheralDelegate
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

-- | Overrides record for @\@protocol CBPeripheralDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CBPeripheralDelegateOverrides = CBPeripheralDelegateOverrides
  { _peripheralDidUpdateName :: !(Maybe (RawId -> IO ()))
  , _peripheral_didModifyServices :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheralDidUpdateRSSI_error :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheral_didReadRSSI_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didDiscoverServices :: !(Maybe (RawId -> RawId -> IO ()))
  , _peripheral_didDiscoverIncludedServicesForService_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didDiscoverCharacteristicsForService_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didUpdateValueForCharacteristic_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didWriteValueForCharacteristic_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didUpdateNotificationStateForCharacteristic_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didDiscoverDescriptorsForCharacteristic_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didUpdateValueForDescriptor_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheral_didWriteValueForDescriptor_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _peripheralIsReadyToSendWriteWithoutResponse :: !(Maybe (RawId -> IO ()))
  , _peripheral_didOpenL2CAPChannel_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCBPeripheralDelegateOverrides :: CBPeripheralDelegateOverrides
defaultCBPeripheralDelegateOverrides = CBPeripheralDelegateOverrides
  { _peripheralDidUpdateName = Nothing
  , _peripheral_didModifyServices = Nothing
  , _peripheralDidUpdateRSSI_error = Nothing
  , _peripheral_didReadRSSI_error = Nothing
  , _peripheral_didDiscoverServices = Nothing
  , _peripheral_didDiscoverIncludedServicesForService_error = Nothing
  , _peripheral_didDiscoverCharacteristicsForService_error = Nothing
  , _peripheral_didUpdateValueForCharacteristic_error = Nothing
  , _peripheral_didWriteValueForCharacteristic_error = Nothing
  , _peripheral_didUpdateNotificationStateForCharacteristic_error = Nothing
  , _peripheral_didDiscoverDescriptorsForCharacteristic_error = Nothing
  , _peripheral_didUpdateValueForDescriptor_error = Nothing
  , _peripheral_didWriteValueForDescriptor_error = Nothing
  , _peripheralIsReadyToSendWriteWithoutResponse = Nothing
  , _peripheral_didOpenL2CAPChannel_error = Nothing
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
{-# NOINLINE cbPeripheralDelegateDelegateClass #-}
cbPeripheralDelegateDelegateClass :: Class
cbPeripheralDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCBPeripheralDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_peripheralDidUpdateName = unSelector (mkSelector "peripheralDidUpdateName:")
      sel_peripheral_didModifyServices = unSelector (mkSelector "peripheral:didModifyServices:")
      sel_peripheralDidUpdateRSSI_error = unSelector (mkSelector "peripheralDidUpdateRSSI:error:")
      sel_peripheral_didReadRSSI_error = unSelector (mkSelector "peripheral:didReadRSSI:error:")
      sel_peripheral_didDiscoverServices = unSelector (mkSelector "peripheral:didDiscoverServices:")
      sel_peripheral_didDiscoverIncludedServicesForService_error = unSelector (mkSelector "peripheral:didDiscoverIncludedServicesForService:error:")
      sel_peripheral_didDiscoverCharacteristicsForService_error = unSelector (mkSelector "peripheral:didDiscoverCharacteristicsForService:error:")
      sel_peripheral_didUpdateValueForCharacteristic_error = unSelector (mkSelector "peripheral:didUpdateValueForCharacteristic:error:")
      sel_peripheral_didWriteValueForCharacteristic_error = unSelector (mkSelector "peripheral:didWriteValueForCharacteristic:error:")
      sel_peripheral_didUpdateNotificationStateForCharacteristic_error = unSelector (mkSelector "peripheral:didUpdateNotificationStateForCharacteristic:error:")
      sel_peripheral_didDiscoverDescriptorsForCharacteristic_error = unSelector (mkSelector "peripheral:didDiscoverDescriptorsForCharacteristic:error:")
      sel_peripheral_didUpdateValueForDescriptor_error = unSelector (mkSelector "peripheral:didUpdateValueForDescriptor:error:")
      sel_peripheral_didWriteValueForDescriptor_error = unSelector (mkSelector "peripheral:didWriteValueForDescriptor:error:")
      sel_peripheralIsReadyToSendWriteWithoutResponse = unSelector (mkSelector "peripheralIsReadyToSendWriteWithoutResponse:")
      sel_peripheral_didOpenL2CAPChannel_error = unSelector (mkSelector "peripheral:didOpenL2CAPChannel:error:")
  -- peripheralDidUpdateName:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheralDidUpdateName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "peripheralDidUpdateName:" "v@:@" stub_0

  -- peripheral:didModifyServices:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didModifyServices rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheral:didModifyServices:" "v@:@@" stub_1

  -- peripheralDidUpdateRSSI:error:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheralDidUpdateRSSI_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheralDidUpdateRSSI:error:" "v@:@@" stub_2

  -- peripheral:didReadRSSI:error:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didReadRSSI_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didReadRSSI:error:" "v@:@@@" stub_3

  -- peripheral:didDiscoverServices:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didDiscoverServices rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "peripheral:didDiscoverServices:" "v@:@@" stub_4

  -- peripheral:didDiscoverIncludedServicesForService:error:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didDiscoverIncludedServicesForService_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didDiscoverIncludedServicesForService:error:" "v@:@@@" stub_5

  -- peripheral:didDiscoverCharacteristicsForService:error:
  stub_6 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didDiscoverCharacteristicsForService_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didDiscoverCharacteristicsForService:error:" "v@:@@@" stub_6

  -- peripheral:didUpdateValueForCharacteristic:error:
  stub_7 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didUpdateValueForCharacteristic_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didUpdateValueForCharacteristic:error:" "v@:@@@" stub_7

  -- peripheral:didWriteValueForCharacteristic:error:
  stub_8 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didWriteValueForCharacteristic_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didWriteValueForCharacteristic:error:" "v@:@@@" stub_8

  -- peripheral:didUpdateNotificationStateForCharacteristic:error:
  stub_9 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didUpdateNotificationStateForCharacteristic_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didUpdateNotificationStateForCharacteristic:error:" "v@:@@@" stub_9

  -- peripheral:didDiscoverDescriptorsForCharacteristic:error:
  stub_10 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didDiscoverDescriptorsForCharacteristic_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didDiscoverDescriptorsForCharacteristic:error:" "v@:@@@" stub_10

  -- peripheral:didUpdateValueForDescriptor:error:
  stub_11 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didUpdateValueForDescriptor_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didUpdateValueForDescriptor:error:" "v@:@@@" stub_11

  -- peripheral:didWriteValueForDescriptor:error:
  stub_12 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didWriteValueForDescriptor_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didWriteValueForDescriptor:error:" "v@:@@@" stub_12

  -- peripheralIsReadyToSendWriteWithoutResponse:
  stub_13 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheralIsReadyToSendWriteWithoutResponse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "peripheralIsReadyToSendWriteWithoutResponse:" "v@:@" stub_13

  -- peripheral:didOpenL2CAPChannel:error:
  stub_14 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    case _peripheral_didOpenL2CAPChannel_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "peripheral:didOpenL2CAPChannel:error:" "v@:@@@" stub_14

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CBPeripheralDelegateOverrides
    if queriedSel == sel_peripheralDidUpdateName then pure (maybe 0 (const 1) (_peripheralDidUpdateName rec_))
    else if queriedSel == sel_peripheral_didModifyServices then pure (maybe 0 (const 1) (_peripheral_didModifyServices rec_))
    else if queriedSel == sel_peripheralDidUpdateRSSI_error then pure (maybe 0 (const 1) (_peripheralDidUpdateRSSI_error rec_))
    else if queriedSel == sel_peripheral_didReadRSSI_error then pure (maybe 0 (const 1) (_peripheral_didReadRSSI_error rec_))
    else if queriedSel == sel_peripheral_didDiscoverServices then pure (maybe 0 (const 1) (_peripheral_didDiscoverServices rec_))
    else if queriedSel == sel_peripheral_didDiscoverIncludedServicesForService_error then pure (maybe 0 (const 1) (_peripheral_didDiscoverIncludedServicesForService_error rec_))
    else if queriedSel == sel_peripheral_didDiscoverCharacteristicsForService_error then pure (maybe 0 (const 1) (_peripheral_didDiscoverCharacteristicsForService_error rec_))
    else if queriedSel == sel_peripheral_didUpdateValueForCharacteristic_error then pure (maybe 0 (const 1) (_peripheral_didUpdateValueForCharacteristic_error rec_))
    else if queriedSel == sel_peripheral_didWriteValueForCharacteristic_error then pure (maybe 0 (const 1) (_peripheral_didWriteValueForCharacteristic_error rec_))
    else if queriedSel == sel_peripheral_didUpdateNotificationStateForCharacteristic_error then pure (maybe 0 (const 1) (_peripheral_didUpdateNotificationStateForCharacteristic_error rec_))
    else if queriedSel == sel_peripheral_didDiscoverDescriptorsForCharacteristic_error then pure (maybe 0 (const 1) (_peripheral_didDiscoverDescriptorsForCharacteristic_error rec_))
    else if queriedSel == sel_peripheral_didUpdateValueForDescriptor_error then pure (maybe 0 (const 1) (_peripheral_didUpdateValueForDescriptor_error rec_))
    else if queriedSel == sel_peripheral_didWriteValueForDescriptor_error then pure (maybe 0 (const 1) (_peripheral_didWriteValueForDescriptor_error rec_))
    else if queriedSel == sel_peripheralIsReadyToSendWriteWithoutResponse then pure (maybe 0 (const 1) (_peripheralIsReadyToSendWriteWithoutResponse rec_))
    else if queriedSel == sel_peripheral_didOpenL2CAPChannel_error then pure (maybe 0 (const 1) (_peripheral_didOpenL2CAPChannel_error rec_))
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
newCBPeripheralDelegate :: CBPeripheralDelegateOverrides -> IO RawId
newCBPeripheralDelegate overrides = do
  inst <- class_createInstance cbPeripheralDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
