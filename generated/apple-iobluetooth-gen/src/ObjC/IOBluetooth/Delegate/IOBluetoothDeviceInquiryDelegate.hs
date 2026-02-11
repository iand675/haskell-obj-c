{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothDeviceInquiryDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothDeviceInquiryDelegate defaultIOBluetoothDeviceInquiryDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothDeviceInquiryDelegate
  ( IOBluetoothDeviceInquiryDelegateOverrides(..)
  , defaultIOBluetoothDeviceInquiryDelegateOverrides
  , newIOBluetoothDeviceInquiryDelegate
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

-- | Overrides record for @\@protocol IOBluetoothDeviceInquiryDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothDeviceInquiryDelegateOverrides = IOBluetoothDeviceInquiryDelegateOverrides
  { _deviceInquiryStarted :: !(Maybe (RawId -> IO ()))
  , _deviceInquiryDeviceFound_device :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceInquiryUpdatingDeviceNamesStarted_devicesRemaining :: !(Maybe (RawId -> Int -> IO ()))
  , _deviceInquiryDeviceNameUpdated_device_devicesRemaining :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _deviceInquiryComplete_error_aborted :: !(Maybe (RawId -> Int -> Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothDeviceInquiryDelegateOverrides :: IOBluetoothDeviceInquiryDelegateOverrides
defaultIOBluetoothDeviceInquiryDelegateOverrides = IOBluetoothDeviceInquiryDelegateOverrides
  { _deviceInquiryStarted = Nothing
  , _deviceInquiryDeviceFound_device = Nothing
  , _deviceInquiryUpdatingDeviceNamesStarted_devicesRemaining = Nothing
  , _deviceInquiryDeviceNameUpdated_device_devicesRemaining = Nothing
  , _deviceInquiryComplete_error_aborted = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_i_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_I_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CUInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CUInt -> IO ()))

foreign import ccall "wrapper"
  wrap_at_I_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CUInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CUInt -> IO ()))

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
{-# NOINLINE ioBluetoothDeviceInquiryDelegateDelegateClass #-}
ioBluetoothDeviceInquiryDelegateDelegateClass :: Class
ioBluetoothDeviceInquiryDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothDeviceInquiryDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_deviceInquiryStarted = unSelector (mkSelector "deviceInquiryStarted:")
      sel_deviceInquiryDeviceFound_device = unSelector (mkSelector "deviceInquiryDeviceFound:device:")
      sel_deviceInquiryUpdatingDeviceNamesStarted_devicesRemaining = unSelector (mkSelector "deviceInquiryUpdatingDeviceNamesStarted:devicesRemaining:")
      sel_deviceInquiryDeviceNameUpdated_device_devicesRemaining = unSelector (mkSelector "deviceInquiryDeviceNameUpdated:device:devicesRemaining:")
      sel_deviceInquiryComplete_error_aborted = unSelector (mkSelector "deviceInquiryComplete:error:aborted:")
  -- deviceInquiryStarted:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceInquiryDelegateOverrides
    case _deviceInquiryStarted rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceInquiryStarted:" "v@:@" stub_0

  -- deviceInquiryDeviceFound:device:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceInquiryDelegateOverrides
    case _deviceInquiryDeviceFound_device rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceInquiryDeviceFound:device:" "v@:@@" stub_1

  -- deviceInquiryUpdatingDeviceNamesStarted:devicesRemaining:
  stub_2 <- wrap_at_I_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceInquiryDelegateOverrides
    case _deviceInquiryUpdatingDeviceNamesStarted_devicesRemaining rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "deviceInquiryUpdatingDeviceNamesStarted:devicesRemaining:" "v@:@I" stub_2

  -- deviceInquiryDeviceNameUpdated:device:devicesRemaining:
  stub_3 <- wrap_at_at_I_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceInquiryDelegateOverrides
    case _deviceInquiryDeviceNameUpdated_device_devicesRemaining rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "deviceInquiryDeviceNameUpdated:device:devicesRemaining:" "v@:@@I" stub_3

  -- deviceInquiryComplete:error:aborted:
  stub_4 <- wrap_at_i_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceInquiryDelegateOverrides
    case _deviceInquiryComplete_error_aborted rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (arg2 /= 0)
  addObjCMethod cls "deviceInquiryComplete:error:aborted:" "v@:@iB" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceInquiryDelegateOverrides
    if queriedSel == sel_deviceInquiryStarted then pure (maybe 0 (const 1) (_deviceInquiryStarted rec_))
    else if queriedSel == sel_deviceInquiryDeviceFound_device then pure (maybe 0 (const 1) (_deviceInquiryDeviceFound_device rec_))
    else if queriedSel == sel_deviceInquiryUpdatingDeviceNamesStarted_devicesRemaining then pure (maybe 0 (const 1) (_deviceInquiryUpdatingDeviceNamesStarted_devicesRemaining rec_))
    else if queriedSel == sel_deviceInquiryDeviceNameUpdated_device_devicesRemaining then pure (maybe 0 (const 1) (_deviceInquiryDeviceNameUpdated_device_devicesRemaining rec_))
    else if queriedSel == sel_deviceInquiryComplete_error_aborted then pure (maybe 0 (const 1) (_deviceInquiryComplete_error_aborted rec_))
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
newIOBluetoothDeviceInquiryDelegate :: IOBluetoothDeviceInquiryDelegateOverrides -> IO RawId
newIOBluetoothDeviceInquiryDelegate overrides = do
  inst <- class_createInstance ioBluetoothDeviceInquiryDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
