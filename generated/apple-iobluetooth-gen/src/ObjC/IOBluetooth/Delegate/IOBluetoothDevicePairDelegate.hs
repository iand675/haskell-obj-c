{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothDevicePairDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothDevicePairDelegate defaultIOBluetoothDevicePairDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothDevicePairDelegate
  ( IOBluetoothDevicePairDelegateOverrides(..)
  , defaultIOBluetoothDevicePairDelegateOverrides
  , newIOBluetoothDevicePairDelegate
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

-- | Overrides record for @\@protocol IOBluetoothDevicePairDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothDevicePairDelegateOverrides = IOBluetoothDevicePairDelegateOverrides
  { _devicePairingStarted :: !(Maybe (RawId -> IO ()))
  , _devicePairingConnecting :: !(Maybe (RawId -> IO ()))
  , _devicePairingConnected :: !(Maybe (RawId -> IO ()))
  , _devicePairingPINCodeRequest :: !(Maybe (RawId -> IO ()))
  , _devicePairingUserConfirmationRequest_numericValue :: !(Maybe (RawId -> Int -> IO ()))
  , _devicePairingUserPasskeyNotification_passkey :: !(Maybe (RawId -> Int -> IO ()))
  , _devicePairingFinished_error :: !(Maybe (RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothDevicePairDelegateOverrides :: IOBluetoothDevicePairDelegateOverrides
defaultIOBluetoothDevicePairDelegateOverrides = IOBluetoothDevicePairDelegateOverrides
  { _devicePairingStarted = Nothing
  , _devicePairingConnecting = Nothing
  , _devicePairingConnected = Nothing
  , _devicePairingPINCodeRequest = Nothing
  , _devicePairingUserConfirmationRequest_numericValue = Nothing
  , _devicePairingUserPasskeyNotification_passkey = Nothing
  , _devicePairingFinished_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_i_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> IO ()))

foreign import ccall "wrapper"
  wrap_at_I_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CUInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CUInt -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ioBluetoothDevicePairDelegateDelegateClass #-}
ioBluetoothDevicePairDelegateDelegateClass :: Class
ioBluetoothDevicePairDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothDevicePairDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_devicePairingStarted = unSelector (mkSelector "devicePairingStarted:")
      sel_devicePairingConnecting = unSelector (mkSelector "devicePairingConnecting:")
      sel_devicePairingConnected = unSelector (mkSelector "devicePairingConnected:")
      sel_devicePairingPINCodeRequest = unSelector (mkSelector "devicePairingPINCodeRequest:")
      sel_devicePairingUserConfirmationRequest_numericValue = unSelector (mkSelector "devicePairingUserConfirmationRequest:numericValue:")
      sel_devicePairingUserPasskeyNotification_passkey = unSelector (mkSelector "devicePairingUserPasskeyNotification:passkey:")
      sel_devicePairingFinished_error = unSelector (mkSelector "devicePairingFinished:error:")
  -- devicePairingStarted:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingStarted rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "devicePairingStarted:" "v@:@" stub_0

  -- devicePairingConnecting:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingConnecting rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "devicePairingConnecting:" "v@:@" stub_1

  -- devicePairingConnected:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingConnected rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "devicePairingConnected:" "v@:@" stub_2

  -- devicePairingPINCodeRequest:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingPINCodeRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "devicePairingPINCodeRequest:" "v@:@" stub_3

  -- devicePairingUserConfirmationRequest:numericValue:
  stub_4 <- wrap_at_I_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingUserConfirmationRequest_numericValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "devicePairingUserConfirmationRequest:numericValue:" "v@:@I" stub_4

  -- devicePairingUserPasskeyNotification:passkey:
  stub_5 <- wrap_at_I_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingUserPasskeyNotification_passkey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "devicePairingUserPasskeyNotification:passkey:" "v@:@I" stub_5

  -- devicePairingFinished:error:
  stub_6 <- wrap_at_i_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    case _devicePairingFinished_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "devicePairingFinished:error:" "v@:@i" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDevicePairDelegateOverrides
    if queriedSel == sel_devicePairingStarted then pure (maybe 0 (const 1) (_devicePairingStarted rec_))
    else if queriedSel == sel_devicePairingConnecting then pure (maybe 0 (const 1) (_devicePairingConnecting rec_))
    else if queriedSel == sel_devicePairingConnected then pure (maybe 0 (const 1) (_devicePairingConnected rec_))
    else if queriedSel == sel_devicePairingPINCodeRequest then pure (maybe 0 (const 1) (_devicePairingPINCodeRequest rec_))
    else if queriedSel == sel_devicePairingUserConfirmationRequest_numericValue then pure (maybe 0 (const 1) (_devicePairingUserConfirmationRequest_numericValue rec_))
    else if queriedSel == sel_devicePairingUserPasskeyNotification_passkey then pure (maybe 0 (const 1) (_devicePairingUserPasskeyNotification_passkey rec_))
    else if queriedSel == sel_devicePairingFinished_error then pure (maybe 0 (const 1) (_devicePairingFinished_error rec_))
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
newIOBluetoothDevicePairDelegate :: IOBluetoothDevicePairDelegateOverrides -> IO RawId
newIOBluetoothDevicePairDelegate overrides = do
  inst <- class_createInstance ioBluetoothDevicePairDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
