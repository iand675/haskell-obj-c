{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothHandsFreeDeviceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothHandsFreeDeviceDelegate defaultIOBluetoothHandsFreeDeviceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothHandsFreeDeviceDelegate
  ( IOBluetoothHandsFreeDeviceDelegateOverrides(..)
  , defaultIOBluetoothHandsFreeDeviceDelegateOverrides
  , newIOBluetoothHandsFreeDeviceDelegate
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

-- | Overrides record for @\@protocol IOBluetoothHandsFreeDeviceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothHandsFreeDeviceDelegateOverrides = IOBluetoothHandsFreeDeviceDelegateOverrides
  { _handsFree_isServiceAvailable :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_isCallActive :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_callSetupMode :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_callHoldState :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_signalStrength :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_isRoaming :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_batteryCharge :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_incomingCallFrom :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_ringAttempt :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_currentCall :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_subscriberNumber :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_incomingSMS :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_unhandledResultCode :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothHandsFreeDeviceDelegateOverrides :: IOBluetoothHandsFreeDeviceDelegateOverrides
defaultIOBluetoothHandsFreeDeviceDelegateOverrides = IOBluetoothHandsFreeDeviceDelegateOverrides
  { _handsFree_isServiceAvailable = Nothing
  , _handsFree_isCallActive = Nothing
  , _handsFree_callSetupMode = Nothing
  , _handsFree_callHoldState = Nothing
  , _handsFree_signalStrength = Nothing
  , _handsFree_isRoaming = Nothing
  , _handsFree_batteryCharge = Nothing
  , _handsFree_incomingCallFrom = Nothing
  , _handsFree_ringAttempt = Nothing
  , _handsFree_currentCall = Nothing
  , _handsFree_subscriberNumber = Nothing
  , _handsFree_incomingSMS = Nothing
  , _handsFree_unhandledResultCode = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ioBluetoothHandsFreeDeviceDelegateDelegateClass #-}
ioBluetoothHandsFreeDeviceDelegateDelegateClass :: Class
ioBluetoothHandsFreeDeviceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothHandsFreeDeviceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handsFree_isServiceAvailable = unSelector (mkSelector "handsFree:isServiceAvailable:")
      sel_handsFree_isCallActive = unSelector (mkSelector "handsFree:isCallActive:")
      sel_handsFree_callSetupMode = unSelector (mkSelector "handsFree:callSetupMode:")
      sel_handsFree_callHoldState = unSelector (mkSelector "handsFree:callHoldState:")
      sel_handsFree_signalStrength = unSelector (mkSelector "handsFree:signalStrength:")
      sel_handsFree_isRoaming = unSelector (mkSelector "handsFree:isRoaming:")
      sel_handsFree_batteryCharge = unSelector (mkSelector "handsFree:batteryCharge:")
      sel_handsFree_incomingCallFrom = unSelector (mkSelector "handsFree:incomingCallFrom:")
      sel_handsFree_ringAttempt = unSelector (mkSelector "handsFree:ringAttempt:")
      sel_handsFree_currentCall = unSelector (mkSelector "handsFree:currentCall:")
      sel_handsFree_subscriberNumber = unSelector (mkSelector "handsFree:subscriberNumber:")
      sel_handsFree_incomingSMS = unSelector (mkSelector "handsFree:incomingSMS:")
      sel_handsFree_unhandledResultCode = unSelector (mkSelector "handsFree:unhandledResultCode:")
  -- handsFree:isServiceAvailable:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_isServiceAvailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:isServiceAvailable:" "v@:@@" stub_0

  -- handsFree:isCallActive:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_isCallActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:isCallActive:" "v@:@@" stub_1

  -- handsFree:callSetupMode:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_callSetupMode rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:callSetupMode:" "v@:@@" stub_2

  -- handsFree:callHoldState:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_callHoldState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:callHoldState:" "v@:@@" stub_3

  -- handsFree:signalStrength:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_signalStrength rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:signalStrength:" "v@:@@" stub_4

  -- handsFree:isRoaming:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_isRoaming rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:isRoaming:" "v@:@@" stub_5

  -- handsFree:batteryCharge:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_batteryCharge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:batteryCharge:" "v@:@@" stub_6

  -- handsFree:incomingCallFrom:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_incomingCallFrom rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:incomingCallFrom:" "v@:@@" stub_7

  -- handsFree:ringAttempt:
  stub_8 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_ringAttempt rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:ringAttempt:" "v@:@@" stub_8

  -- handsFree:currentCall:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_currentCall rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:currentCall:" "v@:@@" stub_9

  -- handsFree:subscriberNumber:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_subscriberNumber rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:subscriberNumber:" "v@:@@" stub_10

  -- handsFree:incomingSMS:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_incomingSMS rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:incomingSMS:" "v@:@@" stub_11

  -- handsFree:unhandledResultCode:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    case _handsFree_unhandledResultCode rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:unhandledResultCode:" "v@:@@" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDeviceDelegateOverrides
    if queriedSel == sel_handsFree_isServiceAvailable then pure (maybe 0 (const 1) (_handsFree_isServiceAvailable rec_))
    else if queriedSel == sel_handsFree_isCallActive then pure (maybe 0 (const 1) (_handsFree_isCallActive rec_))
    else if queriedSel == sel_handsFree_callSetupMode then pure (maybe 0 (const 1) (_handsFree_callSetupMode rec_))
    else if queriedSel == sel_handsFree_callHoldState then pure (maybe 0 (const 1) (_handsFree_callHoldState rec_))
    else if queriedSel == sel_handsFree_signalStrength then pure (maybe 0 (const 1) (_handsFree_signalStrength rec_))
    else if queriedSel == sel_handsFree_isRoaming then pure (maybe 0 (const 1) (_handsFree_isRoaming rec_))
    else if queriedSel == sel_handsFree_batteryCharge then pure (maybe 0 (const 1) (_handsFree_batteryCharge rec_))
    else if queriedSel == sel_handsFree_incomingCallFrom then pure (maybe 0 (const 1) (_handsFree_incomingCallFrom rec_))
    else if queriedSel == sel_handsFree_ringAttempt then pure (maybe 0 (const 1) (_handsFree_ringAttempt rec_))
    else if queriedSel == sel_handsFree_currentCall then pure (maybe 0 (const 1) (_handsFree_currentCall rec_))
    else if queriedSel == sel_handsFree_subscriberNumber then pure (maybe 0 (const 1) (_handsFree_subscriberNumber rec_))
    else if queriedSel == sel_handsFree_incomingSMS then pure (maybe 0 (const 1) (_handsFree_incomingSMS rec_))
    else if queriedSel == sel_handsFree_unhandledResultCode then pure (maybe 0 (const 1) (_handsFree_unhandledResultCode rec_))
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
newIOBluetoothHandsFreeDeviceDelegate :: IOBluetoothHandsFreeDeviceDelegateOverrides -> IO RawId
newIOBluetoothHandsFreeDeviceDelegate overrides = do
  inst <- class_createInstance ioBluetoothHandsFreeDeviceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
