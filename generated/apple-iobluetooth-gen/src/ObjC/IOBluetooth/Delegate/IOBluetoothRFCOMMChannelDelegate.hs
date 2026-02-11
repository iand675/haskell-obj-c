{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothRFCOMMChannelDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothRFCOMMChannelDelegate defaultIOBluetoothRFCOMMChannelDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothRFCOMMChannelDelegate
  ( IOBluetoothRFCOMMChannelDelegateOverrides(..)
  , defaultIOBluetoothRFCOMMChannelDelegateOverrides
  , newIOBluetoothRFCOMMChannelDelegate
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

-- | Overrides record for @\@protocol IOBluetoothRFCOMMChannelDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothRFCOMMChannelDelegateOverrides = IOBluetoothRFCOMMChannelDelegateOverrides
  { _rfcommChannelOpenComplete_status :: !(Maybe (RawId -> Int -> IO ()))
  , _rfcommChannelClosed :: !(Maybe (RawId -> IO ()))
  , _rfcommChannelControlSignalsChanged :: !(Maybe (RawId -> IO ()))
  , _rfcommChannelFlowControlChanged :: !(Maybe (RawId -> IO ()))
  , _rfcommChannelQueueSpaceAvailable :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothRFCOMMChannelDelegateOverrides :: IOBluetoothRFCOMMChannelDelegateOverrides
defaultIOBluetoothRFCOMMChannelDelegateOverrides = IOBluetoothRFCOMMChannelDelegateOverrides
  { _rfcommChannelOpenComplete_status = Nothing
  , _rfcommChannelClosed = Nothing
  , _rfcommChannelControlSignalsChanged = Nothing
  , _rfcommChannelFlowControlChanged = Nothing
  , _rfcommChannelQueueSpaceAvailable = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_i_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ioBluetoothRFCOMMChannelDelegateDelegateClass #-}
ioBluetoothRFCOMMChannelDelegateDelegateClass :: Class
ioBluetoothRFCOMMChannelDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothRFCOMMChannelDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_rfcommChannelOpenComplete_status = unSelector (mkSelector "rfcommChannelOpenComplete:status:")
      sel_rfcommChannelClosed = unSelector (mkSelector "rfcommChannelClosed:")
      sel_rfcommChannelControlSignalsChanged = unSelector (mkSelector "rfcommChannelControlSignalsChanged:")
      sel_rfcommChannelFlowControlChanged = unSelector (mkSelector "rfcommChannelFlowControlChanged:")
      sel_rfcommChannelQueueSpaceAvailable = unSelector (mkSelector "rfcommChannelQueueSpaceAvailable:")
  -- rfcommChannelOpenComplete:status:
  stub_0 <- wrap_at_i_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothRFCOMMChannelDelegateOverrides
    case _rfcommChannelOpenComplete_status rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "rfcommChannelOpenComplete:status:" "v@:@i" stub_0

  -- rfcommChannelClosed:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothRFCOMMChannelDelegateOverrides
    case _rfcommChannelClosed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "rfcommChannelClosed:" "v@:@" stub_1

  -- rfcommChannelControlSignalsChanged:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothRFCOMMChannelDelegateOverrides
    case _rfcommChannelControlSignalsChanged rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "rfcommChannelControlSignalsChanged:" "v@:@" stub_2

  -- rfcommChannelFlowControlChanged:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothRFCOMMChannelDelegateOverrides
    case _rfcommChannelFlowControlChanged rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "rfcommChannelFlowControlChanged:" "v@:@" stub_3

  -- rfcommChannelQueueSpaceAvailable:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothRFCOMMChannelDelegateOverrides
    case _rfcommChannelQueueSpaceAvailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "rfcommChannelQueueSpaceAvailable:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothRFCOMMChannelDelegateOverrides
    if queriedSel == sel_rfcommChannelOpenComplete_status then pure (maybe 0 (const 1) (_rfcommChannelOpenComplete_status rec_))
    else if queriedSel == sel_rfcommChannelClosed then pure (maybe 0 (const 1) (_rfcommChannelClosed rec_))
    else if queriedSel == sel_rfcommChannelControlSignalsChanged then pure (maybe 0 (const 1) (_rfcommChannelControlSignalsChanged rec_))
    else if queriedSel == sel_rfcommChannelFlowControlChanged then pure (maybe 0 (const 1) (_rfcommChannelFlowControlChanged rec_))
    else if queriedSel == sel_rfcommChannelQueueSpaceAvailable then pure (maybe 0 (const 1) (_rfcommChannelQueueSpaceAvailable rec_))
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
newIOBluetoothRFCOMMChannelDelegate :: IOBluetoothRFCOMMChannelDelegateOverrides -> IO RawId
newIOBluetoothRFCOMMChannelDelegate overrides = do
  inst <- class_createInstance ioBluetoothRFCOMMChannelDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
