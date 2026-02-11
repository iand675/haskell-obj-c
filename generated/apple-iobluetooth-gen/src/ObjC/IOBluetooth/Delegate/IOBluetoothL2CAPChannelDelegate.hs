{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothL2CAPChannelDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothL2CAPChannelDelegate defaultIOBluetoothL2CAPChannelDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothL2CAPChannelDelegate
  ( IOBluetoothL2CAPChannelDelegateOverrides(..)
  , defaultIOBluetoothL2CAPChannelDelegateOverrides
  , newIOBluetoothL2CAPChannelDelegate
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

-- | Overrides record for @\@protocol IOBluetoothL2CAPChannelDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothL2CAPChannelDelegateOverrides = IOBluetoothL2CAPChannelDelegateOverrides
  { _l2capChannelOpenComplete_status :: !(Maybe (RawId -> Int -> IO ()))
  , _l2capChannelClosed :: !(Maybe (RawId -> IO ()))
  , _l2capChannelReconfigured :: !(Maybe (RawId -> IO ()))
  , _l2capChannelQueueSpaceAvailable :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothL2CAPChannelDelegateOverrides :: IOBluetoothL2CAPChannelDelegateOverrides
defaultIOBluetoothL2CAPChannelDelegateOverrides = IOBluetoothL2CAPChannelDelegateOverrides
  { _l2capChannelOpenComplete_status = Nothing
  , _l2capChannelClosed = Nothing
  , _l2capChannelReconfigured = Nothing
  , _l2capChannelQueueSpaceAvailable = Nothing
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
{-# NOINLINE ioBluetoothL2CAPChannelDelegateDelegateClass #-}
ioBluetoothL2CAPChannelDelegateDelegateClass :: Class
ioBluetoothL2CAPChannelDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothL2CAPChannelDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_l2capChannelOpenComplete_status = unSelector (mkSelector "l2capChannelOpenComplete:status:")
      sel_l2capChannelClosed = unSelector (mkSelector "l2capChannelClosed:")
      sel_l2capChannelReconfigured = unSelector (mkSelector "l2capChannelReconfigured:")
      sel_l2capChannelQueueSpaceAvailable = unSelector (mkSelector "l2capChannelQueueSpaceAvailable:")
  -- l2capChannelOpenComplete:status:
  stub_0 <- wrap_at_i_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothL2CAPChannelDelegateOverrides
    case _l2capChannelOpenComplete_status rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "l2capChannelOpenComplete:status:" "v@:@i" stub_0

  -- l2capChannelClosed:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothL2CAPChannelDelegateOverrides
    case _l2capChannelClosed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "l2capChannelClosed:" "v@:@" stub_1

  -- l2capChannelReconfigured:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothL2CAPChannelDelegateOverrides
    case _l2capChannelReconfigured rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "l2capChannelReconfigured:" "v@:@" stub_2

  -- l2capChannelQueueSpaceAvailable:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothL2CAPChannelDelegateOverrides
    case _l2capChannelQueueSpaceAvailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "l2capChannelQueueSpaceAvailable:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothL2CAPChannelDelegateOverrides
    if queriedSel == sel_l2capChannelOpenComplete_status then pure (maybe 0 (const 1) (_l2capChannelOpenComplete_status rec_))
    else if queriedSel == sel_l2capChannelClosed then pure (maybe 0 (const 1) (_l2capChannelClosed rec_))
    else if queriedSel == sel_l2capChannelReconfigured then pure (maybe 0 (const 1) (_l2capChannelReconfigured rec_))
    else if queriedSel == sel_l2capChannelQueueSpaceAvailable then pure (maybe 0 (const 1) (_l2capChannelQueueSpaceAvailable rec_))
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
newIOBluetoothL2CAPChannelDelegate :: IOBluetoothL2CAPChannelDelegateOverrides -> IO RawId
newIOBluetoothL2CAPChannelDelegate overrides = do
  inst <- class_createInstance ioBluetoothL2CAPChannelDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
