{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothDeviceAsyncCallbacks@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothDeviceAsyncCallbacks defaultIOBluetoothDeviceAsyncCallbacksOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothDeviceAsyncCallbacks
  ( IOBluetoothDeviceAsyncCallbacksOverrides(..)
  , defaultIOBluetoothDeviceAsyncCallbacksOverrides
  , newIOBluetoothDeviceAsyncCallbacks
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

-- | Overrides record for @\@protocol IOBluetoothDeviceAsyncCallbacks@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothDeviceAsyncCallbacksOverrides = IOBluetoothDeviceAsyncCallbacksOverrides
  { _remoteNameRequestComplete_status :: !(Maybe (RawId -> Int -> IO ()))
  , _connectionComplete_status :: !(Maybe (RawId -> Int -> IO ()))
  , _sdpQueryComplete_status :: !(Maybe (RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothDeviceAsyncCallbacksOverrides :: IOBluetoothDeviceAsyncCallbacksOverrides
defaultIOBluetoothDeviceAsyncCallbacksOverrides = IOBluetoothDeviceAsyncCallbacksOverrides
  { _remoteNameRequestComplete_status = Nothing
  , _connectionComplete_status = Nothing
  , _sdpQueryComplete_status = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_i_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CInt -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ioBluetoothDeviceAsyncCallbacksDelegateClass #-}
ioBluetoothDeviceAsyncCallbacksDelegateClass :: Class
ioBluetoothDeviceAsyncCallbacksDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothDeviceAsyncCallbacks" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_remoteNameRequestComplete_status = unSelector (mkSelector "remoteNameRequestComplete:status:")
      sel_connectionComplete_status = unSelector (mkSelector "connectionComplete:status:")
      sel_sdpQueryComplete_status = unSelector (mkSelector "sdpQueryComplete:status:")
  -- remoteNameRequestComplete:status:
  stub_0 <- wrap_at_i_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceAsyncCallbacksOverrides
    case _remoteNameRequestComplete_status rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "remoteNameRequestComplete:status:" "v@:@i" stub_0

  -- connectionComplete:status:
  stub_1 <- wrap_at_i_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceAsyncCallbacksOverrides
    case _connectionComplete_status rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "connectionComplete:status:" "v@:@i" stub_1

  -- sdpQueryComplete:status:
  stub_2 <- wrap_at_i_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceAsyncCallbacksOverrides
    case _sdpQueryComplete_status rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "sdpQueryComplete:status:" "v@:@i" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothDeviceAsyncCallbacksOverrides
    if queriedSel == sel_remoteNameRequestComplete_status then pure (maybe 0 (const 1) (_remoteNameRequestComplete_status rec_))
    else if queriedSel == sel_connectionComplete_status then pure (maybe 0 (const 1) (_connectionComplete_status rec_))
    else if queriedSel == sel_sdpQueryComplete_status then pure (maybe 0 (const 1) (_sdpQueryComplete_status rec_))
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
newIOBluetoothDeviceAsyncCallbacks :: IOBluetoothDeviceAsyncCallbacksOverrides -> IO RawId
newIOBluetoothDeviceAsyncCallbacks overrides = do
  inst <- class_createInstance ioBluetoothDeviceAsyncCallbacksDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
