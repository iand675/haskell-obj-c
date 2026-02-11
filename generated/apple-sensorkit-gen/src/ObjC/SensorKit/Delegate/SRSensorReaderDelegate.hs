{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SRSensorReaderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSRSensorReaderDelegate defaultSRSensorReaderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SensorKit.Delegate.SRSensorReaderDelegate
  ( SRSensorReaderDelegateOverrides(..)
  , defaultSRSensorReaderDelegateOverrides
  , newSRSensorReaderDelegate
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

-- | Overrides record for @\@protocol SRSensorReaderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SRSensorReaderDelegateOverrides = SRSensorReaderDelegateOverrides
  { _sensorReader_fetchingRequest_didFetchResult :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _sensorReader_didCompleteFetch :: !(Maybe (RawId -> RawId -> IO ()))
  , _sensorReader_fetchingRequest_failedWithError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _sensorReaderWillStartRecording :: !(Maybe (RawId -> IO ()))
  , _sensorReader_startRecordingFailedWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _sensorReaderDidStopRecording :: !(Maybe (RawId -> IO ()))
  , _sensorReader_stopRecordingFailedWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _sensorReader_didFetchDevices :: !(Maybe (RawId -> RawId -> IO ()))
  , _sensorReader_fetchDevicesDidFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSRSensorReaderDelegateOverrides :: SRSensorReaderDelegateOverrides
defaultSRSensorReaderDelegateOverrides = SRSensorReaderDelegateOverrides
  { _sensorReader_fetchingRequest_didFetchResult = Nothing
  , _sensorReader_didCompleteFetch = Nothing
  , _sensorReader_fetchingRequest_failedWithError = Nothing
  , _sensorReaderWillStartRecording = Nothing
  , _sensorReader_startRecordingFailedWithError = Nothing
  , _sensorReaderDidStopRecording = Nothing
  , _sensorReader_stopRecordingFailedWithError = Nothing
  , _sensorReader_didFetchDevices = Nothing
  , _sensorReader_fetchDevicesDidFailWithError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE srSensorReaderDelegateDelegateClass #-}
srSensorReaderDelegateDelegateClass :: Class
srSensorReaderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSRSensorReaderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sensorReader_fetchingRequest_didFetchResult = unSelector (mkSelector "sensorReader:fetchingRequest:didFetchResult:")
      sel_sensorReader_didCompleteFetch = unSelector (mkSelector "sensorReader:didCompleteFetch:")
      sel_sensorReader_fetchingRequest_failedWithError = unSelector (mkSelector "sensorReader:fetchingRequest:failedWithError:")
      sel_sensorReaderWillStartRecording = unSelector (mkSelector "sensorReaderWillStartRecording:")
      sel_sensorReader_startRecordingFailedWithError = unSelector (mkSelector "sensorReader:startRecordingFailedWithError:")
      sel_sensorReaderDidStopRecording = unSelector (mkSelector "sensorReaderDidStopRecording:")
      sel_sensorReader_stopRecordingFailedWithError = unSelector (mkSelector "sensorReader:stopRecordingFailedWithError:")
      sel_sensorReader_didFetchDevices = unSelector (mkSelector "sensorReader:didFetchDevices:")
      sel_sensorReader_fetchDevicesDidFailWithError = unSelector (mkSelector "sensorReader:fetchDevicesDidFailWithError:")
  -- sensorReader:fetchingRequest:didFetchResult:
  stub_0 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_fetchingRequest_didFetchResult rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "sensorReader:fetchingRequest:didFetchResult:" "B@:@@@" stub_0

  -- sensorReader:didCompleteFetch:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_didCompleteFetch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sensorReader:didCompleteFetch:" "v@:@@" stub_1

  -- sensorReader:fetchingRequest:failedWithError:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_fetchingRequest_failedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "sensorReader:fetchingRequest:failedWithError:" "v@:@@@" stub_2

  -- sensorReaderWillStartRecording:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReaderWillStartRecording rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sensorReaderWillStartRecording:" "v@:@" stub_3

  -- sensorReader:startRecordingFailedWithError:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_startRecordingFailedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sensorReader:startRecordingFailedWithError:" "v@:@@" stub_4

  -- sensorReaderDidStopRecording:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReaderDidStopRecording rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sensorReaderDidStopRecording:" "v@:@" stub_5

  -- sensorReader:stopRecordingFailedWithError:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_stopRecordingFailedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sensorReader:stopRecordingFailedWithError:" "v@:@@" stub_6

  -- sensorReader:didFetchDevices:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_didFetchDevices rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sensorReader:didFetchDevices:" "v@:@@" stub_7

  -- sensorReader:fetchDevicesDidFailWithError:
  stub_8 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    case _sensorReader_fetchDevicesDidFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sensorReader:fetchDevicesDidFailWithError:" "v@:@@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SRSensorReaderDelegateOverrides
    if queriedSel == sel_sensorReader_fetchingRequest_didFetchResult then pure (maybe 0 (const 1) (_sensorReader_fetchingRequest_didFetchResult rec_))
    else if queriedSel == sel_sensorReader_didCompleteFetch then pure (maybe 0 (const 1) (_sensorReader_didCompleteFetch rec_))
    else if queriedSel == sel_sensorReader_fetchingRequest_failedWithError then pure (maybe 0 (const 1) (_sensorReader_fetchingRequest_failedWithError rec_))
    else if queriedSel == sel_sensorReaderWillStartRecording then pure (maybe 0 (const 1) (_sensorReaderWillStartRecording rec_))
    else if queriedSel == sel_sensorReader_startRecordingFailedWithError then pure (maybe 0 (const 1) (_sensorReader_startRecordingFailedWithError rec_))
    else if queriedSel == sel_sensorReaderDidStopRecording then pure (maybe 0 (const 1) (_sensorReaderDidStopRecording rec_))
    else if queriedSel == sel_sensorReader_stopRecordingFailedWithError then pure (maybe 0 (const 1) (_sensorReader_stopRecordingFailedWithError rec_))
    else if queriedSel == sel_sensorReader_didFetchDevices then pure (maybe 0 (const 1) (_sensorReader_didFetchDevices rec_))
    else if queriedSel == sel_sensorReader_fetchDevicesDidFailWithError then pure (maybe 0 (const 1) (_sensorReader_fetchDevicesDidFailWithError rec_))
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
newSRSensorReaderDelegate :: SRSensorReaderDelegateOverrides -> IO RawId
newSRSensorReaderDelegate overrides = do
  inst <- class_createInstance srSensorReaderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
