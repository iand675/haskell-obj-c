{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol HKWorkoutSessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newHKWorkoutSessionDelegate defaultHKWorkoutSessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.HealthKit.Delegate.HKWorkoutSessionDelegate
  ( HKWorkoutSessionDelegateOverrides(..)
  , defaultHKWorkoutSessionDelegateOverrides
  , newHKWorkoutSessionDelegate
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

-- | Overrides record for @\@protocol HKWorkoutSessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data HKWorkoutSessionDelegateOverrides = HKWorkoutSessionDelegateOverrides
  { _workoutSession_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _workoutSession_didGenerateEvent :: !(Maybe (RawId -> RawId -> IO ()))
  , _workoutSession_didBeginActivityWithConfiguration_date :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _workoutSession_didEndActivityWithConfiguration_date :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _workoutSession_didReceiveDataFromRemoteWorkoutSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _workoutSession_didDisconnectFromRemoteDeviceWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultHKWorkoutSessionDelegateOverrides :: HKWorkoutSessionDelegateOverrides
defaultHKWorkoutSessionDelegateOverrides = HKWorkoutSessionDelegateOverrides
  { _workoutSession_didFailWithError = Nothing
  , _workoutSession_didGenerateEvent = Nothing
  , _workoutSession_didBeginActivityWithConfiguration_date = Nothing
  , _workoutSession_didEndActivityWithConfiguration_date = Nothing
  , _workoutSession_didReceiveDataFromRemoteWorkoutSession = Nothing
  , _workoutSession_didDisconnectFromRemoteDeviceWithError = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE hkWorkoutSessionDelegateDelegateClass #-}
hkWorkoutSessionDelegateDelegateClass :: Class
hkWorkoutSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsHKWorkoutSessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_workoutSession_didFailWithError = unSelector (mkSelector "workoutSession:didFailWithError:")
      sel_workoutSession_didGenerateEvent = unSelector (mkSelector "workoutSession:didGenerateEvent:")
      sel_workoutSession_didBeginActivityWithConfiguration_date = unSelector (mkSelector "workoutSession:didBeginActivityWithConfiguration:date:")
      sel_workoutSession_didEndActivityWithConfiguration_date = unSelector (mkSelector "workoutSession:didEndActivityWithConfiguration:date:")
      sel_workoutSession_didReceiveDataFromRemoteWorkoutSession = unSelector (mkSelector "workoutSession:didReceiveDataFromRemoteWorkoutSession:")
      sel_workoutSession_didDisconnectFromRemoteDeviceWithError = unSelector (mkSelector "workoutSession:didDisconnectFromRemoteDeviceWithError:")
  -- workoutSession:didFailWithError:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    case _workoutSession_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutSession:didFailWithError:" "v@:@@" stub_0

  -- workoutSession:didGenerateEvent:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    case _workoutSession_didGenerateEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutSession:didGenerateEvent:" "v@:@@" stub_1

  -- workoutSession:didBeginActivityWithConfiguration:date:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    case _workoutSession_didBeginActivityWithConfiguration_date rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "workoutSession:didBeginActivityWithConfiguration:date:" "v@:@@@" stub_2

  -- workoutSession:didEndActivityWithConfiguration:date:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    case _workoutSession_didEndActivityWithConfiguration_date rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "workoutSession:didEndActivityWithConfiguration:date:" "v@:@@@" stub_3

  -- workoutSession:didReceiveDataFromRemoteWorkoutSession:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    case _workoutSession_didReceiveDataFromRemoteWorkoutSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutSession:didReceiveDataFromRemoteWorkoutSession:" "v@:@@" stub_4

  -- workoutSession:didDisconnectFromRemoteDeviceWithError:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    case _workoutSession_didDisconnectFromRemoteDeviceWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutSession:didDisconnectFromRemoteDeviceWithError:" "v@:@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKWorkoutSessionDelegateOverrides
    if queriedSel == sel_workoutSession_didFailWithError then pure (maybe 0 (const 1) (_workoutSession_didFailWithError rec_))
    else if queriedSel == sel_workoutSession_didGenerateEvent then pure (maybe 0 (const 1) (_workoutSession_didGenerateEvent rec_))
    else if queriedSel == sel_workoutSession_didBeginActivityWithConfiguration_date then pure (maybe 0 (const 1) (_workoutSession_didBeginActivityWithConfiguration_date rec_))
    else if queriedSel == sel_workoutSession_didEndActivityWithConfiguration_date then pure (maybe 0 (const 1) (_workoutSession_didEndActivityWithConfiguration_date rec_))
    else if queriedSel == sel_workoutSession_didReceiveDataFromRemoteWorkoutSession then pure (maybe 0 (const 1) (_workoutSession_didReceiveDataFromRemoteWorkoutSession rec_))
    else if queriedSel == sel_workoutSession_didDisconnectFromRemoteDeviceWithError then pure (maybe 0 (const 1) (_workoutSession_didDisconnectFromRemoteDeviceWithError rec_))
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
newHKWorkoutSessionDelegate :: HKWorkoutSessionDelegateOverrides -> IO RawId
newHKWorkoutSessionDelegate overrides = do
  inst <- class_createInstance hkWorkoutSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
