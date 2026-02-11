{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CLLocationManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCLLocationManagerDelegate defaultCLLocationManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreLocation.Delegate.CLLocationManagerDelegate
  ( CLLocationManagerDelegateOverrides(..)
  , defaultCLLocationManagerDelegateOverrides
  , newCLLocationManagerDelegate
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

-- | Overrides record for @\@protocol CLLocationManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CLLocationManagerDelegateOverrides = CLLocationManagerDelegateOverrides
  { _locationManager_didUpdateToLocation_fromLocation :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _locationManager_didUpdateLocations :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManager_didUpdateHeading :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManagerShouldDisplayHeadingCalibration :: !(Maybe (RawId -> IO Bool))
  , _locationManager_didRangeBeacons_inRegion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _locationManager_rangingBeaconsDidFailForRegion_withError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _locationManager_didRangeBeacons_satisfyingConstraint :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _locationManager_didFailRangingBeaconsForConstraint_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _locationManager_didEnterRegion :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManager_didExitRegion :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManager_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManager_monitoringDidFailForRegion_withError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _locationManagerDidChangeAuthorization :: !(Maybe (RawId -> IO ()))
  , _locationManager_didStartMonitoringForRegion :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManagerDidPauseLocationUpdates :: !(Maybe (RawId -> IO ()))
  , _locationManagerDidResumeLocationUpdates :: !(Maybe (RawId -> IO ()))
  , _locationManager_didFinishDeferredUpdatesWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationManager_didVisit :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCLLocationManagerDelegateOverrides :: CLLocationManagerDelegateOverrides
defaultCLLocationManagerDelegateOverrides = CLLocationManagerDelegateOverrides
  { _locationManager_didUpdateToLocation_fromLocation = Nothing
  , _locationManager_didUpdateLocations = Nothing
  , _locationManager_didUpdateHeading = Nothing
  , _locationManagerShouldDisplayHeadingCalibration = Nothing
  , _locationManager_didRangeBeacons_inRegion = Nothing
  , _locationManager_rangingBeaconsDidFailForRegion_withError = Nothing
  , _locationManager_didRangeBeacons_satisfyingConstraint = Nothing
  , _locationManager_didFailRangingBeaconsForConstraint_error = Nothing
  , _locationManager_didEnterRegion = Nothing
  , _locationManager_didExitRegion = Nothing
  , _locationManager_didFailWithError = Nothing
  , _locationManager_monitoringDidFailForRegion_withError = Nothing
  , _locationManagerDidChangeAuthorization = Nothing
  , _locationManager_didStartMonitoringForRegion = Nothing
  , _locationManagerDidPauseLocationUpdates = Nothing
  , _locationManagerDidResumeLocationUpdates = Nothing
  , _locationManager_didFinishDeferredUpdatesWithError = Nothing
  , _locationManager_didVisit = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE clLocationManagerDelegateDelegateClass #-}
clLocationManagerDelegateDelegateClass :: Class
clLocationManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCLLocationManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_locationManager_didUpdateToLocation_fromLocation = unSelector (mkSelector "locationManager:didUpdateToLocation:fromLocation:")
      sel_locationManager_didUpdateLocations = unSelector (mkSelector "locationManager:didUpdateLocations:")
      sel_locationManager_didUpdateHeading = unSelector (mkSelector "locationManager:didUpdateHeading:")
      sel_locationManagerShouldDisplayHeadingCalibration = unSelector (mkSelector "locationManagerShouldDisplayHeadingCalibration:")
      sel_locationManager_didRangeBeacons_inRegion = unSelector (mkSelector "locationManager:didRangeBeacons:inRegion:")
      sel_locationManager_rangingBeaconsDidFailForRegion_withError = unSelector (mkSelector "locationManager:rangingBeaconsDidFailForRegion:withError:")
      sel_locationManager_didRangeBeacons_satisfyingConstraint = unSelector (mkSelector "locationManager:didRangeBeacons:satisfyingConstraint:")
      sel_locationManager_didFailRangingBeaconsForConstraint_error = unSelector (mkSelector "locationManager:didFailRangingBeaconsForConstraint:error:")
      sel_locationManager_didEnterRegion = unSelector (mkSelector "locationManager:didEnterRegion:")
      sel_locationManager_didExitRegion = unSelector (mkSelector "locationManager:didExitRegion:")
      sel_locationManager_didFailWithError = unSelector (mkSelector "locationManager:didFailWithError:")
      sel_locationManager_monitoringDidFailForRegion_withError = unSelector (mkSelector "locationManager:monitoringDidFailForRegion:withError:")
      sel_locationManagerDidChangeAuthorization = unSelector (mkSelector "locationManagerDidChangeAuthorization:")
      sel_locationManager_didStartMonitoringForRegion = unSelector (mkSelector "locationManager:didStartMonitoringForRegion:")
      sel_locationManagerDidPauseLocationUpdates = unSelector (mkSelector "locationManagerDidPauseLocationUpdates:")
      sel_locationManagerDidResumeLocationUpdates = unSelector (mkSelector "locationManagerDidResumeLocationUpdates:")
      sel_locationManager_didFinishDeferredUpdatesWithError = unSelector (mkSelector "locationManager:didFinishDeferredUpdatesWithError:")
      sel_locationManager_didVisit = unSelector (mkSelector "locationManager:didVisit:")
  -- locationManager:didUpdateToLocation:fromLocation:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didUpdateToLocation_fromLocation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "locationManager:didUpdateToLocation:fromLocation:" "v@:@@@" stub_0

  -- locationManager:didUpdateLocations:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didUpdateLocations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didUpdateLocations:" "v@:@@" stub_1

  -- locationManager:didUpdateHeading:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didUpdateHeading rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didUpdateHeading:" "v@:@@" stub_2

  -- locationManagerShouldDisplayHeadingCalibration:
  stub_3 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManagerShouldDisplayHeadingCalibration rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "locationManagerShouldDisplayHeadingCalibration:" "B@:@" stub_3

  -- locationManager:didRangeBeacons:inRegion:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didRangeBeacons_inRegion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "locationManager:didRangeBeacons:inRegion:" "v@:@@@" stub_4

  -- locationManager:rangingBeaconsDidFailForRegion:withError:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_rangingBeaconsDidFailForRegion_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "locationManager:rangingBeaconsDidFailForRegion:withError:" "v@:@@@" stub_5

  -- locationManager:didRangeBeacons:satisfyingConstraint:
  stub_6 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didRangeBeacons_satisfyingConstraint rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "locationManager:didRangeBeacons:satisfyingConstraint:" "v@:@@@" stub_6

  -- locationManager:didFailRangingBeaconsForConstraint:error:
  stub_7 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didFailRangingBeaconsForConstraint_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "locationManager:didFailRangingBeaconsForConstraint:error:" "v@:@@@" stub_7

  -- locationManager:didEnterRegion:
  stub_8 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didEnterRegion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didEnterRegion:" "v@:@@" stub_8

  -- locationManager:didExitRegion:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didExitRegion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didExitRegion:" "v@:@@" stub_9

  -- locationManager:didFailWithError:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didFailWithError:" "v@:@@" stub_10

  -- locationManager:monitoringDidFailForRegion:withError:
  stub_11 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_monitoringDidFailForRegion_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "locationManager:monitoringDidFailForRegion:withError:" "v@:@@@" stub_11

  -- locationManagerDidChangeAuthorization:
  stub_12 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManagerDidChangeAuthorization rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "locationManagerDidChangeAuthorization:" "v@:@" stub_12

  -- locationManager:didStartMonitoringForRegion:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didStartMonitoringForRegion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didStartMonitoringForRegion:" "v@:@@" stub_13

  -- locationManagerDidPauseLocationUpdates:
  stub_14 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManagerDidPauseLocationUpdates rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "locationManagerDidPauseLocationUpdates:" "v@:@" stub_14

  -- locationManagerDidResumeLocationUpdates:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManagerDidResumeLocationUpdates rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "locationManagerDidResumeLocationUpdates:" "v@:@" stub_15

  -- locationManager:didFinishDeferredUpdatesWithError:
  stub_16 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didFinishDeferredUpdatesWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didFinishDeferredUpdatesWithError:" "v@:@@" stub_16

  -- locationManager:didVisit:
  stub_17 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    case _locationManager_didVisit rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "locationManager:didVisit:" "v@:@@" stub_17

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CLLocationManagerDelegateOverrides
    if queriedSel == sel_locationManager_didUpdateToLocation_fromLocation then pure (maybe 0 (const 1) (_locationManager_didUpdateToLocation_fromLocation rec_))
    else if queriedSel == sel_locationManager_didUpdateLocations then pure (maybe 0 (const 1) (_locationManager_didUpdateLocations rec_))
    else if queriedSel == sel_locationManager_didUpdateHeading then pure (maybe 0 (const 1) (_locationManager_didUpdateHeading rec_))
    else if queriedSel == sel_locationManagerShouldDisplayHeadingCalibration then pure (maybe 0 (const 1) (_locationManagerShouldDisplayHeadingCalibration rec_))
    else if queriedSel == sel_locationManager_didRangeBeacons_inRegion then pure (maybe 0 (const 1) (_locationManager_didRangeBeacons_inRegion rec_))
    else if queriedSel == sel_locationManager_rangingBeaconsDidFailForRegion_withError then pure (maybe 0 (const 1) (_locationManager_rangingBeaconsDidFailForRegion_withError rec_))
    else if queriedSel == sel_locationManager_didRangeBeacons_satisfyingConstraint then pure (maybe 0 (const 1) (_locationManager_didRangeBeacons_satisfyingConstraint rec_))
    else if queriedSel == sel_locationManager_didFailRangingBeaconsForConstraint_error then pure (maybe 0 (const 1) (_locationManager_didFailRangingBeaconsForConstraint_error rec_))
    else if queriedSel == sel_locationManager_didEnterRegion then pure (maybe 0 (const 1) (_locationManager_didEnterRegion rec_))
    else if queriedSel == sel_locationManager_didExitRegion then pure (maybe 0 (const 1) (_locationManager_didExitRegion rec_))
    else if queriedSel == sel_locationManager_didFailWithError then pure (maybe 0 (const 1) (_locationManager_didFailWithError rec_))
    else if queriedSel == sel_locationManager_monitoringDidFailForRegion_withError then pure (maybe 0 (const 1) (_locationManager_monitoringDidFailForRegion_withError rec_))
    else if queriedSel == sel_locationManagerDidChangeAuthorization then pure (maybe 0 (const 1) (_locationManagerDidChangeAuthorization rec_))
    else if queriedSel == sel_locationManager_didStartMonitoringForRegion then pure (maybe 0 (const 1) (_locationManager_didStartMonitoringForRegion rec_))
    else if queriedSel == sel_locationManagerDidPauseLocationUpdates then pure (maybe 0 (const 1) (_locationManagerDidPauseLocationUpdates rec_))
    else if queriedSel == sel_locationManagerDidResumeLocationUpdates then pure (maybe 0 (const 1) (_locationManagerDidResumeLocationUpdates rec_))
    else if queriedSel == sel_locationManager_didFinishDeferredUpdatesWithError then pure (maybe 0 (const 1) (_locationManager_didFinishDeferredUpdatesWithError rec_))
    else if queriedSel == sel_locationManager_didVisit then pure (maybe 0 (const 1) (_locationManager_didVisit rec_))
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
newCLLocationManagerDelegate :: CLLocationManagerDelegateOverrides -> IO RawId
newCLLocationManagerDelegate overrides = do
  inst <- class_createInstance clLocationManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
