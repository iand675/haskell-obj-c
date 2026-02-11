{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLLocationManager@.
module ObjC.CoreLocation.CLLocationManager
  ( CLLocationManager
  , IsCLLocationManager(..)
  , clLocationManagerLocationServicesEnabled
  , clLocationManagerHeadingAvailable
  , significantLocationChangeMonitoringAvailable
  , isMonitoringAvailableForClass
  , regionMonitoringAvailable
  , regionMonitoringEnabled
  , isRangingAvailable
  , clLocationManagerAuthorizationStatus
  , requestWhenInUseAuthorization
  , requestAlwaysAuthorization
  , requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completion
  , requestTemporaryFullAccuracyAuthorizationWithPurposeKey
  , startUpdatingLocation
  , stopUpdatingLocation
  , requestLocation
  , startUpdatingHeading
  , stopUpdatingHeading
  , dismissHeadingCalibrationDisplay
  , startMonitoringSignificantLocationChanges
  , stopMonitoringSignificantLocationChanges
  , startMonitoringLocationPushesWithCompletion
  , stopMonitoringLocationPushes
  , startMonitoringForRegion_desiredAccuracy
  , stopMonitoringForRegion
  , startMonitoringForRegion
  , requestStateForRegion
  , startRangingBeaconsInRegion
  , stopRangingBeaconsInRegion
  , startRangingBeaconsSatisfyingConstraint
  , stopRangingBeaconsSatisfyingConstraint
  , allowDeferredLocationUpdatesUntilTraveled_timeout
  , disallowDeferredLocationUpdates
  , deferredLocationUpdatesAvailable
  , startMonitoringVisits
  , stopMonitoringVisits
  , authorizationStatus
  , accuracyAuthorization
  , authorizedForWidgetUpdates
  , locationServicesEnabled
  , purpose
  , setPurpose
  , activityType
  , setActivityType
  , distanceFilter
  , setDistanceFilter
  , desiredAccuracy
  , setDesiredAccuracy
  , pausesLocationUpdatesAutomatically
  , setPausesLocationUpdatesAutomatically
  , allowsBackgroundLocationUpdates
  , setAllowsBackgroundLocationUpdates
  , showsBackgroundLocationIndicator
  , setShowsBackgroundLocationIndicator
  , location
  , headingAvailable
  , headingFilter
  , setHeadingFilter
  , headingOrientation
  , setHeadingOrientation
  , heading
  , maximumRegionMonitoringDistance
  , monitoredRegions
  , rangedRegions
  , rangedBeaconConstraints
  , locationServicesEnabledSelector
  , headingAvailableSelector
  , significantLocationChangeMonitoringAvailableSelector
  , isMonitoringAvailableForClassSelector
  , regionMonitoringAvailableSelector
  , regionMonitoringEnabledSelector
  , isRangingAvailableSelector
  , authorizationStatusSelector
  , requestWhenInUseAuthorizationSelector
  , requestAlwaysAuthorizationSelector
  , requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector
  , requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector
  , startUpdatingLocationSelector
  , stopUpdatingLocationSelector
  , requestLocationSelector
  , startUpdatingHeadingSelector
  , stopUpdatingHeadingSelector
  , dismissHeadingCalibrationDisplaySelector
  , startMonitoringSignificantLocationChangesSelector
  , stopMonitoringSignificantLocationChangesSelector
  , startMonitoringLocationPushesWithCompletionSelector
  , stopMonitoringLocationPushesSelector
  , startMonitoringForRegion_desiredAccuracySelector
  , stopMonitoringForRegionSelector
  , startMonitoringForRegionSelector
  , requestStateForRegionSelector
  , startRangingBeaconsInRegionSelector
  , stopRangingBeaconsInRegionSelector
  , startRangingBeaconsSatisfyingConstraintSelector
  , stopRangingBeaconsSatisfyingConstraintSelector
  , allowDeferredLocationUpdatesUntilTraveled_timeoutSelector
  , disallowDeferredLocationUpdatesSelector
  , deferredLocationUpdatesAvailableSelector
  , startMonitoringVisitsSelector
  , stopMonitoringVisitsSelector
  , accuracyAuthorizationSelector
  , authorizedForWidgetUpdatesSelector
  , purposeSelector
  , setPurposeSelector
  , activityTypeSelector
  , setActivityTypeSelector
  , distanceFilterSelector
  , setDistanceFilterSelector
  , desiredAccuracySelector
  , setDesiredAccuracySelector
  , pausesLocationUpdatesAutomaticallySelector
  , setPausesLocationUpdatesAutomaticallySelector
  , allowsBackgroundLocationUpdatesSelector
  , setAllowsBackgroundLocationUpdatesSelector
  , showsBackgroundLocationIndicatorSelector
  , setShowsBackgroundLocationIndicatorSelector
  , locationSelector
  , headingFilterSelector
  , setHeadingFilterSelector
  , headingOrientationSelector
  , setHeadingOrientationSelector
  , headingSelector
  , maximumRegionMonitoringDistanceSelector
  , monitoredRegionsSelector
  , rangedRegionsSelector
  , rangedBeaconConstraintsSelector

  -- * Enum types
  , CLAccuracyAuthorization(CLAccuracyAuthorization)
  , pattern CLAccuracyAuthorizationFullAccuracy
  , pattern CLAccuracyAuthorizationReducedAccuracy
  , CLActivityType(CLActivityType)
  , pattern CLActivityTypeOther
  , pattern CLActivityTypeAutomotiveNavigation
  , pattern CLActivityTypeFitness
  , pattern CLActivityTypeOtherNavigation
  , pattern CLActivityTypeAirborne
  , CLAuthorizationStatus(CLAuthorizationStatus)
  , pattern KCLAuthorizationStatusNotDetermined
  , pattern KCLAuthorizationStatusRestricted
  , pattern KCLAuthorizationStatusDenied
  , pattern KCLAuthorizationStatusAuthorizedAlways
  , pattern KCLAuthorizationStatusAuthorizedWhenInUse
  , pattern KCLAuthorizationStatusAuthorized
  , CLDeviceOrientation(CLDeviceOrientation)
  , pattern CLDeviceOrientationUnknown
  , pattern CLDeviceOrientationPortrait
  , pattern CLDeviceOrientationPortraitUpsideDown
  , pattern CLDeviceOrientationLandscapeLeft
  , pattern CLDeviceOrientationLandscapeRight
  , pattern CLDeviceOrientationFaceUp
  , pattern CLDeviceOrientationFaceDown

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ locationServicesEnabled@
clLocationManagerLocationServicesEnabled :: IO Bool
clLocationManagerLocationServicesEnabled  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "locationServicesEnabled") retCULong []

-- | @+ headingAvailable@
clLocationManagerHeadingAvailable :: IO Bool
clLocationManagerHeadingAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "headingAvailable") retCULong []

-- | @+ significantLocationChangeMonitoringAvailable@
significantLocationChangeMonitoringAvailable :: IO Bool
significantLocationChangeMonitoringAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "significantLocationChangeMonitoringAvailable") retCULong []

-- | @+ isMonitoringAvailableForClass:@
isMonitoringAvailableForClass :: Class -> IO Bool
isMonitoringAvailableForClass regionClass =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isMonitoringAvailableForClass:") retCULong [argPtr (unClass regionClass)]

-- | @+ regionMonitoringAvailable@
regionMonitoringAvailable :: IO Bool
regionMonitoringAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "regionMonitoringAvailable") retCULong []

-- | @+ regionMonitoringEnabled@
regionMonitoringEnabled :: IO Bool
regionMonitoringEnabled  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "regionMonitoringEnabled") retCULong []

-- | @+ isRangingAvailable@
isRangingAvailable :: IO Bool
isRangingAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isRangingAvailable") retCULong []

-- | @+ authorizationStatus@
clLocationManagerAuthorizationStatus :: IO CLAuthorizationStatus
clLocationManagerAuthorizationStatus  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap (coerce :: CInt -> CLAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCInt []

-- | @- requestWhenInUseAuthorization@
requestWhenInUseAuthorization :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
requestWhenInUseAuthorization clLocationManager  =
  sendMsg clLocationManager (mkSelector "requestWhenInUseAuthorization") retVoid []

-- | @- requestAlwaysAuthorization@
requestAlwaysAuthorization :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
requestAlwaysAuthorization clLocationManager  =
  sendMsg clLocationManager (mkSelector "requestAlwaysAuthorization") retVoid []

-- | @- requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completion :: (IsCLLocationManager clLocationManager, IsNSString purposeKey) => clLocationManager -> purposeKey -> Ptr () -> IO ()
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completion clLocationManager  purposeKey completion =
withObjCPtr purposeKey $ \raw_purposeKey ->
    sendMsg clLocationManager (mkSelector "requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:") retVoid [argPtr (castPtr raw_purposeKey :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- requestTemporaryFullAccuracyAuthorizationWithPurposeKey:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKey :: (IsCLLocationManager clLocationManager, IsNSString purposeKey) => clLocationManager -> purposeKey -> IO ()
requestTemporaryFullAccuracyAuthorizationWithPurposeKey clLocationManager  purposeKey =
withObjCPtr purposeKey $ \raw_purposeKey ->
    sendMsg clLocationManager (mkSelector "requestTemporaryFullAccuracyAuthorizationWithPurposeKey:") retVoid [argPtr (castPtr raw_purposeKey :: Ptr ())]

-- | @- startUpdatingLocation@
startUpdatingLocation :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startUpdatingLocation clLocationManager  =
  sendMsg clLocationManager (mkSelector "startUpdatingLocation") retVoid []

-- | @- stopUpdatingLocation@
stopUpdatingLocation :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopUpdatingLocation clLocationManager  =
  sendMsg clLocationManager (mkSelector "stopUpdatingLocation") retVoid []

-- | @- requestLocation@
requestLocation :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
requestLocation clLocationManager  =
  sendMsg clLocationManager (mkSelector "requestLocation") retVoid []

-- | @- startUpdatingHeading@
startUpdatingHeading :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startUpdatingHeading clLocationManager  =
  sendMsg clLocationManager (mkSelector "startUpdatingHeading") retVoid []

-- | @- stopUpdatingHeading@
stopUpdatingHeading :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopUpdatingHeading clLocationManager  =
  sendMsg clLocationManager (mkSelector "stopUpdatingHeading") retVoid []

-- | @- dismissHeadingCalibrationDisplay@
dismissHeadingCalibrationDisplay :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
dismissHeadingCalibrationDisplay clLocationManager  =
  sendMsg clLocationManager (mkSelector "dismissHeadingCalibrationDisplay") retVoid []

-- | @- startMonitoringSignificantLocationChanges@
startMonitoringSignificantLocationChanges :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startMonitoringSignificantLocationChanges clLocationManager  =
  sendMsg clLocationManager (mkSelector "startMonitoringSignificantLocationChanges") retVoid []

-- | @- stopMonitoringSignificantLocationChanges@
stopMonitoringSignificantLocationChanges :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopMonitoringSignificantLocationChanges clLocationManager  =
  sendMsg clLocationManager (mkSelector "stopMonitoringSignificantLocationChanges") retVoid []

-- | @- startMonitoringLocationPushesWithCompletion:@
startMonitoringLocationPushesWithCompletion :: IsCLLocationManager clLocationManager => clLocationManager -> Ptr () -> IO ()
startMonitoringLocationPushesWithCompletion clLocationManager  completion =
  sendMsg clLocationManager (mkSelector "startMonitoringLocationPushesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- stopMonitoringLocationPushes@
stopMonitoringLocationPushes :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopMonitoringLocationPushes clLocationManager  =
  sendMsg clLocationManager (mkSelector "stopMonitoringLocationPushes") retVoid []

-- | @- startMonitoringForRegion:desiredAccuracy:@
startMonitoringForRegion_desiredAccuracy :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> CDouble -> IO ()
startMonitoringForRegion_desiredAccuracy clLocationManager  region accuracy =
withObjCPtr region $ \raw_region ->
    sendMsg clLocationManager (mkSelector "startMonitoringForRegion:desiredAccuracy:") retVoid [argPtr (castPtr raw_region :: Ptr ()), argCDouble (fromIntegral accuracy)]

-- | @- stopMonitoringForRegion:@
stopMonitoringForRegion :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> IO ()
stopMonitoringForRegion clLocationManager  region =
withObjCPtr region $ \raw_region ->
    sendMsg clLocationManager (mkSelector "stopMonitoringForRegion:") retVoid [argPtr (castPtr raw_region :: Ptr ())]

-- | @- startMonitoringForRegion:@
startMonitoringForRegion :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> IO ()
startMonitoringForRegion clLocationManager  region =
withObjCPtr region $ \raw_region ->
    sendMsg clLocationManager (mkSelector "startMonitoringForRegion:") retVoid [argPtr (castPtr raw_region :: Ptr ())]

-- | @- requestStateForRegion:@
requestStateForRegion :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> IO ()
requestStateForRegion clLocationManager  region =
withObjCPtr region $ \raw_region ->
    sendMsg clLocationManager (mkSelector "requestStateForRegion:") retVoid [argPtr (castPtr raw_region :: Ptr ())]

-- | @- startRangingBeaconsInRegion:@
startRangingBeaconsInRegion :: (IsCLLocationManager clLocationManager, IsCLBeaconRegion region) => clLocationManager -> region -> IO ()
startRangingBeaconsInRegion clLocationManager  region =
withObjCPtr region $ \raw_region ->
    sendMsg clLocationManager (mkSelector "startRangingBeaconsInRegion:") retVoid [argPtr (castPtr raw_region :: Ptr ())]

-- | @- stopRangingBeaconsInRegion:@
stopRangingBeaconsInRegion :: (IsCLLocationManager clLocationManager, IsCLBeaconRegion region) => clLocationManager -> region -> IO ()
stopRangingBeaconsInRegion clLocationManager  region =
withObjCPtr region $ \raw_region ->
    sendMsg clLocationManager (mkSelector "stopRangingBeaconsInRegion:") retVoid [argPtr (castPtr raw_region :: Ptr ())]

-- | @- startRangingBeaconsSatisfyingConstraint:@
startRangingBeaconsSatisfyingConstraint :: (IsCLLocationManager clLocationManager, IsCLBeaconIdentityConstraint constraint) => clLocationManager -> constraint -> IO ()
startRangingBeaconsSatisfyingConstraint clLocationManager  constraint =
withObjCPtr constraint $ \raw_constraint ->
    sendMsg clLocationManager (mkSelector "startRangingBeaconsSatisfyingConstraint:") retVoid [argPtr (castPtr raw_constraint :: Ptr ())]

-- | @- stopRangingBeaconsSatisfyingConstraint:@
stopRangingBeaconsSatisfyingConstraint :: (IsCLLocationManager clLocationManager, IsCLBeaconIdentityConstraint constraint) => clLocationManager -> constraint -> IO ()
stopRangingBeaconsSatisfyingConstraint clLocationManager  constraint =
withObjCPtr constraint $ \raw_constraint ->
    sendMsg clLocationManager (mkSelector "stopRangingBeaconsSatisfyingConstraint:") retVoid [argPtr (castPtr raw_constraint :: Ptr ())]

-- | @- allowDeferredLocationUpdatesUntilTraveled:timeout:@
allowDeferredLocationUpdatesUntilTraveled_timeout :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> CDouble -> IO ()
allowDeferredLocationUpdatesUntilTraveled_timeout clLocationManager  distance timeout =
  sendMsg clLocationManager (mkSelector "allowDeferredLocationUpdatesUntilTraveled:timeout:") retVoid [argCDouble (fromIntegral distance), argCDouble (fromIntegral timeout)]

-- | @- disallowDeferredLocationUpdates@
disallowDeferredLocationUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
disallowDeferredLocationUpdates clLocationManager  =
  sendMsg clLocationManager (mkSelector "disallowDeferredLocationUpdates") retVoid []

-- | @+ deferredLocationUpdatesAvailable@
deferredLocationUpdatesAvailable :: IO Bool
deferredLocationUpdatesAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "deferredLocationUpdatesAvailable") retCULong []

-- | @- startMonitoringVisits@
startMonitoringVisits :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startMonitoringVisits clLocationManager  =
  sendMsg clLocationManager (mkSelector "startMonitoringVisits") retVoid []

-- | @- stopMonitoringVisits@
stopMonitoringVisits :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopMonitoringVisits clLocationManager  =
  sendMsg clLocationManager (mkSelector "stopMonitoringVisits") retVoid []

-- | @- authorizationStatus@
authorizationStatus :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLAuthorizationStatus
authorizationStatus clLocationManager  =
  fmap (coerce :: CInt -> CLAuthorizationStatus) $ sendMsg clLocationManager (mkSelector "authorizationStatus") retCInt []

-- | @- accuracyAuthorization@
accuracyAuthorization :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLAccuracyAuthorization
accuracyAuthorization clLocationManager  =
  fmap (coerce :: CLong -> CLAccuracyAuthorization) $ sendMsg clLocationManager (mkSelector "accuracyAuthorization") retCLong []

-- | @- authorizedForWidgetUpdates@
authorizedForWidgetUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
authorizedForWidgetUpdates clLocationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationManager (mkSelector "authorizedForWidgetUpdates") retCULong []

-- | @- locationServicesEnabled@
locationServicesEnabled :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
locationServicesEnabled clLocationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationManager (mkSelector "locationServicesEnabled") retCULong []

-- | @- purpose@
purpose :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSString)
purpose clLocationManager  =
  sendMsg clLocationManager (mkSelector "purpose") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPurpose:@
setPurpose :: (IsCLLocationManager clLocationManager, IsNSString value) => clLocationManager -> value -> IO ()
setPurpose clLocationManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg clLocationManager (mkSelector "setPurpose:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activityType@
activityType :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLActivityType
activityType clLocationManager  =
  fmap (coerce :: CLong -> CLActivityType) $ sendMsg clLocationManager (mkSelector "activityType") retCLong []

-- | @- setActivityType:@
setActivityType :: IsCLLocationManager clLocationManager => clLocationManager -> CLActivityType -> IO ()
setActivityType clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setActivityType:") retVoid [argCLong (coerce value)]

-- | @- distanceFilter@
distanceFilter :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
distanceFilter clLocationManager  =
  sendMsg clLocationManager (mkSelector "distanceFilter") retCDouble []

-- | @- setDistanceFilter:@
setDistanceFilter :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> IO ()
setDistanceFilter clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setDistanceFilter:") retVoid [argCDouble (fromIntegral value)]

-- | @- desiredAccuracy@
desiredAccuracy :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
desiredAccuracy clLocationManager  =
  sendMsg clLocationManager (mkSelector "desiredAccuracy") retCDouble []

-- | @- setDesiredAccuracy:@
setDesiredAccuracy :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> IO ()
setDesiredAccuracy clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setDesiredAccuracy:") retVoid [argCDouble (fromIntegral value)]

-- | @- pausesLocationUpdatesAutomatically@
pausesLocationUpdatesAutomatically :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
pausesLocationUpdatesAutomatically clLocationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationManager (mkSelector "pausesLocationUpdatesAutomatically") retCULong []

-- | @- setPausesLocationUpdatesAutomatically:@
setPausesLocationUpdatesAutomatically :: IsCLLocationManager clLocationManager => clLocationManager -> Bool -> IO ()
setPausesLocationUpdatesAutomatically clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setPausesLocationUpdatesAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsBackgroundLocationUpdates@
allowsBackgroundLocationUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
allowsBackgroundLocationUpdates clLocationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationManager (mkSelector "allowsBackgroundLocationUpdates") retCULong []

-- | @- setAllowsBackgroundLocationUpdates:@
setAllowsBackgroundLocationUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> Bool -> IO ()
setAllowsBackgroundLocationUpdates clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setAllowsBackgroundLocationUpdates:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsBackgroundLocationIndicator@
showsBackgroundLocationIndicator :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
showsBackgroundLocationIndicator clLocationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationManager (mkSelector "showsBackgroundLocationIndicator") retCULong []

-- | @- setShowsBackgroundLocationIndicator:@
setShowsBackgroundLocationIndicator :: IsCLLocationManager clLocationManager => clLocationManager -> Bool -> IO ()
setShowsBackgroundLocationIndicator clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setShowsBackgroundLocationIndicator:") retVoid [argCULong (if value then 1 else 0)]

-- | @- location@
location :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id CLLocation)
location clLocationManager  =
  sendMsg clLocationManager (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- headingAvailable@
headingAvailable :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
headingAvailable clLocationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationManager (mkSelector "headingAvailable") retCULong []

-- | @- headingFilter@
headingFilter :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
headingFilter clLocationManager  =
  sendMsg clLocationManager (mkSelector "headingFilter") retCDouble []

-- | @- setHeadingFilter:@
setHeadingFilter :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> IO ()
setHeadingFilter clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setHeadingFilter:") retVoid [argCDouble (fromIntegral value)]

-- | @- headingOrientation@
headingOrientation :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLDeviceOrientation
headingOrientation clLocationManager  =
  fmap (coerce :: CInt -> CLDeviceOrientation) $ sendMsg clLocationManager (mkSelector "headingOrientation") retCInt []

-- | @- setHeadingOrientation:@
setHeadingOrientation :: IsCLLocationManager clLocationManager => clLocationManager -> CLDeviceOrientation -> IO ()
setHeadingOrientation clLocationManager  value =
  sendMsg clLocationManager (mkSelector "setHeadingOrientation:") retVoid [argCInt (coerce value)]

-- | @- heading@
heading :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id CLHeading)
heading clLocationManager  =
  sendMsg clLocationManager (mkSelector "heading") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maximumRegionMonitoringDistance@
maximumRegionMonitoringDistance :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
maximumRegionMonitoringDistance clLocationManager  =
  sendMsg clLocationManager (mkSelector "maximumRegionMonitoringDistance") retCDouble []

-- | @- monitoredRegions@
monitoredRegions :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSSet)
monitoredRegions clLocationManager  =
  sendMsg clLocationManager (mkSelector "monitoredRegions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangedRegions@
rangedRegions :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSSet)
rangedRegions clLocationManager  =
  sendMsg clLocationManager (mkSelector "rangedRegions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangedBeaconConstraints@
rangedBeaconConstraints :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSSet)
rangedBeaconConstraints clLocationManager  =
  sendMsg clLocationManager (mkSelector "rangedBeaconConstraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationServicesEnabled@
locationServicesEnabledSelector :: Selector
locationServicesEnabledSelector = mkSelector "locationServicesEnabled"

-- | @Selector@ for @headingAvailable@
headingAvailableSelector :: Selector
headingAvailableSelector = mkSelector "headingAvailable"

-- | @Selector@ for @significantLocationChangeMonitoringAvailable@
significantLocationChangeMonitoringAvailableSelector :: Selector
significantLocationChangeMonitoringAvailableSelector = mkSelector "significantLocationChangeMonitoringAvailable"

-- | @Selector@ for @isMonitoringAvailableForClass:@
isMonitoringAvailableForClassSelector :: Selector
isMonitoringAvailableForClassSelector = mkSelector "isMonitoringAvailableForClass:"

-- | @Selector@ for @regionMonitoringAvailable@
regionMonitoringAvailableSelector :: Selector
regionMonitoringAvailableSelector = mkSelector "regionMonitoringAvailable"

-- | @Selector@ for @regionMonitoringEnabled@
regionMonitoringEnabledSelector :: Selector
regionMonitoringEnabledSelector = mkSelector "regionMonitoringEnabled"

-- | @Selector@ for @isRangingAvailable@
isRangingAvailableSelector :: Selector
isRangingAvailableSelector = mkSelector "isRangingAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestWhenInUseAuthorization@
requestWhenInUseAuthorizationSelector :: Selector
requestWhenInUseAuthorizationSelector = mkSelector "requestWhenInUseAuthorization"

-- | @Selector@ for @requestAlwaysAuthorization@
requestAlwaysAuthorizationSelector :: Selector
requestAlwaysAuthorizationSelector = mkSelector "requestAlwaysAuthorization"

-- | @Selector@ for @requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector :: Selector
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector = mkSelector "requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:"

-- | @Selector@ for @requestTemporaryFullAccuracyAuthorizationWithPurposeKey:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector :: Selector
requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector = mkSelector "requestTemporaryFullAccuracyAuthorizationWithPurposeKey:"

-- | @Selector@ for @startUpdatingLocation@
startUpdatingLocationSelector :: Selector
startUpdatingLocationSelector = mkSelector "startUpdatingLocation"

-- | @Selector@ for @stopUpdatingLocation@
stopUpdatingLocationSelector :: Selector
stopUpdatingLocationSelector = mkSelector "stopUpdatingLocation"

-- | @Selector@ for @requestLocation@
requestLocationSelector :: Selector
requestLocationSelector = mkSelector "requestLocation"

-- | @Selector@ for @startUpdatingHeading@
startUpdatingHeadingSelector :: Selector
startUpdatingHeadingSelector = mkSelector "startUpdatingHeading"

-- | @Selector@ for @stopUpdatingHeading@
stopUpdatingHeadingSelector :: Selector
stopUpdatingHeadingSelector = mkSelector "stopUpdatingHeading"

-- | @Selector@ for @dismissHeadingCalibrationDisplay@
dismissHeadingCalibrationDisplaySelector :: Selector
dismissHeadingCalibrationDisplaySelector = mkSelector "dismissHeadingCalibrationDisplay"

-- | @Selector@ for @startMonitoringSignificantLocationChanges@
startMonitoringSignificantLocationChangesSelector :: Selector
startMonitoringSignificantLocationChangesSelector = mkSelector "startMonitoringSignificantLocationChanges"

-- | @Selector@ for @stopMonitoringSignificantLocationChanges@
stopMonitoringSignificantLocationChangesSelector :: Selector
stopMonitoringSignificantLocationChangesSelector = mkSelector "stopMonitoringSignificantLocationChanges"

-- | @Selector@ for @startMonitoringLocationPushesWithCompletion:@
startMonitoringLocationPushesWithCompletionSelector :: Selector
startMonitoringLocationPushesWithCompletionSelector = mkSelector "startMonitoringLocationPushesWithCompletion:"

-- | @Selector@ for @stopMonitoringLocationPushes@
stopMonitoringLocationPushesSelector :: Selector
stopMonitoringLocationPushesSelector = mkSelector "stopMonitoringLocationPushes"

-- | @Selector@ for @startMonitoringForRegion:desiredAccuracy:@
startMonitoringForRegion_desiredAccuracySelector :: Selector
startMonitoringForRegion_desiredAccuracySelector = mkSelector "startMonitoringForRegion:desiredAccuracy:"

-- | @Selector@ for @stopMonitoringForRegion:@
stopMonitoringForRegionSelector :: Selector
stopMonitoringForRegionSelector = mkSelector "stopMonitoringForRegion:"

-- | @Selector@ for @startMonitoringForRegion:@
startMonitoringForRegionSelector :: Selector
startMonitoringForRegionSelector = mkSelector "startMonitoringForRegion:"

-- | @Selector@ for @requestStateForRegion:@
requestStateForRegionSelector :: Selector
requestStateForRegionSelector = mkSelector "requestStateForRegion:"

-- | @Selector@ for @startRangingBeaconsInRegion:@
startRangingBeaconsInRegionSelector :: Selector
startRangingBeaconsInRegionSelector = mkSelector "startRangingBeaconsInRegion:"

-- | @Selector@ for @stopRangingBeaconsInRegion:@
stopRangingBeaconsInRegionSelector :: Selector
stopRangingBeaconsInRegionSelector = mkSelector "stopRangingBeaconsInRegion:"

-- | @Selector@ for @startRangingBeaconsSatisfyingConstraint:@
startRangingBeaconsSatisfyingConstraintSelector :: Selector
startRangingBeaconsSatisfyingConstraintSelector = mkSelector "startRangingBeaconsSatisfyingConstraint:"

-- | @Selector@ for @stopRangingBeaconsSatisfyingConstraint:@
stopRangingBeaconsSatisfyingConstraintSelector :: Selector
stopRangingBeaconsSatisfyingConstraintSelector = mkSelector "stopRangingBeaconsSatisfyingConstraint:"

-- | @Selector@ for @allowDeferredLocationUpdatesUntilTraveled:timeout:@
allowDeferredLocationUpdatesUntilTraveled_timeoutSelector :: Selector
allowDeferredLocationUpdatesUntilTraveled_timeoutSelector = mkSelector "allowDeferredLocationUpdatesUntilTraveled:timeout:"

-- | @Selector@ for @disallowDeferredLocationUpdates@
disallowDeferredLocationUpdatesSelector :: Selector
disallowDeferredLocationUpdatesSelector = mkSelector "disallowDeferredLocationUpdates"

-- | @Selector@ for @deferredLocationUpdatesAvailable@
deferredLocationUpdatesAvailableSelector :: Selector
deferredLocationUpdatesAvailableSelector = mkSelector "deferredLocationUpdatesAvailable"

-- | @Selector@ for @startMonitoringVisits@
startMonitoringVisitsSelector :: Selector
startMonitoringVisitsSelector = mkSelector "startMonitoringVisits"

-- | @Selector@ for @stopMonitoringVisits@
stopMonitoringVisitsSelector :: Selector
stopMonitoringVisitsSelector = mkSelector "stopMonitoringVisits"

-- | @Selector@ for @accuracyAuthorization@
accuracyAuthorizationSelector :: Selector
accuracyAuthorizationSelector = mkSelector "accuracyAuthorization"

-- | @Selector@ for @authorizedForWidgetUpdates@
authorizedForWidgetUpdatesSelector :: Selector
authorizedForWidgetUpdatesSelector = mkSelector "authorizedForWidgetUpdates"

-- | @Selector@ for @purpose@
purposeSelector :: Selector
purposeSelector = mkSelector "purpose"

-- | @Selector@ for @setPurpose:@
setPurposeSelector :: Selector
setPurposeSelector = mkSelector "setPurpose:"

-- | @Selector@ for @activityType@
activityTypeSelector :: Selector
activityTypeSelector = mkSelector "activityType"

-- | @Selector@ for @setActivityType:@
setActivityTypeSelector :: Selector
setActivityTypeSelector = mkSelector "setActivityType:"

-- | @Selector@ for @distanceFilter@
distanceFilterSelector :: Selector
distanceFilterSelector = mkSelector "distanceFilter"

-- | @Selector@ for @setDistanceFilter:@
setDistanceFilterSelector :: Selector
setDistanceFilterSelector = mkSelector "setDistanceFilter:"

-- | @Selector@ for @desiredAccuracy@
desiredAccuracySelector :: Selector
desiredAccuracySelector = mkSelector "desiredAccuracy"

-- | @Selector@ for @setDesiredAccuracy:@
setDesiredAccuracySelector :: Selector
setDesiredAccuracySelector = mkSelector "setDesiredAccuracy:"

-- | @Selector@ for @pausesLocationUpdatesAutomatically@
pausesLocationUpdatesAutomaticallySelector :: Selector
pausesLocationUpdatesAutomaticallySelector = mkSelector "pausesLocationUpdatesAutomatically"

-- | @Selector@ for @setPausesLocationUpdatesAutomatically:@
setPausesLocationUpdatesAutomaticallySelector :: Selector
setPausesLocationUpdatesAutomaticallySelector = mkSelector "setPausesLocationUpdatesAutomatically:"

-- | @Selector@ for @allowsBackgroundLocationUpdates@
allowsBackgroundLocationUpdatesSelector :: Selector
allowsBackgroundLocationUpdatesSelector = mkSelector "allowsBackgroundLocationUpdates"

-- | @Selector@ for @setAllowsBackgroundLocationUpdates:@
setAllowsBackgroundLocationUpdatesSelector :: Selector
setAllowsBackgroundLocationUpdatesSelector = mkSelector "setAllowsBackgroundLocationUpdates:"

-- | @Selector@ for @showsBackgroundLocationIndicator@
showsBackgroundLocationIndicatorSelector :: Selector
showsBackgroundLocationIndicatorSelector = mkSelector "showsBackgroundLocationIndicator"

-- | @Selector@ for @setShowsBackgroundLocationIndicator:@
setShowsBackgroundLocationIndicatorSelector :: Selector
setShowsBackgroundLocationIndicatorSelector = mkSelector "setShowsBackgroundLocationIndicator:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @headingFilter@
headingFilterSelector :: Selector
headingFilterSelector = mkSelector "headingFilter"

-- | @Selector@ for @setHeadingFilter:@
setHeadingFilterSelector :: Selector
setHeadingFilterSelector = mkSelector "setHeadingFilter:"

-- | @Selector@ for @headingOrientation@
headingOrientationSelector :: Selector
headingOrientationSelector = mkSelector "headingOrientation"

-- | @Selector@ for @setHeadingOrientation:@
setHeadingOrientationSelector :: Selector
setHeadingOrientationSelector = mkSelector "setHeadingOrientation:"

-- | @Selector@ for @heading@
headingSelector :: Selector
headingSelector = mkSelector "heading"

-- | @Selector@ for @maximumRegionMonitoringDistance@
maximumRegionMonitoringDistanceSelector :: Selector
maximumRegionMonitoringDistanceSelector = mkSelector "maximumRegionMonitoringDistance"

-- | @Selector@ for @monitoredRegions@
monitoredRegionsSelector :: Selector
monitoredRegionsSelector = mkSelector "monitoredRegions"

-- | @Selector@ for @rangedRegions@
rangedRegionsSelector :: Selector
rangedRegionsSelector = mkSelector "rangedRegions"

-- | @Selector@ for @rangedBeaconConstraints@
rangedBeaconConstraintsSelector :: Selector
rangedBeaconConstraintsSelector = mkSelector "rangedBeaconConstraints"

