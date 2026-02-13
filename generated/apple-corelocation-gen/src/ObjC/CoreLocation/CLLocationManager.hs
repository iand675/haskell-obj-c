{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , accuracyAuthorizationSelector
  , activityTypeSelector
  , allowDeferredLocationUpdatesUntilTraveled_timeoutSelector
  , allowsBackgroundLocationUpdatesSelector
  , authorizationStatusSelector
  , authorizedForWidgetUpdatesSelector
  , clLocationManagerAuthorizationStatusSelector
  , clLocationManagerHeadingAvailableSelector
  , clLocationManagerLocationServicesEnabledSelector
  , deferredLocationUpdatesAvailableSelector
  , delegateSelector
  , desiredAccuracySelector
  , disallowDeferredLocationUpdatesSelector
  , dismissHeadingCalibrationDisplaySelector
  , distanceFilterSelector
  , headingAvailableSelector
  , headingFilterSelector
  , headingOrientationSelector
  , headingSelector
  , isMonitoringAvailableForClassSelector
  , isRangingAvailableSelector
  , locationServicesEnabledSelector
  , maximumRegionMonitoringDistanceSelector
  , monitoredRegionsSelector
  , pausesLocationUpdatesAutomaticallySelector
  , purposeSelector
  , rangedBeaconConstraintsSelector
  , rangedRegionsSelector
  , regionMonitoringAvailableSelector
  , regionMonitoringEnabledSelector
  , requestAlwaysAuthorizationSelector
  , requestLocationSelector
  , requestStateForRegionSelector
  , requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector
  , requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector
  , requestWhenInUseAuthorizationSelector
  , setActivityTypeSelector
  , setAllowsBackgroundLocationUpdatesSelector
  , setDelegateSelector
  , setDesiredAccuracySelector
  , setDistanceFilterSelector
  , setHeadingFilterSelector
  , setHeadingOrientationSelector
  , setPausesLocationUpdatesAutomaticallySelector
  , setPurposeSelector
  , setShowsBackgroundLocationIndicatorSelector
  , showsBackgroundLocationIndicatorSelector
  , significantLocationChangeMonitoringAvailableSelector
  , startMonitoringForRegionSelector
  , startMonitoringForRegion_desiredAccuracySelector
  , startMonitoringLocationPushesWithCompletionSelector
  , startMonitoringSignificantLocationChangesSelector
  , startMonitoringVisitsSelector
  , startRangingBeaconsInRegionSelector
  , startRangingBeaconsSatisfyingConstraintSelector
  , startUpdatingHeadingSelector
  , startUpdatingLocationSelector
  , stopMonitoringForRegionSelector
  , stopMonitoringLocationPushesSelector
  , stopMonitoringSignificantLocationChangesSelector
  , stopMonitoringVisitsSelector
  , stopRangingBeaconsInRegionSelector
  , stopRangingBeaconsSatisfyingConstraintSelector
  , stopUpdatingHeadingSelector
  , stopUpdatingLocationSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' clLocationManagerLocationServicesEnabledSelector

-- | @+ headingAvailable@
clLocationManagerHeadingAvailable :: IO Bool
clLocationManagerHeadingAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' clLocationManagerHeadingAvailableSelector

-- | @+ significantLocationChangeMonitoringAvailable@
significantLocationChangeMonitoringAvailable :: IO Bool
significantLocationChangeMonitoringAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' significantLocationChangeMonitoringAvailableSelector

-- | @+ isMonitoringAvailableForClass:@
isMonitoringAvailableForClass :: Class -> IO Bool
isMonitoringAvailableForClass regionClass =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' isMonitoringAvailableForClassSelector regionClass

-- | @+ regionMonitoringAvailable@
regionMonitoringAvailable :: IO Bool
regionMonitoringAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' regionMonitoringAvailableSelector

-- | @+ regionMonitoringEnabled@
regionMonitoringEnabled :: IO Bool
regionMonitoringEnabled  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' regionMonitoringEnabledSelector

-- | @+ isRangingAvailable@
isRangingAvailable :: IO Bool
isRangingAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' isRangingAvailableSelector

-- | @+ authorizationStatus@
clLocationManagerAuthorizationStatus :: IO CLAuthorizationStatus
clLocationManagerAuthorizationStatus  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' clLocationManagerAuthorizationStatusSelector

-- | @- requestWhenInUseAuthorization@
requestWhenInUseAuthorization :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
requestWhenInUseAuthorization clLocationManager =
  sendMessage clLocationManager requestWhenInUseAuthorizationSelector

-- | @- requestAlwaysAuthorization@
requestAlwaysAuthorization :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
requestAlwaysAuthorization clLocationManager =
  sendMessage clLocationManager requestAlwaysAuthorizationSelector

-- | @- requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completion :: (IsCLLocationManager clLocationManager, IsNSString purposeKey) => clLocationManager -> purposeKey -> Ptr () -> IO ()
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completion clLocationManager purposeKey completion =
  sendMessage clLocationManager requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector (toNSString purposeKey) completion

-- | @- requestTemporaryFullAccuracyAuthorizationWithPurposeKey:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKey :: (IsCLLocationManager clLocationManager, IsNSString purposeKey) => clLocationManager -> purposeKey -> IO ()
requestTemporaryFullAccuracyAuthorizationWithPurposeKey clLocationManager purposeKey =
  sendMessage clLocationManager requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector (toNSString purposeKey)

-- | @- startUpdatingLocation@
startUpdatingLocation :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startUpdatingLocation clLocationManager =
  sendMessage clLocationManager startUpdatingLocationSelector

-- | @- stopUpdatingLocation@
stopUpdatingLocation :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopUpdatingLocation clLocationManager =
  sendMessage clLocationManager stopUpdatingLocationSelector

-- | @- requestLocation@
requestLocation :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
requestLocation clLocationManager =
  sendMessage clLocationManager requestLocationSelector

-- | @- startUpdatingHeading@
startUpdatingHeading :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startUpdatingHeading clLocationManager =
  sendMessage clLocationManager startUpdatingHeadingSelector

-- | @- stopUpdatingHeading@
stopUpdatingHeading :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopUpdatingHeading clLocationManager =
  sendMessage clLocationManager stopUpdatingHeadingSelector

-- | @- dismissHeadingCalibrationDisplay@
dismissHeadingCalibrationDisplay :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
dismissHeadingCalibrationDisplay clLocationManager =
  sendMessage clLocationManager dismissHeadingCalibrationDisplaySelector

-- | @- startMonitoringSignificantLocationChanges@
startMonitoringSignificantLocationChanges :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startMonitoringSignificantLocationChanges clLocationManager =
  sendMessage clLocationManager startMonitoringSignificantLocationChangesSelector

-- | @- stopMonitoringSignificantLocationChanges@
stopMonitoringSignificantLocationChanges :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopMonitoringSignificantLocationChanges clLocationManager =
  sendMessage clLocationManager stopMonitoringSignificantLocationChangesSelector

-- | @- startMonitoringLocationPushesWithCompletion:@
startMonitoringLocationPushesWithCompletion :: IsCLLocationManager clLocationManager => clLocationManager -> Ptr () -> IO ()
startMonitoringLocationPushesWithCompletion clLocationManager completion =
  sendMessage clLocationManager startMonitoringLocationPushesWithCompletionSelector completion

-- | @- stopMonitoringLocationPushes@
stopMonitoringLocationPushes :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopMonitoringLocationPushes clLocationManager =
  sendMessage clLocationManager stopMonitoringLocationPushesSelector

-- | @- startMonitoringForRegion:desiredAccuracy:@
startMonitoringForRegion_desiredAccuracy :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> CDouble -> IO ()
startMonitoringForRegion_desiredAccuracy clLocationManager region accuracy =
  sendMessage clLocationManager startMonitoringForRegion_desiredAccuracySelector (toCLRegion region) accuracy

-- | @- stopMonitoringForRegion:@
stopMonitoringForRegion :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> IO ()
stopMonitoringForRegion clLocationManager region =
  sendMessage clLocationManager stopMonitoringForRegionSelector (toCLRegion region)

-- | @- startMonitoringForRegion:@
startMonitoringForRegion :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> IO ()
startMonitoringForRegion clLocationManager region =
  sendMessage clLocationManager startMonitoringForRegionSelector (toCLRegion region)

-- | @- requestStateForRegion:@
requestStateForRegion :: (IsCLLocationManager clLocationManager, IsCLRegion region) => clLocationManager -> region -> IO ()
requestStateForRegion clLocationManager region =
  sendMessage clLocationManager requestStateForRegionSelector (toCLRegion region)

-- | @- startRangingBeaconsInRegion:@
startRangingBeaconsInRegion :: (IsCLLocationManager clLocationManager, IsCLBeaconRegion region) => clLocationManager -> region -> IO ()
startRangingBeaconsInRegion clLocationManager region =
  sendMessage clLocationManager startRangingBeaconsInRegionSelector (toCLBeaconRegion region)

-- | @- stopRangingBeaconsInRegion:@
stopRangingBeaconsInRegion :: (IsCLLocationManager clLocationManager, IsCLBeaconRegion region) => clLocationManager -> region -> IO ()
stopRangingBeaconsInRegion clLocationManager region =
  sendMessage clLocationManager stopRangingBeaconsInRegionSelector (toCLBeaconRegion region)

-- | @- startRangingBeaconsSatisfyingConstraint:@
startRangingBeaconsSatisfyingConstraint :: (IsCLLocationManager clLocationManager, IsCLBeaconIdentityConstraint constraint) => clLocationManager -> constraint -> IO ()
startRangingBeaconsSatisfyingConstraint clLocationManager constraint =
  sendMessage clLocationManager startRangingBeaconsSatisfyingConstraintSelector (toCLBeaconIdentityConstraint constraint)

-- | @- stopRangingBeaconsSatisfyingConstraint:@
stopRangingBeaconsSatisfyingConstraint :: (IsCLLocationManager clLocationManager, IsCLBeaconIdentityConstraint constraint) => clLocationManager -> constraint -> IO ()
stopRangingBeaconsSatisfyingConstraint clLocationManager constraint =
  sendMessage clLocationManager stopRangingBeaconsSatisfyingConstraintSelector (toCLBeaconIdentityConstraint constraint)

-- | @- allowDeferredLocationUpdatesUntilTraveled:timeout:@
allowDeferredLocationUpdatesUntilTraveled_timeout :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> CDouble -> IO ()
allowDeferredLocationUpdatesUntilTraveled_timeout clLocationManager distance timeout =
  sendMessage clLocationManager allowDeferredLocationUpdatesUntilTraveled_timeoutSelector distance timeout

-- | @- disallowDeferredLocationUpdates@
disallowDeferredLocationUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
disallowDeferredLocationUpdates clLocationManager =
  sendMessage clLocationManager disallowDeferredLocationUpdatesSelector

-- | @+ deferredLocationUpdatesAvailable@
deferredLocationUpdatesAvailable :: IO Bool
deferredLocationUpdatesAvailable  =
  do
    cls' <- getRequiredClass "CLLocationManager"
    sendClassMessage cls' deferredLocationUpdatesAvailableSelector

-- | @- startMonitoringVisits@
startMonitoringVisits :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
startMonitoringVisits clLocationManager =
  sendMessage clLocationManager startMonitoringVisitsSelector

-- | @- stopMonitoringVisits@
stopMonitoringVisits :: IsCLLocationManager clLocationManager => clLocationManager -> IO ()
stopMonitoringVisits clLocationManager =
  sendMessage clLocationManager stopMonitoringVisitsSelector

-- | @- authorizationStatus@
authorizationStatus :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLAuthorizationStatus
authorizationStatus clLocationManager =
  sendMessage clLocationManager authorizationStatusSelector

-- | @- accuracyAuthorization@
accuracyAuthorization :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLAccuracyAuthorization
accuracyAuthorization clLocationManager =
  sendMessage clLocationManager accuracyAuthorizationSelector

-- | @- authorizedForWidgetUpdates@
authorizedForWidgetUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
authorizedForWidgetUpdates clLocationManager =
  sendMessage clLocationManager authorizedForWidgetUpdatesSelector

-- | @- delegate@
delegate :: IsCLLocationManager clLocationManager => clLocationManager -> IO RawId
delegate clLocationManager =
  sendMessage clLocationManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCLLocationManager clLocationManager => clLocationManager -> RawId -> IO ()
setDelegate clLocationManager value =
  sendMessage clLocationManager setDelegateSelector value

-- | @- locationServicesEnabled@
locationServicesEnabled :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
locationServicesEnabled clLocationManager =
  sendMessage clLocationManager locationServicesEnabledSelector

-- | @- purpose@
purpose :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSString)
purpose clLocationManager =
  sendMessage clLocationManager purposeSelector

-- | @- setPurpose:@
setPurpose :: (IsCLLocationManager clLocationManager, IsNSString value) => clLocationManager -> value -> IO ()
setPurpose clLocationManager value =
  sendMessage clLocationManager setPurposeSelector (toNSString value)

-- | @- activityType@
activityType :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLActivityType
activityType clLocationManager =
  sendMessage clLocationManager activityTypeSelector

-- | @- setActivityType:@
setActivityType :: IsCLLocationManager clLocationManager => clLocationManager -> CLActivityType -> IO ()
setActivityType clLocationManager value =
  sendMessage clLocationManager setActivityTypeSelector value

-- | @- distanceFilter@
distanceFilter :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
distanceFilter clLocationManager =
  sendMessage clLocationManager distanceFilterSelector

-- | @- setDistanceFilter:@
setDistanceFilter :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> IO ()
setDistanceFilter clLocationManager value =
  sendMessage clLocationManager setDistanceFilterSelector value

-- | @- desiredAccuracy@
desiredAccuracy :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
desiredAccuracy clLocationManager =
  sendMessage clLocationManager desiredAccuracySelector

-- | @- setDesiredAccuracy:@
setDesiredAccuracy :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> IO ()
setDesiredAccuracy clLocationManager value =
  sendMessage clLocationManager setDesiredAccuracySelector value

-- | @- pausesLocationUpdatesAutomatically@
pausesLocationUpdatesAutomatically :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
pausesLocationUpdatesAutomatically clLocationManager =
  sendMessage clLocationManager pausesLocationUpdatesAutomaticallySelector

-- | @- setPausesLocationUpdatesAutomatically:@
setPausesLocationUpdatesAutomatically :: IsCLLocationManager clLocationManager => clLocationManager -> Bool -> IO ()
setPausesLocationUpdatesAutomatically clLocationManager value =
  sendMessage clLocationManager setPausesLocationUpdatesAutomaticallySelector value

-- | @- allowsBackgroundLocationUpdates@
allowsBackgroundLocationUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
allowsBackgroundLocationUpdates clLocationManager =
  sendMessage clLocationManager allowsBackgroundLocationUpdatesSelector

-- | @- setAllowsBackgroundLocationUpdates:@
setAllowsBackgroundLocationUpdates :: IsCLLocationManager clLocationManager => clLocationManager -> Bool -> IO ()
setAllowsBackgroundLocationUpdates clLocationManager value =
  sendMessage clLocationManager setAllowsBackgroundLocationUpdatesSelector value

-- | @- showsBackgroundLocationIndicator@
showsBackgroundLocationIndicator :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
showsBackgroundLocationIndicator clLocationManager =
  sendMessage clLocationManager showsBackgroundLocationIndicatorSelector

-- | @- setShowsBackgroundLocationIndicator:@
setShowsBackgroundLocationIndicator :: IsCLLocationManager clLocationManager => clLocationManager -> Bool -> IO ()
setShowsBackgroundLocationIndicator clLocationManager value =
  sendMessage clLocationManager setShowsBackgroundLocationIndicatorSelector value

-- | @- headingAvailable@
headingAvailable :: IsCLLocationManager clLocationManager => clLocationManager -> IO Bool
headingAvailable clLocationManager =
  sendMessage clLocationManager headingAvailableSelector

-- | @- headingFilter@
headingFilter :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
headingFilter clLocationManager =
  sendMessage clLocationManager headingFilterSelector

-- | @- setHeadingFilter:@
setHeadingFilter :: IsCLLocationManager clLocationManager => clLocationManager -> CDouble -> IO ()
setHeadingFilter clLocationManager value =
  sendMessage clLocationManager setHeadingFilterSelector value

-- | @- headingOrientation@
headingOrientation :: IsCLLocationManager clLocationManager => clLocationManager -> IO CLDeviceOrientation
headingOrientation clLocationManager =
  sendMessage clLocationManager headingOrientationSelector

-- | @- setHeadingOrientation:@
setHeadingOrientation :: IsCLLocationManager clLocationManager => clLocationManager -> CLDeviceOrientation -> IO ()
setHeadingOrientation clLocationManager value =
  sendMessage clLocationManager setHeadingOrientationSelector value

-- | @- heading@
heading :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id CLHeading)
heading clLocationManager =
  sendMessage clLocationManager headingSelector

-- | @- maximumRegionMonitoringDistance@
maximumRegionMonitoringDistance :: IsCLLocationManager clLocationManager => clLocationManager -> IO CDouble
maximumRegionMonitoringDistance clLocationManager =
  sendMessage clLocationManager maximumRegionMonitoringDistanceSelector

-- | @- monitoredRegions@
monitoredRegions :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSSet)
monitoredRegions clLocationManager =
  sendMessage clLocationManager monitoredRegionsSelector

-- | @- rangedRegions@
rangedRegions :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSSet)
rangedRegions clLocationManager =
  sendMessage clLocationManager rangedRegionsSelector

-- | @- rangedBeaconConstraints@
rangedBeaconConstraints :: IsCLLocationManager clLocationManager => clLocationManager -> IO (Id NSSet)
rangedBeaconConstraints clLocationManager =
  sendMessage clLocationManager rangedBeaconConstraintsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationServicesEnabled@
clLocationManagerLocationServicesEnabledSelector :: Selector '[] Bool
clLocationManagerLocationServicesEnabledSelector = mkSelector "locationServicesEnabled"

-- | @Selector@ for @headingAvailable@
clLocationManagerHeadingAvailableSelector :: Selector '[] Bool
clLocationManagerHeadingAvailableSelector = mkSelector "headingAvailable"

-- | @Selector@ for @significantLocationChangeMonitoringAvailable@
significantLocationChangeMonitoringAvailableSelector :: Selector '[] Bool
significantLocationChangeMonitoringAvailableSelector = mkSelector "significantLocationChangeMonitoringAvailable"

-- | @Selector@ for @isMonitoringAvailableForClass:@
isMonitoringAvailableForClassSelector :: Selector '[Class] Bool
isMonitoringAvailableForClassSelector = mkSelector "isMonitoringAvailableForClass:"

-- | @Selector@ for @regionMonitoringAvailable@
regionMonitoringAvailableSelector :: Selector '[] Bool
regionMonitoringAvailableSelector = mkSelector "regionMonitoringAvailable"

-- | @Selector@ for @regionMonitoringEnabled@
regionMonitoringEnabledSelector :: Selector '[] Bool
regionMonitoringEnabledSelector = mkSelector "regionMonitoringEnabled"

-- | @Selector@ for @isRangingAvailable@
isRangingAvailableSelector :: Selector '[] Bool
isRangingAvailableSelector = mkSelector "isRangingAvailable"

-- | @Selector@ for @authorizationStatus@
clLocationManagerAuthorizationStatusSelector :: Selector '[] CLAuthorizationStatus
clLocationManagerAuthorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestWhenInUseAuthorization@
requestWhenInUseAuthorizationSelector :: Selector '[] ()
requestWhenInUseAuthorizationSelector = mkSelector "requestWhenInUseAuthorization"

-- | @Selector@ for @requestAlwaysAuthorization@
requestAlwaysAuthorizationSelector :: Selector '[] ()
requestAlwaysAuthorizationSelector = mkSelector "requestAlwaysAuthorization"

-- | @Selector@ for @requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector :: Selector '[Id NSString, Ptr ()] ()
requestTemporaryFullAccuracyAuthorizationWithPurposeKey_completionSelector = mkSelector "requestTemporaryFullAccuracyAuthorizationWithPurposeKey:completion:"

-- | @Selector@ for @requestTemporaryFullAccuracyAuthorizationWithPurposeKey:@
requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector :: Selector '[Id NSString] ()
requestTemporaryFullAccuracyAuthorizationWithPurposeKeySelector = mkSelector "requestTemporaryFullAccuracyAuthorizationWithPurposeKey:"

-- | @Selector@ for @startUpdatingLocation@
startUpdatingLocationSelector :: Selector '[] ()
startUpdatingLocationSelector = mkSelector "startUpdatingLocation"

-- | @Selector@ for @stopUpdatingLocation@
stopUpdatingLocationSelector :: Selector '[] ()
stopUpdatingLocationSelector = mkSelector "stopUpdatingLocation"

-- | @Selector@ for @requestLocation@
requestLocationSelector :: Selector '[] ()
requestLocationSelector = mkSelector "requestLocation"

-- | @Selector@ for @startUpdatingHeading@
startUpdatingHeadingSelector :: Selector '[] ()
startUpdatingHeadingSelector = mkSelector "startUpdatingHeading"

-- | @Selector@ for @stopUpdatingHeading@
stopUpdatingHeadingSelector :: Selector '[] ()
stopUpdatingHeadingSelector = mkSelector "stopUpdatingHeading"

-- | @Selector@ for @dismissHeadingCalibrationDisplay@
dismissHeadingCalibrationDisplaySelector :: Selector '[] ()
dismissHeadingCalibrationDisplaySelector = mkSelector "dismissHeadingCalibrationDisplay"

-- | @Selector@ for @startMonitoringSignificantLocationChanges@
startMonitoringSignificantLocationChangesSelector :: Selector '[] ()
startMonitoringSignificantLocationChangesSelector = mkSelector "startMonitoringSignificantLocationChanges"

-- | @Selector@ for @stopMonitoringSignificantLocationChanges@
stopMonitoringSignificantLocationChangesSelector :: Selector '[] ()
stopMonitoringSignificantLocationChangesSelector = mkSelector "stopMonitoringSignificantLocationChanges"

-- | @Selector@ for @startMonitoringLocationPushesWithCompletion:@
startMonitoringLocationPushesWithCompletionSelector :: Selector '[Ptr ()] ()
startMonitoringLocationPushesWithCompletionSelector = mkSelector "startMonitoringLocationPushesWithCompletion:"

-- | @Selector@ for @stopMonitoringLocationPushes@
stopMonitoringLocationPushesSelector :: Selector '[] ()
stopMonitoringLocationPushesSelector = mkSelector "stopMonitoringLocationPushes"

-- | @Selector@ for @startMonitoringForRegion:desiredAccuracy:@
startMonitoringForRegion_desiredAccuracySelector :: Selector '[Id CLRegion, CDouble] ()
startMonitoringForRegion_desiredAccuracySelector = mkSelector "startMonitoringForRegion:desiredAccuracy:"

-- | @Selector@ for @stopMonitoringForRegion:@
stopMonitoringForRegionSelector :: Selector '[Id CLRegion] ()
stopMonitoringForRegionSelector = mkSelector "stopMonitoringForRegion:"

-- | @Selector@ for @startMonitoringForRegion:@
startMonitoringForRegionSelector :: Selector '[Id CLRegion] ()
startMonitoringForRegionSelector = mkSelector "startMonitoringForRegion:"

-- | @Selector@ for @requestStateForRegion:@
requestStateForRegionSelector :: Selector '[Id CLRegion] ()
requestStateForRegionSelector = mkSelector "requestStateForRegion:"

-- | @Selector@ for @startRangingBeaconsInRegion:@
startRangingBeaconsInRegionSelector :: Selector '[Id CLBeaconRegion] ()
startRangingBeaconsInRegionSelector = mkSelector "startRangingBeaconsInRegion:"

-- | @Selector@ for @stopRangingBeaconsInRegion:@
stopRangingBeaconsInRegionSelector :: Selector '[Id CLBeaconRegion] ()
stopRangingBeaconsInRegionSelector = mkSelector "stopRangingBeaconsInRegion:"

-- | @Selector@ for @startRangingBeaconsSatisfyingConstraint:@
startRangingBeaconsSatisfyingConstraintSelector :: Selector '[Id CLBeaconIdentityConstraint] ()
startRangingBeaconsSatisfyingConstraintSelector = mkSelector "startRangingBeaconsSatisfyingConstraint:"

-- | @Selector@ for @stopRangingBeaconsSatisfyingConstraint:@
stopRangingBeaconsSatisfyingConstraintSelector :: Selector '[Id CLBeaconIdentityConstraint] ()
stopRangingBeaconsSatisfyingConstraintSelector = mkSelector "stopRangingBeaconsSatisfyingConstraint:"

-- | @Selector@ for @allowDeferredLocationUpdatesUntilTraveled:timeout:@
allowDeferredLocationUpdatesUntilTraveled_timeoutSelector :: Selector '[CDouble, CDouble] ()
allowDeferredLocationUpdatesUntilTraveled_timeoutSelector = mkSelector "allowDeferredLocationUpdatesUntilTraveled:timeout:"

-- | @Selector@ for @disallowDeferredLocationUpdates@
disallowDeferredLocationUpdatesSelector :: Selector '[] ()
disallowDeferredLocationUpdatesSelector = mkSelector "disallowDeferredLocationUpdates"

-- | @Selector@ for @deferredLocationUpdatesAvailable@
deferredLocationUpdatesAvailableSelector :: Selector '[] Bool
deferredLocationUpdatesAvailableSelector = mkSelector "deferredLocationUpdatesAvailable"

-- | @Selector@ for @startMonitoringVisits@
startMonitoringVisitsSelector :: Selector '[] ()
startMonitoringVisitsSelector = mkSelector "startMonitoringVisits"

-- | @Selector@ for @stopMonitoringVisits@
stopMonitoringVisitsSelector :: Selector '[] ()
stopMonitoringVisitsSelector = mkSelector "stopMonitoringVisits"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CLAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @accuracyAuthorization@
accuracyAuthorizationSelector :: Selector '[] CLAccuracyAuthorization
accuracyAuthorizationSelector = mkSelector "accuracyAuthorization"

-- | @Selector@ for @authorizedForWidgetUpdates@
authorizedForWidgetUpdatesSelector :: Selector '[] Bool
authorizedForWidgetUpdatesSelector = mkSelector "authorizedForWidgetUpdates"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @locationServicesEnabled@
locationServicesEnabledSelector :: Selector '[] Bool
locationServicesEnabledSelector = mkSelector "locationServicesEnabled"

-- | @Selector@ for @purpose@
purposeSelector :: Selector '[] (Id NSString)
purposeSelector = mkSelector "purpose"

-- | @Selector@ for @setPurpose:@
setPurposeSelector :: Selector '[Id NSString] ()
setPurposeSelector = mkSelector "setPurpose:"

-- | @Selector@ for @activityType@
activityTypeSelector :: Selector '[] CLActivityType
activityTypeSelector = mkSelector "activityType"

-- | @Selector@ for @setActivityType:@
setActivityTypeSelector :: Selector '[CLActivityType] ()
setActivityTypeSelector = mkSelector "setActivityType:"

-- | @Selector@ for @distanceFilter@
distanceFilterSelector :: Selector '[] CDouble
distanceFilterSelector = mkSelector "distanceFilter"

-- | @Selector@ for @setDistanceFilter:@
setDistanceFilterSelector :: Selector '[CDouble] ()
setDistanceFilterSelector = mkSelector "setDistanceFilter:"

-- | @Selector@ for @desiredAccuracy@
desiredAccuracySelector :: Selector '[] CDouble
desiredAccuracySelector = mkSelector "desiredAccuracy"

-- | @Selector@ for @setDesiredAccuracy:@
setDesiredAccuracySelector :: Selector '[CDouble] ()
setDesiredAccuracySelector = mkSelector "setDesiredAccuracy:"

-- | @Selector@ for @pausesLocationUpdatesAutomatically@
pausesLocationUpdatesAutomaticallySelector :: Selector '[] Bool
pausesLocationUpdatesAutomaticallySelector = mkSelector "pausesLocationUpdatesAutomatically"

-- | @Selector@ for @setPausesLocationUpdatesAutomatically:@
setPausesLocationUpdatesAutomaticallySelector :: Selector '[Bool] ()
setPausesLocationUpdatesAutomaticallySelector = mkSelector "setPausesLocationUpdatesAutomatically:"

-- | @Selector@ for @allowsBackgroundLocationUpdates@
allowsBackgroundLocationUpdatesSelector :: Selector '[] Bool
allowsBackgroundLocationUpdatesSelector = mkSelector "allowsBackgroundLocationUpdates"

-- | @Selector@ for @setAllowsBackgroundLocationUpdates:@
setAllowsBackgroundLocationUpdatesSelector :: Selector '[Bool] ()
setAllowsBackgroundLocationUpdatesSelector = mkSelector "setAllowsBackgroundLocationUpdates:"

-- | @Selector@ for @showsBackgroundLocationIndicator@
showsBackgroundLocationIndicatorSelector :: Selector '[] Bool
showsBackgroundLocationIndicatorSelector = mkSelector "showsBackgroundLocationIndicator"

-- | @Selector@ for @setShowsBackgroundLocationIndicator:@
setShowsBackgroundLocationIndicatorSelector :: Selector '[Bool] ()
setShowsBackgroundLocationIndicatorSelector = mkSelector "setShowsBackgroundLocationIndicator:"

-- | @Selector@ for @headingAvailable@
headingAvailableSelector :: Selector '[] Bool
headingAvailableSelector = mkSelector "headingAvailable"

-- | @Selector@ for @headingFilter@
headingFilterSelector :: Selector '[] CDouble
headingFilterSelector = mkSelector "headingFilter"

-- | @Selector@ for @setHeadingFilter:@
setHeadingFilterSelector :: Selector '[CDouble] ()
setHeadingFilterSelector = mkSelector "setHeadingFilter:"

-- | @Selector@ for @headingOrientation@
headingOrientationSelector :: Selector '[] CLDeviceOrientation
headingOrientationSelector = mkSelector "headingOrientation"

-- | @Selector@ for @setHeadingOrientation:@
setHeadingOrientationSelector :: Selector '[CLDeviceOrientation] ()
setHeadingOrientationSelector = mkSelector "setHeadingOrientation:"

-- | @Selector@ for @heading@
headingSelector :: Selector '[] (Id CLHeading)
headingSelector = mkSelector "heading"

-- | @Selector@ for @maximumRegionMonitoringDistance@
maximumRegionMonitoringDistanceSelector :: Selector '[] CDouble
maximumRegionMonitoringDistanceSelector = mkSelector "maximumRegionMonitoringDistance"

-- | @Selector@ for @monitoredRegions@
monitoredRegionsSelector :: Selector '[] (Id NSSet)
monitoredRegionsSelector = mkSelector "monitoredRegions"

-- | @Selector@ for @rangedRegions@
rangedRegionsSelector :: Selector '[] (Id NSSet)
rangedRegionsSelector = mkSelector "rangedRegions"

-- | @Selector@ for @rangedBeaconConstraints@
rangedBeaconConstraintsSelector :: Selector '[] (Id NSSet)
rangedBeaconConstraintsSelector = mkSelector "rangedBeaconConstraints"

