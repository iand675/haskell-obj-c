{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideStatus@.
module ObjC.Intents.INRideStatus
  ( INRideStatus
  , IsINRideStatus(..)
  , rideIdentifier
  , setRideIdentifier
  , phase
  , setPhase
  , completionStatus
  , setCompletionStatus
  , vehicle
  , setVehicle
  , driver
  , setDriver
  , estimatedPickupDate
  , setEstimatedPickupDate
  , estimatedDropOffDate
  , setEstimatedDropOffDate
  , estimatedPickupEndDate
  , setEstimatedPickupEndDate
  , scheduledPickupTime
  , setScheduledPickupTime
  , pickupLocation
  , setPickupLocation
  , waypoints
  , setWaypoints
  , dropOffLocation
  , setDropOffLocation
  , rideOption
  , setRideOption
  , userActivityForCancelingInApplication
  , setUserActivityForCancelingInApplication
  , additionalActionActivities
  , setAdditionalActionActivities
  , rideIdentifierSelector
  , setRideIdentifierSelector
  , phaseSelector
  , setPhaseSelector
  , completionStatusSelector
  , setCompletionStatusSelector
  , vehicleSelector
  , setVehicleSelector
  , driverSelector
  , setDriverSelector
  , estimatedPickupDateSelector
  , setEstimatedPickupDateSelector
  , estimatedDropOffDateSelector
  , setEstimatedDropOffDateSelector
  , estimatedPickupEndDateSelector
  , setEstimatedPickupEndDateSelector
  , scheduledPickupTimeSelector
  , setScheduledPickupTimeSelector
  , pickupLocationSelector
  , setPickupLocationSelector
  , waypointsSelector
  , setWaypointsSelector
  , dropOffLocationSelector
  , setDropOffLocationSelector
  , rideOptionSelector
  , setRideOptionSelector
  , userActivityForCancelingInApplicationSelector
  , setUserActivityForCancelingInApplicationSelector
  , additionalActionActivitiesSelector
  , setAdditionalActionActivitiesSelector

  -- * Enum types
  , INRidePhase(INRidePhase)
  , pattern INRidePhaseUnknown
  , pattern INRidePhaseReceived
  , pattern INRidePhaseConfirmed
  , pattern INRidePhaseOngoing
  , pattern INRidePhaseCompleted
  , pattern INRidePhaseApproachingPickup
  , pattern INRidePhasePickup

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rideIdentifier@
rideIdentifier :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSString)
rideIdentifier inRideStatus  =
  sendMsg inRideStatus (mkSelector "rideIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRideIdentifier:@
setRideIdentifier :: (IsINRideStatus inRideStatus, IsNSString value) => inRideStatus -> value -> IO ()
setRideIdentifier inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setRideIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phase@
phase :: IsINRideStatus inRideStatus => inRideStatus -> IO INRidePhase
phase inRideStatus  =
  fmap (coerce :: CLong -> INRidePhase) $ sendMsg inRideStatus (mkSelector "phase") retCLong []

-- | @- setPhase:@
setPhase :: IsINRideStatus inRideStatus => inRideStatus -> INRidePhase -> IO ()
setPhase inRideStatus  value =
  sendMsg inRideStatus (mkSelector "setPhase:") retVoid [argCLong (coerce value)]

-- | @- completionStatus@
completionStatus :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideCompletionStatus)
completionStatus inRideStatus  =
  sendMsg inRideStatus (mkSelector "completionStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletionStatus:@
setCompletionStatus :: (IsINRideStatus inRideStatus, IsINRideCompletionStatus value) => inRideStatus -> value -> IO ()
setCompletionStatus inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setCompletionStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vehicle@
vehicle :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideVehicle)
vehicle inRideStatus  =
  sendMsg inRideStatus (mkSelector "vehicle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVehicle:@
setVehicle :: (IsINRideStatus inRideStatus, IsINRideVehicle value) => inRideStatus -> value -> IO ()
setVehicle inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setVehicle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- driver@
driver :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideDriver)
driver inRideStatus  =
  sendMsg inRideStatus (mkSelector "driver") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDriver:@
setDriver :: (IsINRideStatus inRideStatus, IsINRideDriver value) => inRideStatus -> value -> IO ()
setDriver inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setDriver:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- estimatedPickupDate@
estimatedPickupDate :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSDate)
estimatedPickupDate inRideStatus  =
  sendMsg inRideStatus (mkSelector "estimatedPickupDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEstimatedPickupDate:@
setEstimatedPickupDate :: (IsINRideStatus inRideStatus, IsNSDate value) => inRideStatus -> value -> IO ()
setEstimatedPickupDate inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setEstimatedPickupDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- estimatedDropOffDate@
estimatedDropOffDate :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSDate)
estimatedDropOffDate inRideStatus  =
  sendMsg inRideStatus (mkSelector "estimatedDropOffDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEstimatedDropOffDate:@
setEstimatedDropOffDate :: (IsINRideStatus inRideStatus, IsNSDate value) => inRideStatus -> value -> IO ()
setEstimatedDropOffDate inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setEstimatedDropOffDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- estimatedPickupEndDate@
estimatedPickupEndDate :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSDate)
estimatedPickupEndDate inRideStatus  =
  sendMsg inRideStatus (mkSelector "estimatedPickupEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEstimatedPickupEndDate:@
setEstimatedPickupEndDate :: (IsINRideStatus inRideStatus, IsNSDate value) => inRideStatus -> value -> IO ()
setEstimatedPickupEndDate inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setEstimatedPickupEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scheduledPickupTime@
scheduledPickupTime :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INDateComponentsRange)
scheduledPickupTime inRideStatus  =
  sendMsg inRideStatus (mkSelector "scheduledPickupTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScheduledPickupTime:@
setScheduledPickupTime :: (IsINRideStatus inRideStatus, IsINDateComponentsRange value) => inRideStatus -> value -> IO ()
setScheduledPickupTime inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setScheduledPickupTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pickupLocation@
pickupLocation :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id CLPlacemark)
pickupLocation inRideStatus  =
  sendMsg inRideStatus (mkSelector "pickupLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPickupLocation:@
setPickupLocation :: (IsINRideStatus inRideStatus, IsCLPlacemark value) => inRideStatus -> value -> IO ()
setPickupLocation inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setPickupLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- waypoints@
waypoints :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSArray)
waypoints inRideStatus  =
  sendMsg inRideStatus (mkSelector "waypoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWaypoints:@
setWaypoints :: (IsINRideStatus inRideStatus, IsNSArray value) => inRideStatus -> value -> IO ()
setWaypoints inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setWaypoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dropOffLocation@
dropOffLocation :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id CLPlacemark)
dropOffLocation inRideStatus  =
  sendMsg inRideStatus (mkSelector "dropOffLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDropOffLocation:@
setDropOffLocation :: (IsINRideStatus inRideStatus, IsCLPlacemark value) => inRideStatus -> value -> IO ()
setDropOffLocation inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setDropOffLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rideOption@
rideOption :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideOption)
rideOption inRideStatus  =
  sendMsg inRideStatus (mkSelector "rideOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRideOption:@
setRideOption :: (IsINRideStatus inRideStatus, IsINRideOption value) => inRideStatus -> value -> IO ()
setRideOption inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setRideOption:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userActivityForCancelingInApplication@
userActivityForCancelingInApplication :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSUserActivity)
userActivityForCancelingInApplication inRideStatus  =
  sendMsg inRideStatus (mkSelector "userActivityForCancelingInApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserActivityForCancelingInApplication:@
setUserActivityForCancelingInApplication :: (IsINRideStatus inRideStatus, IsNSUserActivity value) => inRideStatus -> value -> IO ()
setUserActivityForCancelingInApplication inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setUserActivityForCancelingInApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- additionalActionActivities@
additionalActionActivities :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSArray)
additionalActionActivities inRideStatus  =
  sendMsg inRideStatus (mkSelector "additionalActionActivities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdditionalActionActivities:@
setAdditionalActionActivities :: (IsINRideStatus inRideStatus, IsNSArray value) => inRideStatus -> value -> IO ()
setAdditionalActionActivities inRideStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideStatus (mkSelector "setAdditionalActionActivities:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rideIdentifier@
rideIdentifierSelector :: Selector
rideIdentifierSelector = mkSelector "rideIdentifier"

-- | @Selector@ for @setRideIdentifier:@
setRideIdentifierSelector :: Selector
setRideIdentifierSelector = mkSelector "setRideIdentifier:"

-- | @Selector@ for @phase@
phaseSelector :: Selector
phaseSelector = mkSelector "phase"

-- | @Selector@ for @setPhase:@
setPhaseSelector :: Selector
setPhaseSelector = mkSelector "setPhase:"

-- | @Selector@ for @completionStatus@
completionStatusSelector :: Selector
completionStatusSelector = mkSelector "completionStatus"

-- | @Selector@ for @setCompletionStatus:@
setCompletionStatusSelector :: Selector
setCompletionStatusSelector = mkSelector "setCompletionStatus:"

-- | @Selector@ for @vehicle@
vehicleSelector :: Selector
vehicleSelector = mkSelector "vehicle"

-- | @Selector@ for @setVehicle:@
setVehicleSelector :: Selector
setVehicleSelector = mkSelector "setVehicle:"

-- | @Selector@ for @driver@
driverSelector :: Selector
driverSelector = mkSelector "driver"

-- | @Selector@ for @setDriver:@
setDriverSelector :: Selector
setDriverSelector = mkSelector "setDriver:"

-- | @Selector@ for @estimatedPickupDate@
estimatedPickupDateSelector :: Selector
estimatedPickupDateSelector = mkSelector "estimatedPickupDate"

-- | @Selector@ for @setEstimatedPickupDate:@
setEstimatedPickupDateSelector :: Selector
setEstimatedPickupDateSelector = mkSelector "setEstimatedPickupDate:"

-- | @Selector@ for @estimatedDropOffDate@
estimatedDropOffDateSelector :: Selector
estimatedDropOffDateSelector = mkSelector "estimatedDropOffDate"

-- | @Selector@ for @setEstimatedDropOffDate:@
setEstimatedDropOffDateSelector :: Selector
setEstimatedDropOffDateSelector = mkSelector "setEstimatedDropOffDate:"

-- | @Selector@ for @estimatedPickupEndDate@
estimatedPickupEndDateSelector :: Selector
estimatedPickupEndDateSelector = mkSelector "estimatedPickupEndDate"

-- | @Selector@ for @setEstimatedPickupEndDate:@
setEstimatedPickupEndDateSelector :: Selector
setEstimatedPickupEndDateSelector = mkSelector "setEstimatedPickupEndDate:"

-- | @Selector@ for @scheduledPickupTime@
scheduledPickupTimeSelector :: Selector
scheduledPickupTimeSelector = mkSelector "scheduledPickupTime"

-- | @Selector@ for @setScheduledPickupTime:@
setScheduledPickupTimeSelector :: Selector
setScheduledPickupTimeSelector = mkSelector "setScheduledPickupTime:"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @setPickupLocation:@
setPickupLocationSelector :: Selector
setPickupLocationSelector = mkSelector "setPickupLocation:"

-- | @Selector@ for @waypoints@
waypointsSelector :: Selector
waypointsSelector = mkSelector "waypoints"

-- | @Selector@ for @setWaypoints:@
setWaypointsSelector :: Selector
setWaypointsSelector = mkSelector "setWaypoints:"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector
dropOffLocationSelector = mkSelector "dropOffLocation"

-- | @Selector@ for @setDropOffLocation:@
setDropOffLocationSelector :: Selector
setDropOffLocationSelector = mkSelector "setDropOffLocation:"

-- | @Selector@ for @rideOption@
rideOptionSelector :: Selector
rideOptionSelector = mkSelector "rideOption"

-- | @Selector@ for @setRideOption:@
setRideOptionSelector :: Selector
setRideOptionSelector = mkSelector "setRideOption:"

-- | @Selector@ for @userActivityForCancelingInApplication@
userActivityForCancelingInApplicationSelector :: Selector
userActivityForCancelingInApplicationSelector = mkSelector "userActivityForCancelingInApplication"

-- | @Selector@ for @setUserActivityForCancelingInApplication:@
setUserActivityForCancelingInApplicationSelector :: Selector
setUserActivityForCancelingInApplicationSelector = mkSelector "setUserActivityForCancelingInApplication:"

-- | @Selector@ for @additionalActionActivities@
additionalActionActivitiesSelector :: Selector
additionalActionActivitiesSelector = mkSelector "additionalActionActivities"

-- | @Selector@ for @setAdditionalActionActivities:@
setAdditionalActionActivitiesSelector :: Selector
setAdditionalActionActivitiesSelector = mkSelector "setAdditionalActionActivities:"

