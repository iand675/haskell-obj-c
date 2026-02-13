{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , additionalActionActivitiesSelector
  , completionStatusSelector
  , driverSelector
  , dropOffLocationSelector
  , estimatedDropOffDateSelector
  , estimatedPickupDateSelector
  , estimatedPickupEndDateSelector
  , phaseSelector
  , pickupLocationSelector
  , rideIdentifierSelector
  , rideOptionSelector
  , scheduledPickupTimeSelector
  , setAdditionalActionActivitiesSelector
  , setCompletionStatusSelector
  , setDriverSelector
  , setDropOffLocationSelector
  , setEstimatedDropOffDateSelector
  , setEstimatedPickupDateSelector
  , setEstimatedPickupEndDateSelector
  , setPhaseSelector
  , setPickupLocationSelector
  , setRideIdentifierSelector
  , setRideOptionSelector
  , setScheduledPickupTimeSelector
  , setUserActivityForCancelingInApplicationSelector
  , setVehicleSelector
  , setWaypointsSelector
  , userActivityForCancelingInApplicationSelector
  , vehicleSelector
  , waypointsSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rideIdentifier@
rideIdentifier :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSString)
rideIdentifier inRideStatus =
  sendMessage inRideStatus rideIdentifierSelector

-- | @- setRideIdentifier:@
setRideIdentifier :: (IsINRideStatus inRideStatus, IsNSString value) => inRideStatus -> value -> IO ()
setRideIdentifier inRideStatus value =
  sendMessage inRideStatus setRideIdentifierSelector (toNSString value)

-- | @- phase@
phase :: IsINRideStatus inRideStatus => inRideStatus -> IO INRidePhase
phase inRideStatus =
  sendMessage inRideStatus phaseSelector

-- | @- setPhase:@
setPhase :: IsINRideStatus inRideStatus => inRideStatus -> INRidePhase -> IO ()
setPhase inRideStatus value =
  sendMessage inRideStatus setPhaseSelector value

-- | @- completionStatus@
completionStatus :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideCompletionStatus)
completionStatus inRideStatus =
  sendMessage inRideStatus completionStatusSelector

-- | @- setCompletionStatus:@
setCompletionStatus :: (IsINRideStatus inRideStatus, IsINRideCompletionStatus value) => inRideStatus -> value -> IO ()
setCompletionStatus inRideStatus value =
  sendMessage inRideStatus setCompletionStatusSelector (toINRideCompletionStatus value)

-- | @- vehicle@
vehicle :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideVehicle)
vehicle inRideStatus =
  sendMessage inRideStatus vehicleSelector

-- | @- setVehicle:@
setVehicle :: (IsINRideStatus inRideStatus, IsINRideVehicle value) => inRideStatus -> value -> IO ()
setVehicle inRideStatus value =
  sendMessage inRideStatus setVehicleSelector (toINRideVehicle value)

-- | @- driver@
driver :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideDriver)
driver inRideStatus =
  sendMessage inRideStatus driverSelector

-- | @- setDriver:@
setDriver :: (IsINRideStatus inRideStatus, IsINRideDriver value) => inRideStatus -> value -> IO ()
setDriver inRideStatus value =
  sendMessage inRideStatus setDriverSelector (toINRideDriver value)

-- | @- estimatedPickupDate@
estimatedPickupDate :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSDate)
estimatedPickupDate inRideStatus =
  sendMessage inRideStatus estimatedPickupDateSelector

-- | @- setEstimatedPickupDate:@
setEstimatedPickupDate :: (IsINRideStatus inRideStatus, IsNSDate value) => inRideStatus -> value -> IO ()
setEstimatedPickupDate inRideStatus value =
  sendMessage inRideStatus setEstimatedPickupDateSelector (toNSDate value)

-- | @- estimatedDropOffDate@
estimatedDropOffDate :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSDate)
estimatedDropOffDate inRideStatus =
  sendMessage inRideStatus estimatedDropOffDateSelector

-- | @- setEstimatedDropOffDate:@
setEstimatedDropOffDate :: (IsINRideStatus inRideStatus, IsNSDate value) => inRideStatus -> value -> IO ()
setEstimatedDropOffDate inRideStatus value =
  sendMessage inRideStatus setEstimatedDropOffDateSelector (toNSDate value)

-- | @- estimatedPickupEndDate@
estimatedPickupEndDate :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSDate)
estimatedPickupEndDate inRideStatus =
  sendMessage inRideStatus estimatedPickupEndDateSelector

-- | @- setEstimatedPickupEndDate:@
setEstimatedPickupEndDate :: (IsINRideStatus inRideStatus, IsNSDate value) => inRideStatus -> value -> IO ()
setEstimatedPickupEndDate inRideStatus value =
  sendMessage inRideStatus setEstimatedPickupEndDateSelector (toNSDate value)

-- | @- scheduledPickupTime@
scheduledPickupTime :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INDateComponentsRange)
scheduledPickupTime inRideStatus =
  sendMessage inRideStatus scheduledPickupTimeSelector

-- | @- setScheduledPickupTime:@
setScheduledPickupTime :: (IsINRideStatus inRideStatus, IsINDateComponentsRange value) => inRideStatus -> value -> IO ()
setScheduledPickupTime inRideStatus value =
  sendMessage inRideStatus setScheduledPickupTimeSelector (toINDateComponentsRange value)

-- | @- pickupLocation@
pickupLocation :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id CLPlacemark)
pickupLocation inRideStatus =
  sendMessage inRideStatus pickupLocationSelector

-- | @- setPickupLocation:@
setPickupLocation :: (IsINRideStatus inRideStatus, IsCLPlacemark value) => inRideStatus -> value -> IO ()
setPickupLocation inRideStatus value =
  sendMessage inRideStatus setPickupLocationSelector (toCLPlacemark value)

-- | @- waypoints@
waypoints :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSArray)
waypoints inRideStatus =
  sendMessage inRideStatus waypointsSelector

-- | @- setWaypoints:@
setWaypoints :: (IsINRideStatus inRideStatus, IsNSArray value) => inRideStatus -> value -> IO ()
setWaypoints inRideStatus value =
  sendMessage inRideStatus setWaypointsSelector (toNSArray value)

-- | @- dropOffLocation@
dropOffLocation :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id CLPlacemark)
dropOffLocation inRideStatus =
  sendMessage inRideStatus dropOffLocationSelector

-- | @- setDropOffLocation:@
setDropOffLocation :: (IsINRideStatus inRideStatus, IsCLPlacemark value) => inRideStatus -> value -> IO ()
setDropOffLocation inRideStatus value =
  sendMessage inRideStatus setDropOffLocationSelector (toCLPlacemark value)

-- | @- rideOption@
rideOption :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id INRideOption)
rideOption inRideStatus =
  sendMessage inRideStatus rideOptionSelector

-- | @- setRideOption:@
setRideOption :: (IsINRideStatus inRideStatus, IsINRideOption value) => inRideStatus -> value -> IO ()
setRideOption inRideStatus value =
  sendMessage inRideStatus setRideOptionSelector (toINRideOption value)

-- | @- userActivityForCancelingInApplication@
userActivityForCancelingInApplication :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSUserActivity)
userActivityForCancelingInApplication inRideStatus =
  sendMessage inRideStatus userActivityForCancelingInApplicationSelector

-- | @- setUserActivityForCancelingInApplication:@
setUserActivityForCancelingInApplication :: (IsINRideStatus inRideStatus, IsNSUserActivity value) => inRideStatus -> value -> IO ()
setUserActivityForCancelingInApplication inRideStatus value =
  sendMessage inRideStatus setUserActivityForCancelingInApplicationSelector (toNSUserActivity value)

-- | @- additionalActionActivities@
additionalActionActivities :: IsINRideStatus inRideStatus => inRideStatus -> IO (Id NSArray)
additionalActionActivities inRideStatus =
  sendMessage inRideStatus additionalActionActivitiesSelector

-- | @- setAdditionalActionActivities:@
setAdditionalActionActivities :: (IsINRideStatus inRideStatus, IsNSArray value) => inRideStatus -> value -> IO ()
setAdditionalActionActivities inRideStatus value =
  sendMessage inRideStatus setAdditionalActionActivitiesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rideIdentifier@
rideIdentifierSelector :: Selector '[] (Id NSString)
rideIdentifierSelector = mkSelector "rideIdentifier"

-- | @Selector@ for @setRideIdentifier:@
setRideIdentifierSelector :: Selector '[Id NSString] ()
setRideIdentifierSelector = mkSelector "setRideIdentifier:"

-- | @Selector@ for @phase@
phaseSelector :: Selector '[] INRidePhase
phaseSelector = mkSelector "phase"

-- | @Selector@ for @setPhase:@
setPhaseSelector :: Selector '[INRidePhase] ()
setPhaseSelector = mkSelector "setPhase:"

-- | @Selector@ for @completionStatus@
completionStatusSelector :: Selector '[] (Id INRideCompletionStatus)
completionStatusSelector = mkSelector "completionStatus"

-- | @Selector@ for @setCompletionStatus:@
setCompletionStatusSelector :: Selector '[Id INRideCompletionStatus] ()
setCompletionStatusSelector = mkSelector "setCompletionStatus:"

-- | @Selector@ for @vehicle@
vehicleSelector :: Selector '[] (Id INRideVehicle)
vehicleSelector = mkSelector "vehicle"

-- | @Selector@ for @setVehicle:@
setVehicleSelector :: Selector '[Id INRideVehicle] ()
setVehicleSelector = mkSelector "setVehicle:"

-- | @Selector@ for @driver@
driverSelector :: Selector '[] (Id INRideDriver)
driverSelector = mkSelector "driver"

-- | @Selector@ for @setDriver:@
setDriverSelector :: Selector '[Id INRideDriver] ()
setDriverSelector = mkSelector "setDriver:"

-- | @Selector@ for @estimatedPickupDate@
estimatedPickupDateSelector :: Selector '[] (Id NSDate)
estimatedPickupDateSelector = mkSelector "estimatedPickupDate"

-- | @Selector@ for @setEstimatedPickupDate:@
setEstimatedPickupDateSelector :: Selector '[Id NSDate] ()
setEstimatedPickupDateSelector = mkSelector "setEstimatedPickupDate:"

-- | @Selector@ for @estimatedDropOffDate@
estimatedDropOffDateSelector :: Selector '[] (Id NSDate)
estimatedDropOffDateSelector = mkSelector "estimatedDropOffDate"

-- | @Selector@ for @setEstimatedDropOffDate:@
setEstimatedDropOffDateSelector :: Selector '[Id NSDate] ()
setEstimatedDropOffDateSelector = mkSelector "setEstimatedDropOffDate:"

-- | @Selector@ for @estimatedPickupEndDate@
estimatedPickupEndDateSelector :: Selector '[] (Id NSDate)
estimatedPickupEndDateSelector = mkSelector "estimatedPickupEndDate"

-- | @Selector@ for @setEstimatedPickupEndDate:@
setEstimatedPickupEndDateSelector :: Selector '[Id NSDate] ()
setEstimatedPickupEndDateSelector = mkSelector "setEstimatedPickupEndDate:"

-- | @Selector@ for @scheduledPickupTime@
scheduledPickupTimeSelector :: Selector '[] (Id INDateComponentsRange)
scheduledPickupTimeSelector = mkSelector "scheduledPickupTime"

-- | @Selector@ for @setScheduledPickupTime:@
setScheduledPickupTimeSelector :: Selector '[Id INDateComponentsRange] ()
setScheduledPickupTimeSelector = mkSelector "setScheduledPickupTime:"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector '[] (Id CLPlacemark)
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @setPickupLocation:@
setPickupLocationSelector :: Selector '[Id CLPlacemark] ()
setPickupLocationSelector = mkSelector "setPickupLocation:"

-- | @Selector@ for @waypoints@
waypointsSelector :: Selector '[] (Id NSArray)
waypointsSelector = mkSelector "waypoints"

-- | @Selector@ for @setWaypoints:@
setWaypointsSelector :: Selector '[Id NSArray] ()
setWaypointsSelector = mkSelector "setWaypoints:"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector '[] (Id CLPlacemark)
dropOffLocationSelector = mkSelector "dropOffLocation"

-- | @Selector@ for @setDropOffLocation:@
setDropOffLocationSelector :: Selector '[Id CLPlacemark] ()
setDropOffLocationSelector = mkSelector "setDropOffLocation:"

-- | @Selector@ for @rideOption@
rideOptionSelector :: Selector '[] (Id INRideOption)
rideOptionSelector = mkSelector "rideOption"

-- | @Selector@ for @setRideOption:@
setRideOptionSelector :: Selector '[Id INRideOption] ()
setRideOptionSelector = mkSelector "setRideOption:"

-- | @Selector@ for @userActivityForCancelingInApplication@
userActivityForCancelingInApplicationSelector :: Selector '[] (Id NSUserActivity)
userActivityForCancelingInApplicationSelector = mkSelector "userActivityForCancelingInApplication"

-- | @Selector@ for @setUserActivityForCancelingInApplication:@
setUserActivityForCancelingInApplicationSelector :: Selector '[Id NSUserActivity] ()
setUserActivityForCancelingInApplicationSelector = mkSelector "setUserActivityForCancelingInApplication:"

-- | @Selector@ for @additionalActionActivities@
additionalActionActivitiesSelector :: Selector '[] (Id NSArray)
additionalActionActivitiesSelector = mkSelector "additionalActionActivities"

-- | @Selector@ for @setAdditionalActionActivities:@
setAdditionalActionActivitiesSelector :: Selector '[Id NSArray] ()
setAdditionalActionActivitiesSelector = mkSelector "setAdditionalActionActivities:"

