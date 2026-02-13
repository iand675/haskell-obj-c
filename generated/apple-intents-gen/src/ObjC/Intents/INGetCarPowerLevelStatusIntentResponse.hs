{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarPowerLevelStatusIntentResponse@.
module ObjC.Intents.INGetCarPowerLevelStatusIntentResponse
  ( INGetCarPowerLevelStatusIntentResponse
  , IsINGetCarPowerLevelStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , carIdentifier
  , setCarIdentifier
  , fuelPercentRemaining
  , setFuelPercentRemaining
  , chargePercentRemaining
  , setChargePercentRemaining
  , distanceRemaining
  , setDistanceRemaining
  , charging
  , setCharging
  , minutesToFull
  , setMinutesToFull
  , maximumDistance
  , setMaximumDistance
  , distanceRemainingElectric
  , setDistanceRemainingElectric
  , maximumDistanceElectric
  , setMaximumDistanceElectric
  , distanceRemainingFuel
  , setDistanceRemainingFuel
  , maximumDistanceFuel
  , setMaximumDistanceFuel
  , consumptionFormulaArguments
  , setConsumptionFormulaArguments
  , chargingFormulaArguments
  , setChargingFormulaArguments
  , dateOfLastStateUpdate
  , setDateOfLastStateUpdate
  , activeConnector
  , setActiveConnector
  , maximumBatteryCapacity
  , setMaximumBatteryCapacity
  , currentBatteryCapacity
  , setCurrentBatteryCapacity
  , minimumBatteryCapacity
  , setMinimumBatteryCapacity
  , activeConnectorSelector
  , carIdentifierSelector
  , chargePercentRemainingSelector
  , chargingFormulaArgumentsSelector
  , chargingSelector
  , codeSelector
  , consumptionFormulaArgumentsSelector
  , currentBatteryCapacitySelector
  , dateOfLastStateUpdateSelector
  , distanceRemainingElectricSelector
  , distanceRemainingFuelSelector
  , distanceRemainingSelector
  , fuelPercentRemainingSelector
  , initSelector
  , initWithCode_userActivitySelector
  , maximumBatteryCapacitySelector
  , maximumDistanceElectricSelector
  , maximumDistanceFuelSelector
  , maximumDistanceSelector
  , minimumBatteryCapacitySelector
  , minutesToFullSelector
  , setActiveConnectorSelector
  , setCarIdentifierSelector
  , setChargePercentRemainingSelector
  , setChargingFormulaArgumentsSelector
  , setChargingSelector
  , setConsumptionFormulaArgumentsSelector
  , setCurrentBatteryCapacitySelector
  , setDateOfLastStateUpdateSelector
  , setDistanceRemainingElectricSelector
  , setDistanceRemainingFuelSelector
  , setDistanceRemainingSelector
  , setFuelPercentRemainingSelector
  , setMaximumBatteryCapacitySelector
  , setMaximumDistanceElectricSelector
  , setMaximumDistanceFuelSelector
  , setMaximumDistanceSelector
  , setMinimumBatteryCapacitySelector
  , setMinutesToFullSelector

  -- * Enum types
  , INGetCarPowerLevelStatusIntentResponseCode(INGetCarPowerLevelStatusIntentResponseCode)
  , pattern INGetCarPowerLevelStatusIntentResponseCodeUnspecified
  , pattern INGetCarPowerLevelStatusIntentResponseCodeReady
  , pattern INGetCarPowerLevelStatusIntentResponseCodeInProgress
  , pattern INGetCarPowerLevelStatusIntentResponseCodeSuccess
  , pattern INGetCarPowerLevelStatusIntentResponseCodeFailure
  , pattern INGetCarPowerLevelStatusIntentResponseCodeFailureRequiringAppLaunch

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO RawId
init_ inGetCarPowerLevelStatusIntentResponse =
  sendOwnedMessage inGetCarPowerLevelStatusIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSUserActivity userActivity) => inGetCarPowerLevelStatusIntentResponse -> INGetCarPowerLevelStatusIntentResponseCode -> userActivity -> IO (Id INGetCarPowerLevelStatusIntentResponse)
initWithCode_userActivity inGetCarPowerLevelStatusIntentResponse code userActivity =
  sendOwnedMessage inGetCarPowerLevelStatusIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO INGetCarPowerLevelStatusIntentResponseCode
code inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse codeSelector

-- | @- carIdentifier@
carIdentifier :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSString)
carIdentifier inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse carIdentifierSelector

-- | @- setCarIdentifier:@
setCarIdentifier :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSString value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setCarIdentifier inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setCarIdentifierSelector (toNSString value)

-- | @- fuelPercentRemaining@
fuelPercentRemaining :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
fuelPercentRemaining inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse fuelPercentRemainingSelector

-- | @- setFuelPercentRemaining:@
setFuelPercentRemaining :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setFuelPercentRemaining inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setFuelPercentRemainingSelector (toNSNumber value)

-- | @- chargePercentRemaining@
chargePercentRemaining :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
chargePercentRemaining inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse chargePercentRemainingSelector

-- | @- setChargePercentRemaining:@
setChargePercentRemaining :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setChargePercentRemaining inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setChargePercentRemainingSelector (toNSNumber value)

-- | @- distanceRemaining@
distanceRemaining :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
distanceRemaining inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse distanceRemainingSelector

-- | @- setDistanceRemaining:@
setDistanceRemaining :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDistanceRemaining inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setDistanceRemainingSelector (toNSMeasurement value)

-- | @- charging@
charging :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
charging inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse chargingSelector

-- | @- setCharging:@
setCharging :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setCharging inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setChargingSelector (toNSNumber value)

-- | @- minutesToFull@
minutesToFull :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
minutesToFull inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse minutesToFullSelector

-- | @- setMinutesToFull:@
setMinutesToFull :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMinutesToFull inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setMinutesToFullSelector (toNSNumber value)

-- | @- maximumDistance@
maximumDistance :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumDistance inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse maximumDistanceSelector

-- | @- setMaximumDistance:@
setMaximumDistance :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumDistance inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setMaximumDistanceSelector (toNSMeasurement value)

-- | @- distanceRemainingElectric@
distanceRemainingElectric :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
distanceRemainingElectric inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse distanceRemainingElectricSelector

-- | @- setDistanceRemainingElectric:@
setDistanceRemainingElectric :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDistanceRemainingElectric inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setDistanceRemainingElectricSelector (toNSMeasurement value)

-- | @- maximumDistanceElectric@
maximumDistanceElectric :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumDistanceElectric inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse maximumDistanceElectricSelector

-- | @- setMaximumDistanceElectric:@
setMaximumDistanceElectric :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumDistanceElectric inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setMaximumDistanceElectricSelector (toNSMeasurement value)

-- | @- distanceRemainingFuel@
distanceRemainingFuel :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
distanceRemainingFuel inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse distanceRemainingFuelSelector

-- | @- setDistanceRemainingFuel:@
setDistanceRemainingFuel :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDistanceRemainingFuel inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setDistanceRemainingFuelSelector (toNSMeasurement value)

-- | @- maximumDistanceFuel@
maximumDistanceFuel :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumDistanceFuel inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse maximumDistanceFuelSelector

-- | @- setMaximumDistanceFuel:@
setMaximumDistanceFuel :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumDistanceFuel inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setMaximumDistanceFuelSelector (toNSMeasurement value)

-- | @- consumptionFormulaArguments@
consumptionFormulaArguments :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSDictionary)
consumptionFormulaArguments inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse consumptionFormulaArgumentsSelector

-- | @- setConsumptionFormulaArguments:@
setConsumptionFormulaArguments :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSDictionary value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setConsumptionFormulaArguments inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setConsumptionFormulaArgumentsSelector (toNSDictionary value)

-- | @- chargingFormulaArguments@
chargingFormulaArguments :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSDictionary)
chargingFormulaArguments inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse chargingFormulaArgumentsSelector

-- | @- setChargingFormulaArguments:@
setChargingFormulaArguments :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSDictionary value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setChargingFormulaArguments inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setChargingFormulaArgumentsSelector (toNSDictionary value)

-- | @- dateOfLastStateUpdate@
dateOfLastStateUpdate :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSDateComponents)
dateOfLastStateUpdate inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse dateOfLastStateUpdateSelector

-- | @- setDateOfLastStateUpdate:@
setDateOfLastStateUpdate :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSDateComponents value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDateOfLastStateUpdate inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setDateOfLastStateUpdateSelector (toNSDateComponents value)

-- | @- activeConnector@
activeConnector :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSString)
activeConnector inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse activeConnectorSelector

-- | @- setActiveConnector:@
setActiveConnector :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSString value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setActiveConnector inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setActiveConnectorSelector (toNSString value)

-- | @- maximumBatteryCapacity@
maximumBatteryCapacity :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumBatteryCapacity inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse maximumBatteryCapacitySelector

-- | @- setMaximumBatteryCapacity:@
setMaximumBatteryCapacity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumBatteryCapacity inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setMaximumBatteryCapacitySelector (toNSMeasurement value)

-- | @- currentBatteryCapacity@
currentBatteryCapacity :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
currentBatteryCapacity inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse currentBatteryCapacitySelector

-- | @- setCurrentBatteryCapacity:@
setCurrentBatteryCapacity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setCurrentBatteryCapacity inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setCurrentBatteryCapacitySelector (toNSMeasurement value)

-- | @- minimumBatteryCapacity@
minimumBatteryCapacity :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
minimumBatteryCapacity inGetCarPowerLevelStatusIntentResponse =
  sendMessage inGetCarPowerLevelStatusIntentResponse minimumBatteryCapacitySelector

-- | @- setMinimumBatteryCapacity:@
setMinimumBatteryCapacity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMinimumBatteryCapacity inGetCarPowerLevelStatusIntentResponse value =
  sendMessage inGetCarPowerLevelStatusIntentResponse setMinimumBatteryCapacitySelector (toNSMeasurement value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INGetCarPowerLevelStatusIntentResponseCode, Id NSUserActivity] (Id INGetCarPowerLevelStatusIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetCarPowerLevelStatusIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @carIdentifier@
carIdentifierSelector :: Selector '[] (Id NSString)
carIdentifierSelector = mkSelector "carIdentifier"

-- | @Selector@ for @setCarIdentifier:@
setCarIdentifierSelector :: Selector '[Id NSString] ()
setCarIdentifierSelector = mkSelector "setCarIdentifier:"

-- | @Selector@ for @fuelPercentRemaining@
fuelPercentRemainingSelector :: Selector '[] (Id NSNumber)
fuelPercentRemainingSelector = mkSelector "fuelPercentRemaining"

-- | @Selector@ for @setFuelPercentRemaining:@
setFuelPercentRemainingSelector :: Selector '[Id NSNumber] ()
setFuelPercentRemainingSelector = mkSelector "setFuelPercentRemaining:"

-- | @Selector@ for @chargePercentRemaining@
chargePercentRemainingSelector :: Selector '[] (Id NSNumber)
chargePercentRemainingSelector = mkSelector "chargePercentRemaining"

-- | @Selector@ for @setChargePercentRemaining:@
setChargePercentRemainingSelector :: Selector '[Id NSNumber] ()
setChargePercentRemainingSelector = mkSelector "setChargePercentRemaining:"

-- | @Selector@ for @distanceRemaining@
distanceRemainingSelector :: Selector '[] (Id NSMeasurement)
distanceRemainingSelector = mkSelector "distanceRemaining"

-- | @Selector@ for @setDistanceRemaining:@
setDistanceRemainingSelector :: Selector '[Id NSMeasurement] ()
setDistanceRemainingSelector = mkSelector "setDistanceRemaining:"

-- | @Selector@ for @charging@
chargingSelector :: Selector '[] (Id NSNumber)
chargingSelector = mkSelector "charging"

-- | @Selector@ for @setCharging:@
setChargingSelector :: Selector '[Id NSNumber] ()
setChargingSelector = mkSelector "setCharging:"

-- | @Selector@ for @minutesToFull@
minutesToFullSelector :: Selector '[] (Id NSNumber)
minutesToFullSelector = mkSelector "minutesToFull"

-- | @Selector@ for @setMinutesToFull:@
setMinutesToFullSelector :: Selector '[Id NSNumber] ()
setMinutesToFullSelector = mkSelector "setMinutesToFull:"

-- | @Selector@ for @maximumDistance@
maximumDistanceSelector :: Selector '[] (Id NSMeasurement)
maximumDistanceSelector = mkSelector "maximumDistance"

-- | @Selector@ for @setMaximumDistance:@
setMaximumDistanceSelector :: Selector '[Id NSMeasurement] ()
setMaximumDistanceSelector = mkSelector "setMaximumDistance:"

-- | @Selector@ for @distanceRemainingElectric@
distanceRemainingElectricSelector :: Selector '[] (Id NSMeasurement)
distanceRemainingElectricSelector = mkSelector "distanceRemainingElectric"

-- | @Selector@ for @setDistanceRemainingElectric:@
setDistanceRemainingElectricSelector :: Selector '[Id NSMeasurement] ()
setDistanceRemainingElectricSelector = mkSelector "setDistanceRemainingElectric:"

-- | @Selector@ for @maximumDistanceElectric@
maximumDistanceElectricSelector :: Selector '[] (Id NSMeasurement)
maximumDistanceElectricSelector = mkSelector "maximumDistanceElectric"

-- | @Selector@ for @setMaximumDistanceElectric:@
setMaximumDistanceElectricSelector :: Selector '[Id NSMeasurement] ()
setMaximumDistanceElectricSelector = mkSelector "setMaximumDistanceElectric:"

-- | @Selector@ for @distanceRemainingFuel@
distanceRemainingFuelSelector :: Selector '[] (Id NSMeasurement)
distanceRemainingFuelSelector = mkSelector "distanceRemainingFuel"

-- | @Selector@ for @setDistanceRemainingFuel:@
setDistanceRemainingFuelSelector :: Selector '[Id NSMeasurement] ()
setDistanceRemainingFuelSelector = mkSelector "setDistanceRemainingFuel:"

-- | @Selector@ for @maximumDistanceFuel@
maximumDistanceFuelSelector :: Selector '[] (Id NSMeasurement)
maximumDistanceFuelSelector = mkSelector "maximumDistanceFuel"

-- | @Selector@ for @setMaximumDistanceFuel:@
setMaximumDistanceFuelSelector :: Selector '[Id NSMeasurement] ()
setMaximumDistanceFuelSelector = mkSelector "setMaximumDistanceFuel:"

-- | @Selector@ for @consumptionFormulaArguments@
consumptionFormulaArgumentsSelector :: Selector '[] (Id NSDictionary)
consumptionFormulaArgumentsSelector = mkSelector "consumptionFormulaArguments"

-- | @Selector@ for @setConsumptionFormulaArguments:@
setConsumptionFormulaArgumentsSelector :: Selector '[Id NSDictionary] ()
setConsumptionFormulaArgumentsSelector = mkSelector "setConsumptionFormulaArguments:"

-- | @Selector@ for @chargingFormulaArguments@
chargingFormulaArgumentsSelector :: Selector '[] (Id NSDictionary)
chargingFormulaArgumentsSelector = mkSelector "chargingFormulaArguments"

-- | @Selector@ for @setChargingFormulaArguments:@
setChargingFormulaArgumentsSelector :: Selector '[Id NSDictionary] ()
setChargingFormulaArgumentsSelector = mkSelector "setChargingFormulaArguments:"

-- | @Selector@ for @dateOfLastStateUpdate@
dateOfLastStateUpdateSelector :: Selector '[] (Id NSDateComponents)
dateOfLastStateUpdateSelector = mkSelector "dateOfLastStateUpdate"

-- | @Selector@ for @setDateOfLastStateUpdate:@
setDateOfLastStateUpdateSelector :: Selector '[Id NSDateComponents] ()
setDateOfLastStateUpdateSelector = mkSelector "setDateOfLastStateUpdate:"

-- | @Selector@ for @activeConnector@
activeConnectorSelector :: Selector '[] (Id NSString)
activeConnectorSelector = mkSelector "activeConnector"

-- | @Selector@ for @setActiveConnector:@
setActiveConnectorSelector :: Selector '[Id NSString] ()
setActiveConnectorSelector = mkSelector "setActiveConnector:"

-- | @Selector@ for @maximumBatteryCapacity@
maximumBatteryCapacitySelector :: Selector '[] (Id NSMeasurement)
maximumBatteryCapacitySelector = mkSelector "maximumBatteryCapacity"

-- | @Selector@ for @setMaximumBatteryCapacity:@
setMaximumBatteryCapacitySelector :: Selector '[Id NSMeasurement] ()
setMaximumBatteryCapacitySelector = mkSelector "setMaximumBatteryCapacity:"

-- | @Selector@ for @currentBatteryCapacity@
currentBatteryCapacitySelector :: Selector '[] (Id NSMeasurement)
currentBatteryCapacitySelector = mkSelector "currentBatteryCapacity"

-- | @Selector@ for @setCurrentBatteryCapacity:@
setCurrentBatteryCapacitySelector :: Selector '[Id NSMeasurement] ()
setCurrentBatteryCapacitySelector = mkSelector "setCurrentBatteryCapacity:"

-- | @Selector@ for @minimumBatteryCapacity@
minimumBatteryCapacitySelector :: Selector '[] (Id NSMeasurement)
minimumBatteryCapacitySelector = mkSelector "minimumBatteryCapacity"

-- | @Selector@ for @setMinimumBatteryCapacity:@
setMinimumBatteryCapacitySelector :: Selector '[Id NSMeasurement] ()
setMinimumBatteryCapacitySelector = mkSelector "setMinimumBatteryCapacity:"

