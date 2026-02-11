{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , carIdentifierSelector
  , setCarIdentifierSelector
  , fuelPercentRemainingSelector
  , setFuelPercentRemainingSelector
  , chargePercentRemainingSelector
  , setChargePercentRemainingSelector
  , distanceRemainingSelector
  , setDistanceRemainingSelector
  , chargingSelector
  , setChargingSelector
  , minutesToFullSelector
  , setMinutesToFullSelector
  , maximumDistanceSelector
  , setMaximumDistanceSelector
  , distanceRemainingElectricSelector
  , setDistanceRemainingElectricSelector
  , maximumDistanceElectricSelector
  , setMaximumDistanceElectricSelector
  , distanceRemainingFuelSelector
  , setDistanceRemainingFuelSelector
  , maximumDistanceFuelSelector
  , setMaximumDistanceFuelSelector
  , consumptionFormulaArgumentsSelector
  , setConsumptionFormulaArgumentsSelector
  , chargingFormulaArgumentsSelector
  , setChargingFormulaArgumentsSelector
  , dateOfLastStateUpdateSelector
  , setDateOfLastStateUpdateSelector
  , activeConnectorSelector
  , setActiveConnectorSelector
  , maximumBatteryCapacitySelector
  , setMaximumBatteryCapacitySelector
  , currentBatteryCapacitySelector
  , setCurrentBatteryCapacitySelector
  , minimumBatteryCapacitySelector
  , setMinimumBatteryCapacitySelector

  -- * Enum types
  , INGetCarPowerLevelStatusIntentResponseCode(INGetCarPowerLevelStatusIntentResponseCode)
  , pattern INGetCarPowerLevelStatusIntentResponseCodeUnspecified
  , pattern INGetCarPowerLevelStatusIntentResponseCodeReady
  , pattern INGetCarPowerLevelStatusIntentResponseCodeInProgress
  , pattern INGetCarPowerLevelStatusIntentResponseCodeSuccess
  , pattern INGetCarPowerLevelStatusIntentResponseCodeFailure
  , pattern INGetCarPowerLevelStatusIntentResponseCodeFailureRequiringAppLaunch

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO RawId
init_ inGetCarPowerLevelStatusIntentResponse  =
    fmap (RawId . castPtr) $ sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSUserActivity userActivity) => inGetCarPowerLevelStatusIntentResponse -> INGetCarPowerLevelStatusIntentResponseCode -> userActivity -> IO (Id INGetCarPowerLevelStatusIntentResponse)
initWithCode_userActivity inGetCarPowerLevelStatusIntentResponse  code userActivity =
  withObjCPtr userActivity $ \raw_userActivity ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO INGetCarPowerLevelStatusIntentResponseCode
code inGetCarPowerLevelStatusIntentResponse  =
    fmap (coerce :: CLong -> INGetCarPowerLevelStatusIntentResponseCode) $ sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "code") retCLong []

-- | @- carIdentifier@
carIdentifier :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSString)
carIdentifier inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "carIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCarIdentifier:@
setCarIdentifier :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSString value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setCarIdentifier inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setCarIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fuelPercentRemaining@
fuelPercentRemaining :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
fuelPercentRemaining inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "fuelPercentRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFuelPercentRemaining:@
setFuelPercentRemaining :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setFuelPercentRemaining inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setFuelPercentRemaining:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chargePercentRemaining@
chargePercentRemaining :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
chargePercentRemaining inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "chargePercentRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChargePercentRemaining:@
setChargePercentRemaining :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setChargePercentRemaining inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setChargePercentRemaining:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- distanceRemaining@
distanceRemaining :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
distanceRemaining inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "distanceRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDistanceRemaining:@
setDistanceRemaining :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDistanceRemaining inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setDistanceRemaining:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- charging@
charging :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
charging inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "charging") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharging:@
setCharging :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setCharging inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setCharging:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minutesToFull@
minutesToFull :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSNumber)
minutesToFull inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "minutesToFull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinutesToFull:@
setMinutesToFull :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSNumber value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMinutesToFull inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setMinutesToFull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumDistance@
maximumDistance :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumDistance inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "maximumDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumDistance:@
setMaximumDistance :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumDistance inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setMaximumDistance:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- distanceRemainingElectric@
distanceRemainingElectric :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
distanceRemainingElectric inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "distanceRemainingElectric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDistanceRemainingElectric:@
setDistanceRemainingElectric :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDistanceRemainingElectric inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setDistanceRemainingElectric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumDistanceElectric@
maximumDistanceElectric :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumDistanceElectric inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "maximumDistanceElectric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumDistanceElectric:@
setMaximumDistanceElectric :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumDistanceElectric inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setMaximumDistanceElectric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- distanceRemainingFuel@
distanceRemainingFuel :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
distanceRemainingFuel inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "distanceRemainingFuel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDistanceRemainingFuel:@
setDistanceRemainingFuel :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDistanceRemainingFuel inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setDistanceRemainingFuel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumDistanceFuel@
maximumDistanceFuel :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumDistanceFuel inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "maximumDistanceFuel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumDistanceFuel:@
setMaximumDistanceFuel :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumDistanceFuel inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setMaximumDistanceFuel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- consumptionFormulaArguments@
consumptionFormulaArguments :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSDictionary)
consumptionFormulaArguments inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "consumptionFormulaArguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConsumptionFormulaArguments:@
setConsumptionFormulaArguments :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSDictionary value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setConsumptionFormulaArguments inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setConsumptionFormulaArguments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chargingFormulaArguments@
chargingFormulaArguments :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSDictionary)
chargingFormulaArguments inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "chargingFormulaArguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChargingFormulaArguments:@
setChargingFormulaArguments :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSDictionary value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setChargingFormulaArguments inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setChargingFormulaArguments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateOfLastStateUpdate@
dateOfLastStateUpdate :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSDateComponents)
dateOfLastStateUpdate inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "dateOfLastStateUpdate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateOfLastStateUpdate:@
setDateOfLastStateUpdate :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSDateComponents value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setDateOfLastStateUpdate inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setDateOfLastStateUpdate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activeConnector@
activeConnector :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSString)
activeConnector inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "activeConnector") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActiveConnector:@
setActiveConnector :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSString value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setActiveConnector inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setActiveConnector:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumBatteryCapacity@
maximumBatteryCapacity :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
maximumBatteryCapacity inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "maximumBatteryCapacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumBatteryCapacity:@
setMaximumBatteryCapacity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMaximumBatteryCapacity inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setMaximumBatteryCapacity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentBatteryCapacity@
currentBatteryCapacity :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
currentBatteryCapacity inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "currentBatteryCapacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentBatteryCapacity:@
setCurrentBatteryCapacity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setCurrentBatteryCapacity inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setCurrentBatteryCapacity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minimumBatteryCapacity@
minimumBatteryCapacity :: IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse => inGetCarPowerLevelStatusIntentResponse -> IO (Id NSMeasurement)
minimumBatteryCapacity inGetCarPowerLevelStatusIntentResponse  =
    sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "minimumBatteryCapacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinimumBatteryCapacity:@
setMinimumBatteryCapacity :: (IsINGetCarPowerLevelStatusIntentResponse inGetCarPowerLevelStatusIntentResponse, IsNSMeasurement value) => inGetCarPowerLevelStatusIntentResponse -> value -> IO ()
setMinimumBatteryCapacity inGetCarPowerLevelStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarPowerLevelStatusIntentResponse (mkSelector "setMinimumBatteryCapacity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @carIdentifier@
carIdentifierSelector :: Selector
carIdentifierSelector = mkSelector "carIdentifier"

-- | @Selector@ for @setCarIdentifier:@
setCarIdentifierSelector :: Selector
setCarIdentifierSelector = mkSelector "setCarIdentifier:"

-- | @Selector@ for @fuelPercentRemaining@
fuelPercentRemainingSelector :: Selector
fuelPercentRemainingSelector = mkSelector "fuelPercentRemaining"

-- | @Selector@ for @setFuelPercentRemaining:@
setFuelPercentRemainingSelector :: Selector
setFuelPercentRemainingSelector = mkSelector "setFuelPercentRemaining:"

-- | @Selector@ for @chargePercentRemaining@
chargePercentRemainingSelector :: Selector
chargePercentRemainingSelector = mkSelector "chargePercentRemaining"

-- | @Selector@ for @setChargePercentRemaining:@
setChargePercentRemainingSelector :: Selector
setChargePercentRemainingSelector = mkSelector "setChargePercentRemaining:"

-- | @Selector@ for @distanceRemaining@
distanceRemainingSelector :: Selector
distanceRemainingSelector = mkSelector "distanceRemaining"

-- | @Selector@ for @setDistanceRemaining:@
setDistanceRemainingSelector :: Selector
setDistanceRemainingSelector = mkSelector "setDistanceRemaining:"

-- | @Selector@ for @charging@
chargingSelector :: Selector
chargingSelector = mkSelector "charging"

-- | @Selector@ for @setCharging:@
setChargingSelector :: Selector
setChargingSelector = mkSelector "setCharging:"

-- | @Selector@ for @minutesToFull@
minutesToFullSelector :: Selector
minutesToFullSelector = mkSelector "minutesToFull"

-- | @Selector@ for @setMinutesToFull:@
setMinutesToFullSelector :: Selector
setMinutesToFullSelector = mkSelector "setMinutesToFull:"

-- | @Selector@ for @maximumDistance@
maximumDistanceSelector :: Selector
maximumDistanceSelector = mkSelector "maximumDistance"

-- | @Selector@ for @setMaximumDistance:@
setMaximumDistanceSelector :: Selector
setMaximumDistanceSelector = mkSelector "setMaximumDistance:"

-- | @Selector@ for @distanceRemainingElectric@
distanceRemainingElectricSelector :: Selector
distanceRemainingElectricSelector = mkSelector "distanceRemainingElectric"

-- | @Selector@ for @setDistanceRemainingElectric:@
setDistanceRemainingElectricSelector :: Selector
setDistanceRemainingElectricSelector = mkSelector "setDistanceRemainingElectric:"

-- | @Selector@ for @maximumDistanceElectric@
maximumDistanceElectricSelector :: Selector
maximumDistanceElectricSelector = mkSelector "maximumDistanceElectric"

-- | @Selector@ for @setMaximumDistanceElectric:@
setMaximumDistanceElectricSelector :: Selector
setMaximumDistanceElectricSelector = mkSelector "setMaximumDistanceElectric:"

-- | @Selector@ for @distanceRemainingFuel@
distanceRemainingFuelSelector :: Selector
distanceRemainingFuelSelector = mkSelector "distanceRemainingFuel"

-- | @Selector@ for @setDistanceRemainingFuel:@
setDistanceRemainingFuelSelector :: Selector
setDistanceRemainingFuelSelector = mkSelector "setDistanceRemainingFuel:"

-- | @Selector@ for @maximumDistanceFuel@
maximumDistanceFuelSelector :: Selector
maximumDistanceFuelSelector = mkSelector "maximumDistanceFuel"

-- | @Selector@ for @setMaximumDistanceFuel:@
setMaximumDistanceFuelSelector :: Selector
setMaximumDistanceFuelSelector = mkSelector "setMaximumDistanceFuel:"

-- | @Selector@ for @consumptionFormulaArguments@
consumptionFormulaArgumentsSelector :: Selector
consumptionFormulaArgumentsSelector = mkSelector "consumptionFormulaArguments"

-- | @Selector@ for @setConsumptionFormulaArguments:@
setConsumptionFormulaArgumentsSelector :: Selector
setConsumptionFormulaArgumentsSelector = mkSelector "setConsumptionFormulaArguments:"

-- | @Selector@ for @chargingFormulaArguments@
chargingFormulaArgumentsSelector :: Selector
chargingFormulaArgumentsSelector = mkSelector "chargingFormulaArguments"

-- | @Selector@ for @setChargingFormulaArguments:@
setChargingFormulaArgumentsSelector :: Selector
setChargingFormulaArgumentsSelector = mkSelector "setChargingFormulaArguments:"

-- | @Selector@ for @dateOfLastStateUpdate@
dateOfLastStateUpdateSelector :: Selector
dateOfLastStateUpdateSelector = mkSelector "dateOfLastStateUpdate"

-- | @Selector@ for @setDateOfLastStateUpdate:@
setDateOfLastStateUpdateSelector :: Selector
setDateOfLastStateUpdateSelector = mkSelector "setDateOfLastStateUpdate:"

-- | @Selector@ for @activeConnector@
activeConnectorSelector :: Selector
activeConnectorSelector = mkSelector "activeConnector"

-- | @Selector@ for @setActiveConnector:@
setActiveConnectorSelector :: Selector
setActiveConnectorSelector = mkSelector "setActiveConnector:"

-- | @Selector@ for @maximumBatteryCapacity@
maximumBatteryCapacitySelector :: Selector
maximumBatteryCapacitySelector = mkSelector "maximumBatteryCapacity"

-- | @Selector@ for @setMaximumBatteryCapacity:@
setMaximumBatteryCapacitySelector :: Selector
setMaximumBatteryCapacitySelector = mkSelector "setMaximumBatteryCapacity:"

-- | @Selector@ for @currentBatteryCapacity@
currentBatteryCapacitySelector :: Selector
currentBatteryCapacitySelector = mkSelector "currentBatteryCapacity"

-- | @Selector@ for @setCurrentBatteryCapacity:@
setCurrentBatteryCapacitySelector :: Selector
setCurrentBatteryCapacitySelector = mkSelector "setCurrentBatteryCapacity:"

-- | @Selector@ for @minimumBatteryCapacity@
minimumBatteryCapacitySelector :: Selector
minimumBatteryCapacitySelector = mkSelector "minimumBatteryCapacity"

-- | @Selector@ for @setMinimumBatteryCapacity:@
setMinimumBatteryCapacitySelector :: Selector
setMinimumBatteryCapacitySelector = mkSelector "setMinimumBatteryCapacity:"

