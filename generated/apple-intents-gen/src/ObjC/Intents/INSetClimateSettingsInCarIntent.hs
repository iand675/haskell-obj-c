{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetClimateSettingsInCarIntent@.
module ObjC.Intents.INSetClimateSettingsInCarIntent
  ( INSetClimateSettingsInCarIntent
  , IsINSetClimateSettingsInCarIntent(..)
  , initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carName
  , initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone
  , enableFan
  , enableAirConditioner
  , enableClimateControl
  , enableAutoMode
  , airCirculationMode
  , fanSpeedIndex
  , fanSpeedPercentage
  , relativeFanSpeedSetting
  , temperature
  , relativeTemperatureSetting
  , climateZone
  , carName
  , airCirculationModeSelector
  , carNameSelector
  , climateZoneSelector
  , enableAirConditionerSelector
  , enableAutoModeSelector
  , enableClimateControlSelector
  , enableFanSelector
  , fanSpeedIndexSelector
  , fanSpeedPercentageSelector
  , initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector
  , initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector
  , relativeFanSpeedSettingSelector
  , relativeTemperatureSettingSelector
  , temperatureSelector

  -- * Enum types
  , INCarAirCirculationMode(INCarAirCirculationMode)
  , pattern INCarAirCirculationModeUnknown
  , pattern INCarAirCirculationModeFreshAir
  , pattern INCarAirCirculationModeRecirculateAir
  , INCarSeat(INCarSeat)
  , pattern INCarSeatUnknown
  , pattern INCarSeatDriver
  , pattern INCarSeatPassenger
  , pattern INCarSeatFrontLeft
  , pattern INCarSeatFrontRight
  , pattern INCarSeatFront
  , pattern INCarSeatRearLeft
  , pattern INCarSeatRearRight
  , pattern INCarSeatRear
  , pattern INCarSeatThirdRowLeft
  , pattern INCarSeatThirdRowRight
  , pattern INCarSeatThirdRow
  , pattern INCarSeatAll
  , INRelativeSetting(INRelativeSetting)
  , pattern INRelativeSettingUnknown
  , pattern INRelativeSettingLowest
  , pattern INRelativeSettingLower
  , pattern INRelativeSettingHigher
  , pattern INRelativeSettingHighest

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

-- | @- initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carName :: (IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent, IsNSNumber enableFan, IsNSNumber enableAirConditioner, IsNSNumber enableClimateControl, IsNSNumber enableAutoMode, IsNSNumber fanSpeedIndex, IsNSNumber fanSpeedPercentage, IsNSMeasurement temperature, IsINSpeakableString carName) => inSetClimateSettingsInCarIntent -> enableFan -> enableAirConditioner -> enableClimateControl -> enableAutoMode -> INCarAirCirculationMode -> fanSpeedIndex -> fanSpeedPercentage -> INRelativeSetting -> temperature -> INRelativeSetting -> INCarSeat -> carName -> IO (Id INSetClimateSettingsInCarIntent)
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carName inSetClimateSettingsInCarIntent enableFan enableAirConditioner enableClimateControl enableAutoMode airCirculationMode fanSpeedIndex fanSpeedPercentage relativeFanSpeedSetting temperature relativeTemperatureSetting climateZone carName =
  sendOwnedMessage inSetClimateSettingsInCarIntent initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector (toNSNumber enableFan) (toNSNumber enableAirConditioner) (toNSNumber enableClimateControl) (toNSNumber enableAutoMode) airCirculationMode (toNSNumber fanSpeedIndex) (toNSNumber fanSpeedPercentage) relativeFanSpeedSetting (toNSMeasurement temperature) relativeTemperatureSetting climateZone (toINSpeakableString carName)

-- | @- initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone :: (IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent, IsNSNumber enableFan, IsNSNumber enableAirConditioner, IsNSNumber enableClimateControl, IsNSNumber enableAutoMode, IsNSNumber fanSpeedIndex, IsNSNumber fanSpeedPercentage, IsNSMeasurement temperature) => inSetClimateSettingsInCarIntent -> enableFan -> enableAirConditioner -> enableClimateControl -> enableAutoMode -> INCarAirCirculationMode -> fanSpeedIndex -> fanSpeedPercentage -> INRelativeSetting -> temperature -> INRelativeSetting -> INCarSeat -> IO (Id INSetClimateSettingsInCarIntent)
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone inSetClimateSettingsInCarIntent enableFan enableAirConditioner enableClimateControl enableAutoMode airCirculationMode fanSpeedIndex fanSpeedPercentage relativeFanSpeedSetting temperature relativeTemperatureSetting climateZone =
  sendOwnedMessage inSetClimateSettingsInCarIntent initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector (toNSNumber enableFan) (toNSNumber enableAirConditioner) (toNSNumber enableClimateControl) (toNSNumber enableAutoMode) airCirculationMode (toNSNumber fanSpeedIndex) (toNSNumber fanSpeedPercentage) relativeFanSpeedSetting (toNSMeasurement temperature) relativeTemperatureSetting climateZone

-- | @- enableFan@
enableFan :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableFan inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent enableFanSelector

-- | @- enableAirConditioner@
enableAirConditioner :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableAirConditioner inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent enableAirConditionerSelector

-- | @- enableClimateControl@
enableClimateControl :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableClimateControl inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent enableClimateControlSelector

-- | @- enableAutoMode@
enableAutoMode :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableAutoMode inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent enableAutoModeSelector

-- | @- airCirculationMode@
airCirculationMode :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INCarAirCirculationMode
airCirculationMode inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent airCirculationModeSelector

-- | @- fanSpeedIndex@
fanSpeedIndex :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
fanSpeedIndex inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent fanSpeedIndexSelector

-- | @- fanSpeedPercentage@
fanSpeedPercentage :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
fanSpeedPercentage inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent fanSpeedPercentageSelector

-- | @- relativeFanSpeedSetting@
relativeFanSpeedSetting :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INRelativeSetting
relativeFanSpeedSetting inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent relativeFanSpeedSettingSelector

-- | @- temperature@
temperature :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSMeasurement)
temperature inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent temperatureSelector

-- | @- relativeTemperatureSetting@
relativeTemperatureSetting :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INRelativeSetting
relativeTemperatureSetting inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent relativeTemperatureSettingSelector

-- | @- climateZone@
climateZone :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INCarSeat
climateZone inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent climateZoneSelector

-- | @- carName@
carName :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id INSpeakableString)
carName inSetClimateSettingsInCarIntent =
  sendMessage inSetClimateSettingsInCarIntent carNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, INCarAirCirculationMode, Id NSNumber, Id NSNumber, INRelativeSetting, Id NSMeasurement, INRelativeSetting, INCarSeat, Id INSpeakableString] (Id INSetClimateSettingsInCarIntent)
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector = mkSelector "initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:"

-- | @Selector@ for @initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, INCarAirCirculationMode, Id NSNumber, Id NSNumber, INRelativeSetting, Id NSMeasurement, INRelativeSetting, INCarSeat] (Id INSetClimateSettingsInCarIntent)
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector = mkSelector "initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:"

-- | @Selector@ for @enableFan@
enableFanSelector :: Selector '[] (Id NSNumber)
enableFanSelector = mkSelector "enableFan"

-- | @Selector@ for @enableAirConditioner@
enableAirConditionerSelector :: Selector '[] (Id NSNumber)
enableAirConditionerSelector = mkSelector "enableAirConditioner"

-- | @Selector@ for @enableClimateControl@
enableClimateControlSelector :: Selector '[] (Id NSNumber)
enableClimateControlSelector = mkSelector "enableClimateControl"

-- | @Selector@ for @enableAutoMode@
enableAutoModeSelector :: Selector '[] (Id NSNumber)
enableAutoModeSelector = mkSelector "enableAutoMode"

-- | @Selector@ for @airCirculationMode@
airCirculationModeSelector :: Selector '[] INCarAirCirculationMode
airCirculationModeSelector = mkSelector "airCirculationMode"

-- | @Selector@ for @fanSpeedIndex@
fanSpeedIndexSelector :: Selector '[] (Id NSNumber)
fanSpeedIndexSelector = mkSelector "fanSpeedIndex"

-- | @Selector@ for @fanSpeedPercentage@
fanSpeedPercentageSelector :: Selector '[] (Id NSNumber)
fanSpeedPercentageSelector = mkSelector "fanSpeedPercentage"

-- | @Selector@ for @relativeFanSpeedSetting@
relativeFanSpeedSettingSelector :: Selector '[] INRelativeSetting
relativeFanSpeedSettingSelector = mkSelector "relativeFanSpeedSetting"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector '[] (Id NSMeasurement)
temperatureSelector = mkSelector "temperature"

-- | @Selector@ for @relativeTemperatureSetting@
relativeTemperatureSettingSelector :: Selector '[] INRelativeSetting
relativeTemperatureSettingSelector = mkSelector "relativeTemperatureSetting"

-- | @Selector@ for @climateZone@
climateZoneSelector :: Selector '[] INCarSeat
climateZoneSelector = mkSelector "climateZone"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

