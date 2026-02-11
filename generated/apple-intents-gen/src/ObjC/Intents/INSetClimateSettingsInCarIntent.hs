{-# LANGUAGE PatternSynonyms #-}
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
  , initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector
  , initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector
  , enableFanSelector
  , enableAirConditionerSelector
  , enableClimateControlSelector
  , enableAutoModeSelector
  , airCirculationModeSelector
  , fanSpeedIndexSelector
  , fanSpeedPercentageSelector
  , relativeFanSpeedSettingSelector
  , temperatureSelector
  , relativeTemperatureSettingSelector
  , climateZoneSelector
  , carNameSelector

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

-- | @- initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carName :: (IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent, IsNSNumber enableFan, IsNSNumber enableAirConditioner, IsNSNumber enableClimateControl, IsNSNumber enableAutoMode, IsNSNumber fanSpeedIndex, IsNSNumber fanSpeedPercentage, IsNSMeasurement temperature, IsINSpeakableString carName) => inSetClimateSettingsInCarIntent -> enableFan -> enableAirConditioner -> enableClimateControl -> enableAutoMode -> INCarAirCirculationMode -> fanSpeedIndex -> fanSpeedPercentage -> INRelativeSetting -> temperature -> INRelativeSetting -> INCarSeat -> carName -> IO (Id INSetClimateSettingsInCarIntent)
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carName inSetClimateSettingsInCarIntent  enableFan enableAirConditioner enableClimateControl enableAutoMode airCirculationMode fanSpeedIndex fanSpeedPercentage relativeFanSpeedSetting temperature relativeTemperatureSetting climateZone carName =
  withObjCPtr enableFan $ \raw_enableFan ->
    withObjCPtr enableAirConditioner $ \raw_enableAirConditioner ->
      withObjCPtr enableClimateControl $ \raw_enableClimateControl ->
        withObjCPtr enableAutoMode $ \raw_enableAutoMode ->
          withObjCPtr fanSpeedIndex $ \raw_fanSpeedIndex ->
            withObjCPtr fanSpeedPercentage $ \raw_fanSpeedPercentage ->
              withObjCPtr temperature $ \raw_temperature ->
                withObjCPtr carName $ \raw_carName ->
                    sendMsg inSetClimateSettingsInCarIntent (mkSelector "initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:") (retPtr retVoid) [argPtr (castPtr raw_enableFan :: Ptr ()), argPtr (castPtr raw_enableAirConditioner :: Ptr ()), argPtr (castPtr raw_enableClimateControl :: Ptr ()), argPtr (castPtr raw_enableAutoMode :: Ptr ()), argCLong (coerce airCirculationMode), argPtr (castPtr raw_fanSpeedIndex :: Ptr ()), argPtr (castPtr raw_fanSpeedPercentage :: Ptr ()), argCLong (coerce relativeFanSpeedSetting), argPtr (castPtr raw_temperature :: Ptr ()), argCLong (coerce relativeTemperatureSetting), argCLong (coerce climateZone), argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone :: (IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent, IsNSNumber enableFan, IsNSNumber enableAirConditioner, IsNSNumber enableClimateControl, IsNSNumber enableAutoMode, IsNSNumber fanSpeedIndex, IsNSNumber fanSpeedPercentage, IsNSMeasurement temperature) => inSetClimateSettingsInCarIntent -> enableFan -> enableAirConditioner -> enableClimateControl -> enableAutoMode -> INCarAirCirculationMode -> fanSpeedIndex -> fanSpeedPercentage -> INRelativeSetting -> temperature -> INRelativeSetting -> INCarSeat -> IO (Id INSetClimateSettingsInCarIntent)
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone inSetClimateSettingsInCarIntent  enableFan enableAirConditioner enableClimateControl enableAutoMode airCirculationMode fanSpeedIndex fanSpeedPercentage relativeFanSpeedSetting temperature relativeTemperatureSetting climateZone =
  withObjCPtr enableFan $ \raw_enableFan ->
    withObjCPtr enableAirConditioner $ \raw_enableAirConditioner ->
      withObjCPtr enableClimateControl $ \raw_enableClimateControl ->
        withObjCPtr enableAutoMode $ \raw_enableAutoMode ->
          withObjCPtr fanSpeedIndex $ \raw_fanSpeedIndex ->
            withObjCPtr fanSpeedPercentage $ \raw_fanSpeedPercentage ->
              withObjCPtr temperature $ \raw_temperature ->
                  sendMsg inSetClimateSettingsInCarIntent (mkSelector "initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:") (retPtr retVoid) [argPtr (castPtr raw_enableFan :: Ptr ()), argPtr (castPtr raw_enableAirConditioner :: Ptr ()), argPtr (castPtr raw_enableClimateControl :: Ptr ()), argPtr (castPtr raw_enableAutoMode :: Ptr ()), argCLong (coerce airCirculationMode), argPtr (castPtr raw_fanSpeedIndex :: Ptr ()), argPtr (castPtr raw_fanSpeedPercentage :: Ptr ()), argCLong (coerce relativeFanSpeedSetting), argPtr (castPtr raw_temperature :: Ptr ()), argCLong (coerce relativeTemperatureSetting), argCLong (coerce climateZone)] >>= ownedObject . castPtr

-- | @- enableFan@
enableFan :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableFan inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "enableFan") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enableAirConditioner@
enableAirConditioner :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableAirConditioner inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "enableAirConditioner") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enableClimateControl@
enableClimateControl :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableClimateControl inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "enableClimateControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enableAutoMode@
enableAutoMode :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
enableAutoMode inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "enableAutoMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- airCirculationMode@
airCirculationMode :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INCarAirCirculationMode
airCirculationMode inSetClimateSettingsInCarIntent  =
    fmap (coerce :: CLong -> INCarAirCirculationMode) $ sendMsg inSetClimateSettingsInCarIntent (mkSelector "airCirculationMode") retCLong []

-- | @- fanSpeedIndex@
fanSpeedIndex :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
fanSpeedIndex inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "fanSpeedIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fanSpeedPercentage@
fanSpeedPercentage :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSNumber)
fanSpeedPercentage inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "fanSpeedPercentage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relativeFanSpeedSetting@
relativeFanSpeedSetting :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INRelativeSetting
relativeFanSpeedSetting inSetClimateSettingsInCarIntent  =
    fmap (coerce :: CLong -> INRelativeSetting) $ sendMsg inSetClimateSettingsInCarIntent (mkSelector "relativeFanSpeedSetting") retCLong []

-- | @- temperature@
temperature :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id NSMeasurement)
temperature inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "temperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relativeTemperatureSetting@
relativeTemperatureSetting :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INRelativeSetting
relativeTemperatureSetting inSetClimateSettingsInCarIntent  =
    fmap (coerce :: CLong -> INRelativeSetting) $ sendMsg inSetClimateSettingsInCarIntent (mkSelector "relativeTemperatureSetting") retCLong []

-- | @- climateZone@
climateZone :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO INCarSeat
climateZone inSetClimateSettingsInCarIntent  =
    fmap (coerce :: CLong -> INCarSeat) $ sendMsg inSetClimateSettingsInCarIntent (mkSelector "climateZone") retCLong []

-- | @- carName@
carName :: IsINSetClimateSettingsInCarIntent inSetClimateSettingsInCarIntent => inSetClimateSettingsInCarIntent -> IO (Id INSpeakableString)
carName inSetClimateSettingsInCarIntent  =
    sendMsg inSetClimateSettingsInCarIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector :: Selector
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZone_carNameSelector = mkSelector "initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:carName:"

-- | @Selector@ for @initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:@
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector :: Selector
initWithEnableFan_enableAirConditioner_enableClimateControl_enableAutoMode_airCirculationMode_fanSpeedIndex_fanSpeedPercentage_relativeFanSpeedSetting_temperature_relativeTemperatureSetting_climateZoneSelector = mkSelector "initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:climateZone:"

-- | @Selector@ for @enableFan@
enableFanSelector :: Selector
enableFanSelector = mkSelector "enableFan"

-- | @Selector@ for @enableAirConditioner@
enableAirConditionerSelector :: Selector
enableAirConditionerSelector = mkSelector "enableAirConditioner"

-- | @Selector@ for @enableClimateControl@
enableClimateControlSelector :: Selector
enableClimateControlSelector = mkSelector "enableClimateControl"

-- | @Selector@ for @enableAutoMode@
enableAutoModeSelector :: Selector
enableAutoModeSelector = mkSelector "enableAutoMode"

-- | @Selector@ for @airCirculationMode@
airCirculationModeSelector :: Selector
airCirculationModeSelector = mkSelector "airCirculationMode"

-- | @Selector@ for @fanSpeedIndex@
fanSpeedIndexSelector :: Selector
fanSpeedIndexSelector = mkSelector "fanSpeedIndex"

-- | @Selector@ for @fanSpeedPercentage@
fanSpeedPercentageSelector :: Selector
fanSpeedPercentageSelector = mkSelector "fanSpeedPercentage"

-- | @Selector@ for @relativeFanSpeedSetting@
relativeFanSpeedSettingSelector :: Selector
relativeFanSpeedSettingSelector = mkSelector "relativeFanSpeedSetting"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector
temperatureSelector = mkSelector "temperature"

-- | @Selector@ for @relativeTemperatureSetting@
relativeTemperatureSettingSelector :: Selector
relativeTemperatureSettingSelector = mkSelector "relativeTemperatureSetting"

-- | @Selector@ for @climateZone@
climateZoneSelector :: Selector
climateZoneSelector = mkSelector "climateZone"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

