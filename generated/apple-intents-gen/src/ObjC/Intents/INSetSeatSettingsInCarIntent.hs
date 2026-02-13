{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetSeatSettingsInCarIntent@.
module ObjC.Intents.INSetSeatSettingsInCarIntent
  ( INSetSeatSettingsInCarIntent
  , IsINSetSeatSettingsInCarIntent(..)
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carName
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting
  , enableHeating
  , enableCooling
  , enableMassage
  , seat
  , level
  , relativeLevelSetting
  , carName
  , carNameSelector
  , enableCoolingSelector
  , enableHeatingSelector
  , enableMassageSelector
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector
  , levelSelector
  , relativeLevelSettingSelector
  , seatSelector

  -- * Enum types
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

-- | @- initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carName :: (IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent, IsNSNumber enableHeating, IsNSNumber enableCooling, IsNSNumber enableMassage, IsNSNumber level, IsINSpeakableString carName) => inSetSeatSettingsInCarIntent -> enableHeating -> enableCooling -> enableMassage -> INCarSeat -> level -> INRelativeSetting -> carName -> IO (Id INSetSeatSettingsInCarIntent)
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carName inSetSeatSettingsInCarIntent enableHeating enableCooling enableMassage seat level relativeLevelSetting carName =
  sendOwnedMessage inSetSeatSettingsInCarIntent initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector (toNSNumber enableHeating) (toNSNumber enableCooling) (toNSNumber enableMassage) seat (toNSNumber level) relativeLevelSetting (toINSpeakableString carName)

-- | @- initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting :: (IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent, IsNSNumber enableHeating, IsNSNumber enableCooling, IsNSNumber enableMassage, IsNSNumber level) => inSetSeatSettingsInCarIntent -> enableHeating -> enableCooling -> enableMassage -> INCarSeat -> level -> INRelativeSetting -> IO (Id INSetSeatSettingsInCarIntent)
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting inSetSeatSettingsInCarIntent enableHeating enableCooling enableMassage seat level relativeLevelSetting =
  sendOwnedMessage inSetSeatSettingsInCarIntent initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector (toNSNumber enableHeating) (toNSNumber enableCooling) (toNSNumber enableMassage) seat (toNSNumber level) relativeLevelSetting

-- | @- enableHeating@
enableHeating :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
enableHeating inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent enableHeatingSelector

-- | @- enableCooling@
enableCooling :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
enableCooling inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent enableCoolingSelector

-- | @- enableMassage@
enableMassage :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
enableMassage inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent enableMassageSelector

-- | @- seat@
seat :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO INCarSeat
seat inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent seatSelector

-- | @- level@
level :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
level inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent levelSelector

-- | @- relativeLevelSetting@
relativeLevelSetting :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO INRelativeSetting
relativeLevelSetting inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent relativeLevelSettingSelector

-- | @- carName@
carName :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id INSpeakableString)
carName inSetSeatSettingsInCarIntent =
  sendMessage inSetSeatSettingsInCarIntent carNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, INCarSeat, Id NSNumber, INRelativeSetting, Id INSpeakableString] (Id INSetSeatSettingsInCarIntent)
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector = mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:"

-- | @Selector@ for @initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, INCarSeat, Id NSNumber, INRelativeSetting] (Id INSetSeatSettingsInCarIntent)
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector = mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:"

-- | @Selector@ for @enableHeating@
enableHeatingSelector :: Selector '[] (Id NSNumber)
enableHeatingSelector = mkSelector "enableHeating"

-- | @Selector@ for @enableCooling@
enableCoolingSelector :: Selector '[] (Id NSNumber)
enableCoolingSelector = mkSelector "enableCooling"

-- | @Selector@ for @enableMassage@
enableMassageSelector :: Selector '[] (Id NSNumber)
enableMassageSelector = mkSelector "enableMassage"

-- | @Selector@ for @seat@
seatSelector :: Selector '[] INCarSeat
seatSelector = mkSelector "seat"

-- | @Selector@ for @level@
levelSelector :: Selector '[] (Id NSNumber)
levelSelector = mkSelector "level"

-- | @Selector@ for @relativeLevelSetting@
relativeLevelSettingSelector :: Selector '[] INRelativeSetting
relativeLevelSettingSelector = mkSelector "relativeLevelSetting"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

