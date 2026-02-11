{-# LANGUAGE PatternSynonyms #-}
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
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector
  , enableHeatingSelector
  , enableCoolingSelector
  , enableMassageSelector
  , seatSelector
  , levelSelector
  , relativeLevelSettingSelector
  , carNameSelector

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

-- | @- initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carName :: (IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent, IsNSNumber enableHeating, IsNSNumber enableCooling, IsNSNumber enableMassage, IsNSNumber level, IsINSpeakableString carName) => inSetSeatSettingsInCarIntent -> enableHeating -> enableCooling -> enableMassage -> INCarSeat -> level -> INRelativeSetting -> carName -> IO (Id INSetSeatSettingsInCarIntent)
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carName inSetSeatSettingsInCarIntent  enableHeating enableCooling enableMassage seat level relativeLevelSetting carName =
  withObjCPtr enableHeating $ \raw_enableHeating ->
    withObjCPtr enableCooling $ \raw_enableCooling ->
      withObjCPtr enableMassage $ \raw_enableMassage ->
        withObjCPtr level $ \raw_level ->
          withObjCPtr carName $ \raw_carName ->
              sendMsg inSetSeatSettingsInCarIntent (mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:") (retPtr retVoid) [argPtr (castPtr raw_enableHeating :: Ptr ()), argPtr (castPtr raw_enableCooling :: Ptr ()), argPtr (castPtr raw_enableMassage :: Ptr ()), argCLong (coerce seat), argPtr (castPtr raw_level :: Ptr ()), argCLong (coerce relativeLevelSetting), argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting :: (IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent, IsNSNumber enableHeating, IsNSNumber enableCooling, IsNSNumber enableMassage, IsNSNumber level) => inSetSeatSettingsInCarIntent -> enableHeating -> enableCooling -> enableMassage -> INCarSeat -> level -> INRelativeSetting -> IO (Id INSetSeatSettingsInCarIntent)
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting inSetSeatSettingsInCarIntent  enableHeating enableCooling enableMassage seat level relativeLevelSetting =
  withObjCPtr enableHeating $ \raw_enableHeating ->
    withObjCPtr enableCooling $ \raw_enableCooling ->
      withObjCPtr enableMassage $ \raw_enableMassage ->
        withObjCPtr level $ \raw_level ->
            sendMsg inSetSeatSettingsInCarIntent (mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:") (retPtr retVoid) [argPtr (castPtr raw_enableHeating :: Ptr ()), argPtr (castPtr raw_enableCooling :: Ptr ()), argPtr (castPtr raw_enableMassage :: Ptr ()), argCLong (coerce seat), argPtr (castPtr raw_level :: Ptr ()), argCLong (coerce relativeLevelSetting)] >>= ownedObject . castPtr

-- | @- enableHeating@
enableHeating :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
enableHeating inSetSeatSettingsInCarIntent  =
    sendMsg inSetSeatSettingsInCarIntent (mkSelector "enableHeating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enableCooling@
enableCooling :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
enableCooling inSetSeatSettingsInCarIntent  =
    sendMsg inSetSeatSettingsInCarIntent (mkSelector "enableCooling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enableMassage@
enableMassage :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
enableMassage inSetSeatSettingsInCarIntent  =
    sendMsg inSetSeatSettingsInCarIntent (mkSelector "enableMassage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- seat@
seat :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO INCarSeat
seat inSetSeatSettingsInCarIntent  =
    fmap (coerce :: CLong -> INCarSeat) $ sendMsg inSetSeatSettingsInCarIntent (mkSelector "seat") retCLong []

-- | @- level@
level :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id NSNumber)
level inSetSeatSettingsInCarIntent  =
    sendMsg inSetSeatSettingsInCarIntent (mkSelector "level") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relativeLevelSetting@
relativeLevelSetting :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO INRelativeSetting
relativeLevelSetting inSetSeatSettingsInCarIntent  =
    fmap (coerce :: CLong -> INRelativeSetting) $ sendMsg inSetSeatSettingsInCarIntent (mkSelector "relativeLevelSetting") retCLong []

-- | @- carName@
carName :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO (Id INSpeakableString)
carName inSetSeatSettingsInCarIntent  =
    sendMsg inSetSeatSettingsInCarIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector :: Selector
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector = mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:"

-- | @Selector@ for @initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector :: Selector
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector = mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:"

-- | @Selector@ for @enableHeating@
enableHeatingSelector :: Selector
enableHeatingSelector = mkSelector "enableHeating"

-- | @Selector@ for @enableCooling@
enableCoolingSelector :: Selector
enableCoolingSelector = mkSelector "enableCooling"

-- | @Selector@ for @enableMassage@
enableMassageSelector :: Selector
enableMassageSelector = mkSelector "enableMassage"

-- | @Selector@ for @seat@
seatSelector :: Selector
seatSelector = mkSelector "seat"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @relativeLevelSetting@
relativeLevelSettingSelector :: Selector
relativeLevelSettingSelector = mkSelector "relativeLevelSetting"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

