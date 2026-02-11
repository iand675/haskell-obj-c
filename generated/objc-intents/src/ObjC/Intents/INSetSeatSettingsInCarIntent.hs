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
  , seat
  , relativeLevelSetting
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector
  , initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector
  , seatSelector
  , relativeLevelSettingSelector

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

-- | @- seat@
seat :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO INCarSeat
seat inSetSeatSettingsInCarIntent  =
  fmap (coerce :: CLong -> INCarSeat) $ sendMsg inSetSeatSettingsInCarIntent (mkSelector "seat") retCLong []

-- | @- relativeLevelSetting@
relativeLevelSetting :: IsINSetSeatSettingsInCarIntent inSetSeatSettingsInCarIntent => inSetSeatSettingsInCarIntent -> IO INRelativeSetting
relativeLevelSetting inSetSeatSettingsInCarIntent  =
  fmap (coerce :: CLong -> INRelativeSetting) $ sendMsg inSetSeatSettingsInCarIntent (mkSelector "relativeLevelSetting") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector :: Selector
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSetting_carNameSelector = mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:carName:"

-- | @Selector@ for @initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:@
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector :: Selector
initWithEnableHeating_enableCooling_enableMassage_seat_level_relativeLevelSettingSelector = mkSelector "initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:"

-- | @Selector@ for @seat@
seatSelector :: Selector
seatSelector = mkSelector "seat"

-- | @Selector@ for @relativeLevelSetting@
relativeLevelSettingSelector :: Selector
relativeLevelSettingSelector = mkSelector "relativeLevelSetting"

