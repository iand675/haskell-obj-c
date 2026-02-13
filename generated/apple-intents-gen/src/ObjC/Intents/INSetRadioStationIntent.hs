{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetRadioStationIntent@.
module ObjC.Intents.INSetRadioStationIntent
  ( INSetRadioStationIntent
  , IsINSetRadioStationIntent(..)
  , initWithRadioType_frequency_stationName_channel_presetNumber
  , radioType
  , frequency
  , stationName
  , channel
  , presetNumber
  , channelSelector
  , frequencySelector
  , initWithRadioType_frequency_stationName_channel_presetNumberSelector
  , presetNumberSelector
  , radioTypeSelector
  , stationNameSelector

  -- * Enum types
  , INRadioType(INRadioType)
  , pattern INRadioTypeUnknown
  , pattern INRadioTypeAM
  , pattern INRadioTypeFM
  , pattern INRadioTypeHD
  , pattern INRadioTypeSatellite
  , pattern INRadioTypeDAB

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

-- | @- initWithRadioType:frequency:stationName:channel:presetNumber:@
initWithRadioType_frequency_stationName_channel_presetNumber :: (IsINSetRadioStationIntent inSetRadioStationIntent, IsNSNumber frequency, IsNSString stationName, IsNSString channel, IsNSNumber presetNumber) => inSetRadioStationIntent -> INRadioType -> frequency -> stationName -> channel -> presetNumber -> IO (Id INSetRadioStationIntent)
initWithRadioType_frequency_stationName_channel_presetNumber inSetRadioStationIntent radioType frequency stationName channel presetNumber =
  sendOwnedMessage inSetRadioStationIntent initWithRadioType_frequency_stationName_channel_presetNumberSelector radioType (toNSNumber frequency) (toNSString stationName) (toNSString channel) (toNSNumber presetNumber)

-- | @- radioType@
radioType :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO INRadioType
radioType inSetRadioStationIntent =
  sendMessage inSetRadioStationIntent radioTypeSelector

-- | @- frequency@
frequency :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSNumber)
frequency inSetRadioStationIntent =
  sendMessage inSetRadioStationIntent frequencySelector

-- | @- stationName@
stationName :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSString)
stationName inSetRadioStationIntent =
  sendMessage inSetRadioStationIntent stationNameSelector

-- | @- channel@
channel :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSString)
channel inSetRadioStationIntent =
  sendMessage inSetRadioStationIntent channelSelector

-- | @- presetNumber@
presetNumber :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSNumber)
presetNumber inSetRadioStationIntent =
  sendMessage inSetRadioStationIntent presetNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRadioType:frequency:stationName:channel:presetNumber:@
initWithRadioType_frequency_stationName_channel_presetNumberSelector :: Selector '[INRadioType, Id NSNumber, Id NSString, Id NSString, Id NSNumber] (Id INSetRadioStationIntent)
initWithRadioType_frequency_stationName_channel_presetNumberSelector = mkSelector "initWithRadioType:frequency:stationName:channel:presetNumber:"

-- | @Selector@ for @radioType@
radioTypeSelector :: Selector '[] INRadioType
radioTypeSelector = mkSelector "radioType"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] (Id NSNumber)
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @stationName@
stationNameSelector :: Selector '[] (Id NSString)
stationNameSelector = mkSelector "stationName"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] (Id NSString)
channelSelector = mkSelector "channel"

-- | @Selector@ for @presetNumber@
presetNumberSelector :: Selector '[] (Id NSNumber)
presetNumberSelector = mkSelector "presetNumber"

