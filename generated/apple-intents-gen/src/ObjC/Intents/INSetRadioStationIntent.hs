{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRadioType_frequency_stationName_channel_presetNumberSelector
  , radioTypeSelector
  , frequencySelector
  , stationNameSelector
  , channelSelector
  , presetNumberSelector

  -- * Enum types
  , INRadioType(INRadioType)
  , pattern INRadioTypeUnknown
  , pattern INRadioTypeAM
  , pattern INRadioTypeFM
  , pattern INRadioTypeHD
  , pattern INRadioTypeSatellite
  , pattern INRadioTypeDAB

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

-- | @- initWithRadioType:frequency:stationName:channel:presetNumber:@
initWithRadioType_frequency_stationName_channel_presetNumber :: (IsINSetRadioStationIntent inSetRadioStationIntent, IsNSNumber frequency, IsNSString stationName, IsNSString channel, IsNSNumber presetNumber) => inSetRadioStationIntent -> INRadioType -> frequency -> stationName -> channel -> presetNumber -> IO (Id INSetRadioStationIntent)
initWithRadioType_frequency_stationName_channel_presetNumber inSetRadioStationIntent  radioType frequency stationName channel presetNumber =
  withObjCPtr frequency $ \raw_frequency ->
    withObjCPtr stationName $ \raw_stationName ->
      withObjCPtr channel $ \raw_channel ->
        withObjCPtr presetNumber $ \raw_presetNumber ->
            sendMsg inSetRadioStationIntent (mkSelector "initWithRadioType:frequency:stationName:channel:presetNumber:") (retPtr retVoid) [argCLong (coerce radioType), argPtr (castPtr raw_frequency :: Ptr ()), argPtr (castPtr raw_stationName :: Ptr ()), argPtr (castPtr raw_channel :: Ptr ()), argPtr (castPtr raw_presetNumber :: Ptr ())] >>= ownedObject . castPtr

-- | @- radioType@
radioType :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO INRadioType
radioType inSetRadioStationIntent  =
    fmap (coerce :: CLong -> INRadioType) $ sendMsg inSetRadioStationIntent (mkSelector "radioType") retCLong []

-- | @- frequency@
frequency :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSNumber)
frequency inSetRadioStationIntent  =
    sendMsg inSetRadioStationIntent (mkSelector "frequency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stationName@
stationName :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSString)
stationName inSetRadioStationIntent  =
    sendMsg inSetRadioStationIntent (mkSelector "stationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- channel@
channel :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSString)
channel inSetRadioStationIntent  =
    sendMsg inSetRadioStationIntent (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- presetNumber@
presetNumber :: IsINSetRadioStationIntent inSetRadioStationIntent => inSetRadioStationIntent -> IO (Id NSNumber)
presetNumber inSetRadioStationIntent  =
    sendMsg inSetRadioStationIntent (mkSelector "presetNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRadioType:frequency:stationName:channel:presetNumber:@
initWithRadioType_frequency_stationName_channel_presetNumberSelector :: Selector
initWithRadioType_frequency_stationName_channel_presetNumberSelector = mkSelector "initWithRadioType:frequency:stationName:channel:presetNumber:"

-- | @Selector@ for @radioType@
radioTypeSelector :: Selector
radioTypeSelector = mkSelector "radioType"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @stationName@
stationNameSelector :: Selector
stationNameSelector = mkSelector "stationName"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @presetNumber@
presetNumberSelector :: Selector
presetNumberSelector = mkSelector "presetNumber"

