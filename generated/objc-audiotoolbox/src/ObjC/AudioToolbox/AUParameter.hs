{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUParameter
--
-- A node representing a single parameter.
--
-- Generated bindings for @AUParameter@.
module ObjC.AudioToolbox.AUParameter
  ( AUParameter
  , IsAUParameter(..)
  , setValue_originator
  , setValue_originator_atHostTime
  , setValue_originator_atHostTime_eventType
  , valueFromString
  , minValue
  , maxValue
  , unit
  , unitName
  , flags
  , address
  , valueStrings
  , dependentParameters
  , value
  , setValue
  , setValue_originatorSelector
  , setValue_originator_atHostTimeSelector
  , setValue_originator_atHostTime_eventTypeSelector
  , valueFromStringSelector
  , minValueSelector
  , maxValueSelector
  , unitSelector
  , unitNameSelector
  , flagsSelector
  , addressSelector
  , valueStringsSelector
  , dependentParametersSelector
  , valueSelector
  , setValueSelector

  -- * Enum types
  , AUParameterAutomationEventType(AUParameterAutomationEventType)
  , pattern AUParameterAutomationEventTypeValue
  , pattern AUParameterAutomationEventTypeTouch
  , pattern AUParameterAutomationEventTypeRelease

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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.AudioToolbox.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Set the parameter's value, avoiding redundant notifications to the originator.
--
-- Bridged to the v2 function AudioUnitSetParameter.
--
-- ObjC selector: @- setValue:originator:@
setValue_originator :: IsAUParameter auParameter => auParameter -> CFloat -> RawId -> IO ()
setValue_originator auParameter  value originator =
  sendMsg auParameter (mkSelector "setValue:originator:") retVoid [argCFloat (fromIntegral value), argPtr (castPtr (unRawId originator) :: Ptr ())]

-- | Convenience for setValue:originator:atHostTime:eventType:
--
-- Bridged to the v2 function AudioUnitSetParameter.
--
-- ObjC selector: @- setValue:originator:atHostTime:@
setValue_originator_atHostTime :: IsAUParameter auParameter => auParameter -> CFloat -> RawId -> CULong -> IO ()
setValue_originator_atHostTime auParameter  value originator hostTime =
  sendMsg auParameter (mkSelector "setValue:originator:atHostTime:") retVoid [argCFloat (fromIntegral value), argPtr (castPtr (unRawId originator) :: Ptr ()), argCULong (fromIntegral hostTime)]

-- | Set the parameter's value, preserving the host time of the gesture that initiated the			change, and associating an event type such as touch/release.
--
-- In general, this method should only be called from a user interface. It initiates a change		to a parameter in a way that captures the gesture such that it can be recorded later --		any AUParameterAutomationObservers will receive the host time and event type associated		with the parameter change.
--
-- From an audio playback engine, a host should schedule automated parameter changes through		AUAudioUnit's scheduleParameterBlock.
--
-- Bridged to the v2 function AudioUnitSetParameter.
--
-- ObjC selector: @- setValue:originator:atHostTime:eventType:@
setValue_originator_atHostTime_eventType :: IsAUParameter auParameter => auParameter -> CFloat -> RawId -> CULong -> AUParameterAutomationEventType -> IO ()
setValue_originator_atHostTime_eventType auParameter  value originator hostTime eventType =
  sendMsg auParameter (mkSelector "setValue:originator:atHostTime:eventType:") retVoid [argCFloat (fromIntegral value), argPtr (castPtr (unRawId originator) :: Ptr ()), argCULong (fromIntegral hostTime), argCUInt (coerce eventType)]

-- | Convert a textual representation of a value to a numeric one.
--
-- This is currently only supported for parameters whose flags include		kAudioUnitParameterFlag_ValuesHaveStrings.
--
-- ObjC selector: @- valueFromString:@
valueFromString :: (IsAUParameter auParameter, IsNSString string) => auParameter -> string -> IO CFloat
valueFromString auParameter  string =
withObjCPtr string $ \raw_string ->
    sendMsg auParameter (mkSelector "valueFromString:") retCFloat [argPtr (castPtr raw_string :: Ptr ())]

-- | The parameter's minimum value.
--
-- ObjC selector: @- minValue@
minValue :: IsAUParameter auParameter => auParameter -> IO CFloat
minValue auParameter  =
  sendMsg auParameter (mkSelector "minValue") retCFloat []

-- | The parameter's maximum value.
--
-- ObjC selector: @- maxValue@
maxValue :: IsAUParameter auParameter => auParameter -> IO CFloat
maxValue auParameter  =
  sendMsg auParameter (mkSelector "maxValue") retCFloat []

-- | The parameter's unit of measurement.
--
-- ObjC selector: @- unit@
unit :: IsAUParameter auParameter => auParameter -> IO AudioUnitParameterUnit
unit auParameter  =
  fmap (coerce :: CUInt -> AudioUnitParameterUnit) $ sendMsg auParameter (mkSelector "unit") retCUInt []

-- | A localized name for the parameter's unit. Supplied by the AU if kAudioUnitParameterUnit_CustomUnit; else by the framework.
--
-- ObjC selector: @- unitName@
unitName :: IsAUParameter auParameter => auParameter -> IO (Id NSString)
unitName auParameter  =
  sendMsg auParameter (mkSelector "unitName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Various details of the parameter.
--
-- ObjC selector: @- flags@
flags :: IsAUParameter auParameter => auParameter -> IO AudioUnitParameterOptions
flags auParameter  =
  fmap (coerce :: CUInt -> AudioUnitParameterOptions) $ sendMsg auParameter (mkSelector "flags") retCUInt []

-- | The parameter's address.
--
-- ObjC selector: @- address@
address :: IsAUParameter auParameter => auParameter -> IO CULong
address auParameter  =
  sendMsg auParameter (mkSelector "address") retCULong []

-- | For parameters with kAudioUnitParameterUnit_Indexed, localized strings corresponding	to the values.
--
-- ObjC selector: @- valueStrings@
valueStrings :: IsAUParameter auParameter => auParameter -> IO (Id NSArray)
valueStrings auParameter  =
  sendMsg auParameter (mkSelector "valueStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Parameters whose values may change as a side effect of this parameter's value				changing.
--
-- Each array value is an NSNumber representing AUParameterAddress.
--
-- ObjC selector: @- dependentParameters@
dependentParameters :: IsAUParameter auParameter => auParameter -> IO (Id NSArray)
dependentParameters auParameter  =
  sendMsg auParameter (mkSelector "dependentParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The parameter's current value.
--
-- ObjC selector: @- value@
value :: IsAUParameter auParameter => auParameter -> IO CFloat
value auParameter  =
  sendMsg auParameter (mkSelector "value") retCFloat []

-- | The parameter's current value.
--
-- ObjC selector: @- setValue:@
setValue :: IsAUParameter auParameter => auParameter -> CFloat -> IO ()
setValue auParameter  value =
  sendMsg auParameter (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValue:originator:@
setValue_originatorSelector :: Selector
setValue_originatorSelector = mkSelector "setValue:originator:"

-- | @Selector@ for @setValue:originator:atHostTime:@
setValue_originator_atHostTimeSelector :: Selector
setValue_originator_atHostTimeSelector = mkSelector "setValue:originator:atHostTime:"

-- | @Selector@ for @setValue:originator:atHostTime:eventType:@
setValue_originator_atHostTime_eventTypeSelector :: Selector
setValue_originator_atHostTime_eventTypeSelector = mkSelector "setValue:originator:atHostTime:eventType:"

-- | @Selector@ for @valueFromString:@
valueFromStringSelector :: Selector
valueFromStringSelector = mkSelector "valueFromString:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @unit@
unitSelector :: Selector
unitSelector = mkSelector "unit"

-- | @Selector@ for @unitName@
unitNameSelector :: Selector
unitNameSelector = mkSelector "unitName"

-- | @Selector@ for @flags@
flagsSelector :: Selector
flagsSelector = mkSelector "flags"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @valueStrings@
valueStringsSelector :: Selector
valueStringsSelector = mkSelector "valueStrings"

-- | @Selector@ for @dependentParameters@
dependentParametersSelector :: Selector
dependentParametersSelector = mkSelector "dependentParameters"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

