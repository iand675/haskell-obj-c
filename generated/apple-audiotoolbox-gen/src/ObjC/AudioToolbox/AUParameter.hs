{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , stringFromValue
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
  , addressSelector
  , dependentParametersSelector
  , flagsSelector
  , maxValueSelector
  , minValueSelector
  , setValueSelector
  , setValue_originatorSelector
  , setValue_originator_atHostTimeSelector
  , setValue_originator_atHostTime_eventTypeSelector
  , stringFromValueSelector
  , unitNameSelector
  , unitSelector
  , valueFromStringSelector
  , valueSelector
  , valueStringsSelector

  -- * Enum types
  , AUParameterAutomationEventType(AUParameterAutomationEventType)
  , pattern AUParameterAutomationEventTypeValue
  , pattern AUParameterAutomationEventTypeTouch
  , pattern AUParameterAutomationEventTypeRelease
  , AudioUnitParameterOptions(AudioUnitParameterOptions)
  , pattern KAudioUnitParameterFlag_CFNameRelease
  , pattern KAudioUnitParameterFlag_OmitFromPresets
  , pattern KAudioUnitParameterFlag_PlotHistory
  , pattern KAudioUnitParameterFlag_MeterReadOnly
  , pattern KAudioUnitParameterFlag_DisplayMask
  , pattern KAudioUnitParameterFlag_DisplaySquareRoot
  , pattern KAudioUnitParameterFlag_DisplaySquared
  , pattern KAudioUnitParameterFlag_DisplayCubed
  , pattern KAudioUnitParameterFlag_DisplayCubeRoot
  , pattern KAudioUnitParameterFlag_DisplayExponential
  , pattern KAudioUnitParameterFlag_HasClump
  , pattern KAudioUnitParameterFlag_ValuesHaveStrings
  , pattern KAudioUnitParameterFlag_DisplayLogarithmic
  , pattern KAudioUnitParameterFlag_IsHighResolution
  , pattern KAudioUnitParameterFlag_NonRealTime
  , pattern KAudioUnitParameterFlag_CanRamp
  , pattern KAudioUnitParameterFlag_ExpertMode
  , pattern KAudioUnitParameterFlag_HasCFNameString
  , pattern KAudioUnitParameterFlag_IsGlobalMeta
  , pattern KAudioUnitParameterFlag_IsElementMeta
  , pattern KAudioUnitParameterFlag_IsReadable
  , pattern KAudioUnitParameterFlag_IsWritable
  , AudioUnitParameterUnit(AudioUnitParameterUnit)
  , pattern KAudioUnitParameterUnit_Generic
  , pattern KAudioUnitParameterUnit_Indexed
  , pattern KAudioUnitParameterUnit_Boolean
  , pattern KAudioUnitParameterUnit_Percent
  , pattern KAudioUnitParameterUnit_Seconds
  , pattern KAudioUnitParameterUnit_SampleFrames
  , pattern KAudioUnitParameterUnit_Phase
  , pattern KAudioUnitParameterUnit_Rate
  , pattern KAudioUnitParameterUnit_Hertz
  , pattern KAudioUnitParameterUnit_Cents
  , pattern KAudioUnitParameterUnit_RelativeSemiTones
  , pattern KAudioUnitParameterUnit_MIDINoteNumber
  , pattern KAudioUnitParameterUnit_MIDIController
  , pattern KAudioUnitParameterUnit_Decibels
  , pattern KAudioUnitParameterUnit_LinearGain
  , pattern KAudioUnitParameterUnit_Degrees
  , pattern KAudioUnitParameterUnit_EqualPowerCrossfade
  , pattern KAudioUnitParameterUnit_MixerFaderCurve1
  , pattern KAudioUnitParameterUnit_Pan
  , pattern KAudioUnitParameterUnit_Meters
  , pattern KAudioUnitParameterUnit_AbsoluteCents
  , pattern KAudioUnitParameterUnit_Octaves
  , pattern KAudioUnitParameterUnit_BPM
  , pattern KAudioUnitParameterUnit_Beats
  , pattern KAudioUnitParameterUnit_Milliseconds
  , pattern KAudioUnitParameterUnit_Ratio
  , pattern KAudioUnitParameterUnit_CustomUnit
  , pattern KAudioUnitParameterUnit_MIDI2Controller

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
setValue_originator auParameter value originator =
  sendMessage auParameter setValue_originatorSelector value originator

-- | Convenience for setValue:originator:atHostTime:eventType:
--
-- Bridged to the v2 function AudioUnitSetParameter.
--
-- ObjC selector: @- setValue:originator:atHostTime:@
setValue_originator_atHostTime :: IsAUParameter auParameter => auParameter -> CFloat -> RawId -> CULong -> IO ()
setValue_originator_atHostTime auParameter value originator hostTime =
  sendMessage auParameter setValue_originator_atHostTimeSelector value originator hostTime

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
setValue_originator_atHostTime_eventType auParameter value originator hostTime eventType =
  sendMessage auParameter setValue_originator_atHostTime_eventTypeSelector value originator hostTime eventType

-- | Get a textual representation of a value for the parameter. Use value==nil to use the		   current value. Bridged to the v2 property kAudioUnitProperty_ParameterStringFromValue.
--
-- This is currently only supported for parameters whose flags include		kAudioUnitParameterFlag_ValuesHaveStrings.
--
-- ObjC selector: @- stringFromValue:@
stringFromValue :: IsAUParameter auParameter => auParameter -> Const RawId -> IO (Id NSString)
stringFromValue auParameter value =
  sendMessage auParameter stringFromValueSelector value

-- | Convert a textual representation of a value to a numeric one.
--
-- This is currently only supported for parameters whose flags include		kAudioUnitParameterFlag_ValuesHaveStrings.
--
-- ObjC selector: @- valueFromString:@
valueFromString :: (IsAUParameter auParameter, IsNSString string) => auParameter -> string -> IO CFloat
valueFromString auParameter string =
  sendMessage auParameter valueFromStringSelector (toNSString string)

-- | The parameter's minimum value.
--
-- ObjC selector: @- minValue@
minValue :: IsAUParameter auParameter => auParameter -> IO CFloat
minValue auParameter =
  sendMessage auParameter minValueSelector

-- | The parameter's maximum value.
--
-- ObjC selector: @- maxValue@
maxValue :: IsAUParameter auParameter => auParameter -> IO CFloat
maxValue auParameter =
  sendMessage auParameter maxValueSelector

-- | The parameter's unit of measurement.
--
-- ObjC selector: @- unit@
unit :: IsAUParameter auParameter => auParameter -> IO AudioUnitParameterUnit
unit auParameter =
  sendMessage auParameter unitSelector

-- | A localized name for the parameter's unit. Supplied by the AU if kAudioUnitParameterUnit_CustomUnit; else by the framework.
--
-- ObjC selector: @- unitName@
unitName :: IsAUParameter auParameter => auParameter -> IO (Id NSString)
unitName auParameter =
  sendMessage auParameter unitNameSelector

-- | Various details of the parameter.
--
-- ObjC selector: @- flags@
flags :: IsAUParameter auParameter => auParameter -> IO AudioUnitParameterOptions
flags auParameter =
  sendMessage auParameter flagsSelector

-- | The parameter's address.
--
-- ObjC selector: @- address@
address :: IsAUParameter auParameter => auParameter -> IO CULong
address auParameter =
  sendMessage auParameter addressSelector

-- | For parameters with kAudioUnitParameterUnit_Indexed, localized strings corresponding	to the values.
--
-- ObjC selector: @- valueStrings@
valueStrings :: IsAUParameter auParameter => auParameter -> IO (Id NSArray)
valueStrings auParameter =
  sendMessage auParameter valueStringsSelector

-- | Parameters whose values may change as a side effect of this parameter's value				changing.
--
-- Each array value is an NSNumber representing AUParameterAddress.
--
-- ObjC selector: @- dependentParameters@
dependentParameters :: IsAUParameter auParameter => auParameter -> IO (Id NSArray)
dependentParameters auParameter =
  sendMessage auParameter dependentParametersSelector

-- | The parameter's current value.
--
-- ObjC selector: @- value@
value :: IsAUParameter auParameter => auParameter -> IO CFloat
value auParameter =
  sendMessage auParameter valueSelector

-- | The parameter's current value.
--
-- ObjC selector: @- setValue:@
setValue :: IsAUParameter auParameter => auParameter -> CFloat -> IO ()
setValue auParameter value =
  sendMessage auParameter setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValue:originator:@
setValue_originatorSelector :: Selector '[CFloat, RawId] ()
setValue_originatorSelector = mkSelector "setValue:originator:"

-- | @Selector@ for @setValue:originator:atHostTime:@
setValue_originator_atHostTimeSelector :: Selector '[CFloat, RawId, CULong] ()
setValue_originator_atHostTimeSelector = mkSelector "setValue:originator:atHostTime:"

-- | @Selector@ for @setValue:originator:atHostTime:eventType:@
setValue_originator_atHostTime_eventTypeSelector :: Selector '[CFloat, RawId, CULong, AUParameterAutomationEventType] ()
setValue_originator_atHostTime_eventTypeSelector = mkSelector "setValue:originator:atHostTime:eventType:"

-- | @Selector@ for @stringFromValue:@
stringFromValueSelector :: Selector '[Const RawId] (Id NSString)
stringFromValueSelector = mkSelector "stringFromValue:"

-- | @Selector@ for @valueFromString:@
valueFromStringSelector :: Selector '[Id NSString] CFloat
valueFromStringSelector = mkSelector "valueFromString:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CFloat
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CFloat
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @unit@
unitSelector :: Selector '[] AudioUnitParameterUnit
unitSelector = mkSelector "unit"

-- | @Selector@ for @unitName@
unitNameSelector :: Selector '[] (Id NSString)
unitNameSelector = mkSelector "unitName"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] AudioUnitParameterOptions
flagsSelector = mkSelector "flags"

-- | @Selector@ for @address@
addressSelector :: Selector '[] CULong
addressSelector = mkSelector "address"

-- | @Selector@ for @valueStrings@
valueStringsSelector :: Selector '[] (Id NSArray)
valueStringsSelector = mkSelector "valueStrings"

-- | @Selector@ for @dependentParameters@
dependentParametersSelector :: Selector '[] (Id NSArray)
dependentParametersSelector = mkSelector "dependentParameters"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CFloat
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CFloat] ()
setValueSelector = mkSelector "setValue:"

