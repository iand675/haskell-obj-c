{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSlider
--
-- An @AVCaptureControl@ for selecting a value from a bounded range of values.
--
-- @AVCaptureSlider@ is ideal when your control only needs a single float value. Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- Generated bindings for @AVCaptureSlider@.
module ObjC.AVFoundation.AVCaptureSlider
  ( AVCaptureSlider
  , IsAVCaptureSlider(..)
  , initWithLocalizedTitle_symbolName_minValue_maxValue
  , initWithLocalizedTitle_symbolName_minValue_maxValue_step
  , initWithLocalizedTitle_symbolName_values
  , setActionQueue_action
  , value
  , setValue
  , localizedValueFormat
  , setLocalizedValueFormat
  , prominentValues
  , setProminentValues
  , localizedTitle
  , symbolName
  , accessibilityIdentifier
  , setAccessibilityIdentifier
  , initWithLocalizedTitle_symbolName_minValue_maxValueSelector
  , initWithLocalizedTitle_symbolName_minValue_maxValue_stepSelector
  , initWithLocalizedTitle_symbolName_valuesSelector
  , setActionQueue_actionSelector
  , valueSelector
  , setValueSelector
  , localizedValueFormatSelector
  , setLocalizedValueFormatSelector
  , prominentValuesSelector
  , setProminentValuesSelector
  , localizedTitleSelector
  , symbolNameSelector
  , accessibilityIdentifierSelector
  , setAccessibilityIdentifierSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithLocalizedTitle:symbolName:minValue:maxValue:
--
-- Initializes an @AVCaptureSlider@ as a continuous slider between @minValue@ and @maxValue@.
--
-- @localizedTitle@ — A localized string that describes the slider's @action@.
--
-- @symbolName@ — The name of a symbol to represent the slider.
--
-- @minValue@ — The minimum value the slider can have. @minValue@ must be less than @maxValue@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- @maxValue@ — The maximum value the slider can have. @maxValue@ must be greater than @minValue@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- Returns: An @AVCaptureSlider@ instance as a continuous slider between @minValue@ and @maxValue@.
--
-- Continuous sliders are used when any value in the range @minValue...maxValue@ is supported.
--
-- ObjC selector: @- initWithLocalizedTitle:symbolName:minValue:maxValue:@
initWithLocalizedTitle_symbolName_minValue_maxValue :: (IsAVCaptureSlider avCaptureSlider, IsNSString localizedTitle, IsNSString symbolName) => avCaptureSlider -> localizedTitle -> symbolName -> CFloat -> CFloat -> IO (Id AVCaptureSlider)
initWithLocalizedTitle_symbolName_minValue_maxValue avCaptureSlider  localizedTitle symbolName minValue maxValue =
  withObjCPtr localizedTitle $ \raw_localizedTitle ->
    withObjCPtr symbolName $ \raw_symbolName ->
        sendMsg avCaptureSlider (mkSelector "initWithLocalizedTitle:symbolName:minValue:maxValue:") (retPtr retVoid) [argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_symbolName :: Ptr ()), argCFloat minValue, argCFloat maxValue] >>= ownedObject . castPtr

-- | initWithLocalizedTitle:symbolName:minValue:maxValue:step:
--
-- Initializes an @AVCaptureSlider@ as a discrete slider where the valid values are between @minValue@ and @maxValue@ with @step@ distance between each value.
--
-- @localizedTitle@ — A localized string that describes the slider's @action@.
--
-- @symbolName@ — The name of a symbol to represent the slider.
--
-- @minValue@ — The minimum value the slider can have. @minValue@ must be less than @maxValue@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- @maxValue@ — The maximum value the slider can have. @maxValue@ must be greater than @minValue@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- @step@ — The distance between each valid value. @step@ must be greater than 0, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- Returns: An @AVCaptureSlider@ instance as a discrete slider where the valid values are between @minValue@ and @maxValue@ with @step@ distance between each value.
--
-- Discrete sliders are used when only specific values are valid.
--
-- ObjC selector: @- initWithLocalizedTitle:symbolName:minValue:maxValue:step:@
initWithLocalizedTitle_symbolName_minValue_maxValue_step :: (IsAVCaptureSlider avCaptureSlider, IsNSString localizedTitle, IsNSString symbolName) => avCaptureSlider -> localizedTitle -> symbolName -> CFloat -> CFloat -> CFloat -> IO (Id AVCaptureSlider)
initWithLocalizedTitle_symbolName_minValue_maxValue_step avCaptureSlider  localizedTitle symbolName minValue maxValue step =
  withObjCPtr localizedTitle $ \raw_localizedTitle ->
    withObjCPtr symbolName $ \raw_symbolName ->
        sendMsg avCaptureSlider (mkSelector "initWithLocalizedTitle:symbolName:minValue:maxValue:step:") (retPtr retVoid) [argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_symbolName :: Ptr ()), argCFloat minValue, argCFloat maxValue, argCFloat step] >>= ownedObject . castPtr

-- | initWithLocalizedTitle:symbolName:values:
--
-- Initializes an @AVCaptureSlider@ as a discrete slider where @values@ contains the valid values.
--
-- @localizedTitle@ — A localized string that describes the slider's @action@.
--
-- @symbolName@ — The name of a symbol to represent the slider.
--
-- @values@ — The only values the slider can have.
--
-- Returns: An @AVCaptureSlider@ instance as a discrete slider where @values@ contains the valid values.
--
-- Discrete sliders are used when only specific values are valid.
--
-- ObjC selector: @- initWithLocalizedTitle:symbolName:values:@
initWithLocalizedTitle_symbolName_values :: (IsAVCaptureSlider avCaptureSlider, IsNSString localizedTitle, IsNSString symbolName, IsNSArray values) => avCaptureSlider -> localizedTitle -> symbolName -> values -> IO (Id AVCaptureSlider)
initWithLocalizedTitle_symbolName_values avCaptureSlider  localizedTitle symbolName values =
  withObjCPtr localizedTitle $ \raw_localizedTitle ->
    withObjCPtr symbolName $ \raw_symbolName ->
      withObjCPtr values $ \raw_values ->
          sendMsg avCaptureSlider (mkSelector "initWithLocalizedTitle:symbolName:values:") (retPtr retVoid) [argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_symbolName :: Ptr ()), argPtr (castPtr raw_values :: Ptr ())] >>= ownedObject . castPtr

-- | setActionQueue:action:
--
-- Configures the slider's @action@ which is called on @actionQueue@ whenever the value of the slider is changed.
--
-- @actionQueue@ — A queue for the @action@ to be called.
--
-- @action@ — An action called on @actionQueue@ whenever the value of the slider is changed.
--
-- Because the camera system may be independent from the main thread or `@, @action@ is always called on an internal @DispatchSerialQueue@ targeted at @actionQueue`.
--
-- If @action@ modifies a property of the camera system, @actionQueue@ must represent the same exclusive execution context as the camera system (see @isSameExclusiveExecutionContext@).
--
-- ObjC selector: @- setActionQueue:action:@
setActionQueue_action :: (IsAVCaptureSlider avCaptureSlider, IsNSObject actionQueue) => avCaptureSlider -> actionQueue -> Ptr () -> IO ()
setActionQueue_action avCaptureSlider  actionQueue action =
  withObjCPtr actionQueue $ \raw_actionQueue ->
      sendMsg avCaptureSlider (mkSelector "setActionQueue:action:") retVoid [argPtr (castPtr raw_actionQueue :: Ptr ()), argPtr (castPtr action :: Ptr ())]

-- | value
--
-- The current value of the slider.
--
-- Because the camera system may be independent from the main thread or `@, @value@ must be changed on @actionQueue@ – the queue provided to @setActionQueue:action:@. The default value is the slider's minimum value. A value may only be set if it is within the slider's minimum and maximum values, otherwise an @NSInvalidArgumentException` is thrown.
--
-- ObjC selector: @- value@
value :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> IO CFloat
value avCaptureSlider  =
    sendMsg avCaptureSlider (mkSelector "value") retCFloat []

-- | value
--
-- The current value of the slider.
--
-- Because the camera system may be independent from the main thread or `@, @value@ must be changed on @actionQueue@ – the queue provided to @setActionQueue:action:@. The default value is the slider's minimum value. A value may only be set if it is within the slider's minimum and maximum values, otherwise an @NSInvalidArgumentException` is thrown.
--
-- ObjC selector: @- setValue:@
setValue :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> CFloat -> IO ()
setValue avCaptureSlider  value =
    sendMsg avCaptureSlider (mkSelector "setValue:") retVoid [argCFloat value]

-- | localizedValueFormat
--
-- A localized string defining the presentation of the slider's value.
--
-- To modify the presentation of the slider's value, set @localizedValueFormat@ to a format string to display the slider's value with any annotation.
--
-- The format string may only contain `%\@@ and no other placeholders like @%d@, @%s`, etc. Invalid format strings will result in the value's default presentation.
--
-- Examples of valid format strings are:    - `%%%` for "40%"    - `%\@ fps` for "60 fps"    - `+ %\@` for "+ 20"
--
-- ObjC selector: @- localizedValueFormat@
localizedValueFormat :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> IO (Id NSString)
localizedValueFormat avCaptureSlider  =
    sendMsg avCaptureSlider (mkSelector "localizedValueFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedValueFormat
--
-- A localized string defining the presentation of the slider's value.
--
-- To modify the presentation of the slider's value, set @localizedValueFormat@ to a format string to display the slider's value with any annotation.
--
-- The format string may only contain `%\@@ and no other placeholders like @%d@, @%s`, etc. Invalid format strings will result in the value's default presentation.
--
-- Examples of valid format strings are:    - `%%%` for "40%"    - `%\@ fps` for "60 fps"    - `+ %\@` for "+ 20"
--
-- ObjC selector: @- setLocalizedValueFormat:@
setLocalizedValueFormat :: (IsAVCaptureSlider avCaptureSlider, IsNSString value) => avCaptureSlider -> value -> IO ()
setLocalizedValueFormat avCaptureSlider  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureSlider (mkSelector "setLocalizedValueFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | prominentValues
--
-- Values in this array may receive unique visual representations or behaviors.
--
-- ObjC selector: @- prominentValues@
prominentValues :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> IO (Id NSArray)
prominentValues avCaptureSlider  =
    sendMsg avCaptureSlider (mkSelector "prominentValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | prominentValues
--
-- Values in this array may receive unique visual representations or behaviors.
--
-- ObjC selector: @- setProminentValues:@
setProminentValues :: (IsAVCaptureSlider avCaptureSlider, IsNSArray value) => avCaptureSlider -> value -> IO ()
setProminentValues avCaptureSlider  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureSlider (mkSelector "setProminentValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | localizedTitle
--
-- A localized string that describes the slider's @action@.
--
-- ObjC selector: @- localizedTitle@
localizedTitle :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> IO (Id NSString)
localizedTitle avCaptureSlider  =
    sendMsg avCaptureSlider (mkSelector "localizedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | symbolName
--
-- The name of a symbol to represent the slider.
--
-- ObjC selector: @- symbolName@
symbolName :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> IO (Id NSString)
symbolName avCaptureSlider  =
    sendMsg avCaptureSlider (mkSelector "symbolName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | accessibilityIdentifier
--
-- A string that identifies the slider.
--
-- ObjC selector: @- accessibilityIdentifier@
accessibilityIdentifier :: IsAVCaptureSlider avCaptureSlider => avCaptureSlider -> IO (Id NSString)
accessibilityIdentifier avCaptureSlider  =
    sendMsg avCaptureSlider (mkSelector "accessibilityIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | accessibilityIdentifier
--
-- A string that identifies the slider.
--
-- ObjC selector: @- setAccessibilityIdentifier:@
setAccessibilityIdentifier :: (IsAVCaptureSlider avCaptureSlider, IsNSString value) => avCaptureSlider -> value -> IO ()
setAccessibilityIdentifier avCaptureSlider  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureSlider (mkSelector "setAccessibilityIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedTitle:symbolName:minValue:maxValue:@
initWithLocalizedTitle_symbolName_minValue_maxValueSelector :: Selector
initWithLocalizedTitle_symbolName_minValue_maxValueSelector = mkSelector "initWithLocalizedTitle:symbolName:minValue:maxValue:"

-- | @Selector@ for @initWithLocalizedTitle:symbolName:minValue:maxValue:step:@
initWithLocalizedTitle_symbolName_minValue_maxValue_stepSelector :: Selector
initWithLocalizedTitle_symbolName_minValue_maxValue_stepSelector = mkSelector "initWithLocalizedTitle:symbolName:minValue:maxValue:step:"

-- | @Selector@ for @initWithLocalizedTitle:symbolName:values:@
initWithLocalizedTitle_symbolName_valuesSelector :: Selector
initWithLocalizedTitle_symbolName_valuesSelector = mkSelector "initWithLocalizedTitle:symbolName:values:"

-- | @Selector@ for @setActionQueue:action:@
setActionQueue_actionSelector :: Selector
setActionQueue_actionSelector = mkSelector "setActionQueue:action:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @localizedValueFormat@
localizedValueFormatSelector :: Selector
localizedValueFormatSelector = mkSelector "localizedValueFormat"

-- | @Selector@ for @setLocalizedValueFormat:@
setLocalizedValueFormatSelector :: Selector
setLocalizedValueFormatSelector = mkSelector "setLocalizedValueFormat:"

-- | @Selector@ for @prominentValues@
prominentValuesSelector :: Selector
prominentValuesSelector = mkSelector "prominentValues"

-- | @Selector@ for @setProminentValues:@
setProminentValuesSelector :: Selector
setProminentValuesSelector = mkSelector "setProminentValues:"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @symbolName@
symbolNameSelector :: Selector
symbolNameSelector = mkSelector "symbolName"

-- | @Selector@ for @accessibilityIdentifier@
accessibilityIdentifierSelector :: Selector
accessibilityIdentifierSelector = mkSelector "accessibilityIdentifier"

-- | @Selector@ for @setAccessibilityIdentifier:@
setAccessibilityIdentifierSelector :: Selector
setAccessibilityIdentifierSelector = mkSelector "setAccessibilityIdentifier:"

