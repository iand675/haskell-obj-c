{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSliderTouchBarItem@.
module ObjC.AppKit.NSSliderTouchBarItem
  ( NSSliderTouchBarItem
  , IsNSSliderTouchBarItem(..)
  , view
  , slider
  , setSlider
  , doubleValue
  , setDoubleValue
  , minimumSliderWidth
  , setMinimumSliderWidth
  , maximumSliderWidth
  , setMaximumSliderWidth
  , label
  , setLabel
  , minimumValueAccessory
  , setMinimumValueAccessory
  , maximumValueAccessory
  , setMaximumValueAccessory
  , valueAccessoryWidth
  , setValueAccessoryWidth
  , target
  , setTarget
  , action
  , setAction
  , customizationLabel
  , setCustomizationLabel
  , viewSelector
  , sliderSelector
  , setSliderSelector
  , doubleValueSelector
  , setDoubleValueSelector
  , minimumSliderWidthSelector
  , setMinimumSliderWidthSelector
  , maximumSliderWidthSelector
  , setMaximumSliderWidthSelector
  , labelSelector
  , setLabelSelector
  , minimumValueAccessorySelector
  , setMinimumValueAccessorySelector
  , maximumValueAccessorySelector
  , setMaximumValueAccessorySelector
  , valueAccessoryWidthSelector
  , setValueAccessoryWidthSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- view@
view :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSView)
view nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The slider displayed by the bar item. It is automatically created, but can be set to a custom subclass. doubleValue, minValue, maxValue, etc can all be read and set through the slider.
--
-- ObjC selector: @- slider@
slider :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSSlider)
slider nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "slider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The slider displayed by the bar item. It is automatically created, but can be set to a custom subclass. doubleValue, minValue, maxValue, etc can all be read and set through the slider.
--
-- ObjC selector: @- setSlider:@
setSlider :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSSlider value) => nsSliderTouchBarItem -> value -> IO ()
setSlider nsSliderTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSliderTouchBarItem (mkSelector "setSlider:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The double value of the control
--
-- ObjC selector: @- doubleValue@
doubleValue :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
doubleValue nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "doubleValue") retCDouble []

-- | The double value of the control
--
-- ObjC selector: @- setDoubleValue:@
setDoubleValue :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setDoubleValue nsSliderTouchBarItem  value =
    sendMsg nsSliderTouchBarItem (mkSelector "setDoubleValue:") retVoid [argCDouble value]

-- | The width boundaries of the slider track of this item. The system defines the default minimum. The maximum defaults to MAXFLOAT
--
-- ObjC selector: @- minimumSliderWidth@
minimumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
minimumSliderWidth nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "minimumSliderWidth") retCDouble []

-- | The width boundaries of the slider track of this item. The system defines the default minimum. The maximum defaults to MAXFLOAT
--
-- ObjC selector: @- setMinimumSliderWidth:@
setMinimumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setMinimumSliderWidth nsSliderTouchBarItem  value =
    sendMsg nsSliderTouchBarItem (mkSelector "setMinimumSliderWidth:") retVoid [argCDouble value]

-- | @- maximumSliderWidth@
maximumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
maximumSliderWidth nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "maximumSliderWidth") retCDouble []

-- | @- setMaximumSliderWidth:@
setMaximumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setMaximumSliderWidth nsSliderTouchBarItem  value =
    sendMsg nsSliderTouchBarItem (mkSelector "setMaximumSliderWidth:") retVoid [argCDouble value]

-- | The text label displayed along with the slider. If set to nil, the label will not have space reserved in the item.
--
-- ObjC selector: @- label@
label :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSString)
label nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The text label displayed along with the slider. If set to nil, the label will not have space reserved in the item.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSString value) => nsSliderTouchBarItem -> value -> IO ()
setLabel nsSliderTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSliderTouchBarItem (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory that appears on the end of the slider with the minimum value
--
-- ObjC selector: @- minimumValueAccessory@
minimumValueAccessory :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSSliderAccessory)
minimumValueAccessory nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "minimumValueAccessory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory that appears on the end of the slider with the minimum value
--
-- ObjC selector: @- setMinimumValueAccessory:@
setMinimumValueAccessory :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSSliderAccessory value) => nsSliderTouchBarItem -> value -> IO ()
setMinimumValueAccessory nsSliderTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSliderTouchBarItem (mkSelector "setMinimumValueAccessory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory that appears on the end of the slider with the maximum value
--
-- ObjC selector: @- maximumValueAccessory@
maximumValueAccessory :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSSliderAccessory)
maximumValueAccessory nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "maximumValueAccessory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory that appears on the end of the slider with the maximum value
--
-- ObjC selector: @- setMaximumValueAccessory:@
setMaximumValueAccessory :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSSliderAccessory value) => nsSliderTouchBarItem -> value -> IO ()
setMaximumValueAccessory nsSliderTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSliderTouchBarItem (mkSelector "setMaximumValueAccessory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The width of the value accessories. Defaults to @.default@, but can be set to @.wide@ or a custom value.
--
-- ObjC selector: @- valueAccessoryWidth@
valueAccessoryWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
valueAccessoryWidth nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "valueAccessoryWidth") retCDouble []

-- | The width of the value accessories. Defaults to @.default@, but can be set to @.wide@ or a custom value.
--
-- ObjC selector: @- setValueAccessoryWidth:@
setValueAccessoryWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setValueAccessoryWidth nsSliderTouchBarItem  value =
    sendMsg nsSliderTouchBarItem (mkSelector "setValueAccessoryWidth:") retVoid [argCDouble value]

-- | The target of the item, notified when the slider or accessories receive user interaction.
--
-- ObjC selector: @- target@
target :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO RawId
target nsSliderTouchBarItem  =
    fmap (RawId . castPtr) $ sendMsg nsSliderTouchBarItem (mkSelector "target") (retPtr retVoid) []

-- | The target of the item, notified when the slider or accessories receive user interaction.
--
-- ObjC selector: @- setTarget:@
setTarget :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> RawId -> IO ()
setTarget nsSliderTouchBarItem  value =
    sendMsg nsSliderTouchBarItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The action of the item, called when the slider or accessories receive user interaction.
--
-- ObjC selector: @- action@
action :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO Selector
action nsSliderTouchBarItem  =
    fmap (Selector . castPtr) $ sendMsg nsSliderTouchBarItem (mkSelector "action") (retPtr retVoid) []

-- | The action of the item, called when the slider or accessories receive user interaction.
--
-- ObjC selector: @- setAction:@
setAction :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> Selector -> IO ()
setAction nsSliderTouchBarItem  value =
    sendMsg nsSliderTouchBarItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSString)
customizationLabel nsSliderTouchBarItem  =
    sendMsg nsSliderTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSString value) => nsSliderTouchBarItem -> value -> IO ()
setCustomizationLabel nsSliderTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSliderTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @slider@
sliderSelector :: Selector
sliderSelector = mkSelector "slider"

-- | @Selector@ for @setSlider:@
setSliderSelector :: Selector
setSliderSelector = mkSelector "setSlider:"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @setDoubleValue:@
setDoubleValueSelector :: Selector
setDoubleValueSelector = mkSelector "setDoubleValue:"

-- | @Selector@ for @minimumSliderWidth@
minimumSliderWidthSelector :: Selector
minimumSliderWidthSelector = mkSelector "minimumSliderWidth"

-- | @Selector@ for @setMinimumSliderWidth:@
setMinimumSliderWidthSelector :: Selector
setMinimumSliderWidthSelector = mkSelector "setMinimumSliderWidth:"

-- | @Selector@ for @maximumSliderWidth@
maximumSliderWidthSelector :: Selector
maximumSliderWidthSelector = mkSelector "maximumSliderWidth"

-- | @Selector@ for @setMaximumSliderWidth:@
setMaximumSliderWidthSelector :: Selector
setMaximumSliderWidthSelector = mkSelector "setMaximumSliderWidth:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @minimumValueAccessory@
minimumValueAccessorySelector :: Selector
minimumValueAccessorySelector = mkSelector "minimumValueAccessory"

-- | @Selector@ for @setMinimumValueAccessory:@
setMinimumValueAccessorySelector :: Selector
setMinimumValueAccessorySelector = mkSelector "setMinimumValueAccessory:"

-- | @Selector@ for @maximumValueAccessory@
maximumValueAccessorySelector :: Selector
maximumValueAccessorySelector = mkSelector "maximumValueAccessory"

-- | @Selector@ for @setMaximumValueAccessory:@
setMaximumValueAccessorySelector :: Selector
setMaximumValueAccessorySelector = mkSelector "setMaximumValueAccessory:"

-- | @Selector@ for @valueAccessoryWidth@
valueAccessoryWidthSelector :: Selector
valueAccessoryWidthSelector = mkSelector "valueAccessoryWidth"

-- | @Selector@ for @setValueAccessoryWidth:@
setValueAccessoryWidthSelector :: Selector
setValueAccessoryWidthSelector = mkSelector "setValueAccessoryWidth:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

