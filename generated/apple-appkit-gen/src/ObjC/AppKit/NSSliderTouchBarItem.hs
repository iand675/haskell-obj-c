{-# LANGUAGE DataKinds #-}
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
  , actionSelector
  , customizationLabelSelector
  , doubleValueSelector
  , labelSelector
  , maximumSliderWidthSelector
  , maximumValueAccessorySelector
  , minimumSliderWidthSelector
  , minimumValueAccessorySelector
  , setActionSelector
  , setCustomizationLabelSelector
  , setDoubleValueSelector
  , setLabelSelector
  , setMaximumSliderWidthSelector
  , setMaximumValueAccessorySelector
  , setMinimumSliderWidthSelector
  , setMinimumValueAccessorySelector
  , setSliderSelector
  , setTargetSelector
  , setValueAccessoryWidthSelector
  , sliderSelector
  , targetSelector
  , valueAccessoryWidthSelector
  , viewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- view@
view :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSView)
view nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem viewSelector

-- | The slider displayed by the bar item. It is automatically created, but can be set to a custom subclass. doubleValue, minValue, maxValue, etc can all be read and set through the slider.
--
-- ObjC selector: @- slider@
slider :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSSlider)
slider nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem sliderSelector

-- | The slider displayed by the bar item. It is automatically created, but can be set to a custom subclass. doubleValue, minValue, maxValue, etc can all be read and set through the slider.
--
-- ObjC selector: @- setSlider:@
setSlider :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSSlider value) => nsSliderTouchBarItem -> value -> IO ()
setSlider nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setSliderSelector (toNSSlider value)

-- | The double value of the control
--
-- ObjC selector: @- doubleValue@
doubleValue :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
doubleValue nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem doubleValueSelector

-- | The double value of the control
--
-- ObjC selector: @- setDoubleValue:@
setDoubleValue :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setDoubleValue nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setDoubleValueSelector value

-- | The width boundaries of the slider track of this item. The system defines the default minimum. The maximum defaults to MAXFLOAT
--
-- ObjC selector: @- minimumSliderWidth@
minimumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
minimumSliderWidth nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem minimumSliderWidthSelector

-- | The width boundaries of the slider track of this item. The system defines the default minimum. The maximum defaults to MAXFLOAT
--
-- ObjC selector: @- setMinimumSliderWidth:@
setMinimumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setMinimumSliderWidth nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setMinimumSliderWidthSelector value

-- | @- maximumSliderWidth@
maximumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
maximumSliderWidth nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem maximumSliderWidthSelector

-- | @- setMaximumSliderWidth:@
setMaximumSliderWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setMaximumSliderWidth nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setMaximumSliderWidthSelector value

-- | The text label displayed along with the slider. If set to nil, the label will not have space reserved in the item.
--
-- ObjC selector: @- label@
label :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSString)
label nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem labelSelector

-- | The text label displayed along with the slider. If set to nil, the label will not have space reserved in the item.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSString value) => nsSliderTouchBarItem -> value -> IO ()
setLabel nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setLabelSelector (toNSString value)

-- | The accessory that appears on the end of the slider with the minimum value
--
-- ObjC selector: @- minimumValueAccessory@
minimumValueAccessory :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSSliderAccessory)
minimumValueAccessory nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem minimumValueAccessorySelector

-- | The accessory that appears on the end of the slider with the minimum value
--
-- ObjC selector: @- setMinimumValueAccessory:@
setMinimumValueAccessory :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSSliderAccessory value) => nsSliderTouchBarItem -> value -> IO ()
setMinimumValueAccessory nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setMinimumValueAccessorySelector (toNSSliderAccessory value)

-- | The accessory that appears on the end of the slider with the maximum value
--
-- ObjC selector: @- maximumValueAccessory@
maximumValueAccessory :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSSliderAccessory)
maximumValueAccessory nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem maximumValueAccessorySelector

-- | The accessory that appears on the end of the slider with the maximum value
--
-- ObjC selector: @- setMaximumValueAccessory:@
setMaximumValueAccessory :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSSliderAccessory value) => nsSliderTouchBarItem -> value -> IO ()
setMaximumValueAccessory nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setMaximumValueAccessorySelector (toNSSliderAccessory value)

-- | The width of the value accessories. Defaults to @.default@, but can be set to @.wide@ or a custom value.
--
-- ObjC selector: @- valueAccessoryWidth@
valueAccessoryWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO CDouble
valueAccessoryWidth nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem valueAccessoryWidthSelector

-- | The width of the value accessories. Defaults to @.default@, but can be set to @.wide@ or a custom value.
--
-- ObjC selector: @- setValueAccessoryWidth:@
setValueAccessoryWidth :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> CDouble -> IO ()
setValueAccessoryWidth nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setValueAccessoryWidthSelector value

-- | The target of the item, notified when the slider or accessories receive user interaction.
--
-- ObjC selector: @- target@
target :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO RawId
target nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem targetSelector

-- | The target of the item, notified when the slider or accessories receive user interaction.
--
-- ObjC selector: @- setTarget:@
setTarget :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> RawId -> IO ()
setTarget nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setTargetSelector value

-- | The action of the item, called when the slider or accessories receive user interaction.
--
-- ObjC selector: @- action@
action :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO Sel
action nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem actionSelector

-- | The action of the item, called when the slider or accessories receive user interaction.
--
-- ObjC selector: @- setAction:@
setAction :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> Sel -> IO ()
setAction nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setActionSelector value

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSSliderTouchBarItem nsSliderTouchBarItem => nsSliderTouchBarItem -> IO (Id NSString)
customizationLabel nsSliderTouchBarItem =
  sendMessage nsSliderTouchBarItem customizationLabelSelector

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSSliderTouchBarItem nsSliderTouchBarItem, IsNSString value) => nsSliderTouchBarItem -> value -> IO ()
setCustomizationLabel nsSliderTouchBarItem value =
  sendMessage nsSliderTouchBarItem setCustomizationLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @slider@
sliderSelector :: Selector '[] (Id NSSlider)
sliderSelector = mkSelector "slider"

-- | @Selector@ for @setSlider:@
setSliderSelector :: Selector '[Id NSSlider] ()
setSliderSelector = mkSelector "setSlider:"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @setDoubleValue:@
setDoubleValueSelector :: Selector '[CDouble] ()
setDoubleValueSelector = mkSelector "setDoubleValue:"

-- | @Selector@ for @minimumSliderWidth@
minimumSliderWidthSelector :: Selector '[] CDouble
minimumSliderWidthSelector = mkSelector "minimumSliderWidth"

-- | @Selector@ for @setMinimumSliderWidth:@
setMinimumSliderWidthSelector :: Selector '[CDouble] ()
setMinimumSliderWidthSelector = mkSelector "setMinimumSliderWidth:"

-- | @Selector@ for @maximumSliderWidth@
maximumSliderWidthSelector :: Selector '[] CDouble
maximumSliderWidthSelector = mkSelector "maximumSliderWidth"

-- | @Selector@ for @setMaximumSliderWidth:@
setMaximumSliderWidthSelector :: Selector '[CDouble] ()
setMaximumSliderWidthSelector = mkSelector "setMaximumSliderWidth:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @minimumValueAccessory@
minimumValueAccessorySelector :: Selector '[] (Id NSSliderAccessory)
minimumValueAccessorySelector = mkSelector "minimumValueAccessory"

-- | @Selector@ for @setMinimumValueAccessory:@
setMinimumValueAccessorySelector :: Selector '[Id NSSliderAccessory] ()
setMinimumValueAccessorySelector = mkSelector "setMinimumValueAccessory:"

-- | @Selector@ for @maximumValueAccessory@
maximumValueAccessorySelector :: Selector '[] (Id NSSliderAccessory)
maximumValueAccessorySelector = mkSelector "maximumValueAccessory"

-- | @Selector@ for @setMaximumValueAccessory:@
setMaximumValueAccessorySelector :: Selector '[Id NSSliderAccessory] ()
setMaximumValueAccessorySelector = mkSelector "setMaximumValueAccessory:"

-- | @Selector@ for @valueAccessoryWidth@
valueAccessoryWidthSelector :: Selector '[] CDouble
valueAccessoryWidthSelector = mkSelector "valueAccessoryWidth"

-- | @Selector@ for @setValueAccessoryWidth:@
setValueAccessoryWidthSelector :: Selector '[CDouble] ()
setValueAccessoryWidthSelector = mkSelector "setValueAccessoryWidth:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

