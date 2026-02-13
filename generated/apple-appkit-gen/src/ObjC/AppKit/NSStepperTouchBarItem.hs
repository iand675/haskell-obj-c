{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStepperTouchBarItem@.
module ObjC.AppKit.NSStepperTouchBarItem
  ( NSStepperTouchBarItem
  , IsNSStepperTouchBarItem(..)
  , stepperTouchBarItemWithIdentifier_formatter
  , stepperTouchBarItemWithIdentifier_drawingHandler
  , maxValue
  , setMaxValue
  , minValue
  , setMinValue
  , increment
  , setIncrement
  , value
  , setValue
  , target
  , setTarget
  , action
  , setAction
  , customizationLabel
  , setCustomizationLabel
  , actionSelector
  , customizationLabelSelector
  , incrementSelector
  , maxValueSelector
  , minValueSelector
  , setActionSelector
  , setCustomizationLabelSelector
  , setIncrementSelector
  , setMaxValueSelector
  , setMinValueSelector
  , setTargetSelector
  , setValueSelector
  , stepperTouchBarItemWithIdentifier_drawingHandlerSelector
  , stepperTouchBarItemWithIdentifier_formatterSelector
  , targetSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an @NSStepperTouchBarItem@ with a @formatter@ to display the stepper's value as text
--
-- @formatter@ — A formatter used to display a textual representation of the stepper's value
--
-- ObjC selector: @+ stepperTouchBarItemWithIdentifier:formatter:@
stepperTouchBarItemWithIdentifier_formatter :: (IsNSString identifier, IsNSFormatter formatter) => identifier -> formatter -> IO (Id NSStepperTouchBarItem)
stepperTouchBarItemWithIdentifier_formatter identifier formatter =
  do
    cls' <- getRequiredClass "NSStepperTouchBarItem"
    sendClassMessage cls' stepperTouchBarItemWithIdentifier_formatterSelector (toNSString identifier) (toNSFormatter formatter)

-- | Creates an @NSStepperTouchBarItem@ using the result of @drawingHandler@ to display the stepper's value as an image
--
-- @drawingHandler@ — A block that draws a graphical representation of the stepper's value in the specified rectangle. The coordinates of this rectangle are specified in points.
--
-- ObjC selector: @+ stepperTouchBarItemWithIdentifier:drawingHandler:@
stepperTouchBarItemWithIdentifier_drawingHandler :: IsNSString identifier => identifier -> Ptr () -> IO (Id NSStepperTouchBarItem)
stepperTouchBarItemWithIdentifier_drawingHandler identifier drawingHandler =
  do
    cls' <- getRequiredClass "NSStepperTouchBarItem"
    sendClassMessage cls' stepperTouchBarItemWithIdentifier_drawingHandlerSelector (toNSString identifier) drawingHandler

-- | The stepper's maximum value. The default is 59.0.
--
-- ObjC selector: @- maxValue@
maxValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
maxValue nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem maxValueSelector

-- | The stepper's maximum value. The default is 59.0.
--
-- ObjC selector: @- setMaxValue:@
setMaxValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setMaxValue nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setMaxValueSelector value

-- | The stepper's minimum value. The default is 0.0.
--
-- ObjC selector: @- minValue@
minValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
minValue nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem minValueSelector

-- | The stepper's minimum value. The default is 0.0.
--
-- ObjC selector: @- setMinValue:@
setMinValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setMinValue nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setMinValueSelector value

-- | The stepper's increment value. The default is 1.0.
--
-- ObjC selector: @- increment@
increment :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
increment nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem incrementSelector

-- | The stepper's increment value. The default is 1.0.
--
-- ObjC selector: @- setIncrement:@
setIncrement :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setIncrement nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setIncrementSelector value

-- | The current value of the stepper.
--
-- ObjC selector: @- value@
value :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
value nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem valueSelector

-- | The current value of the stepper.
--
-- ObjC selector: @- setValue:@
setValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setValue nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setValueSelector value

-- | The target object that receives action messages from the stepper.
--
-- ObjC selector: @- target@
target :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO RawId
target nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem targetSelector

-- | The target object that receives action messages from the stepper.
--
-- ObjC selector: @- setTarget:@
setTarget :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> RawId -> IO ()
setTarget nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setTargetSelector value

-- | The action-message selector associated with the stepper.
--
-- ObjC selector: @- action@
action :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO Sel
action nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem actionSelector

-- | The action-message selector associated with the stepper.
--
-- ObjC selector: @- setAction:@
setAction :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> Sel -> IO ()
setAction nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setActionSelector value

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO (Id NSString)
customizationLabel nsStepperTouchBarItem =
  sendMessage nsStepperTouchBarItem customizationLabelSelector

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSStepperTouchBarItem nsStepperTouchBarItem, IsNSString value) => nsStepperTouchBarItem -> value -> IO ()
setCustomizationLabel nsStepperTouchBarItem value =
  sendMessage nsStepperTouchBarItem setCustomizationLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepperTouchBarItemWithIdentifier:formatter:@
stepperTouchBarItemWithIdentifier_formatterSelector :: Selector '[Id NSString, Id NSFormatter] (Id NSStepperTouchBarItem)
stepperTouchBarItemWithIdentifier_formatterSelector = mkSelector "stepperTouchBarItemWithIdentifier:formatter:"

-- | @Selector@ for @stepperTouchBarItemWithIdentifier:drawingHandler:@
stepperTouchBarItemWithIdentifier_drawingHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSStepperTouchBarItem)
stepperTouchBarItemWithIdentifier_drawingHandlerSelector = mkSelector "stepperTouchBarItemWithIdentifier:drawingHandler:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CDouble
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector '[CDouble] ()
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CDouble
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector '[CDouble] ()
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @increment@
incrementSelector :: Selector '[] CDouble
incrementSelector = mkSelector "increment"

-- | @Selector@ for @setIncrement:@
setIncrementSelector :: Selector '[CDouble] ()
setIncrementSelector = mkSelector "setIncrement:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CDouble
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CDouble] ()
setValueSelector = mkSelector "setValue:"

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

