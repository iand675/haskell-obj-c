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
  , stepperTouchBarItemWithIdentifier_formatterSelector
  , stepperTouchBarItemWithIdentifier_drawingHandlerSelector
  , maxValueSelector
  , setMaxValueSelector
  , minValueSelector
  , setMinValueSelector
  , incrementSelector
  , setIncrementSelector
  , valueSelector
  , setValueSelector
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

-- | Creates an @NSStepperTouchBarItem@ with a @formatter@ to display the stepper's value as text
--
-- @formatter@ — A formatter used to display a textual representation of the stepper's value
--
-- ObjC selector: @+ stepperTouchBarItemWithIdentifier:formatter:@
stepperTouchBarItemWithIdentifier_formatter :: (IsNSString identifier, IsNSFormatter formatter) => identifier -> formatter -> IO (Id NSStepperTouchBarItem)
stepperTouchBarItemWithIdentifier_formatter identifier formatter =
  do
    cls' <- getRequiredClass "NSStepperTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr formatter $ \raw_formatter ->
        sendClassMsg cls' (mkSelector "stepperTouchBarItemWithIdentifier:formatter:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_formatter :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an @NSStepperTouchBarItem@ using the result of @drawingHandler@ to display the stepper's value as an image
--
-- @drawingHandler@ — A block that draws a graphical representation of the stepper's value in the specified rectangle. The coordinates of this rectangle are specified in points.
--
-- ObjC selector: @+ stepperTouchBarItemWithIdentifier:drawingHandler:@
stepperTouchBarItemWithIdentifier_drawingHandler :: IsNSString identifier => identifier -> Ptr () -> IO (Id NSStepperTouchBarItem)
stepperTouchBarItemWithIdentifier_drawingHandler identifier drawingHandler =
  do
    cls' <- getRequiredClass "NSStepperTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "stepperTouchBarItemWithIdentifier:drawingHandler:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr drawingHandler :: Ptr ())] >>= retainedObject . castPtr

-- | The stepper's maximum value. The default is 59.0.
--
-- ObjC selector: @- maxValue@
maxValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
maxValue nsStepperTouchBarItem  =
  sendMsg nsStepperTouchBarItem (mkSelector "maxValue") retCDouble []

-- | The stepper's maximum value. The default is 59.0.
--
-- ObjC selector: @- setMaxValue:@
setMaxValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setMaxValue nsStepperTouchBarItem  value =
  sendMsg nsStepperTouchBarItem (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | The stepper's minimum value. The default is 0.0.
--
-- ObjC selector: @- minValue@
minValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
minValue nsStepperTouchBarItem  =
  sendMsg nsStepperTouchBarItem (mkSelector "minValue") retCDouble []

-- | The stepper's minimum value. The default is 0.0.
--
-- ObjC selector: @- setMinValue:@
setMinValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setMinValue nsStepperTouchBarItem  value =
  sendMsg nsStepperTouchBarItem (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | The stepper's increment value. The default is 1.0.
--
-- ObjC selector: @- increment@
increment :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
increment nsStepperTouchBarItem  =
  sendMsg nsStepperTouchBarItem (mkSelector "increment") retCDouble []

-- | The stepper's increment value. The default is 1.0.
--
-- ObjC selector: @- setIncrement:@
setIncrement :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setIncrement nsStepperTouchBarItem  value =
  sendMsg nsStepperTouchBarItem (mkSelector "setIncrement:") retVoid [argCDouble (fromIntegral value)]

-- | The current value of the stepper.
--
-- ObjC selector: @- value@
value :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO CDouble
value nsStepperTouchBarItem  =
  sendMsg nsStepperTouchBarItem (mkSelector "value") retCDouble []

-- | The current value of the stepper.
--
-- ObjC selector: @- setValue:@
setValue :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> CDouble -> IO ()
setValue nsStepperTouchBarItem  value =
  sendMsg nsStepperTouchBarItem (mkSelector "setValue:") retVoid [argCDouble (fromIntegral value)]

-- | The target object that receives action messages from the stepper.
--
-- ObjC selector: @- target@
target :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO RawId
target nsStepperTouchBarItem  =
  fmap (RawId . castPtr) $ sendMsg nsStepperTouchBarItem (mkSelector "target") (retPtr retVoid) []

-- | The target object that receives action messages from the stepper.
--
-- ObjC selector: @- setTarget:@
setTarget :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> RawId -> IO ()
setTarget nsStepperTouchBarItem  value =
  sendMsg nsStepperTouchBarItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The action-message selector associated with the stepper.
--
-- ObjC selector: @- action@
action :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO Selector
action nsStepperTouchBarItem  =
  fmap (Selector . castPtr) $ sendMsg nsStepperTouchBarItem (mkSelector "action") (retPtr retVoid) []

-- | The action-message selector associated with the stepper.
--
-- ObjC selector: @- setAction:@
setAction :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> Selector -> IO ()
setAction nsStepperTouchBarItem  value =
  sendMsg nsStepperTouchBarItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSStepperTouchBarItem nsStepperTouchBarItem => nsStepperTouchBarItem -> IO (Id NSString)
customizationLabel nsStepperTouchBarItem  =
  sendMsg nsStepperTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSStepperTouchBarItem nsStepperTouchBarItem, IsNSString value) => nsStepperTouchBarItem -> value -> IO ()
setCustomizationLabel nsStepperTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsStepperTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepperTouchBarItemWithIdentifier:formatter:@
stepperTouchBarItemWithIdentifier_formatterSelector :: Selector
stepperTouchBarItemWithIdentifier_formatterSelector = mkSelector "stepperTouchBarItemWithIdentifier:formatter:"

-- | @Selector@ for @stepperTouchBarItemWithIdentifier:drawingHandler:@
stepperTouchBarItemWithIdentifier_drawingHandlerSelector :: Selector
stepperTouchBarItemWithIdentifier_drawingHandlerSelector = mkSelector "stepperTouchBarItemWithIdentifier:drawingHandler:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @increment@
incrementSelector :: Selector
incrementSelector = mkSelector "increment"

-- | @Selector@ for @setIncrement:@
setIncrementSelector :: Selector
setIncrementSelector = mkSelector "setIncrement:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

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

