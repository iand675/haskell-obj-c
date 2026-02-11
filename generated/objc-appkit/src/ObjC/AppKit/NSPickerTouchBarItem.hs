{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPickerTouchBarItem@.
module ObjC.AppKit.NSPickerTouchBarItem
  ( NSPickerTouchBarItem
  , IsNSPickerTouchBarItem(..)
  , pickerTouchBarItemWithIdentifier_labels_selectionMode_target_action
  , pickerTouchBarItemWithIdentifier_images_selectionMode_target_action
  , setImage_atIndex
  , imageAtIndex
  , setLabel_atIndex
  , labelAtIndex
  , setEnabled_atIndex
  , isEnabledAtIndex
  , controlRepresentation
  , setControlRepresentation
  , collapsedRepresentationLabel
  , setCollapsedRepresentationLabel
  , collapsedRepresentationImage
  , setCollapsedRepresentationImage
  , selectedIndex
  , setSelectedIndex
  , selectionColor
  , setSelectionColor
  , selectionMode
  , setSelectionMode
  , numberOfOptions
  , setNumberOfOptions
  , target
  , setTarget
  , action
  , setAction
  , enabled
  , setEnabled
  , customizationLabel
  , setCustomizationLabel
  , pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector
  , pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector
  , setImage_atIndexSelector
  , imageAtIndexSelector
  , setLabel_atIndexSelector
  , labelAtIndexSelector
  , setEnabled_atIndexSelector
  , isEnabledAtIndexSelector
  , controlRepresentationSelector
  , setControlRepresentationSelector
  , collapsedRepresentationLabelSelector
  , setCollapsedRepresentationLabelSelector
  , collapsedRepresentationImageSelector
  , setCollapsedRepresentationImageSelector
  , selectedIndexSelector
  , setSelectedIndexSelector
  , selectionColorSelector
  , setSelectionColorSelector
  , selectionModeSelector
  , setSelectionModeSelector
  , numberOfOptionsSelector
  , setNumberOfOptionsSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , enabledSelector
  , setEnabledSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector

  -- * Enum types
  , NSPickerTouchBarItemControlRepresentation(NSPickerTouchBarItemControlRepresentation)
  , pattern NSPickerTouchBarItemControlRepresentationAutomatic
  , pattern NSPickerTouchBarItemControlRepresentationExpanded
  , pattern NSPickerTouchBarItemControlRepresentationCollapsed
  , NSPickerTouchBarItemSelectionMode(NSPickerTouchBarItemSelectionMode)
  , pattern NSPickerTouchBarItemSelectionModeSelectOne
  , pattern NSPickerTouchBarItemSelectionModeSelectAny
  , pattern NSPickerTouchBarItemSelectionModeMomentary

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_action :: (IsNSString identifier, IsNSArray labels) => identifier -> labels -> NSPickerTouchBarItemSelectionMode -> RawId -> Selector -> IO (Id NSPickerTouchBarItem)
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_action identifier labels selectionMode target action =
  do
    cls' <- getRequiredClass "NSPickerTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr labels $ \raw_labels ->
        sendClassMsg cls' (mkSelector "pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argCLong (coerce selectionMode), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @+ pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_images_selectionMode_target_action :: (IsNSString identifier, IsNSArray images) => identifier -> images -> NSPickerTouchBarItemSelectionMode -> RawId -> Selector -> IO (Id NSPickerTouchBarItem)
pickerTouchBarItemWithIdentifier_images_selectionMode_target_action identifier images selectionMode target action =
  do
    cls' <- getRequiredClass "NSPickerTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr images $ \raw_images ->
        sendClassMsg cls' (mkSelector "pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_images :: Ptr ()), argCLong (coerce selectionMode), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @- setImage:atIndex:@
setImage_atIndex :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSImage image) => nsPickerTouchBarItem -> image -> CLong -> IO ()
setImage_atIndex nsPickerTouchBarItem  image index =
withObjCPtr image $ \raw_image ->
    sendMsg nsPickerTouchBarItem (mkSelector "setImage:atIndex:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argCLong (fromIntegral index)]

-- | @- imageAtIndex:@
imageAtIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO (Id NSImage)
imageAtIndex nsPickerTouchBarItem  index =
  sendMsg nsPickerTouchBarItem (mkSelector "imageAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setLabel:atIndex:@
setLabel_atIndex :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSString label) => nsPickerTouchBarItem -> label -> CLong -> IO ()
setLabel_atIndex nsPickerTouchBarItem  label index =
withObjCPtr label $ \raw_label ->
    sendMsg nsPickerTouchBarItem (mkSelector "setLabel:atIndex:") retVoid [argPtr (castPtr raw_label :: Ptr ()), argCLong (fromIntegral index)]

-- | @- labelAtIndex:@
labelAtIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO (Id NSString)
labelAtIndex nsPickerTouchBarItem  index =
  sendMsg nsPickerTouchBarItem (mkSelector "labelAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setEnabled:atIndex:@
setEnabled_atIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> Bool -> CLong -> IO ()
setEnabled_atIndex nsPickerTouchBarItem  enabled index =
  sendMsg nsPickerTouchBarItem (mkSelector "setEnabled:atIndex:") retVoid [argCULong (if enabled then 1 else 0), argCLong (fromIntegral index)]

-- | @- isEnabledAtIndex:@
isEnabledAtIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO Bool
isEnabledAtIndex nsPickerTouchBarItem  index =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPickerTouchBarItem (mkSelector "isEnabledAtIndex:") retCULong [argCLong (fromIntegral index)]

-- | @- controlRepresentation@
controlRepresentation :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO NSPickerTouchBarItemControlRepresentation
controlRepresentation nsPickerTouchBarItem  =
  fmap (coerce :: CLong -> NSPickerTouchBarItemControlRepresentation) $ sendMsg nsPickerTouchBarItem (mkSelector "controlRepresentation") retCLong []

-- | @- setControlRepresentation:@
setControlRepresentation :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> NSPickerTouchBarItemControlRepresentation -> IO ()
setControlRepresentation nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setControlRepresentation:") retVoid [argCLong (coerce value)]

-- | @- collapsedRepresentationLabel@
collapsedRepresentationLabel :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSString)
collapsedRepresentationLabel nsPickerTouchBarItem  =
  sendMsg nsPickerTouchBarItem (mkSelector "collapsedRepresentationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabel :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSString value) => nsPickerTouchBarItem -> value -> IO ()
setCollapsedRepresentationLabel nsPickerTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPickerTouchBarItem (mkSelector "setCollapsedRepresentationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- collapsedRepresentationImage@
collapsedRepresentationImage :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSImage)
collapsedRepresentationImage nsPickerTouchBarItem  =
  sendMsg nsPickerTouchBarItem (mkSelector "collapsedRepresentationImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCollapsedRepresentationImage:@
setCollapsedRepresentationImage :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSImage value) => nsPickerTouchBarItem -> value -> IO ()
setCollapsedRepresentationImage nsPickerTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPickerTouchBarItem (mkSelector "setCollapsedRepresentationImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectedIndex@
selectedIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO CLong
selectedIndex nsPickerTouchBarItem  =
  sendMsg nsPickerTouchBarItem (mkSelector "selectedIndex") retCLong []

-- | @- setSelectedIndex:@
setSelectedIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO ()
setSelectedIndex nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setSelectedIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- selectionColor@
selectionColor :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSColor)
selectionColor nsPickerTouchBarItem  =
  sendMsg nsPickerTouchBarItem (mkSelector "selectionColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectionColor:@
setSelectionColor :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSColor value) => nsPickerTouchBarItem -> value -> IO ()
setSelectionColor nsPickerTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPickerTouchBarItem (mkSelector "setSelectionColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectionMode@
selectionMode :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO NSPickerTouchBarItemSelectionMode
selectionMode nsPickerTouchBarItem  =
  fmap (coerce :: CLong -> NSPickerTouchBarItemSelectionMode) $ sendMsg nsPickerTouchBarItem (mkSelector "selectionMode") retCLong []

-- | @- setSelectionMode:@
setSelectionMode :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> NSPickerTouchBarItemSelectionMode -> IO ()
setSelectionMode nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setSelectionMode:") retVoid [argCLong (coerce value)]

-- | @- numberOfOptions@
numberOfOptions :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO CLong
numberOfOptions nsPickerTouchBarItem  =
  sendMsg nsPickerTouchBarItem (mkSelector "numberOfOptions") retCLong []

-- | @- setNumberOfOptions:@
setNumberOfOptions :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO ()
setNumberOfOptions nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setNumberOfOptions:") retVoid [argCLong (fromIntegral value)]

-- | @- target@
target :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO RawId
target nsPickerTouchBarItem  =
  fmap (RawId . castPtr) $ sendMsg nsPickerTouchBarItem (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> RawId -> IO ()
setTarget nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO Selector
action nsPickerTouchBarItem  =
  fmap (Selector . castPtr) $ sendMsg nsPickerTouchBarItem (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> Selector -> IO ()
setAction nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- enabled@
enabled :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO Bool
enabled nsPickerTouchBarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPickerTouchBarItem (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> Bool -> IO ()
setEnabled nsPickerTouchBarItem  value =
  sendMsg nsPickerTouchBarItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSString)
customizationLabel nsPickerTouchBarItem  =
  sendMsg nsPickerTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSString value) => nsPickerTouchBarItem -> value -> IO ()
setCustomizationLabel nsPickerTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPickerTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector :: Selector
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector = mkSelector "pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:"

-- | @Selector@ for @pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector :: Selector
pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector = mkSelector "pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:"

-- | @Selector@ for @setImage:atIndex:@
setImage_atIndexSelector :: Selector
setImage_atIndexSelector = mkSelector "setImage:atIndex:"

-- | @Selector@ for @imageAtIndex:@
imageAtIndexSelector :: Selector
imageAtIndexSelector = mkSelector "imageAtIndex:"

-- | @Selector@ for @setLabel:atIndex:@
setLabel_atIndexSelector :: Selector
setLabel_atIndexSelector = mkSelector "setLabel:atIndex:"

-- | @Selector@ for @labelAtIndex:@
labelAtIndexSelector :: Selector
labelAtIndexSelector = mkSelector "labelAtIndex:"

-- | @Selector@ for @setEnabled:atIndex:@
setEnabled_atIndexSelector :: Selector
setEnabled_atIndexSelector = mkSelector "setEnabled:atIndex:"

-- | @Selector@ for @isEnabledAtIndex:@
isEnabledAtIndexSelector :: Selector
isEnabledAtIndexSelector = mkSelector "isEnabledAtIndex:"

-- | @Selector@ for @controlRepresentation@
controlRepresentationSelector :: Selector
controlRepresentationSelector = mkSelector "controlRepresentation"

-- | @Selector@ for @setControlRepresentation:@
setControlRepresentationSelector :: Selector
setControlRepresentationSelector = mkSelector "setControlRepresentation:"

-- | @Selector@ for @collapsedRepresentationLabel@
collapsedRepresentationLabelSelector :: Selector
collapsedRepresentationLabelSelector = mkSelector "collapsedRepresentationLabel"

-- | @Selector@ for @setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabelSelector :: Selector
setCollapsedRepresentationLabelSelector = mkSelector "setCollapsedRepresentationLabel:"

-- | @Selector@ for @collapsedRepresentationImage@
collapsedRepresentationImageSelector :: Selector
collapsedRepresentationImageSelector = mkSelector "collapsedRepresentationImage"

-- | @Selector@ for @setCollapsedRepresentationImage:@
setCollapsedRepresentationImageSelector :: Selector
setCollapsedRepresentationImageSelector = mkSelector "setCollapsedRepresentationImage:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @selectionColor@
selectionColorSelector :: Selector
selectionColorSelector = mkSelector "selectionColor"

-- | @Selector@ for @setSelectionColor:@
setSelectionColorSelector :: Selector
setSelectionColorSelector = mkSelector "setSelectionColor:"

-- | @Selector@ for @selectionMode@
selectionModeSelector :: Selector
selectionModeSelector = mkSelector "selectionMode"

-- | @Selector@ for @setSelectionMode:@
setSelectionModeSelector :: Selector
setSelectionModeSelector = mkSelector "setSelectionMode:"

-- | @Selector@ for @numberOfOptions@
numberOfOptionsSelector :: Selector
numberOfOptionsSelector = mkSelector "numberOfOptions"

-- | @Selector@ for @setNumberOfOptions:@
setNumberOfOptionsSelector :: Selector
setNumberOfOptionsSelector = mkSelector "setNumberOfOptions:"

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

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

