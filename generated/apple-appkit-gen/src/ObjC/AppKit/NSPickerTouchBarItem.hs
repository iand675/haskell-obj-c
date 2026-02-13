{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , actionSelector
  , collapsedRepresentationImageSelector
  , collapsedRepresentationLabelSelector
  , controlRepresentationSelector
  , customizationLabelSelector
  , enabledSelector
  , imageAtIndexSelector
  , isEnabledAtIndexSelector
  , labelAtIndexSelector
  , numberOfOptionsSelector
  , pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector
  , pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector
  , selectedIndexSelector
  , selectionColorSelector
  , selectionModeSelector
  , setActionSelector
  , setCollapsedRepresentationImageSelector
  , setCollapsedRepresentationLabelSelector
  , setControlRepresentationSelector
  , setCustomizationLabelSelector
  , setEnabledSelector
  , setEnabled_atIndexSelector
  , setImage_atIndexSelector
  , setLabel_atIndexSelector
  , setNumberOfOptionsSelector
  , setSelectedIndexSelector
  , setSelectionColorSelector
  , setSelectionModeSelector
  , setTargetSelector
  , targetSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_action :: (IsNSString identifier, IsNSArray labels) => identifier -> labels -> NSPickerTouchBarItemSelectionMode -> RawId -> Sel -> IO (Id NSPickerTouchBarItem)
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_action identifier labels selectionMode target action =
  do
    cls' <- getRequiredClass "NSPickerTouchBarItem"
    sendClassMessage cls' pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector (toNSString identifier) (toNSArray labels) selectionMode target action

-- | @+ pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_images_selectionMode_target_action :: (IsNSString identifier, IsNSArray images) => identifier -> images -> NSPickerTouchBarItemSelectionMode -> RawId -> Sel -> IO (Id NSPickerTouchBarItem)
pickerTouchBarItemWithIdentifier_images_selectionMode_target_action identifier images selectionMode target action =
  do
    cls' <- getRequiredClass "NSPickerTouchBarItem"
    sendClassMessage cls' pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector (toNSString identifier) (toNSArray images) selectionMode target action

-- | @- setImage:atIndex:@
setImage_atIndex :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSImage image) => nsPickerTouchBarItem -> image -> CLong -> IO ()
setImage_atIndex nsPickerTouchBarItem image index =
  sendMessage nsPickerTouchBarItem setImage_atIndexSelector (toNSImage image) index

-- | @- imageAtIndex:@
imageAtIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO (Id NSImage)
imageAtIndex nsPickerTouchBarItem index =
  sendMessage nsPickerTouchBarItem imageAtIndexSelector index

-- | @- setLabel:atIndex:@
setLabel_atIndex :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSString label) => nsPickerTouchBarItem -> label -> CLong -> IO ()
setLabel_atIndex nsPickerTouchBarItem label index =
  sendMessage nsPickerTouchBarItem setLabel_atIndexSelector (toNSString label) index

-- | @- labelAtIndex:@
labelAtIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO (Id NSString)
labelAtIndex nsPickerTouchBarItem index =
  sendMessage nsPickerTouchBarItem labelAtIndexSelector index

-- | @- setEnabled:atIndex:@
setEnabled_atIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> Bool -> CLong -> IO ()
setEnabled_atIndex nsPickerTouchBarItem enabled index =
  sendMessage nsPickerTouchBarItem setEnabled_atIndexSelector enabled index

-- | @- isEnabledAtIndex:@
isEnabledAtIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO Bool
isEnabledAtIndex nsPickerTouchBarItem index =
  sendMessage nsPickerTouchBarItem isEnabledAtIndexSelector index

-- | @- controlRepresentation@
controlRepresentation :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO NSPickerTouchBarItemControlRepresentation
controlRepresentation nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem controlRepresentationSelector

-- | @- setControlRepresentation:@
setControlRepresentation :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> NSPickerTouchBarItemControlRepresentation -> IO ()
setControlRepresentation nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setControlRepresentationSelector value

-- | @- collapsedRepresentationLabel@
collapsedRepresentationLabel :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSString)
collapsedRepresentationLabel nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem collapsedRepresentationLabelSelector

-- | @- setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabel :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSString value) => nsPickerTouchBarItem -> value -> IO ()
setCollapsedRepresentationLabel nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setCollapsedRepresentationLabelSelector (toNSString value)

-- | @- collapsedRepresentationImage@
collapsedRepresentationImage :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSImage)
collapsedRepresentationImage nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem collapsedRepresentationImageSelector

-- | @- setCollapsedRepresentationImage:@
setCollapsedRepresentationImage :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSImage value) => nsPickerTouchBarItem -> value -> IO ()
setCollapsedRepresentationImage nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setCollapsedRepresentationImageSelector (toNSImage value)

-- | @- selectedIndex@
selectedIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO CLong
selectedIndex nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem selectedIndexSelector

-- | @- setSelectedIndex:@
setSelectedIndex :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO ()
setSelectedIndex nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setSelectedIndexSelector value

-- | @- selectionColor@
selectionColor :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSColor)
selectionColor nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem selectionColorSelector

-- | @- setSelectionColor:@
setSelectionColor :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSColor value) => nsPickerTouchBarItem -> value -> IO ()
setSelectionColor nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setSelectionColorSelector (toNSColor value)

-- | @- selectionMode@
selectionMode :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO NSPickerTouchBarItemSelectionMode
selectionMode nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem selectionModeSelector

-- | @- setSelectionMode:@
setSelectionMode :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> NSPickerTouchBarItemSelectionMode -> IO ()
setSelectionMode nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setSelectionModeSelector value

-- | @- numberOfOptions@
numberOfOptions :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO CLong
numberOfOptions nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem numberOfOptionsSelector

-- | @- setNumberOfOptions:@
setNumberOfOptions :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> CLong -> IO ()
setNumberOfOptions nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setNumberOfOptionsSelector value

-- | @- target@
target :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO RawId
target nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem targetSelector

-- | @- setTarget:@
setTarget :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> RawId -> IO ()
setTarget nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setTargetSelector value

-- | @- action@
action :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO Sel
action nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem actionSelector

-- | @- setAction:@
setAction :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> Sel -> IO ()
setAction nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setActionSelector value

-- | @- enabled@
enabled :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO Bool
enabled nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> Bool -> IO ()
setEnabled nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setEnabledSelector value

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSPickerTouchBarItem nsPickerTouchBarItem => nsPickerTouchBarItem -> IO (Id NSString)
customizationLabel nsPickerTouchBarItem =
  sendMessage nsPickerTouchBarItem customizationLabelSelector

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSPickerTouchBarItem nsPickerTouchBarItem, IsNSString value) => nsPickerTouchBarItem -> value -> IO ()
setCustomizationLabel nsPickerTouchBarItem value =
  sendMessage nsPickerTouchBarItem setCustomizationLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector :: Selector '[Id NSString, Id NSArray, NSPickerTouchBarItemSelectionMode, RawId, Sel] (Id NSPickerTouchBarItem)
pickerTouchBarItemWithIdentifier_labels_selectionMode_target_actionSelector = mkSelector "pickerTouchBarItemWithIdentifier:labels:selectionMode:target:action:"

-- | @Selector@ for @pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:@
pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector :: Selector '[Id NSString, Id NSArray, NSPickerTouchBarItemSelectionMode, RawId, Sel] (Id NSPickerTouchBarItem)
pickerTouchBarItemWithIdentifier_images_selectionMode_target_actionSelector = mkSelector "pickerTouchBarItemWithIdentifier:images:selectionMode:target:action:"

-- | @Selector@ for @setImage:atIndex:@
setImage_atIndexSelector :: Selector '[Id NSImage, CLong] ()
setImage_atIndexSelector = mkSelector "setImage:atIndex:"

-- | @Selector@ for @imageAtIndex:@
imageAtIndexSelector :: Selector '[CLong] (Id NSImage)
imageAtIndexSelector = mkSelector "imageAtIndex:"

-- | @Selector@ for @setLabel:atIndex:@
setLabel_atIndexSelector :: Selector '[Id NSString, CLong] ()
setLabel_atIndexSelector = mkSelector "setLabel:atIndex:"

-- | @Selector@ for @labelAtIndex:@
labelAtIndexSelector :: Selector '[CLong] (Id NSString)
labelAtIndexSelector = mkSelector "labelAtIndex:"

-- | @Selector@ for @setEnabled:atIndex:@
setEnabled_atIndexSelector :: Selector '[Bool, CLong] ()
setEnabled_atIndexSelector = mkSelector "setEnabled:atIndex:"

-- | @Selector@ for @isEnabledAtIndex:@
isEnabledAtIndexSelector :: Selector '[CLong] Bool
isEnabledAtIndexSelector = mkSelector "isEnabledAtIndex:"

-- | @Selector@ for @controlRepresentation@
controlRepresentationSelector :: Selector '[] NSPickerTouchBarItemControlRepresentation
controlRepresentationSelector = mkSelector "controlRepresentation"

-- | @Selector@ for @setControlRepresentation:@
setControlRepresentationSelector :: Selector '[NSPickerTouchBarItemControlRepresentation] ()
setControlRepresentationSelector = mkSelector "setControlRepresentation:"

-- | @Selector@ for @collapsedRepresentationLabel@
collapsedRepresentationLabelSelector :: Selector '[] (Id NSString)
collapsedRepresentationLabelSelector = mkSelector "collapsedRepresentationLabel"

-- | @Selector@ for @setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabelSelector :: Selector '[Id NSString] ()
setCollapsedRepresentationLabelSelector = mkSelector "setCollapsedRepresentationLabel:"

-- | @Selector@ for @collapsedRepresentationImage@
collapsedRepresentationImageSelector :: Selector '[] (Id NSImage)
collapsedRepresentationImageSelector = mkSelector "collapsedRepresentationImage"

-- | @Selector@ for @setCollapsedRepresentationImage:@
setCollapsedRepresentationImageSelector :: Selector '[Id NSImage] ()
setCollapsedRepresentationImageSelector = mkSelector "setCollapsedRepresentationImage:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CLong
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CLong] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @selectionColor@
selectionColorSelector :: Selector '[] (Id NSColor)
selectionColorSelector = mkSelector "selectionColor"

-- | @Selector@ for @setSelectionColor:@
setSelectionColorSelector :: Selector '[Id NSColor] ()
setSelectionColorSelector = mkSelector "setSelectionColor:"

-- | @Selector@ for @selectionMode@
selectionModeSelector :: Selector '[] NSPickerTouchBarItemSelectionMode
selectionModeSelector = mkSelector "selectionMode"

-- | @Selector@ for @setSelectionMode:@
setSelectionModeSelector :: Selector '[NSPickerTouchBarItemSelectionMode] ()
setSelectionModeSelector = mkSelector "setSelectionMode:"

-- | @Selector@ for @numberOfOptions@
numberOfOptionsSelector :: Selector '[] CLong
numberOfOptionsSelector = mkSelector "numberOfOptions"

-- | @Selector@ for @setNumberOfOptions:@
setNumberOfOptionsSelector :: Selector '[CLong] ()
setNumberOfOptionsSelector = mkSelector "setNumberOfOptions:"

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

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

