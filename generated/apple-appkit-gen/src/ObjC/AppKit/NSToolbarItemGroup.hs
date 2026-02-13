{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSToolbarItemGroup@.
module ObjC.AppKit.NSToolbarItemGroup
  ( NSToolbarItemGroup
  , IsNSToolbarItemGroup(..)
  , groupWithItemIdentifier_titles_selectionMode_labels_target_action
  , groupWithItemIdentifier_images_selectionMode_labels_target_action
  , setSelected_atIndex
  , isSelectedAtIndex
  , subitems
  , setSubitems
  , controlRepresentation
  , setControlRepresentation
  , selectionMode
  , setSelectionMode
  , selectedIndex
  , setSelectedIndex
  , controlRepresentationSelector
  , groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector
  , groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector
  , isSelectedAtIndexSelector
  , selectedIndexSelector
  , selectionModeSelector
  , setControlRepresentationSelector
  , setSelectedIndexSelector
  , setSelected_atIndexSelector
  , setSelectionModeSelector
  , setSubitemsSelector
  , subitemsSelector

  -- * Enum types
  , NSToolbarItemGroupControlRepresentation(NSToolbarItemGroupControlRepresentation)
  , pattern NSToolbarItemGroupControlRepresentationAutomatic
  , pattern NSToolbarItemGroupControlRepresentationExpanded
  , pattern NSToolbarItemGroupControlRepresentationCollapsed
  , NSToolbarItemGroupSelectionMode(NSToolbarItemGroupSelectionMode)
  , pattern NSToolbarItemGroupSelectionModeSelectOne
  , pattern NSToolbarItemGroupSelectionModeSelectAny
  , pattern NSToolbarItemGroupSelectionModeMomentary

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

-- | Convenience constructors for creating segmented control based toolbar items with images or text. The item returned will have a custom view for representing the control and automatically create subitems for the group. The labels array, if not nil, will be used to provide individual labels under the item for each segment of the control. When space in the toolbar is tight, the control may switch to a smaller alternate representation as necessary to remain in the toolbar.
--
-- ObjC selector: @+ groupWithItemIdentifier:titles:selectionMode:labels:target:action:@
groupWithItemIdentifier_titles_selectionMode_labels_target_action :: (IsNSString itemIdentifier, IsNSArray titles, IsNSArray labels) => itemIdentifier -> titles -> NSToolbarItemGroupSelectionMode -> labels -> RawId -> Sel -> IO (Id NSToolbarItemGroup)
groupWithItemIdentifier_titles_selectionMode_labels_target_action itemIdentifier titles selectionMode labels target action =
  do
    cls' <- getRequiredClass "NSToolbarItemGroup"
    sendClassMessage cls' groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector (toNSString itemIdentifier) (toNSArray titles) selectionMode (toNSArray labels) target action

-- | @+ groupWithItemIdentifier:images:selectionMode:labels:target:action:@
groupWithItemIdentifier_images_selectionMode_labels_target_action :: (IsNSString itemIdentifier, IsNSArray images, IsNSArray labels) => itemIdentifier -> images -> NSToolbarItemGroupSelectionMode -> labels -> RawId -> Sel -> IO (Id NSToolbarItemGroup)
groupWithItemIdentifier_images_selectionMode_labels_target_action itemIdentifier images selectionMode labels target action =
  do
    cls' <- getRequiredClass "NSToolbarItemGroup"
    sendClassMessage cls' groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector (toNSString itemIdentifier) (toNSArray images) selectionMode (toNSArray labels) target action

-- | Get and set selection of individual subitems of the group item.
--
-- ObjC selector: @- setSelected:atIndex:@
setSelected_atIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> Bool -> CLong -> IO ()
setSelected_atIndex nsToolbarItemGroup selected index =
  sendMessage nsToolbarItemGroup setSelected_atIndexSelector selected index

-- | @- isSelectedAtIndex:@
isSelectedAtIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> CLong -> IO Bool
isSelectedAtIndex nsToolbarItemGroup index =
  sendMessage nsToolbarItemGroup isSelectedAtIndexSelector index

-- | Set or get the array of subitems for the toolbar item. By default, a @NSToolbarItemGroup@ has an empty array of subitems. You should call this to set the subitems before returning the item to the toolbar. @NSToolbarItemGroups@ may not contain other @NSToolbarItemGroups@ as subitems.
--
-- ObjC selector: @- subitems@
subitems :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO (Id NSArray)
subitems nsToolbarItemGroup =
  sendMessage nsToolbarItemGroup subitemsSelector

-- | Set or get the array of subitems for the toolbar item. By default, a @NSToolbarItemGroup@ has an empty array of subitems. You should call this to set the subitems before returning the item to the toolbar. @NSToolbarItemGroups@ may not contain other @NSToolbarItemGroups@ as subitems.
--
-- ObjC selector: @- setSubitems:@
setSubitems :: (IsNSToolbarItemGroup nsToolbarItemGroup, IsNSArray value) => nsToolbarItemGroup -> value -> IO ()
setSubitems nsToolbarItemGroup value =
  sendMessage nsToolbarItemGroup setSubitemsSelector (toNSArray value)

-- | The style in which this item will be represented to the user. Defaults to @NSToolbarItemGroupControlRepresentationAutomatic@.
--
-- ObjC selector: @- controlRepresentation@
controlRepresentation :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO NSToolbarItemGroupControlRepresentation
controlRepresentation nsToolbarItemGroup =
  sendMessage nsToolbarItemGroup controlRepresentationSelector

-- | The style in which this item will be represented to the user. Defaults to @NSToolbarItemGroupControlRepresentationAutomatic@.
--
-- ObjC selector: @- setControlRepresentation:@
setControlRepresentation :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> NSToolbarItemGroupControlRepresentation -> IO ()
setControlRepresentation nsToolbarItemGroup value =
  sendMessage nsToolbarItemGroup setControlRepresentationSelector value

-- | Get and set how selection is handled by the control. Only applies when using one of the constructors to create the item with a system defined control representation.
--
-- ObjC selector: @- selectionMode@
selectionMode :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO NSToolbarItemGroupSelectionMode
selectionMode nsToolbarItemGroup =
  sendMessage nsToolbarItemGroup selectionModeSelector

-- | Get and set how selection is handled by the control. Only applies when using one of the constructors to create the item with a system defined control representation.
--
-- ObjC selector: @- setSelectionMode:@
setSelectionMode :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> NSToolbarItemGroupSelectionMode -> IO ()
setSelectionMode nsToolbarItemGroup value =
  sendMessage nsToolbarItemGroup setSelectionModeSelector value

-- | The most recently selected item of the group, or -1 if nothing is selected.
--
-- ObjC selector: @- selectedIndex@
selectedIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO CLong
selectedIndex nsToolbarItemGroup =
  sendMessage nsToolbarItemGroup selectedIndexSelector

-- | The most recently selected item of the group, or -1 if nothing is selected.
--
-- ObjC selector: @- setSelectedIndex:@
setSelectedIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> CLong -> IO ()
setSelectedIndex nsToolbarItemGroup value =
  sendMessage nsToolbarItemGroup setSelectedIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupWithItemIdentifier:titles:selectionMode:labels:target:action:@
groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector :: Selector '[Id NSString, Id NSArray, NSToolbarItemGroupSelectionMode, Id NSArray, RawId, Sel] (Id NSToolbarItemGroup)
groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector = mkSelector "groupWithItemIdentifier:titles:selectionMode:labels:target:action:"

-- | @Selector@ for @groupWithItemIdentifier:images:selectionMode:labels:target:action:@
groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector :: Selector '[Id NSString, Id NSArray, NSToolbarItemGroupSelectionMode, Id NSArray, RawId, Sel] (Id NSToolbarItemGroup)
groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector = mkSelector "groupWithItemIdentifier:images:selectionMode:labels:target:action:"

-- | @Selector@ for @setSelected:atIndex:@
setSelected_atIndexSelector :: Selector '[Bool, CLong] ()
setSelected_atIndexSelector = mkSelector "setSelected:atIndex:"

-- | @Selector@ for @isSelectedAtIndex:@
isSelectedAtIndexSelector :: Selector '[CLong] Bool
isSelectedAtIndexSelector = mkSelector "isSelectedAtIndex:"

-- | @Selector@ for @subitems@
subitemsSelector :: Selector '[] (Id NSArray)
subitemsSelector = mkSelector "subitems"

-- | @Selector@ for @setSubitems:@
setSubitemsSelector :: Selector '[Id NSArray] ()
setSubitemsSelector = mkSelector "setSubitems:"

-- | @Selector@ for @controlRepresentation@
controlRepresentationSelector :: Selector '[] NSToolbarItemGroupControlRepresentation
controlRepresentationSelector = mkSelector "controlRepresentation"

-- | @Selector@ for @setControlRepresentation:@
setControlRepresentationSelector :: Selector '[NSToolbarItemGroupControlRepresentation] ()
setControlRepresentationSelector = mkSelector "setControlRepresentation:"

-- | @Selector@ for @selectionMode@
selectionModeSelector :: Selector '[] NSToolbarItemGroupSelectionMode
selectionModeSelector = mkSelector "selectionMode"

-- | @Selector@ for @setSelectionMode:@
setSelectionModeSelector :: Selector '[NSToolbarItemGroupSelectionMode] ()
setSelectionModeSelector = mkSelector "setSelectionMode:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CLong
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CLong] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

