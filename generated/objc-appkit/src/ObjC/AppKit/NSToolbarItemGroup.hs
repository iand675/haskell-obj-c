{-# LANGUAGE PatternSynonyms #-}
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
  , groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector
  , groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector
  , setSelected_atIndexSelector
  , isSelectedAtIndexSelector
  , subitemsSelector
  , setSubitemsSelector
  , controlRepresentationSelector
  , setControlRepresentationSelector
  , selectionModeSelector
  , setSelectionModeSelector
  , selectedIndexSelector
  , setSelectedIndexSelector

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

-- | Convenience constructors for creating segmented control based toolbar items with images or text. The item returned will have a custom view for representing the control and automatically create subitems for the group. The labels array, if not nil, will be used to provide individual labels under the item for each segment of the control. When space in the toolbar is tight, the control may switch to a smaller alternate representation as necessary to remain in the toolbar.
--
-- ObjC selector: @+ groupWithItemIdentifier:titles:selectionMode:labels:target:action:@
groupWithItemIdentifier_titles_selectionMode_labels_target_action :: (IsNSString itemIdentifier, IsNSArray titles, IsNSArray labels) => itemIdentifier -> titles -> NSToolbarItemGroupSelectionMode -> labels -> RawId -> Selector -> IO (Id NSToolbarItemGroup)
groupWithItemIdentifier_titles_selectionMode_labels_target_action itemIdentifier titles selectionMode labels target action =
  do
    cls' <- getRequiredClass "NSToolbarItemGroup"
    withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      withObjCPtr titles $ \raw_titles ->
        withObjCPtr labels $ \raw_labels ->
          sendClassMsg cls' (mkSelector "groupWithItemIdentifier:titles:selectionMode:labels:target:action:") (retPtr retVoid) [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_titles :: Ptr ()), argCLong (coerce selectionMode), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @+ groupWithItemIdentifier:images:selectionMode:labels:target:action:@
groupWithItemIdentifier_images_selectionMode_labels_target_action :: (IsNSString itemIdentifier, IsNSArray images, IsNSArray labels) => itemIdentifier -> images -> NSToolbarItemGroupSelectionMode -> labels -> RawId -> Selector -> IO (Id NSToolbarItemGroup)
groupWithItemIdentifier_images_selectionMode_labels_target_action itemIdentifier images selectionMode labels target action =
  do
    cls' <- getRequiredClass "NSToolbarItemGroup"
    withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      withObjCPtr images $ \raw_images ->
        withObjCPtr labels $ \raw_labels ->
          sendClassMsg cls' (mkSelector "groupWithItemIdentifier:images:selectionMode:labels:target:action:") (retPtr retVoid) [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_images :: Ptr ()), argCLong (coerce selectionMode), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Get and set selection of individual subitems of the group item.
--
-- ObjC selector: @- setSelected:atIndex:@
setSelected_atIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> Bool -> CLong -> IO ()
setSelected_atIndex nsToolbarItemGroup  selected index =
  sendMsg nsToolbarItemGroup (mkSelector "setSelected:atIndex:") retVoid [argCULong (if selected then 1 else 0), argCLong (fromIntegral index)]

-- | @- isSelectedAtIndex:@
isSelectedAtIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> CLong -> IO Bool
isSelectedAtIndex nsToolbarItemGroup  index =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItemGroup (mkSelector "isSelectedAtIndex:") retCULong [argCLong (fromIntegral index)]

-- | Set or get the array of subitems for the toolbar item. By default, a @NSToolbarItemGroup@ has an empty array of subitems. You should call this to set the subitems before returning the item to the toolbar. @NSToolbarItemGroups@ may not contain other @NSToolbarItemGroups@ as subitems.
--
-- ObjC selector: @- subitems@
subitems :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO (Id NSArray)
subitems nsToolbarItemGroup  =
  sendMsg nsToolbarItemGroup (mkSelector "subitems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set or get the array of subitems for the toolbar item. By default, a @NSToolbarItemGroup@ has an empty array of subitems. You should call this to set the subitems before returning the item to the toolbar. @NSToolbarItemGroups@ may not contain other @NSToolbarItemGroups@ as subitems.
--
-- ObjC selector: @- setSubitems:@
setSubitems :: (IsNSToolbarItemGroup nsToolbarItemGroup, IsNSArray value) => nsToolbarItemGroup -> value -> IO ()
setSubitems nsToolbarItemGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsToolbarItemGroup (mkSelector "setSubitems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The style in which this item will be represented to the user. Defaults to @NSToolbarItemGroupControlRepresentationAutomatic@.
--
-- ObjC selector: @- controlRepresentation@
controlRepresentation :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO NSToolbarItemGroupControlRepresentation
controlRepresentation nsToolbarItemGroup  =
  fmap (coerce :: CLong -> NSToolbarItemGroupControlRepresentation) $ sendMsg nsToolbarItemGroup (mkSelector "controlRepresentation") retCLong []

-- | The style in which this item will be represented to the user. Defaults to @NSToolbarItemGroupControlRepresentationAutomatic@.
--
-- ObjC selector: @- setControlRepresentation:@
setControlRepresentation :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> NSToolbarItemGroupControlRepresentation -> IO ()
setControlRepresentation nsToolbarItemGroup  value =
  sendMsg nsToolbarItemGroup (mkSelector "setControlRepresentation:") retVoid [argCLong (coerce value)]

-- | Get and set how selection is handled by the control. Only applies when using one of the constructors to create the item with a system defined control representation.
--
-- ObjC selector: @- selectionMode@
selectionMode :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO NSToolbarItemGroupSelectionMode
selectionMode nsToolbarItemGroup  =
  fmap (coerce :: CLong -> NSToolbarItemGroupSelectionMode) $ sendMsg nsToolbarItemGroup (mkSelector "selectionMode") retCLong []

-- | Get and set how selection is handled by the control. Only applies when using one of the constructors to create the item with a system defined control representation.
--
-- ObjC selector: @- setSelectionMode:@
setSelectionMode :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> NSToolbarItemGroupSelectionMode -> IO ()
setSelectionMode nsToolbarItemGroup  value =
  sendMsg nsToolbarItemGroup (mkSelector "setSelectionMode:") retVoid [argCLong (coerce value)]

-- | The most recently selected item of the group, or -1 if nothing is selected.
--
-- ObjC selector: @- selectedIndex@
selectedIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> IO CLong
selectedIndex nsToolbarItemGroup  =
  sendMsg nsToolbarItemGroup (mkSelector "selectedIndex") retCLong []

-- | The most recently selected item of the group, or -1 if nothing is selected.
--
-- ObjC selector: @- setSelectedIndex:@
setSelectedIndex :: IsNSToolbarItemGroup nsToolbarItemGroup => nsToolbarItemGroup -> CLong -> IO ()
setSelectedIndex nsToolbarItemGroup  value =
  sendMsg nsToolbarItemGroup (mkSelector "setSelectedIndex:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupWithItemIdentifier:titles:selectionMode:labels:target:action:@
groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector :: Selector
groupWithItemIdentifier_titles_selectionMode_labels_target_actionSelector = mkSelector "groupWithItemIdentifier:titles:selectionMode:labels:target:action:"

-- | @Selector@ for @groupWithItemIdentifier:images:selectionMode:labels:target:action:@
groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector :: Selector
groupWithItemIdentifier_images_selectionMode_labels_target_actionSelector = mkSelector "groupWithItemIdentifier:images:selectionMode:labels:target:action:"

-- | @Selector@ for @setSelected:atIndex:@
setSelected_atIndexSelector :: Selector
setSelected_atIndexSelector = mkSelector "setSelected:atIndex:"

-- | @Selector@ for @isSelectedAtIndex:@
isSelectedAtIndexSelector :: Selector
isSelectedAtIndexSelector = mkSelector "isSelectedAtIndex:"

-- | @Selector@ for @subitems@
subitemsSelector :: Selector
subitemsSelector = mkSelector "subitems"

-- | @Selector@ for @setSubitems:@
setSubitemsSelector :: Selector
setSubitemsSelector = mkSelector "setSubitems:"

-- | @Selector@ for @controlRepresentation@
controlRepresentationSelector :: Selector
controlRepresentationSelector = mkSelector "controlRepresentation"

-- | @Selector@ for @setControlRepresentation:@
setControlRepresentationSelector :: Selector
setControlRepresentationSelector = mkSelector "setControlRepresentation:"

-- | @Selector@ for @selectionMode@
selectionModeSelector :: Selector
selectionModeSelector = mkSelector "selectionMode"

-- | @Selector@ for @setSelectionMode:@
setSelectionModeSelector :: Selector
setSelectionModeSelector = mkSelector "setSelectionMode:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

