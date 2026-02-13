{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTabView@.
module ObjC.AppKit.NSTabView
  ( NSTabView
  , IsNSTabView(..)
  , selectTabViewItem
  , selectTabViewItemAtIndex
  , selectTabViewItemWithIdentifier
  , takeSelectedTabViewItemFromSender
  , selectFirstTabViewItem
  , selectLastTabViewItem
  , selectNextTabViewItem
  , selectPreviousTabViewItem
  , addTabViewItem
  , insertTabViewItem_atIndex
  , removeTabViewItem
  , tabViewItemAtPoint
  , indexOfTabViewItem
  , tabViewItemAtIndex
  , indexOfTabViewItemWithIdentifier
  , selectedTabViewItem
  , font
  , setFont
  , tabViewType
  , setTabViewType
  , tabPosition
  , setTabPosition
  , tabViewBorderType
  , setTabViewBorderType
  , tabViewItems
  , setTabViewItems
  , allowsTruncatedLabels
  , setAllowsTruncatedLabels
  , minimumSize
  , drawsBackground
  , setDrawsBackground
  , controlSize
  , setControlSize
  , delegate
  , setDelegate
  , contentRect
  , numberOfTabViewItems
  , controlTint
  , setControlTint
  , addTabViewItemSelector
  , allowsTruncatedLabelsSelector
  , contentRectSelector
  , controlSizeSelector
  , controlTintSelector
  , delegateSelector
  , drawsBackgroundSelector
  , fontSelector
  , indexOfTabViewItemSelector
  , indexOfTabViewItemWithIdentifierSelector
  , insertTabViewItem_atIndexSelector
  , minimumSizeSelector
  , numberOfTabViewItemsSelector
  , removeTabViewItemSelector
  , selectFirstTabViewItemSelector
  , selectLastTabViewItemSelector
  , selectNextTabViewItemSelector
  , selectPreviousTabViewItemSelector
  , selectTabViewItemAtIndexSelector
  , selectTabViewItemSelector
  , selectTabViewItemWithIdentifierSelector
  , selectedTabViewItemSelector
  , setAllowsTruncatedLabelsSelector
  , setControlSizeSelector
  , setControlTintSelector
  , setDelegateSelector
  , setDrawsBackgroundSelector
  , setFontSelector
  , setTabPositionSelector
  , setTabViewBorderTypeSelector
  , setTabViewItemsSelector
  , setTabViewTypeSelector
  , tabPositionSelector
  , tabViewBorderTypeSelector
  , tabViewItemAtIndexSelector
  , tabViewItemAtPointSelector
  , tabViewItemsSelector
  , tabViewTypeSelector
  , takeSelectedTabViewItemFromSenderSelector

  -- * Enum types
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge
  , NSControlTint(NSControlTint)
  , pattern NSDefaultControlTint
  , pattern NSBlueControlTint
  , pattern NSGraphiteControlTint
  , pattern NSClearControlTint
  , NSTabPosition(NSTabPosition)
  , pattern NSTabPositionNone
  , pattern NSTabPositionTop
  , pattern NSTabPositionLeft
  , pattern NSTabPositionBottom
  , pattern NSTabPositionRight
  , NSTabViewBorderType(NSTabViewBorderType)
  , pattern NSTabViewBorderTypeNone
  , pattern NSTabViewBorderTypeLine
  , pattern NSTabViewBorderTypeBezel
  , NSTabViewType(NSTabViewType)
  , pattern NSTopTabsBezelBorder
  , pattern NSLeftTabsBezelBorder
  , pattern NSBottomTabsBezelBorder
  , pattern NSRightTabsBezelBorder
  , pattern NSNoTabsBezelBorder
  , pattern NSNoTabsLineBorder
  , pattern NSNoTabsNoBorder

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- selectTabViewItem:@
selectTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO ()
selectTabViewItem nsTabView tabViewItem =
  sendMessage nsTabView selectTabViewItemSelector (toNSTabViewItem tabViewItem)

-- | @- selectTabViewItemAtIndex:@
selectTabViewItemAtIndex :: IsNSTabView nsTabView => nsTabView -> CLong -> IO ()
selectTabViewItemAtIndex nsTabView index =
  sendMessage nsTabView selectTabViewItemAtIndexSelector index

-- | @- selectTabViewItemWithIdentifier:@
selectTabViewItemWithIdentifier :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectTabViewItemWithIdentifier nsTabView identifier =
  sendMessage nsTabView selectTabViewItemWithIdentifierSelector identifier

-- | @- takeSelectedTabViewItemFromSender:@
takeSelectedTabViewItemFromSender :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
takeSelectedTabViewItemFromSender nsTabView sender =
  sendMessage nsTabView takeSelectedTabViewItemFromSenderSelector sender

-- | @- selectFirstTabViewItem:@
selectFirstTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectFirstTabViewItem nsTabView sender =
  sendMessage nsTabView selectFirstTabViewItemSelector sender

-- | @- selectLastTabViewItem:@
selectLastTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectLastTabViewItem nsTabView sender =
  sendMessage nsTabView selectLastTabViewItemSelector sender

-- | @- selectNextTabViewItem:@
selectNextTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectNextTabViewItem nsTabView sender =
  sendMessage nsTabView selectNextTabViewItemSelector sender

-- | @- selectPreviousTabViewItem:@
selectPreviousTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectPreviousTabViewItem nsTabView sender =
  sendMessage nsTabView selectPreviousTabViewItemSelector sender

-- | @- addTabViewItem:@
addTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO ()
addTabViewItem nsTabView tabViewItem =
  sendMessage nsTabView addTabViewItemSelector (toNSTabViewItem tabViewItem)

-- | @- insertTabViewItem:atIndex:@
insertTabViewItem_atIndex :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> CLong -> IO ()
insertTabViewItem_atIndex nsTabView tabViewItem index =
  sendMessage nsTabView insertTabViewItem_atIndexSelector (toNSTabViewItem tabViewItem) index

-- | @- removeTabViewItem:@
removeTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO ()
removeTabViewItem nsTabView tabViewItem =
  sendMessage nsTabView removeTabViewItemSelector (toNSTabViewItem tabViewItem)

-- | @- tabViewItemAtPoint:@
tabViewItemAtPoint :: IsNSTabView nsTabView => nsTabView -> NSPoint -> IO (Id NSTabViewItem)
tabViewItemAtPoint nsTabView point =
  sendMessage nsTabView tabViewItemAtPointSelector point

-- | @- indexOfTabViewItem:@
indexOfTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO CLong
indexOfTabViewItem nsTabView tabViewItem =
  sendMessage nsTabView indexOfTabViewItemSelector (toNSTabViewItem tabViewItem)

-- | @- tabViewItemAtIndex:@
tabViewItemAtIndex :: IsNSTabView nsTabView => nsTabView -> CLong -> IO (Id NSTabViewItem)
tabViewItemAtIndex nsTabView index =
  sendMessage nsTabView tabViewItemAtIndexSelector index

-- | @- indexOfTabViewItemWithIdentifier:@
indexOfTabViewItemWithIdentifier :: IsNSTabView nsTabView => nsTabView -> RawId -> IO CLong
indexOfTabViewItemWithIdentifier nsTabView identifier =
  sendMessage nsTabView indexOfTabViewItemWithIdentifierSelector identifier

-- | @- selectedTabViewItem@
selectedTabViewItem :: IsNSTabView nsTabView => nsTabView -> IO (Id NSTabViewItem)
selectedTabViewItem nsTabView =
  sendMessage nsTabView selectedTabViewItemSelector

-- | @- font@
font :: IsNSTabView nsTabView => nsTabView -> IO (Id NSFont)
font nsTabView =
  sendMessage nsTabView fontSelector

-- | @- setFont:@
setFont :: (IsNSTabView nsTabView, IsNSFont value) => nsTabView -> value -> IO ()
setFont nsTabView value =
  sendMessage nsTabView setFontSelector (toNSFont value)

-- | @- tabViewType@
tabViewType :: IsNSTabView nsTabView => nsTabView -> IO NSTabViewType
tabViewType nsTabView =
  sendMessage nsTabView tabViewTypeSelector

-- | @- setTabViewType:@
setTabViewType :: IsNSTabView nsTabView => nsTabView -> NSTabViewType -> IO ()
setTabViewType nsTabView value =
  sendMessage nsTabView setTabViewTypeSelector value

-- | @- tabPosition@
tabPosition :: IsNSTabView nsTabView => nsTabView -> IO NSTabPosition
tabPosition nsTabView =
  sendMessage nsTabView tabPositionSelector

-- | @- setTabPosition:@
setTabPosition :: IsNSTabView nsTabView => nsTabView -> NSTabPosition -> IO ()
setTabPosition nsTabView value =
  sendMessage nsTabView setTabPositionSelector value

-- | @- tabViewBorderType@
tabViewBorderType :: IsNSTabView nsTabView => nsTabView -> IO NSTabViewBorderType
tabViewBorderType nsTabView =
  sendMessage nsTabView tabViewBorderTypeSelector

-- | @- setTabViewBorderType:@
setTabViewBorderType :: IsNSTabView nsTabView => nsTabView -> NSTabViewBorderType -> IO ()
setTabViewBorderType nsTabView value =
  sendMessage nsTabView setTabViewBorderTypeSelector value

-- | @- tabViewItems@
tabViewItems :: IsNSTabView nsTabView => nsTabView -> IO (Id NSArray)
tabViewItems nsTabView =
  sendMessage nsTabView tabViewItemsSelector

-- | @- setTabViewItems:@
setTabViewItems :: (IsNSTabView nsTabView, IsNSArray value) => nsTabView -> value -> IO ()
setTabViewItems nsTabView value =
  sendMessage nsTabView setTabViewItemsSelector (toNSArray value)

-- | @- allowsTruncatedLabels@
allowsTruncatedLabels :: IsNSTabView nsTabView => nsTabView -> IO Bool
allowsTruncatedLabels nsTabView =
  sendMessage nsTabView allowsTruncatedLabelsSelector

-- | @- setAllowsTruncatedLabels:@
setAllowsTruncatedLabels :: IsNSTabView nsTabView => nsTabView -> Bool -> IO ()
setAllowsTruncatedLabels nsTabView value =
  sendMessage nsTabView setAllowsTruncatedLabelsSelector value

-- | @- minimumSize@
minimumSize :: IsNSTabView nsTabView => nsTabView -> IO NSSize
minimumSize nsTabView =
  sendMessage nsTabView minimumSizeSelector

-- | @- drawsBackground@
drawsBackground :: IsNSTabView nsTabView => nsTabView -> IO Bool
drawsBackground nsTabView =
  sendMessage nsTabView drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTabView nsTabView => nsTabView -> Bool -> IO ()
setDrawsBackground nsTabView value =
  sendMessage nsTabView setDrawsBackgroundSelector value

-- | @- controlSize@
controlSize :: IsNSTabView nsTabView => nsTabView -> IO NSControlSize
controlSize nsTabView =
  sendMessage nsTabView controlSizeSelector

-- | @- setControlSize:@
setControlSize :: IsNSTabView nsTabView => nsTabView -> NSControlSize -> IO ()
setControlSize nsTabView value =
  sendMessage nsTabView setControlSizeSelector value

-- | @- delegate@
delegate :: IsNSTabView nsTabView => nsTabView -> IO RawId
delegate nsTabView =
  sendMessage nsTabView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
setDelegate nsTabView value =
  sendMessage nsTabView setDelegateSelector value

-- | @- contentRect@
contentRect :: IsNSTabView nsTabView => nsTabView -> IO NSRect
contentRect nsTabView =
  sendMessage nsTabView contentRectSelector

-- | @- numberOfTabViewItems@
numberOfTabViewItems :: IsNSTabView nsTabView => nsTabView -> IO CLong
numberOfTabViewItems nsTabView =
  sendMessage nsTabView numberOfTabViewItemsSelector

-- | @- controlTint@
controlTint :: IsNSTabView nsTabView => nsTabView -> IO NSControlTint
controlTint nsTabView =
  sendMessage nsTabView controlTintSelector

-- | @- setControlTint:@
setControlTint :: IsNSTabView nsTabView => nsTabView -> NSControlTint -> IO ()
setControlTint nsTabView value =
  sendMessage nsTabView setControlTintSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectTabViewItem:@
selectTabViewItemSelector :: Selector '[Id NSTabViewItem] ()
selectTabViewItemSelector = mkSelector "selectTabViewItem:"

-- | @Selector@ for @selectTabViewItemAtIndex:@
selectTabViewItemAtIndexSelector :: Selector '[CLong] ()
selectTabViewItemAtIndexSelector = mkSelector "selectTabViewItemAtIndex:"

-- | @Selector@ for @selectTabViewItemWithIdentifier:@
selectTabViewItemWithIdentifierSelector :: Selector '[RawId] ()
selectTabViewItemWithIdentifierSelector = mkSelector "selectTabViewItemWithIdentifier:"

-- | @Selector@ for @takeSelectedTabViewItemFromSender:@
takeSelectedTabViewItemFromSenderSelector :: Selector '[RawId] ()
takeSelectedTabViewItemFromSenderSelector = mkSelector "takeSelectedTabViewItemFromSender:"

-- | @Selector@ for @selectFirstTabViewItem:@
selectFirstTabViewItemSelector :: Selector '[RawId] ()
selectFirstTabViewItemSelector = mkSelector "selectFirstTabViewItem:"

-- | @Selector@ for @selectLastTabViewItem:@
selectLastTabViewItemSelector :: Selector '[RawId] ()
selectLastTabViewItemSelector = mkSelector "selectLastTabViewItem:"

-- | @Selector@ for @selectNextTabViewItem:@
selectNextTabViewItemSelector :: Selector '[RawId] ()
selectNextTabViewItemSelector = mkSelector "selectNextTabViewItem:"

-- | @Selector@ for @selectPreviousTabViewItem:@
selectPreviousTabViewItemSelector :: Selector '[RawId] ()
selectPreviousTabViewItemSelector = mkSelector "selectPreviousTabViewItem:"

-- | @Selector@ for @addTabViewItem:@
addTabViewItemSelector :: Selector '[Id NSTabViewItem] ()
addTabViewItemSelector = mkSelector "addTabViewItem:"

-- | @Selector@ for @insertTabViewItem:atIndex:@
insertTabViewItem_atIndexSelector :: Selector '[Id NSTabViewItem, CLong] ()
insertTabViewItem_atIndexSelector = mkSelector "insertTabViewItem:atIndex:"

-- | @Selector@ for @removeTabViewItem:@
removeTabViewItemSelector :: Selector '[Id NSTabViewItem] ()
removeTabViewItemSelector = mkSelector "removeTabViewItem:"

-- | @Selector@ for @tabViewItemAtPoint:@
tabViewItemAtPointSelector :: Selector '[NSPoint] (Id NSTabViewItem)
tabViewItemAtPointSelector = mkSelector "tabViewItemAtPoint:"

-- | @Selector@ for @indexOfTabViewItem:@
indexOfTabViewItemSelector :: Selector '[Id NSTabViewItem] CLong
indexOfTabViewItemSelector = mkSelector "indexOfTabViewItem:"

-- | @Selector@ for @tabViewItemAtIndex:@
tabViewItemAtIndexSelector :: Selector '[CLong] (Id NSTabViewItem)
tabViewItemAtIndexSelector = mkSelector "tabViewItemAtIndex:"

-- | @Selector@ for @indexOfTabViewItemWithIdentifier:@
indexOfTabViewItemWithIdentifierSelector :: Selector '[RawId] CLong
indexOfTabViewItemWithIdentifierSelector = mkSelector "indexOfTabViewItemWithIdentifier:"

-- | @Selector@ for @selectedTabViewItem@
selectedTabViewItemSelector :: Selector '[] (Id NSTabViewItem)
selectedTabViewItemSelector = mkSelector "selectedTabViewItem"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @tabViewType@
tabViewTypeSelector :: Selector '[] NSTabViewType
tabViewTypeSelector = mkSelector "tabViewType"

-- | @Selector@ for @setTabViewType:@
setTabViewTypeSelector :: Selector '[NSTabViewType] ()
setTabViewTypeSelector = mkSelector "setTabViewType:"

-- | @Selector@ for @tabPosition@
tabPositionSelector :: Selector '[] NSTabPosition
tabPositionSelector = mkSelector "tabPosition"

-- | @Selector@ for @setTabPosition:@
setTabPositionSelector :: Selector '[NSTabPosition] ()
setTabPositionSelector = mkSelector "setTabPosition:"

-- | @Selector@ for @tabViewBorderType@
tabViewBorderTypeSelector :: Selector '[] NSTabViewBorderType
tabViewBorderTypeSelector = mkSelector "tabViewBorderType"

-- | @Selector@ for @setTabViewBorderType:@
setTabViewBorderTypeSelector :: Selector '[NSTabViewBorderType] ()
setTabViewBorderTypeSelector = mkSelector "setTabViewBorderType:"

-- | @Selector@ for @tabViewItems@
tabViewItemsSelector :: Selector '[] (Id NSArray)
tabViewItemsSelector = mkSelector "tabViewItems"

-- | @Selector@ for @setTabViewItems:@
setTabViewItemsSelector :: Selector '[Id NSArray] ()
setTabViewItemsSelector = mkSelector "setTabViewItems:"

-- | @Selector@ for @allowsTruncatedLabels@
allowsTruncatedLabelsSelector :: Selector '[] Bool
allowsTruncatedLabelsSelector = mkSelector "allowsTruncatedLabels"

-- | @Selector@ for @setAllowsTruncatedLabels:@
setAllowsTruncatedLabelsSelector :: Selector '[Bool] ()
setAllowsTruncatedLabelsSelector = mkSelector "setAllowsTruncatedLabels:"

-- | @Selector@ for @minimumSize@
minimumSizeSelector :: Selector '[] NSSize
minimumSizeSelector = mkSelector "minimumSize"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector '[] NSControlSize
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector '[NSControlSize] ()
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @contentRect@
contentRectSelector :: Selector '[] NSRect
contentRectSelector = mkSelector "contentRect"

-- | @Selector@ for @numberOfTabViewItems@
numberOfTabViewItemsSelector :: Selector '[] CLong
numberOfTabViewItemsSelector = mkSelector "numberOfTabViewItems"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector '[] NSControlTint
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector '[NSControlTint] ()
setControlTintSelector = mkSelector "setControlTint:"

