{-# LANGUAGE PatternSynonyms #-}
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
  , selectTabViewItemSelector
  , selectTabViewItemAtIndexSelector
  , selectTabViewItemWithIdentifierSelector
  , takeSelectedTabViewItemFromSenderSelector
  , selectFirstTabViewItemSelector
  , selectLastTabViewItemSelector
  , selectNextTabViewItemSelector
  , selectPreviousTabViewItemSelector
  , addTabViewItemSelector
  , insertTabViewItem_atIndexSelector
  , removeTabViewItemSelector
  , tabViewItemAtPointSelector
  , indexOfTabViewItemSelector
  , tabViewItemAtIndexSelector
  , indexOfTabViewItemWithIdentifierSelector
  , selectedTabViewItemSelector
  , fontSelector
  , setFontSelector
  , tabViewTypeSelector
  , setTabViewTypeSelector
  , tabPositionSelector
  , setTabPositionSelector
  , tabViewBorderTypeSelector
  , setTabViewBorderTypeSelector
  , tabViewItemsSelector
  , setTabViewItemsSelector
  , allowsTruncatedLabelsSelector
  , setAllowsTruncatedLabelsSelector
  , minimumSizeSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , controlSizeSelector
  , setControlSizeSelector
  , delegateSelector
  , setDelegateSelector
  , contentRectSelector
  , numberOfTabViewItemsSelector
  , controlTintSelector
  , setControlTintSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- selectTabViewItem:@
selectTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO ()
selectTabViewItem nsTabView  tabViewItem =
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabView (mkSelector "selectTabViewItem:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- selectTabViewItemAtIndex:@
selectTabViewItemAtIndex :: IsNSTabView nsTabView => nsTabView -> CLong -> IO ()
selectTabViewItemAtIndex nsTabView  index =
    sendMsg nsTabView (mkSelector "selectTabViewItemAtIndex:") retVoid [argCLong index]

-- | @- selectTabViewItemWithIdentifier:@
selectTabViewItemWithIdentifier :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectTabViewItemWithIdentifier nsTabView  identifier =
    sendMsg nsTabView (mkSelector "selectTabViewItemWithIdentifier:") retVoid [argPtr (castPtr (unRawId identifier) :: Ptr ())]

-- | @- takeSelectedTabViewItemFromSender:@
takeSelectedTabViewItemFromSender :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
takeSelectedTabViewItemFromSender nsTabView  sender =
    sendMsg nsTabView (mkSelector "takeSelectedTabViewItemFromSender:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectFirstTabViewItem:@
selectFirstTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectFirstTabViewItem nsTabView  sender =
    sendMsg nsTabView (mkSelector "selectFirstTabViewItem:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectLastTabViewItem:@
selectLastTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectLastTabViewItem nsTabView  sender =
    sendMsg nsTabView (mkSelector "selectLastTabViewItem:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectNextTabViewItem:@
selectNextTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectNextTabViewItem nsTabView  sender =
    sendMsg nsTabView (mkSelector "selectNextTabViewItem:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectPreviousTabViewItem:@
selectPreviousTabViewItem :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
selectPreviousTabViewItem nsTabView  sender =
    sendMsg nsTabView (mkSelector "selectPreviousTabViewItem:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- addTabViewItem:@
addTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO ()
addTabViewItem nsTabView  tabViewItem =
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabView (mkSelector "addTabViewItem:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- insertTabViewItem:atIndex:@
insertTabViewItem_atIndex :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> CLong -> IO ()
insertTabViewItem_atIndex nsTabView  tabViewItem index =
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabView (mkSelector "insertTabViewItem:atIndex:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ()), argCLong index]

-- | @- removeTabViewItem:@
removeTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO ()
removeTabViewItem nsTabView  tabViewItem =
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabView (mkSelector "removeTabViewItem:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- tabViewItemAtPoint:@
tabViewItemAtPoint :: IsNSTabView nsTabView => nsTabView -> NSPoint -> IO (Id NSTabViewItem)
tabViewItemAtPoint nsTabView  point =
    sendMsg nsTabView (mkSelector "tabViewItemAtPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- indexOfTabViewItem:@
indexOfTabViewItem :: (IsNSTabView nsTabView, IsNSTabViewItem tabViewItem) => nsTabView -> tabViewItem -> IO CLong
indexOfTabViewItem nsTabView  tabViewItem =
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabView (mkSelector "indexOfTabViewItem:") retCLong [argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- tabViewItemAtIndex:@
tabViewItemAtIndex :: IsNSTabView nsTabView => nsTabView -> CLong -> IO (Id NSTabViewItem)
tabViewItemAtIndex nsTabView  index =
    sendMsg nsTabView (mkSelector "tabViewItemAtIndex:") (retPtr retVoid) [argCLong index] >>= retainedObject . castPtr

-- | @- indexOfTabViewItemWithIdentifier:@
indexOfTabViewItemWithIdentifier :: IsNSTabView nsTabView => nsTabView -> RawId -> IO CLong
indexOfTabViewItemWithIdentifier nsTabView  identifier =
    sendMsg nsTabView (mkSelector "indexOfTabViewItemWithIdentifier:") retCLong [argPtr (castPtr (unRawId identifier) :: Ptr ())]

-- | @- selectedTabViewItem@
selectedTabViewItem :: IsNSTabView nsTabView => nsTabView -> IO (Id NSTabViewItem)
selectedTabViewItem nsTabView  =
    sendMsg nsTabView (mkSelector "selectedTabViewItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- font@
font :: IsNSTabView nsTabView => nsTabView -> IO (Id NSFont)
font nsTabView  =
    sendMsg nsTabView (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsNSTabView nsTabView, IsNSFont value) => nsTabView -> value -> IO ()
setFont nsTabView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTabView (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tabViewType@
tabViewType :: IsNSTabView nsTabView => nsTabView -> IO NSTabViewType
tabViewType nsTabView  =
    fmap (coerce :: CULong -> NSTabViewType) $ sendMsg nsTabView (mkSelector "tabViewType") retCULong []

-- | @- setTabViewType:@
setTabViewType :: IsNSTabView nsTabView => nsTabView -> NSTabViewType -> IO ()
setTabViewType nsTabView  value =
    sendMsg nsTabView (mkSelector "setTabViewType:") retVoid [argCULong (coerce value)]

-- | @- tabPosition@
tabPosition :: IsNSTabView nsTabView => nsTabView -> IO NSTabPosition
tabPosition nsTabView  =
    fmap (coerce :: CULong -> NSTabPosition) $ sendMsg nsTabView (mkSelector "tabPosition") retCULong []

-- | @- setTabPosition:@
setTabPosition :: IsNSTabView nsTabView => nsTabView -> NSTabPosition -> IO ()
setTabPosition nsTabView  value =
    sendMsg nsTabView (mkSelector "setTabPosition:") retVoid [argCULong (coerce value)]

-- | @- tabViewBorderType@
tabViewBorderType :: IsNSTabView nsTabView => nsTabView -> IO NSTabViewBorderType
tabViewBorderType nsTabView  =
    fmap (coerce :: CULong -> NSTabViewBorderType) $ sendMsg nsTabView (mkSelector "tabViewBorderType") retCULong []

-- | @- setTabViewBorderType:@
setTabViewBorderType :: IsNSTabView nsTabView => nsTabView -> NSTabViewBorderType -> IO ()
setTabViewBorderType nsTabView  value =
    sendMsg nsTabView (mkSelector "setTabViewBorderType:") retVoid [argCULong (coerce value)]

-- | @- tabViewItems@
tabViewItems :: IsNSTabView nsTabView => nsTabView -> IO (Id NSArray)
tabViewItems nsTabView  =
    sendMsg nsTabView (mkSelector "tabViewItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTabViewItems:@
setTabViewItems :: (IsNSTabView nsTabView, IsNSArray value) => nsTabView -> value -> IO ()
setTabViewItems nsTabView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTabView (mkSelector "setTabViewItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsTruncatedLabels@
allowsTruncatedLabels :: IsNSTabView nsTabView => nsTabView -> IO Bool
allowsTruncatedLabels nsTabView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTabView (mkSelector "allowsTruncatedLabels") retCULong []

-- | @- setAllowsTruncatedLabels:@
setAllowsTruncatedLabels :: IsNSTabView nsTabView => nsTabView -> Bool -> IO ()
setAllowsTruncatedLabels nsTabView  value =
    sendMsg nsTabView (mkSelector "setAllowsTruncatedLabels:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minimumSize@
minimumSize :: IsNSTabView nsTabView => nsTabView -> IO NSSize
minimumSize nsTabView  =
    sendMsgStret nsTabView (mkSelector "minimumSize") retNSSize []

-- | @- drawsBackground@
drawsBackground :: IsNSTabView nsTabView => nsTabView -> IO Bool
drawsBackground nsTabView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTabView (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTabView nsTabView => nsTabView -> Bool -> IO ()
setDrawsBackground nsTabView  value =
    sendMsg nsTabView (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- controlSize@
controlSize :: IsNSTabView nsTabView => nsTabView -> IO NSControlSize
controlSize nsTabView  =
    fmap (coerce :: CULong -> NSControlSize) $ sendMsg nsTabView (mkSelector "controlSize") retCULong []

-- | @- setControlSize:@
setControlSize :: IsNSTabView nsTabView => nsTabView -> NSControlSize -> IO ()
setControlSize nsTabView  value =
    sendMsg nsTabView (mkSelector "setControlSize:") retVoid [argCULong (coerce value)]

-- | @- delegate@
delegate :: IsNSTabView nsTabView => nsTabView -> IO RawId
delegate nsTabView  =
    fmap (RawId . castPtr) $ sendMsg nsTabView (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSTabView nsTabView => nsTabView -> RawId -> IO ()
setDelegate nsTabView  value =
    sendMsg nsTabView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- contentRect@
contentRect :: IsNSTabView nsTabView => nsTabView -> IO NSRect
contentRect nsTabView  =
    sendMsgStret nsTabView (mkSelector "contentRect") retNSRect []

-- | @- numberOfTabViewItems@
numberOfTabViewItems :: IsNSTabView nsTabView => nsTabView -> IO CLong
numberOfTabViewItems nsTabView  =
    sendMsg nsTabView (mkSelector "numberOfTabViewItems") retCLong []

-- | @- controlTint@
controlTint :: IsNSTabView nsTabView => nsTabView -> IO NSControlTint
controlTint nsTabView  =
    fmap (coerce :: CULong -> NSControlTint) $ sendMsg nsTabView (mkSelector "controlTint") retCULong []

-- | @- setControlTint:@
setControlTint :: IsNSTabView nsTabView => nsTabView -> NSControlTint -> IO ()
setControlTint nsTabView  value =
    sendMsg nsTabView (mkSelector "setControlTint:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectTabViewItem:@
selectTabViewItemSelector :: Selector
selectTabViewItemSelector = mkSelector "selectTabViewItem:"

-- | @Selector@ for @selectTabViewItemAtIndex:@
selectTabViewItemAtIndexSelector :: Selector
selectTabViewItemAtIndexSelector = mkSelector "selectTabViewItemAtIndex:"

-- | @Selector@ for @selectTabViewItemWithIdentifier:@
selectTabViewItemWithIdentifierSelector :: Selector
selectTabViewItemWithIdentifierSelector = mkSelector "selectTabViewItemWithIdentifier:"

-- | @Selector@ for @takeSelectedTabViewItemFromSender:@
takeSelectedTabViewItemFromSenderSelector :: Selector
takeSelectedTabViewItemFromSenderSelector = mkSelector "takeSelectedTabViewItemFromSender:"

-- | @Selector@ for @selectFirstTabViewItem:@
selectFirstTabViewItemSelector :: Selector
selectFirstTabViewItemSelector = mkSelector "selectFirstTabViewItem:"

-- | @Selector@ for @selectLastTabViewItem:@
selectLastTabViewItemSelector :: Selector
selectLastTabViewItemSelector = mkSelector "selectLastTabViewItem:"

-- | @Selector@ for @selectNextTabViewItem:@
selectNextTabViewItemSelector :: Selector
selectNextTabViewItemSelector = mkSelector "selectNextTabViewItem:"

-- | @Selector@ for @selectPreviousTabViewItem:@
selectPreviousTabViewItemSelector :: Selector
selectPreviousTabViewItemSelector = mkSelector "selectPreviousTabViewItem:"

-- | @Selector@ for @addTabViewItem:@
addTabViewItemSelector :: Selector
addTabViewItemSelector = mkSelector "addTabViewItem:"

-- | @Selector@ for @insertTabViewItem:atIndex:@
insertTabViewItem_atIndexSelector :: Selector
insertTabViewItem_atIndexSelector = mkSelector "insertTabViewItem:atIndex:"

-- | @Selector@ for @removeTabViewItem:@
removeTabViewItemSelector :: Selector
removeTabViewItemSelector = mkSelector "removeTabViewItem:"

-- | @Selector@ for @tabViewItemAtPoint:@
tabViewItemAtPointSelector :: Selector
tabViewItemAtPointSelector = mkSelector "tabViewItemAtPoint:"

-- | @Selector@ for @indexOfTabViewItem:@
indexOfTabViewItemSelector :: Selector
indexOfTabViewItemSelector = mkSelector "indexOfTabViewItem:"

-- | @Selector@ for @tabViewItemAtIndex:@
tabViewItemAtIndexSelector :: Selector
tabViewItemAtIndexSelector = mkSelector "tabViewItemAtIndex:"

-- | @Selector@ for @indexOfTabViewItemWithIdentifier:@
indexOfTabViewItemWithIdentifierSelector :: Selector
indexOfTabViewItemWithIdentifierSelector = mkSelector "indexOfTabViewItemWithIdentifier:"

-- | @Selector@ for @selectedTabViewItem@
selectedTabViewItemSelector :: Selector
selectedTabViewItemSelector = mkSelector "selectedTabViewItem"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @tabViewType@
tabViewTypeSelector :: Selector
tabViewTypeSelector = mkSelector "tabViewType"

-- | @Selector@ for @setTabViewType:@
setTabViewTypeSelector :: Selector
setTabViewTypeSelector = mkSelector "setTabViewType:"

-- | @Selector@ for @tabPosition@
tabPositionSelector :: Selector
tabPositionSelector = mkSelector "tabPosition"

-- | @Selector@ for @setTabPosition:@
setTabPositionSelector :: Selector
setTabPositionSelector = mkSelector "setTabPosition:"

-- | @Selector@ for @tabViewBorderType@
tabViewBorderTypeSelector :: Selector
tabViewBorderTypeSelector = mkSelector "tabViewBorderType"

-- | @Selector@ for @setTabViewBorderType:@
setTabViewBorderTypeSelector :: Selector
setTabViewBorderTypeSelector = mkSelector "setTabViewBorderType:"

-- | @Selector@ for @tabViewItems@
tabViewItemsSelector :: Selector
tabViewItemsSelector = mkSelector "tabViewItems"

-- | @Selector@ for @setTabViewItems:@
setTabViewItemsSelector :: Selector
setTabViewItemsSelector = mkSelector "setTabViewItems:"

-- | @Selector@ for @allowsTruncatedLabels@
allowsTruncatedLabelsSelector :: Selector
allowsTruncatedLabelsSelector = mkSelector "allowsTruncatedLabels"

-- | @Selector@ for @setAllowsTruncatedLabels:@
setAllowsTruncatedLabelsSelector :: Selector
setAllowsTruncatedLabelsSelector = mkSelector "setAllowsTruncatedLabels:"

-- | @Selector@ for @minimumSize@
minimumSizeSelector :: Selector
minimumSizeSelector = mkSelector "minimumSize"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @contentRect@
contentRectSelector :: Selector
contentRectSelector = mkSelector "contentRect"

-- | @Selector@ for @numberOfTabViewItems@
numberOfTabViewItemsSelector :: Selector
numberOfTabViewItemsSelector = mkSelector "numberOfTabViewItems"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector
setControlTintSelector = mkSelector "setControlTint:"

