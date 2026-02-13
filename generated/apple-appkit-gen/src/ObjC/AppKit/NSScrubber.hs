{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubber
--
-- @NSScrubber@ is a control designed for the NSTouchBar environment.
--
-- @NSScrubber@ arranges a finite number of "items" (represented by views of type @NSScrubberItemView@ ) according to a layout object (see @NSScrubberLayout@ ), and provides several methods for navigating and selecting those items.
--
-- Clients provide data to @NSScrubber@ via a data source object (see the @NSScrubberDataSource@ protocol) and react to user interaction via a delegate object (see the @NSScrubberDelegate@ protocol).
--
-- Generated bindings for @NSScrubber@.
module ObjC.AppKit.NSScrubber
  ( NSScrubber
  , IsNSScrubber(..)
  , initWithFrame
  , initWithCoder
  , reloadData
  , performSequentialBatchUpdates
  , insertItemsAtIndexes
  , removeItemsAtIndexes
  , reloadItemsAtIndexes
  , moveItemAtIndex_toIndex
  , scrollItemAtIndex_toAlignment
  , itemViewForItemAtIndex
  , registerClass_forItemIdentifier
  , registerNib_forItemIdentifier
  , makeItemWithIdentifier_owner
  , dataSource
  , setDataSource
  , delegate
  , setDelegate
  , scrubberLayout
  , setScrubberLayout
  , numberOfItems
  , highlightedIndex
  , selectedIndex
  , setSelectedIndex
  , mode
  , setMode
  , itemAlignment
  , setItemAlignment
  , continuous
  , setContinuous
  , floatsSelectionViews
  , setFloatsSelectionViews
  , selectionBackgroundStyle
  , setSelectionBackgroundStyle
  , selectionOverlayStyle
  , setSelectionOverlayStyle
  , showsArrowButtons
  , setShowsArrowButtons
  , showsAdditionalContentIndicators
  , setShowsAdditionalContentIndicators
  , backgroundColor
  , setBackgroundColor
  , backgroundView
  , setBackgroundView
  , backgroundColorSelector
  , backgroundViewSelector
  , continuousSelector
  , dataSourceSelector
  , delegateSelector
  , floatsSelectionViewsSelector
  , highlightedIndexSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , insertItemsAtIndexesSelector
  , itemAlignmentSelector
  , itemViewForItemAtIndexSelector
  , makeItemWithIdentifier_ownerSelector
  , modeSelector
  , moveItemAtIndex_toIndexSelector
  , numberOfItemsSelector
  , performSequentialBatchUpdatesSelector
  , registerClass_forItemIdentifierSelector
  , registerNib_forItemIdentifierSelector
  , reloadDataSelector
  , reloadItemsAtIndexesSelector
  , removeItemsAtIndexesSelector
  , scrollItemAtIndex_toAlignmentSelector
  , scrubberLayoutSelector
  , selectedIndexSelector
  , selectionBackgroundStyleSelector
  , selectionOverlayStyleSelector
  , setBackgroundColorSelector
  , setBackgroundViewSelector
  , setContinuousSelector
  , setDataSourceSelector
  , setDelegateSelector
  , setFloatsSelectionViewsSelector
  , setItemAlignmentSelector
  , setModeSelector
  , setScrubberLayoutSelector
  , setSelectedIndexSelector
  , setSelectionBackgroundStyleSelector
  , setSelectionOverlayStyleSelector
  , setShowsAdditionalContentIndicatorsSelector
  , setShowsArrowButtonsSelector
  , showsAdditionalContentIndicatorsSelector
  , showsArrowButtonsSelector

  -- * Enum types
  , NSScrubberAlignment(NSScrubberAlignment)
  , pattern NSScrubberAlignmentNone
  , pattern NSScrubberAlignmentLeading
  , pattern NSScrubberAlignmentTrailing
  , pattern NSScrubberAlignmentCenter
  , NSScrubberMode(NSScrubberMode)
  , pattern NSScrubberModeFixed
  , pattern NSScrubberModeFree

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

-- | @- initWithFrame:@
initWithFrame :: IsNSScrubber nsScrubber => nsScrubber -> NSRect -> IO (Id NSScrubber)
initWithFrame nsScrubber frameRect =
  sendOwnedMessage nsScrubber initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubber nsScrubber, IsNSCoder coder) => nsScrubber -> coder -> IO (Id NSScrubber)
initWithCoder nsScrubber coder =
  sendOwnedMessage nsScrubber initWithCoderSelector (toNSCoder coder)

-- | Invalidate all data within the scrubber control, triggering a reload of all content, and clearing the current selection.
--
-- ObjC selector: @- reloadData@
reloadData :: IsNSScrubber nsScrubber => nsScrubber -> IO ()
reloadData nsScrubber =
  sendMessage nsScrubber reloadDataSelector

-- | Updates inside the @performSequentialBatchUpdates@ block are processed and displayed all at once, including insertion, removal, moving, reloading items, and changing the selected index. Changes are performed iteratively using the same semantics as @NSMutableArray.@ NSScrubber expects its dataSource to reflect the changes made inside @-performSequentialBatchUpdates:@ immediately after the @updateBlock@ finishes executing.
--
-- ObjC selector: @- performSequentialBatchUpdates:@
performSequentialBatchUpdates :: IsNSScrubber nsScrubber => nsScrubber -> Ptr () -> IO ()
performSequentialBatchUpdates nsScrubber updateBlock =
  sendMessage nsScrubber performSequentialBatchUpdatesSelector updateBlock

-- | Inserts new items at the specified indexes. NSScrubber will request views for each new index from the @dataSource.@ This method uses the same semantics as @NSMutableArray;@ each index in the set specifies the destination index after all previous insertions have occurred. Therefore, an NSIndexSet of [1,2,3] will result in three new contiguous items.
--
-- ObjC selector: @- insertItemsAtIndexes:@
insertItemsAtIndexes :: (IsNSScrubber nsScrubber, IsNSIndexSet indexes) => nsScrubber -> indexes -> IO ()
insertItemsAtIndexes nsScrubber indexes =
  sendMessage nsScrubber insertItemsAtIndexesSelector (toNSIndexSet indexes)

-- | Removes the items at the specified indexes. This method uses the same semantics as @NSMutableArray.@
--
-- ObjC selector: @- removeItemsAtIndexes:@
removeItemsAtIndexes :: (IsNSScrubber nsScrubber, IsNSIndexSet indexes) => nsScrubber -> indexes -> IO ()
removeItemsAtIndexes nsScrubber indexes =
  sendMessage nsScrubber removeItemsAtIndexesSelector (toNSIndexSet indexes)

-- | Reloads the items at the specified indexes. NSScrubber will request new views for each item and smoothly crossfade between them before discarding the original views.
--
-- ObjC selector: @- reloadItemsAtIndexes:@
reloadItemsAtIndexes :: (IsNSScrubber nsScrubber, IsNSIndexSet indexes) => nsScrubber -> indexes -> IO ()
reloadItemsAtIndexes nsScrubber indexes =
  sendMessage nsScrubber reloadItemsAtIndexesSelector (toNSIndexSet indexes)

-- | Moves an item from one index to another. @oldIndex@ refers to the item's index prior to the movement, whereas @newIndex@ refers to the item's final location.
--
-- ObjC selector: @- moveItemAtIndex:toIndex:@
moveItemAtIndex_toIndex :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> CLong -> IO ()
moveItemAtIndex_toIndex nsScrubber oldIndex newIndex =
  sendMessage nsScrubber moveItemAtIndex_toIndexSelector oldIndex newIndex

-- | Scrolls an item to a given alignment within the control. If @NSScrubberAlignmentNone@ is provided, then the control scrolls the minimum amount necessary to make the item visible. Scrolling is animated if called on the animator proxy.
--
-- ObjC selector: @- scrollItemAtIndex:toAlignment:@
scrollItemAtIndex_toAlignment :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> NSScrubberAlignment -> IO ()
scrollItemAtIndex_toAlignment nsScrubber index alignment =
  sendMessage nsScrubber scrollItemAtIndex_toAlignmentSelector index alignment

-- | Returns the @NSScrubberItemView@ for the given index, if one currently exists; returns @nil@ otherwise.
--
-- ObjC selector: @- itemViewForItemAtIndex:@
itemViewForItemAtIndex :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> IO (Id NSScrubberItemView)
itemViewForItemAtIndex nsScrubber index =
  sendMessage nsScrubber itemViewForItemAtIndexSelector index

-- | Registers a @NSScrubberItemView@ class to be instantiated for the given @itemIdentifier.@ Raises an exception if @itemViewClass@ is not a subclass of @NSScrubberItemView.@ Passing @nil@ for @itemViewClass@ removes a previous registration. Registrations made through this method do not persist through NSCoding.
--
-- ObjC selector: @- registerClass:forItemIdentifier:@
registerClass_forItemIdentifier :: (IsNSScrubber nsScrubber, IsNSString itemIdentifier) => nsScrubber -> Class -> itemIdentifier -> IO ()
registerClass_forItemIdentifier nsScrubber itemViewClass itemIdentifier =
  sendMessage nsScrubber registerClass_forItemIdentifierSelector itemViewClass (toNSString itemIdentifier)

-- | Register a nib to be instantiated for the given @itemIdentifier.@ The nib must contain a top-level object which is a subclass of NSScrubberItemView; otherwise, @-makeItemWithIdentifier:@ may return @nil@ for this identifier. Passing @nil@ for @nib@ removes a previous registration.
--
-- ObjC selector: @- registerNib:forItemIdentifier:@
registerNib_forItemIdentifier :: (IsNSScrubber nsScrubber, IsNSNib nib, IsNSString itemIdentifier) => nsScrubber -> nib -> itemIdentifier -> IO ()
registerNib_forItemIdentifier nsScrubber nib itemIdentifier =
  sendMessage nsScrubber registerNib_forItemIdentifierSelector (toNSNib nib) (toNSString itemIdentifier)

-- | Creates or reuses a @NSScrubberItemView@ corresponding to the provided @itemIdentifier.@ @NSScrubber@ searches, in order: the reuse queue, the list of registered classes, and then the list of registered nibs. If the reuse queue is empty, and there is no Class or Interface Builder archive registered for the @itemIdentifier,@ this method returns @nil.@
--
-- ObjC selector: @- makeItemWithIdentifier:owner:@
makeItemWithIdentifier_owner :: (IsNSScrubber nsScrubber, IsNSString itemIdentifier) => nsScrubber -> itemIdentifier -> RawId -> IO (Id NSScrubberItemView)
makeItemWithIdentifier_owner nsScrubber itemIdentifier owner =
  sendMessage nsScrubber makeItemWithIdentifier_ownerSelector (toNSString itemIdentifier) owner

-- | @- dataSource@
dataSource :: IsNSScrubber nsScrubber => nsScrubber -> IO RawId
dataSource nsScrubber =
  sendMessage nsScrubber dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsNSScrubber nsScrubber => nsScrubber -> RawId -> IO ()
setDataSource nsScrubber value =
  sendMessage nsScrubber setDataSourceSelector value

-- | @- delegate@
delegate :: IsNSScrubber nsScrubber => nsScrubber -> IO RawId
delegate nsScrubber =
  sendMessage nsScrubber delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSScrubber nsScrubber => nsScrubber -> RawId -> IO ()
setDelegate nsScrubber value =
  sendMessage nsScrubber setDelegateSelector value

-- | @- scrubberLayout@
scrubberLayout :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSScrubberLayout)
scrubberLayout nsScrubber =
  sendMessage nsScrubber scrubberLayoutSelector

-- | @- setScrubberLayout:@
setScrubberLayout :: (IsNSScrubber nsScrubber, IsNSScrubberLayout value) => nsScrubber -> value -> IO ()
setScrubberLayout nsScrubber value =
  sendMessage nsScrubber setScrubberLayoutSelector (toNSScrubberLayout value)

-- | Returns the number of items represented by the scrubber control.
--
-- ObjC selector: @- numberOfItems@
numberOfItems :: IsNSScrubber nsScrubber => nsScrubber -> IO CLong
numberOfItems nsScrubber =
  sendMessage nsScrubber numberOfItemsSelector

-- | The index of the currently highlighted item within the control. If there is no highlighted item, the value of this property is (-1).
--
-- ObjC selector: @- highlightedIndex@
highlightedIndex :: IsNSScrubber nsScrubber => nsScrubber -> IO CLong
highlightedIndex nsScrubber =
  sendMessage nsScrubber highlightedIndexSelector

-- | The index of the selected item within the control. If there is no selected item, the value of this property is (-1). Setting this property through the animator proxy will animate the selection change. Programmatic selection changes do not trigger delegate callbacks.
--
-- ObjC selector: @- selectedIndex@
selectedIndex :: IsNSScrubber nsScrubber => nsScrubber -> IO CLong
selectedIndex nsScrubber =
  sendMessage nsScrubber selectedIndexSelector

-- | The index of the selected item within the control. If there is no selected item, the value of this property is (-1). Setting this property through the animator proxy will animate the selection change. Programmatic selection changes do not trigger delegate callbacks.
--
-- ObjC selector: @- setSelectedIndex:@
setSelectedIndex :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> IO ()
setSelectedIndex nsScrubber value =
  sendMessage nsScrubber setSelectedIndexSelector value

-- | Describes the interaction mode for the scrubber control. See the @NSScrubberMode@ enumeration for a list of possible values. The default value is @NSScrubberModeFixed.@
--
-- ObjC selector: @- mode@
mode :: IsNSScrubber nsScrubber => nsScrubber -> IO NSScrubberMode
mode nsScrubber =
  sendMessage nsScrubber modeSelector

-- | Describes the interaction mode for the scrubber control. See the @NSScrubberMode@ enumeration for a list of possible values. The default value is @NSScrubberModeFixed.@
--
-- ObjC selector: @- setMode:@
setMode :: IsNSScrubber nsScrubber => nsScrubber -> NSScrubberMode -> IO ()
setMode nsScrubber value =
  sendMessage nsScrubber setModeSelector value

-- | If the value of @itemAlignment@ is not @NSScrubberAlignmentNone,@ the scrubber will ensure that some item rests at the preferred alignment within the control following a scrolling or paging interaction. The default value is @NSScrubberAlignmentNone.@
--
-- ObjC selector: @- itemAlignment@
itemAlignment :: IsNSScrubber nsScrubber => nsScrubber -> IO NSScrubberAlignment
itemAlignment nsScrubber =
  sendMessage nsScrubber itemAlignmentSelector

-- | If the value of @itemAlignment@ is not @NSScrubberAlignmentNone,@ the scrubber will ensure that some item rests at the preferred alignment within the control following a scrolling or paging interaction. The default value is @NSScrubberAlignmentNone.@
--
-- ObjC selector: @- setItemAlignment:@
setItemAlignment :: IsNSScrubber nsScrubber => nsScrubber -> NSScrubberAlignment -> IO ()
setItemAlignment nsScrubber value =
  sendMessage nsScrubber setItemAlignmentSelector value

-- | When @continuous@ is @YES,@ panning over the control in @NSScrubberModeFixed@ will immediately select the item under the user's finger, and scrolling in @NSScrubberModeFree@ will continuously select items as they pass through the current @itemAlignment.@ The default is @NO.@
--
-- ObjC selector: @- continuous@
continuous :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
continuous nsScrubber =
  sendMessage nsScrubber continuousSelector

-- | When @continuous@ is @YES,@ panning over the control in @NSScrubberModeFixed@ will immediately select the item under the user's finger, and scrolling in @NSScrubberModeFree@ will continuously select items as they pass through the current @itemAlignment.@ The default is @NO.@
--
-- ObjC selector: @- setContinuous:@
setContinuous :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setContinuous nsScrubber value =
  sendMessage nsScrubber setContinuousSelector value

-- | When @floatsSelectionViews@ is @YES,@ the selection decorations provided by @selectionBackgroundStyle@ and @selectionOverlayStyle@ will smoothly float between selected items, rather than animating their entrance/exit in-place. The default is @NO.@
--
-- ObjC selector: @- floatsSelectionViews@
floatsSelectionViews :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
floatsSelectionViews nsScrubber =
  sendMessage nsScrubber floatsSelectionViewsSelector

-- | When @floatsSelectionViews@ is @YES,@ the selection decorations provided by @selectionBackgroundStyle@ and @selectionOverlayStyle@ will smoothly float between selected items, rather than animating their entrance/exit in-place. The default is @NO.@
--
-- ObjC selector: @- setFloatsSelectionViews:@
setFloatsSelectionViews :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setFloatsSelectionViews nsScrubber value =
  sendMessage nsScrubber setFloatsSelectionViewsSelector value

-- | Specifies a style of decoration to place behind items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in background decoration.
--
-- ObjC selector: @- selectionBackgroundStyle@
selectionBackgroundStyle :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSScrubberSelectionStyle)
selectionBackgroundStyle nsScrubber =
  sendMessage nsScrubber selectionBackgroundStyleSelector

-- | Specifies a style of decoration to place behind items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in background decoration.
--
-- ObjC selector: @- setSelectionBackgroundStyle:@
setSelectionBackgroundStyle :: (IsNSScrubber nsScrubber, IsNSScrubberSelectionStyle value) => nsScrubber -> value -> IO ()
setSelectionBackgroundStyle nsScrubber value =
  sendMessage nsScrubber setSelectionBackgroundStyleSelector (toNSScrubberSelectionStyle value)

-- | Specifies a style of decoration to place above items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in overlay decoration.
--
-- ObjC selector: @- selectionOverlayStyle@
selectionOverlayStyle :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSScrubberSelectionStyle)
selectionOverlayStyle nsScrubber =
  sendMessage nsScrubber selectionOverlayStyleSelector

-- | Specifies a style of decoration to place above items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in overlay decoration.
--
-- ObjC selector: @- setSelectionOverlayStyle:@
setSelectionOverlayStyle :: (IsNSScrubber nsScrubber, IsNSScrubberSelectionStyle value) => nsScrubber -> value -> IO ()
setSelectionOverlayStyle nsScrubber value =
  sendMessage nsScrubber setSelectionOverlayStyleSelector (toNSScrubberSelectionStyle value)

-- | If @showsArrowButtons@ is @YES,@ the control provides leading and trailing arrow buttons. Tapping an arrow button moves the selection index by one element; pressing and holding repeatedly moves the selection. The default is @NO.@
--
-- ObjC selector: @- showsArrowButtons@
showsArrowButtons :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
showsArrowButtons nsScrubber =
  sendMessage nsScrubber showsArrowButtonsSelector

-- | If @showsArrowButtons@ is @YES,@ the control provides leading and trailing arrow buttons. Tapping an arrow button moves the selection index by one element; pressing and holding repeatedly moves the selection. The default is @NO.@
--
-- ObjC selector: @- setShowsArrowButtons:@
setShowsArrowButtons :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setShowsArrowButtons nsScrubber value =
  sendMessage nsScrubber setShowsArrowButtonsSelector value

-- | If @showsAdditionalContentIndicators@ is @YES,@ the control will draw a fade effect to indicate that there is additional unscrolled content. The default is @NO.@
--
-- ObjC selector: @- showsAdditionalContentIndicators@
showsAdditionalContentIndicators :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
showsAdditionalContentIndicators nsScrubber =
  sendMessage nsScrubber showsAdditionalContentIndicatorsSelector

-- | If @showsAdditionalContentIndicators@ is @YES,@ the control will draw a fade effect to indicate that there is additional unscrolled content. The default is @NO.@
--
-- ObjC selector: @- setShowsAdditionalContentIndicators:@
setShowsAdditionalContentIndicators :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setShowsAdditionalContentIndicators nsScrubber value =
  sendMessage nsScrubber setShowsAdditionalContentIndicatorsSelector value

-- | If set, @backgroundColor@ is displayed behind the scrubber content. The background color is suppressed if the scrubber is assigned a non-nil @backgroundView.@ The default value is @nil.@
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSColor)
backgroundColor nsScrubber =
  sendMessage nsScrubber backgroundColorSelector

-- | If set, @backgroundColor@ is displayed behind the scrubber content. The background color is suppressed if the scrubber is assigned a non-nil @backgroundView.@ The default value is @nil.@
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsNSScrubber nsScrubber, IsNSColor value) => nsScrubber -> value -> IO ()
setBackgroundColor nsScrubber value =
  sendMessage nsScrubber setBackgroundColorSelector (toNSColor value)

-- | If non-nil, the @backgroundView@ is displayed below the scrubber content. The view's layout is managed by @NSScrubber@ to match the content area. If this property is non-nil, the @backgroundColor@ property has no effect. The default value is @nil.@
--
-- ObjC selector: @- backgroundView@
backgroundView :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSView)
backgroundView nsScrubber =
  sendMessage nsScrubber backgroundViewSelector

-- | If non-nil, the @backgroundView@ is displayed below the scrubber content. The view's layout is managed by @NSScrubber@ to match the content area. If this property is non-nil, the @backgroundColor@ property has no effect. The default value is @nil.@
--
-- ObjC selector: @- setBackgroundView:@
setBackgroundView :: (IsNSScrubber nsScrubber, IsNSView value) => nsScrubber -> value -> IO ()
setBackgroundView nsScrubber value =
  sendMessage nsScrubber setBackgroundViewSelector (toNSView value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSScrubber)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScrubber)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @performSequentialBatchUpdates:@
performSequentialBatchUpdatesSelector :: Selector '[Ptr ()] ()
performSequentialBatchUpdatesSelector = mkSelector "performSequentialBatchUpdates:"

-- | @Selector@ for @insertItemsAtIndexes:@
insertItemsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
insertItemsAtIndexesSelector = mkSelector "insertItemsAtIndexes:"

-- | @Selector@ for @removeItemsAtIndexes:@
removeItemsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
removeItemsAtIndexesSelector = mkSelector "removeItemsAtIndexes:"

-- | @Selector@ for @reloadItemsAtIndexes:@
reloadItemsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
reloadItemsAtIndexesSelector = mkSelector "reloadItemsAtIndexes:"

-- | @Selector@ for @moveItemAtIndex:toIndex:@
moveItemAtIndex_toIndexSelector :: Selector '[CLong, CLong] ()
moveItemAtIndex_toIndexSelector = mkSelector "moveItemAtIndex:toIndex:"

-- | @Selector@ for @scrollItemAtIndex:toAlignment:@
scrollItemAtIndex_toAlignmentSelector :: Selector '[CLong, NSScrubberAlignment] ()
scrollItemAtIndex_toAlignmentSelector = mkSelector "scrollItemAtIndex:toAlignment:"

-- | @Selector@ for @itemViewForItemAtIndex:@
itemViewForItemAtIndexSelector :: Selector '[CLong] (Id NSScrubberItemView)
itemViewForItemAtIndexSelector = mkSelector "itemViewForItemAtIndex:"

-- | @Selector@ for @registerClass:forItemIdentifier:@
registerClass_forItemIdentifierSelector :: Selector '[Class, Id NSString] ()
registerClass_forItemIdentifierSelector = mkSelector "registerClass:forItemIdentifier:"

-- | @Selector@ for @registerNib:forItemIdentifier:@
registerNib_forItemIdentifierSelector :: Selector '[Id NSNib, Id NSString] ()
registerNib_forItemIdentifierSelector = mkSelector "registerNib:forItemIdentifier:"

-- | @Selector@ for @makeItemWithIdentifier:owner:@
makeItemWithIdentifier_ownerSelector :: Selector '[Id NSString, RawId] (Id NSScrubberItemView)
makeItemWithIdentifier_ownerSelector = mkSelector "makeItemWithIdentifier:owner:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @scrubberLayout@
scrubberLayoutSelector :: Selector '[] (Id NSScrubberLayout)
scrubberLayoutSelector = mkSelector "scrubberLayout"

-- | @Selector@ for @setScrubberLayout:@
setScrubberLayoutSelector :: Selector '[Id NSScrubberLayout] ()
setScrubberLayoutSelector = mkSelector "setScrubberLayout:"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector '[] CLong
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @highlightedIndex@
highlightedIndexSelector :: Selector '[] CLong
highlightedIndexSelector = mkSelector "highlightedIndex"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CLong
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CLong] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] NSScrubberMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[NSScrubberMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @itemAlignment@
itemAlignmentSelector :: Selector '[] NSScrubberAlignment
itemAlignmentSelector = mkSelector "itemAlignment"

-- | @Selector@ for @setItemAlignment:@
setItemAlignmentSelector :: Selector '[NSScrubberAlignment] ()
setItemAlignmentSelector = mkSelector "setItemAlignment:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector '[] Bool
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector '[Bool] ()
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @floatsSelectionViews@
floatsSelectionViewsSelector :: Selector '[] Bool
floatsSelectionViewsSelector = mkSelector "floatsSelectionViews"

-- | @Selector@ for @setFloatsSelectionViews:@
setFloatsSelectionViewsSelector :: Selector '[Bool] ()
setFloatsSelectionViewsSelector = mkSelector "setFloatsSelectionViews:"

-- | @Selector@ for @selectionBackgroundStyle@
selectionBackgroundStyleSelector :: Selector '[] (Id NSScrubberSelectionStyle)
selectionBackgroundStyleSelector = mkSelector "selectionBackgroundStyle"

-- | @Selector@ for @setSelectionBackgroundStyle:@
setSelectionBackgroundStyleSelector :: Selector '[Id NSScrubberSelectionStyle] ()
setSelectionBackgroundStyleSelector = mkSelector "setSelectionBackgroundStyle:"

-- | @Selector@ for @selectionOverlayStyle@
selectionOverlayStyleSelector :: Selector '[] (Id NSScrubberSelectionStyle)
selectionOverlayStyleSelector = mkSelector "selectionOverlayStyle"

-- | @Selector@ for @setSelectionOverlayStyle:@
setSelectionOverlayStyleSelector :: Selector '[Id NSScrubberSelectionStyle] ()
setSelectionOverlayStyleSelector = mkSelector "setSelectionOverlayStyle:"

-- | @Selector@ for @showsArrowButtons@
showsArrowButtonsSelector :: Selector '[] Bool
showsArrowButtonsSelector = mkSelector "showsArrowButtons"

-- | @Selector@ for @setShowsArrowButtons:@
setShowsArrowButtonsSelector :: Selector '[Bool] ()
setShowsArrowButtonsSelector = mkSelector "setShowsArrowButtons:"

-- | @Selector@ for @showsAdditionalContentIndicators@
showsAdditionalContentIndicatorsSelector :: Selector '[] Bool
showsAdditionalContentIndicatorsSelector = mkSelector "showsAdditionalContentIndicators"

-- | @Selector@ for @setShowsAdditionalContentIndicators:@
setShowsAdditionalContentIndicatorsSelector :: Selector '[Bool] ()
setShowsAdditionalContentIndicatorsSelector = mkSelector "setShowsAdditionalContentIndicators:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @backgroundView@
backgroundViewSelector :: Selector '[] (Id NSView)
backgroundViewSelector = mkSelector "backgroundView"

-- | @Selector@ for @setBackgroundView:@
setBackgroundViewSelector :: Selector '[Id NSView] ()
setBackgroundViewSelector = mkSelector "setBackgroundView:"

