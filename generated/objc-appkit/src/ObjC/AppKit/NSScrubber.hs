{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameSelector
  , initWithCoderSelector
  , reloadDataSelector
  , performSequentialBatchUpdatesSelector
  , insertItemsAtIndexesSelector
  , removeItemsAtIndexesSelector
  , reloadItemsAtIndexesSelector
  , moveItemAtIndex_toIndexSelector
  , scrollItemAtIndex_toAlignmentSelector
  , itemViewForItemAtIndexSelector
  , registerClass_forItemIdentifierSelector
  , registerNib_forItemIdentifierSelector
  , makeItemWithIdentifier_ownerSelector
  , scrubberLayoutSelector
  , setScrubberLayoutSelector
  , numberOfItemsSelector
  , highlightedIndexSelector
  , selectedIndexSelector
  , setSelectedIndexSelector
  , modeSelector
  , setModeSelector
  , itemAlignmentSelector
  , setItemAlignmentSelector
  , continuousSelector
  , setContinuousSelector
  , floatsSelectionViewsSelector
  , setFloatsSelectionViewsSelector
  , selectionBackgroundStyleSelector
  , setSelectionBackgroundStyleSelector
  , selectionOverlayStyleSelector
  , setSelectionOverlayStyleSelector
  , showsArrowButtonsSelector
  , setShowsArrowButtonsSelector
  , showsAdditionalContentIndicatorsSelector
  , setShowsAdditionalContentIndicatorsSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , backgroundViewSelector
  , setBackgroundViewSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSScrubber nsScrubber => nsScrubber -> NSRect -> IO (Id NSScrubber)
initWithFrame nsScrubber  frameRect =
  sendMsg nsScrubber (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubber nsScrubber, IsNSCoder coder) => nsScrubber -> coder -> IO (Id NSScrubber)
initWithCoder nsScrubber  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsScrubber (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | Invalidate all data within the scrubber control, triggering a reload of all content, and clearing the current selection.
--
-- ObjC selector: @- reloadData@
reloadData :: IsNSScrubber nsScrubber => nsScrubber -> IO ()
reloadData nsScrubber  =
  sendMsg nsScrubber (mkSelector "reloadData") retVoid []

-- | Updates inside the @performSequentialBatchUpdates@ block are processed and displayed all at once, including insertion, removal, moving, reloading items, and changing the selected index. Changes are performed iteratively using the same semantics as @NSMutableArray.@ NSScrubber expects its dataSource to reflect the changes made inside @-performSequentialBatchUpdates:@ immediately after the @updateBlock@ finishes executing.
--
-- ObjC selector: @- performSequentialBatchUpdates:@
performSequentialBatchUpdates :: IsNSScrubber nsScrubber => nsScrubber -> Ptr () -> IO ()
performSequentialBatchUpdates nsScrubber  updateBlock =
  sendMsg nsScrubber (mkSelector "performSequentialBatchUpdates:") retVoid [argPtr (castPtr updateBlock :: Ptr ())]

-- | Inserts new items at the specified indexes. NSScrubber will request views for each new index from the @dataSource.@ This method uses the same semantics as @NSMutableArray;@ each index in the set specifies the destination index after all previous insertions have occurred. Therefore, an NSIndexSet of [1,2,3] will result in three new contiguous items.
--
-- ObjC selector: @- insertItemsAtIndexes:@
insertItemsAtIndexes :: (IsNSScrubber nsScrubber, IsNSIndexSet indexes) => nsScrubber -> indexes -> IO ()
insertItemsAtIndexes nsScrubber  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsScrubber (mkSelector "insertItemsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | Removes the items at the specified indexes. This method uses the same semantics as @NSMutableArray.@
--
-- ObjC selector: @- removeItemsAtIndexes:@
removeItemsAtIndexes :: (IsNSScrubber nsScrubber, IsNSIndexSet indexes) => nsScrubber -> indexes -> IO ()
removeItemsAtIndexes nsScrubber  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsScrubber (mkSelector "removeItemsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | Reloads the items at the specified indexes. NSScrubber will request new views for each item and smoothly crossfade between them before discarding the original views.
--
-- ObjC selector: @- reloadItemsAtIndexes:@
reloadItemsAtIndexes :: (IsNSScrubber nsScrubber, IsNSIndexSet indexes) => nsScrubber -> indexes -> IO ()
reloadItemsAtIndexes nsScrubber  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsScrubber (mkSelector "reloadItemsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | Moves an item from one index to another. @oldIndex@ refers to the item's index prior to the movement, whereas @newIndex@ refers to the item's final location.
--
-- ObjC selector: @- moveItemAtIndex:toIndex:@
moveItemAtIndex_toIndex :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> CLong -> IO ()
moveItemAtIndex_toIndex nsScrubber  oldIndex newIndex =
  sendMsg nsScrubber (mkSelector "moveItemAtIndex:toIndex:") retVoid [argCLong (fromIntegral oldIndex), argCLong (fromIntegral newIndex)]

-- | Scrolls an item to a given alignment within the control. If @NSScrubberAlignmentNone@ is provided, then the control scrolls the minimum amount necessary to make the item visible. Scrolling is animated if called on the animator proxy.
--
-- ObjC selector: @- scrollItemAtIndex:toAlignment:@
scrollItemAtIndex_toAlignment :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> NSScrubberAlignment -> IO ()
scrollItemAtIndex_toAlignment nsScrubber  index alignment =
  sendMsg nsScrubber (mkSelector "scrollItemAtIndex:toAlignment:") retVoid [argCLong (fromIntegral index), argCLong (coerce alignment)]

-- | Returns the @NSScrubberItemView@ for the given index, if one currently exists; returns @nil@ otherwise.
--
-- ObjC selector: @- itemViewForItemAtIndex:@
itemViewForItemAtIndex :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> IO (Id NSScrubberItemView)
itemViewForItemAtIndex nsScrubber  index =
  sendMsg nsScrubber (mkSelector "itemViewForItemAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | Registers a @NSScrubberItemView@ class to be instantiated for the given @itemIdentifier.@ Raises an exception if @itemViewClass@ is not a subclass of @NSScrubberItemView.@ Passing @nil@ for @itemViewClass@ removes a previous registration. Registrations made through this method do not persist through NSCoding.
--
-- ObjC selector: @- registerClass:forItemIdentifier:@
registerClass_forItemIdentifier :: (IsNSScrubber nsScrubber, IsNSString itemIdentifier) => nsScrubber -> Class -> itemIdentifier -> IO ()
registerClass_forItemIdentifier nsScrubber  itemViewClass itemIdentifier =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
    sendMsg nsScrubber (mkSelector "registerClass:forItemIdentifier:") retVoid [argPtr (unClass itemViewClass), argPtr (castPtr raw_itemIdentifier :: Ptr ())]

-- | Register a nib to be instantiated for the given @itemIdentifier.@ The nib must contain a top-level object which is a subclass of NSScrubberItemView; otherwise, @-makeItemWithIdentifier:@ may return @nil@ for this identifier. Passing @nil@ for @nib@ removes a previous registration.
--
-- ObjC selector: @- registerNib:forItemIdentifier:@
registerNib_forItemIdentifier :: (IsNSScrubber nsScrubber, IsNSNib nib, IsNSString itemIdentifier) => nsScrubber -> nib -> itemIdentifier -> IO ()
registerNib_forItemIdentifier nsScrubber  nib itemIdentifier =
withObjCPtr nib $ \raw_nib ->
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsScrubber (mkSelector "registerNib:forItemIdentifier:") retVoid [argPtr (castPtr raw_nib :: Ptr ()), argPtr (castPtr raw_itemIdentifier :: Ptr ())]

-- | Creates or reuses a @NSScrubberItemView@ corresponding to the provided @itemIdentifier.@ @NSScrubber@ searches, in order: the reuse queue, the list of registered classes, and then the list of registered nibs. If the reuse queue is empty, and there is no Class or Interface Builder archive registered for the @itemIdentifier,@ this method returns @nil.@
--
-- ObjC selector: @- makeItemWithIdentifier:owner:@
makeItemWithIdentifier_owner :: (IsNSScrubber nsScrubber, IsNSString itemIdentifier) => nsScrubber -> itemIdentifier -> RawId -> IO (Id NSScrubberItemView)
makeItemWithIdentifier_owner nsScrubber  itemIdentifier owner =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
    sendMsg nsScrubber (mkSelector "makeItemWithIdentifier:owner:") (retPtr retVoid) [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())] >>= retainedObject . castPtr

-- | @- scrubberLayout@
scrubberLayout :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSScrubberLayout)
scrubberLayout nsScrubber  =
  sendMsg nsScrubber (mkSelector "scrubberLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScrubberLayout:@
setScrubberLayout :: (IsNSScrubber nsScrubber, IsNSScrubberLayout value) => nsScrubber -> value -> IO ()
setScrubberLayout nsScrubber  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubber (mkSelector "setScrubberLayout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Returns the number of items represented by the scrubber control.
--
-- ObjC selector: @- numberOfItems@
numberOfItems :: IsNSScrubber nsScrubber => nsScrubber -> IO CLong
numberOfItems nsScrubber  =
  sendMsg nsScrubber (mkSelector "numberOfItems") retCLong []

-- | The index of the currently highlighted item within the control. If there is no highlighted item, the value of this property is (-1).
--
-- ObjC selector: @- highlightedIndex@
highlightedIndex :: IsNSScrubber nsScrubber => nsScrubber -> IO CLong
highlightedIndex nsScrubber  =
  sendMsg nsScrubber (mkSelector "highlightedIndex") retCLong []

-- | The index of the selected item within the control. If there is no selected item, the value of this property is (-1). Setting this property through the animator proxy will animate the selection change. Programmatic selection changes do not trigger delegate callbacks.
--
-- ObjC selector: @- selectedIndex@
selectedIndex :: IsNSScrubber nsScrubber => nsScrubber -> IO CLong
selectedIndex nsScrubber  =
  sendMsg nsScrubber (mkSelector "selectedIndex") retCLong []

-- | The index of the selected item within the control. If there is no selected item, the value of this property is (-1). Setting this property through the animator proxy will animate the selection change. Programmatic selection changes do not trigger delegate callbacks.
--
-- ObjC selector: @- setSelectedIndex:@
setSelectedIndex :: IsNSScrubber nsScrubber => nsScrubber -> CLong -> IO ()
setSelectedIndex nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setSelectedIndex:") retVoid [argCLong (fromIntegral value)]

-- | Describes the interaction mode for the scrubber control. See the @NSScrubberMode@ enumeration for a list of possible values. The default value is @NSScrubberModeFixed.@
--
-- ObjC selector: @- mode@
mode :: IsNSScrubber nsScrubber => nsScrubber -> IO NSScrubberMode
mode nsScrubber  =
  fmap (coerce :: CLong -> NSScrubberMode) $ sendMsg nsScrubber (mkSelector "mode") retCLong []

-- | Describes the interaction mode for the scrubber control. See the @NSScrubberMode@ enumeration for a list of possible values. The default value is @NSScrubberModeFixed.@
--
-- ObjC selector: @- setMode:@
setMode :: IsNSScrubber nsScrubber => nsScrubber -> NSScrubberMode -> IO ()
setMode nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- | If the value of @itemAlignment@ is not @NSScrubberAlignmentNone,@ the scrubber will ensure that some item rests at the preferred alignment within the control following a scrolling or paging interaction. The default value is @NSScrubberAlignmentNone.@
--
-- ObjC selector: @- itemAlignment@
itemAlignment :: IsNSScrubber nsScrubber => nsScrubber -> IO NSScrubberAlignment
itemAlignment nsScrubber  =
  fmap (coerce :: CLong -> NSScrubberAlignment) $ sendMsg nsScrubber (mkSelector "itemAlignment") retCLong []

-- | If the value of @itemAlignment@ is not @NSScrubberAlignmentNone,@ the scrubber will ensure that some item rests at the preferred alignment within the control following a scrolling or paging interaction. The default value is @NSScrubberAlignmentNone.@
--
-- ObjC selector: @- setItemAlignment:@
setItemAlignment :: IsNSScrubber nsScrubber => nsScrubber -> NSScrubberAlignment -> IO ()
setItemAlignment nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setItemAlignment:") retVoid [argCLong (coerce value)]

-- | When @continuous@ is @YES,@ panning over the control in @NSScrubberModeFixed@ will immediately select the item under the user's finger, and scrolling in @NSScrubberModeFree@ will continuously select items as they pass through the current @itemAlignment.@ The default is @NO.@
--
-- ObjC selector: @- continuous@
continuous :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
continuous nsScrubber  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrubber (mkSelector "continuous") retCULong []

-- | When @continuous@ is @YES,@ panning over the control in @NSScrubberModeFixed@ will immediately select the item under the user's finger, and scrolling in @NSScrubberModeFree@ will continuously select items as they pass through the current @itemAlignment.@ The default is @NO.@
--
-- ObjC selector: @- setContinuous:@
setContinuous :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setContinuous nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setContinuous:") retVoid [argCULong (if value then 1 else 0)]

-- | When @floatsSelectionViews@ is @YES,@ the selection decorations provided by @selectionBackgroundStyle@ and @selectionOverlayStyle@ will smoothly float between selected items, rather than animating their entrance/exit in-place. The default is @NO.@
--
-- ObjC selector: @- floatsSelectionViews@
floatsSelectionViews :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
floatsSelectionViews nsScrubber  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrubber (mkSelector "floatsSelectionViews") retCULong []

-- | When @floatsSelectionViews@ is @YES,@ the selection decorations provided by @selectionBackgroundStyle@ and @selectionOverlayStyle@ will smoothly float between selected items, rather than animating their entrance/exit in-place. The default is @NO.@
--
-- ObjC selector: @- setFloatsSelectionViews:@
setFloatsSelectionViews :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setFloatsSelectionViews nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setFloatsSelectionViews:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies a style of decoration to place behind items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in background decoration.
--
-- ObjC selector: @- selectionBackgroundStyle@
selectionBackgroundStyle :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSScrubberSelectionStyle)
selectionBackgroundStyle nsScrubber  =
  sendMsg nsScrubber (mkSelector "selectionBackgroundStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies a style of decoration to place behind items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in background decoration.
--
-- ObjC selector: @- setSelectionBackgroundStyle:@
setSelectionBackgroundStyle :: (IsNSScrubber nsScrubber, IsNSScrubberSelectionStyle value) => nsScrubber -> value -> IO ()
setSelectionBackgroundStyle nsScrubber  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubber (mkSelector "setSelectionBackgroundStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies a style of decoration to place above items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in overlay decoration.
--
-- ObjC selector: @- selectionOverlayStyle@
selectionOverlayStyle :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSScrubberSelectionStyle)
selectionOverlayStyle nsScrubber  =
  sendMsg nsScrubber (mkSelector "selectionOverlayStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies a style of decoration to place above items that are selected and/or highlighted. The default value is @nil,@ indicating no built-in overlay decoration.
--
-- ObjC selector: @- setSelectionOverlayStyle:@
setSelectionOverlayStyle :: (IsNSScrubber nsScrubber, IsNSScrubberSelectionStyle value) => nsScrubber -> value -> IO ()
setSelectionOverlayStyle nsScrubber  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubber (mkSelector "setSelectionOverlayStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If @showsArrowButtons@ is @YES,@ the control provides leading and trailing arrow buttons. Tapping an arrow button moves the selection index by one element; pressing and holding repeatedly moves the selection. The default is @NO.@
--
-- ObjC selector: @- showsArrowButtons@
showsArrowButtons :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
showsArrowButtons nsScrubber  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrubber (mkSelector "showsArrowButtons") retCULong []

-- | If @showsArrowButtons@ is @YES,@ the control provides leading and trailing arrow buttons. Tapping an arrow button moves the selection index by one element; pressing and holding repeatedly moves the selection. The default is @NO.@
--
-- ObjC selector: @- setShowsArrowButtons:@
setShowsArrowButtons :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setShowsArrowButtons nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setShowsArrowButtons:") retVoid [argCULong (if value then 1 else 0)]

-- | If @showsAdditionalContentIndicators@ is @YES,@ the control will draw a fade effect to indicate that there is additional unscrolled content. The default is @NO.@
--
-- ObjC selector: @- showsAdditionalContentIndicators@
showsAdditionalContentIndicators :: IsNSScrubber nsScrubber => nsScrubber -> IO Bool
showsAdditionalContentIndicators nsScrubber  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrubber (mkSelector "showsAdditionalContentIndicators") retCULong []

-- | If @showsAdditionalContentIndicators@ is @YES,@ the control will draw a fade effect to indicate that there is additional unscrolled content. The default is @NO.@
--
-- ObjC selector: @- setShowsAdditionalContentIndicators:@
setShowsAdditionalContentIndicators :: IsNSScrubber nsScrubber => nsScrubber -> Bool -> IO ()
setShowsAdditionalContentIndicators nsScrubber  value =
  sendMsg nsScrubber (mkSelector "setShowsAdditionalContentIndicators:") retVoid [argCULong (if value then 1 else 0)]

-- | If set, @backgroundColor@ is displayed behind the scrubber content. The background color is suppressed if the scrubber is assigned a non-nil @backgroundView.@ The default value is @nil.@
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSColor)
backgroundColor nsScrubber  =
  sendMsg nsScrubber (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If set, @backgroundColor@ is displayed behind the scrubber content. The background color is suppressed if the scrubber is assigned a non-nil @backgroundView.@ The default value is @nil.@
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsNSScrubber nsScrubber, IsNSColor value) => nsScrubber -> value -> IO ()
setBackgroundColor nsScrubber  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubber (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If non-nil, the @backgroundView@ is displayed below the scrubber content. The view's layout is managed by @NSScrubber@ to match the content area. If this property is non-nil, the @backgroundColor@ property has no effect. The default value is @nil.@
--
-- ObjC selector: @- backgroundView@
backgroundView :: IsNSScrubber nsScrubber => nsScrubber -> IO (Id NSView)
backgroundView nsScrubber  =
  sendMsg nsScrubber (mkSelector "backgroundView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If non-nil, the @backgroundView@ is displayed below the scrubber content. The view's layout is managed by @NSScrubber@ to match the content area. If this property is non-nil, the @backgroundColor@ property has no effect. The default value is @nil.@
--
-- ObjC selector: @- setBackgroundView:@
setBackgroundView :: (IsNSScrubber nsScrubber, IsNSView value) => nsScrubber -> value -> IO ()
setBackgroundView nsScrubber  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubber (mkSelector "setBackgroundView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @performSequentialBatchUpdates:@
performSequentialBatchUpdatesSelector :: Selector
performSequentialBatchUpdatesSelector = mkSelector "performSequentialBatchUpdates:"

-- | @Selector@ for @insertItemsAtIndexes:@
insertItemsAtIndexesSelector :: Selector
insertItemsAtIndexesSelector = mkSelector "insertItemsAtIndexes:"

-- | @Selector@ for @removeItemsAtIndexes:@
removeItemsAtIndexesSelector :: Selector
removeItemsAtIndexesSelector = mkSelector "removeItemsAtIndexes:"

-- | @Selector@ for @reloadItemsAtIndexes:@
reloadItemsAtIndexesSelector :: Selector
reloadItemsAtIndexesSelector = mkSelector "reloadItemsAtIndexes:"

-- | @Selector@ for @moveItemAtIndex:toIndex:@
moveItemAtIndex_toIndexSelector :: Selector
moveItemAtIndex_toIndexSelector = mkSelector "moveItemAtIndex:toIndex:"

-- | @Selector@ for @scrollItemAtIndex:toAlignment:@
scrollItemAtIndex_toAlignmentSelector :: Selector
scrollItemAtIndex_toAlignmentSelector = mkSelector "scrollItemAtIndex:toAlignment:"

-- | @Selector@ for @itemViewForItemAtIndex:@
itemViewForItemAtIndexSelector :: Selector
itemViewForItemAtIndexSelector = mkSelector "itemViewForItemAtIndex:"

-- | @Selector@ for @registerClass:forItemIdentifier:@
registerClass_forItemIdentifierSelector :: Selector
registerClass_forItemIdentifierSelector = mkSelector "registerClass:forItemIdentifier:"

-- | @Selector@ for @registerNib:forItemIdentifier:@
registerNib_forItemIdentifierSelector :: Selector
registerNib_forItemIdentifierSelector = mkSelector "registerNib:forItemIdentifier:"

-- | @Selector@ for @makeItemWithIdentifier:owner:@
makeItemWithIdentifier_ownerSelector :: Selector
makeItemWithIdentifier_ownerSelector = mkSelector "makeItemWithIdentifier:owner:"

-- | @Selector@ for @scrubberLayout@
scrubberLayoutSelector :: Selector
scrubberLayoutSelector = mkSelector "scrubberLayout"

-- | @Selector@ for @setScrubberLayout:@
setScrubberLayoutSelector :: Selector
setScrubberLayoutSelector = mkSelector "setScrubberLayout:"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @highlightedIndex@
highlightedIndexSelector :: Selector
highlightedIndexSelector = mkSelector "highlightedIndex"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @itemAlignment@
itemAlignmentSelector :: Selector
itemAlignmentSelector = mkSelector "itemAlignment"

-- | @Selector@ for @setItemAlignment:@
setItemAlignmentSelector :: Selector
setItemAlignmentSelector = mkSelector "setItemAlignment:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @floatsSelectionViews@
floatsSelectionViewsSelector :: Selector
floatsSelectionViewsSelector = mkSelector "floatsSelectionViews"

-- | @Selector@ for @setFloatsSelectionViews:@
setFloatsSelectionViewsSelector :: Selector
setFloatsSelectionViewsSelector = mkSelector "setFloatsSelectionViews:"

-- | @Selector@ for @selectionBackgroundStyle@
selectionBackgroundStyleSelector :: Selector
selectionBackgroundStyleSelector = mkSelector "selectionBackgroundStyle"

-- | @Selector@ for @setSelectionBackgroundStyle:@
setSelectionBackgroundStyleSelector :: Selector
setSelectionBackgroundStyleSelector = mkSelector "setSelectionBackgroundStyle:"

-- | @Selector@ for @selectionOverlayStyle@
selectionOverlayStyleSelector :: Selector
selectionOverlayStyleSelector = mkSelector "selectionOverlayStyle"

-- | @Selector@ for @setSelectionOverlayStyle:@
setSelectionOverlayStyleSelector :: Selector
setSelectionOverlayStyleSelector = mkSelector "setSelectionOverlayStyle:"

-- | @Selector@ for @showsArrowButtons@
showsArrowButtonsSelector :: Selector
showsArrowButtonsSelector = mkSelector "showsArrowButtons"

-- | @Selector@ for @setShowsArrowButtons:@
setShowsArrowButtonsSelector :: Selector
setShowsArrowButtonsSelector = mkSelector "setShowsArrowButtons:"

-- | @Selector@ for @showsAdditionalContentIndicators@
showsAdditionalContentIndicatorsSelector :: Selector
showsAdditionalContentIndicatorsSelector = mkSelector "showsAdditionalContentIndicators"

-- | @Selector@ for @setShowsAdditionalContentIndicators:@
setShowsAdditionalContentIndicatorsSelector :: Selector
setShowsAdditionalContentIndicatorsSelector = mkSelector "setShowsAdditionalContentIndicators:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @backgroundView@
backgroundViewSelector :: Selector
backgroundViewSelector = mkSelector "backgroundView"

-- | @Selector@ for @setBackgroundView:@
setBackgroundViewSelector :: Selector
setBackgroundViewSelector = mkSelector "setBackgroundView:"

