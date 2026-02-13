{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSSplitViewController is a container view controller that manages side-by-side (horizontal or vertical) children view controllers. Views are lazily loaded. For instance, adding a collapsed SplitViewItem will not load the associated ViewController's view until it is uncollapsed. The NSSplitViewController is set as the delegate of its managed NSSplitView. Any overrides of NSSplitViewDelegate methods must call super. Only the @-vertical,@ @-autosaveName,@ and divider properties should be manipulated on the managed NSSplitView. Changing other properties (such as delegate, manipulating subviews, holding priorities) will cause an exception to be thrown. Autolayout must be used with NSSplitViewController to properly control the layout of the child views and the animations of collapses and reveals. e.g., Constraints can be used to setup whether a window should grow/shrink or stay the same size when showing and hiding a sidebar. NSViewController's methods @-addChildViewController:,@ @-insertViewController:atIndex:,@ and @-removeChildViewControllerAtIndex:@ can all be used as convience methods to add children; default SplitViewItems will be appropriately created or destroyed.
--
-- Generated bindings for @NSSplitViewController@.
module ObjC.AppKit.NSSplitViewController
  ( NSSplitViewController
  , IsNSSplitViewController(..)
  , addSplitViewItem
  , insertSplitViewItem_atIndex
  , removeSplitViewItem
  , splitViewItemForViewController
  , validateUserInterfaceItem
  , viewDidLoad
  , splitView_canCollapseSubview
  , splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex
  , splitView_shouldHideDividerAtIndex
  , splitView_effectiveRect_forDrawnRect_ofDividerAtIndex
  , splitView_additionalEffectiveRectOfDividerAtIndex
  , toggleSidebar
  , toggleInspector
  , splitView
  , setSplitView
  , splitViewItems
  , setSplitViewItems
  , minimumThicknessForInlineSidebars
  , setMinimumThicknessForInlineSidebars
  , addSplitViewItemSelector
  , insertSplitViewItem_atIndexSelector
  , minimumThicknessForInlineSidebarsSelector
  , removeSplitViewItemSelector
  , setMinimumThicknessForInlineSidebarsSelector
  , setSplitViewItemsSelector
  , setSplitViewSelector
  , splitViewItemForViewControllerSelector
  , splitViewItemsSelector
  , splitViewSelector
  , splitView_additionalEffectiveRectOfDividerAtIndexSelector
  , splitView_canCollapseSubviewSelector
  , splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector
  , splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector
  , splitView_shouldHideDividerAtIndexSelector
  , toggleInspectorSelector
  , toggleSidebarSelector
  , validateUserInterfaceItemSelector
  , viewDidLoadSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Adds a SplitViewItem to the end of the SplitViewController. If the receiver's view is loaded and the SplitViewItem is not collapsed, the SplitViewItem's viewController's view will be loaded and added to the splitView. This calls through to -insertSplitViewItem:atIndex:.
--
-- @splitViewItem@ — The SplitViewItem to add. It must have a viewController set by the time it is added or an exception will be thrown. An exception will also be thrown if splitViewItem is nil.
--
-- ObjC selector: @- addSplitViewItem:@
addSplitViewItem :: (IsNSSplitViewController nsSplitViewController, IsNSSplitViewItem splitViewItem) => nsSplitViewController -> splitViewItem -> IO ()
addSplitViewItem nsSplitViewController splitViewItem =
  sendMessage nsSplitViewController addSplitViewItemSelector (toNSSplitViewItem splitViewItem)

-- | Adds a SplitViewItem to a given index in the SplitViewController. If the receiver's view is loaded and the SplitViewItem is not collapsed, the SplitViewItem's viewController's view will be loaded and added to the @splitView.@ Subclasses must call through @-insertSplitViewItem:atIndex:@ to add a SplitViewItem.
--
-- @splitViewItem@ — The SplitViewItem to add. It must have a @viewController@ set by the time it is added or an exception will be thrown. An exception will also be thrown if splitViewItem is nil.
--
-- @index@ — The index to add the SplitViewItem at. Will throw an exception if @index@ < 0 or @index@ > @splitViewItems.count@
--
-- ObjC selector: @- insertSplitViewItem:atIndex:@
insertSplitViewItem_atIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitViewItem splitViewItem) => nsSplitViewController -> splitViewItem -> CLong -> IO ()
insertSplitViewItem_atIndex nsSplitViewController splitViewItem index =
  sendMessage nsSplitViewController insertSplitViewItem_atIndexSelector (toNSSplitViewItem splitViewItem) index

-- | Removes a SplitViewItem from the receiver. The layout of the @splitView@ will be adjusted for its removal. Subclasses must call through @-removeSplitViewItem:@ to remove a SplitViewItem.
--
-- @splitViewItem@ — The SplitViewItem to remove. An exception will be thrown if @splitViewItem@ is not in the SplitViewController or if it is nil.
--
-- ObjC selector: @- removeSplitViewItem:@
removeSplitViewItem :: (IsNSSplitViewController nsSplitViewController, IsNSSplitViewItem splitViewItem) => nsSplitViewController -> splitViewItem -> IO ()
removeSplitViewItem nsSplitViewController splitViewItem =
  sendMessage nsSplitViewController removeSplitViewItemSelector (toNSSplitViewItem splitViewItem)

-- | Returns the corresponding SplitViewItem for a given child ViewController.
--
-- @viewController@ — The ViewController to look up.
--
-- Returns: The corresponding SplitViewItem. Returns nil if @viewController@ is not a child of the SplitViewController.
--
-- ObjC selector: @- splitViewItemForViewController:@
splitViewItemForViewController :: (IsNSSplitViewController nsSplitViewController, IsNSViewController viewController) => nsSplitViewController -> viewController -> IO (Id NSSplitViewItem)
splitViewItemForViewController nsSplitViewController viewController =
  sendMessage nsSplitViewController splitViewItemForViewControllerSelector (toNSViewController viewController)

-- | Validates items with an action of @toggleSidebar:@ to reflect the status of the sidebar item contained within the receiver.
--
-- ObjC selector: @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> RawId -> IO Bool
validateUserInterfaceItem nsSplitViewController item =
  sendMessage nsSplitViewController validateUserInterfaceItemSelector item

-- | @- viewDidLoad@
viewDidLoad :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO ()
viewDidLoad nsSplitViewController =
  sendMessage nsSplitViewController viewDidLoadSelector

-- | @- splitView:canCollapseSubview:@
splitView_canCollapseSubview :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView, IsNSView subview) => nsSplitViewController -> splitView -> subview -> IO Bool
splitView_canCollapseSubview nsSplitViewController splitView subview =
  sendMessage nsSplitViewController splitView_canCollapseSubviewSelector (toNSSplitView splitView) (toNSView subview)

-- | @- splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:@
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView, IsNSView subview) => nsSplitViewController -> splitView -> subview -> CLong -> IO Bool
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex nsSplitViewController splitView subview dividerIndex =
  sendMessage nsSplitViewController splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector (toNSSplitView splitView) (toNSView subview) dividerIndex

-- | @- splitView:shouldHideDividerAtIndex:@
splitView_shouldHideDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView) => nsSplitViewController -> splitView -> CLong -> IO Bool
splitView_shouldHideDividerAtIndex nsSplitViewController splitView dividerIndex =
  sendMessage nsSplitViewController splitView_shouldHideDividerAtIndexSelector (toNSSplitView splitView) dividerIndex

-- | @- splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:@
splitView_effectiveRect_forDrawnRect_ofDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView) => nsSplitViewController -> splitView -> NSRect -> NSRect -> CLong -> IO NSRect
splitView_effectiveRect_forDrawnRect_ofDividerAtIndex nsSplitViewController splitView proposedEffectiveRect drawnRect dividerIndex =
  sendMessage nsSplitViewController splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector (toNSSplitView splitView) proposedEffectiveRect drawnRect dividerIndex

-- | @- splitView:additionalEffectiveRectOfDividerAtIndex:@
splitView_additionalEffectiveRectOfDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView) => nsSplitViewController -> splitView -> CLong -> IO NSRect
splitView_additionalEffectiveRectOfDividerAtIndex nsSplitViewController splitView dividerIndex =
  sendMessage nsSplitViewController splitView_additionalEffectiveRectOfDividerAtIndexSelector (toNSSplitView splitView) dividerIndex

-- | Animatedly collapses or uncollapses the first sidebar split view item in the receiver. Does nothing if the receiver does not contain any sidebars.
--
-- ObjC selector: @- toggleSidebar:@
toggleSidebar :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> RawId -> IO ()
toggleSidebar nsSplitViewController sender =
  sendMessage nsSplitViewController toggleSidebarSelector sender

-- | Animatedly collapses or uncollapses the first inspector split view item in the receiver. Does nothing if the receiver does not contain any inspectors.
--
-- ObjC selector: @- toggleInspector:@
toggleInspector :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> RawId -> IO ()
toggleInspector nsSplitViewController sender =
  sendMessage nsSplitViewController toggleInspectorSelector sender

-- | The split view managed by the SplitViewController. This can be used to customize view properties such as the dividerStyle, vertical, and autosaveName. It is not guaranteed to be the same view as the receivers 'view' property. The default created splitView is vertical with a dividerStyle of @NSSplitViewDividerStyleThin.@ To provide a custom NSSplitView, set the splitView property anytime before self.viewLoaded is YES.
--
-- ObjC selector: @- splitView@
splitView :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO (Id NSSplitView)
splitView nsSplitViewController =
  sendMessage nsSplitViewController splitViewSelector

-- | The split view managed by the SplitViewController. This can be used to customize view properties such as the dividerStyle, vertical, and autosaveName. It is not guaranteed to be the same view as the receivers 'view' property. The default created splitView is vertical with a dividerStyle of @NSSplitViewDividerStyleThin.@ To provide a custom NSSplitView, set the splitView property anytime before self.viewLoaded is YES.
--
-- ObjC selector: @- setSplitView:@
setSplitView :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView value) => nsSplitViewController -> value -> IO ()
setSplitView nsSplitViewController value =
  sendMessage nsSplitViewController setSplitViewSelector (toNSSplitView value)

-- | The array of SplitViewItems that correspond to the current child view controllers. After a child view controller is added to the receiving splitViewController, a NSSplitViewItem with the default values will be created for it. Once the child is removed, its corresponding splitViewItem will be removed from the splitViewItems array. Setting this will call through to @-insertSplitViewItem:atIndex@ and @-removeSplitViewItem:@ for items that are new or need removal.
--
-- ObjC selector: @- splitViewItems@
splitViewItems :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO (Id NSArray)
splitViewItems nsSplitViewController =
  sendMessage nsSplitViewController splitViewItemsSelector

-- | The array of SplitViewItems that correspond to the current child view controllers. After a child view controller is added to the receiving splitViewController, a NSSplitViewItem with the default values will be created for it. Once the child is removed, its corresponding splitViewItem will be removed from the splitViewItems array. Setting this will call through to @-insertSplitViewItem:atIndex@ and @-removeSplitViewItem:@ for items that are new or need removal.
--
-- ObjC selector: @- setSplitViewItems:@
setSplitViewItems :: (IsNSSplitViewController nsSplitViewController, IsNSArray value) => nsSplitViewController -> value -> IO ()
setSplitViewItems nsSplitViewController value =
  sendMessage nsSplitViewController setSplitViewItemsSelector (toNSArray value)

-- | The minimum thickness in the primary axis of split view (width for "vertical", height otherwise) before sidebar items will automatically collapse. If reshown in fullscreen, they will overlay over the other split items. Auto-collapsed sidebars will automatically uncollapse if the thickness is increased back to or past the minimum thickness. Defaults to @NSSplitViewControllerAutomaticDimension,@ which will use the effective minimum sizes of the split view item views as described by constraints in the window to determine the minimum size for inline sidebars. Once constraints establishing the minimum size can't be satisfied for all non-collapsed split panes, all sidebars will auto-collapse. When fullscreen, if a sidebar tries to uncollapse in this state, it will overlay.
--
-- ObjC selector: @- minimumThicknessForInlineSidebars@
minimumThicknessForInlineSidebars :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO CDouble
minimumThicknessForInlineSidebars nsSplitViewController =
  sendMessage nsSplitViewController minimumThicknessForInlineSidebarsSelector

-- | The minimum thickness in the primary axis of split view (width for "vertical", height otherwise) before sidebar items will automatically collapse. If reshown in fullscreen, they will overlay over the other split items. Auto-collapsed sidebars will automatically uncollapse if the thickness is increased back to or past the minimum thickness. Defaults to @NSSplitViewControllerAutomaticDimension,@ which will use the effective minimum sizes of the split view item views as described by constraints in the window to determine the minimum size for inline sidebars. Once constraints establishing the minimum size can't be satisfied for all non-collapsed split panes, all sidebars will auto-collapse. When fullscreen, if a sidebar tries to uncollapse in this state, it will overlay.
--
-- ObjC selector: @- setMinimumThicknessForInlineSidebars:@
setMinimumThicknessForInlineSidebars :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> CDouble -> IO ()
setMinimumThicknessForInlineSidebars nsSplitViewController value =
  sendMessage nsSplitViewController setMinimumThicknessForInlineSidebarsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSplitViewItem:@
addSplitViewItemSelector :: Selector '[Id NSSplitViewItem] ()
addSplitViewItemSelector = mkSelector "addSplitViewItem:"

-- | @Selector@ for @insertSplitViewItem:atIndex:@
insertSplitViewItem_atIndexSelector :: Selector '[Id NSSplitViewItem, CLong] ()
insertSplitViewItem_atIndexSelector = mkSelector "insertSplitViewItem:atIndex:"

-- | @Selector@ for @removeSplitViewItem:@
removeSplitViewItemSelector :: Selector '[Id NSSplitViewItem] ()
removeSplitViewItemSelector = mkSelector "removeSplitViewItem:"

-- | @Selector@ for @splitViewItemForViewController:@
splitViewItemForViewControllerSelector :: Selector '[Id NSViewController] (Id NSSplitViewItem)
splitViewItemForViewControllerSelector = mkSelector "splitViewItemForViewController:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector '[RawId] Bool
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @viewDidLoad@
viewDidLoadSelector :: Selector '[] ()
viewDidLoadSelector = mkSelector "viewDidLoad"

-- | @Selector@ for @splitView:canCollapseSubview:@
splitView_canCollapseSubviewSelector :: Selector '[Id NSSplitView, Id NSView] Bool
splitView_canCollapseSubviewSelector = mkSelector "splitView:canCollapseSubview:"

-- | @Selector@ for @splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:@
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector :: Selector '[Id NSSplitView, Id NSView, CLong] Bool
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector = mkSelector "splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:"

-- | @Selector@ for @splitView:shouldHideDividerAtIndex:@
splitView_shouldHideDividerAtIndexSelector :: Selector '[Id NSSplitView, CLong] Bool
splitView_shouldHideDividerAtIndexSelector = mkSelector "splitView:shouldHideDividerAtIndex:"

-- | @Selector@ for @splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:@
splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector :: Selector '[Id NSSplitView, NSRect, NSRect, CLong] NSRect
splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector = mkSelector "splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:"

-- | @Selector@ for @splitView:additionalEffectiveRectOfDividerAtIndex:@
splitView_additionalEffectiveRectOfDividerAtIndexSelector :: Selector '[Id NSSplitView, CLong] NSRect
splitView_additionalEffectiveRectOfDividerAtIndexSelector = mkSelector "splitView:additionalEffectiveRectOfDividerAtIndex:"

-- | @Selector@ for @toggleSidebar:@
toggleSidebarSelector :: Selector '[RawId] ()
toggleSidebarSelector = mkSelector "toggleSidebar:"

-- | @Selector@ for @toggleInspector:@
toggleInspectorSelector :: Selector '[RawId] ()
toggleInspectorSelector = mkSelector "toggleInspector:"

-- | @Selector@ for @splitView@
splitViewSelector :: Selector '[] (Id NSSplitView)
splitViewSelector = mkSelector "splitView"

-- | @Selector@ for @setSplitView:@
setSplitViewSelector :: Selector '[Id NSSplitView] ()
setSplitViewSelector = mkSelector "setSplitView:"

-- | @Selector@ for @splitViewItems@
splitViewItemsSelector :: Selector '[] (Id NSArray)
splitViewItemsSelector = mkSelector "splitViewItems"

-- | @Selector@ for @setSplitViewItems:@
setSplitViewItemsSelector :: Selector '[Id NSArray] ()
setSplitViewItemsSelector = mkSelector "setSplitViewItems:"

-- | @Selector@ for @minimumThicknessForInlineSidebars@
minimumThicknessForInlineSidebarsSelector :: Selector '[] CDouble
minimumThicknessForInlineSidebarsSelector = mkSelector "minimumThicknessForInlineSidebars"

-- | @Selector@ for @setMinimumThicknessForInlineSidebars:@
setMinimumThicknessForInlineSidebarsSelector :: Selector '[CDouble] ()
setMinimumThicknessForInlineSidebarsSelector = mkSelector "setMinimumThicknessForInlineSidebars:"

