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
  , removeSplitViewItemSelector
  , splitViewItemForViewControllerSelector
  , validateUserInterfaceItemSelector
  , viewDidLoadSelector
  , splitView_canCollapseSubviewSelector
  , splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector
  , splitView_shouldHideDividerAtIndexSelector
  , splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector
  , splitView_additionalEffectiveRectOfDividerAtIndexSelector
  , toggleSidebarSelector
  , toggleInspectorSelector
  , splitViewSelector
  , setSplitViewSelector
  , splitViewItemsSelector
  , setSplitViewItemsSelector
  , minimumThicknessForInlineSidebarsSelector
  , setMinimumThicknessForInlineSidebarsSelector


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
import ObjC.Foundation.Internal.Classes

-- | Adds a SplitViewItem to the end of the SplitViewController. If the receiver's view is loaded and the SplitViewItem is not collapsed, the SplitViewItem's viewController's view will be loaded and added to the splitView. This calls through to -insertSplitViewItem:atIndex:.
--
-- @splitViewItem@ — The SplitViewItem to add. It must have a viewController set by the time it is added or an exception will be thrown. An exception will also be thrown if splitViewItem is nil.
--
-- ObjC selector: @- addSplitViewItem:@
addSplitViewItem :: (IsNSSplitViewController nsSplitViewController, IsNSSplitViewItem splitViewItem) => nsSplitViewController -> splitViewItem -> IO ()
addSplitViewItem nsSplitViewController  splitViewItem =
withObjCPtr splitViewItem $ \raw_splitViewItem ->
    sendMsg nsSplitViewController (mkSelector "addSplitViewItem:") retVoid [argPtr (castPtr raw_splitViewItem :: Ptr ())]

-- | Adds a SplitViewItem to a given index in the SplitViewController. If the receiver's view is loaded and the SplitViewItem is not collapsed, the SplitViewItem's viewController's view will be loaded and added to the @splitView.@ Subclasses must call through @-insertSplitViewItem:atIndex:@ to add a SplitViewItem.
--
-- @splitViewItem@ — The SplitViewItem to add. It must have a @viewController@ set by the time it is added or an exception will be thrown. An exception will also be thrown if splitViewItem is nil.
--
-- @index@ — The index to add the SplitViewItem at. Will throw an exception if @index@ < 0 or @index@ > @splitViewItems.count@
--
-- ObjC selector: @- insertSplitViewItem:atIndex:@
insertSplitViewItem_atIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitViewItem splitViewItem) => nsSplitViewController -> splitViewItem -> CLong -> IO ()
insertSplitViewItem_atIndex nsSplitViewController  splitViewItem index =
withObjCPtr splitViewItem $ \raw_splitViewItem ->
    sendMsg nsSplitViewController (mkSelector "insertSplitViewItem:atIndex:") retVoid [argPtr (castPtr raw_splitViewItem :: Ptr ()), argCLong (fromIntegral index)]

-- | Removes a SplitViewItem from the receiver. The layout of the @splitView@ will be adjusted for its removal. Subclasses must call through @-removeSplitViewItem:@ to remove a SplitViewItem.
--
-- @splitViewItem@ — The SplitViewItem to remove. An exception will be thrown if @splitViewItem@ is not in the SplitViewController or if it is nil.
--
-- ObjC selector: @- removeSplitViewItem:@
removeSplitViewItem :: (IsNSSplitViewController nsSplitViewController, IsNSSplitViewItem splitViewItem) => nsSplitViewController -> splitViewItem -> IO ()
removeSplitViewItem nsSplitViewController  splitViewItem =
withObjCPtr splitViewItem $ \raw_splitViewItem ->
    sendMsg nsSplitViewController (mkSelector "removeSplitViewItem:") retVoid [argPtr (castPtr raw_splitViewItem :: Ptr ())]

-- | Returns the corresponding SplitViewItem for a given child ViewController.
--
-- @viewController@ — The ViewController to look up.
--
-- Returns: The corresponding SplitViewItem. Returns nil if @viewController@ is not a child of the SplitViewController.
--
-- ObjC selector: @- splitViewItemForViewController:@
splitViewItemForViewController :: (IsNSSplitViewController nsSplitViewController, IsNSViewController viewController) => nsSplitViewController -> viewController -> IO (Id NSSplitViewItem)
splitViewItemForViewController nsSplitViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsSplitViewController (mkSelector "splitViewItemForViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

-- | Validates items with an action of @toggleSidebar:@ to reflect the status of the sidebar item contained within the receiver.
--
-- ObjC selector: @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> RawId -> IO Bool
validateUserInterfaceItem nsSplitViewController  item =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewController (mkSelector "validateUserInterfaceItem:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- viewDidLoad@
viewDidLoad :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO ()
viewDidLoad nsSplitViewController  =
  sendMsg nsSplitViewController (mkSelector "viewDidLoad") retVoid []

-- | @- splitView:canCollapseSubview:@
splitView_canCollapseSubview :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView, IsNSView subview) => nsSplitViewController -> splitView -> subview -> IO Bool
splitView_canCollapseSubview nsSplitViewController  splitView subview =
withObjCPtr splitView $ \raw_splitView ->
  withObjCPtr subview $ \raw_subview ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewController (mkSelector "splitView:canCollapseSubview:") retCULong [argPtr (castPtr raw_splitView :: Ptr ()), argPtr (castPtr raw_subview :: Ptr ())]

-- | @- splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:@
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView, IsNSView subview) => nsSplitViewController -> splitView -> subview -> CLong -> IO Bool
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex nsSplitViewController  splitView subview dividerIndex =
withObjCPtr splitView $ \raw_splitView ->
  withObjCPtr subview $ \raw_subview ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewController (mkSelector "splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:") retCULong [argPtr (castPtr raw_splitView :: Ptr ()), argPtr (castPtr raw_subview :: Ptr ()), argCLong (fromIntegral dividerIndex)]

-- | @- splitView:shouldHideDividerAtIndex:@
splitView_shouldHideDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView) => nsSplitViewController -> splitView -> CLong -> IO Bool
splitView_shouldHideDividerAtIndex nsSplitViewController  splitView dividerIndex =
withObjCPtr splitView $ \raw_splitView ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewController (mkSelector "splitView:shouldHideDividerAtIndex:") retCULong [argPtr (castPtr raw_splitView :: Ptr ()), argCLong (fromIntegral dividerIndex)]

-- | @- splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:@
splitView_effectiveRect_forDrawnRect_ofDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView) => nsSplitViewController -> splitView -> NSRect -> NSRect -> CLong -> IO NSRect
splitView_effectiveRect_forDrawnRect_ofDividerAtIndex nsSplitViewController  splitView proposedEffectiveRect drawnRect dividerIndex =
withObjCPtr splitView $ \raw_splitView ->
    sendMsgStret nsSplitViewController (mkSelector "splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:") retNSRect [argPtr (castPtr raw_splitView :: Ptr ()), argNSRect proposedEffectiveRect, argNSRect drawnRect, argCLong (fromIntegral dividerIndex)]

-- | @- splitView:additionalEffectiveRectOfDividerAtIndex:@
splitView_additionalEffectiveRectOfDividerAtIndex :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView splitView) => nsSplitViewController -> splitView -> CLong -> IO NSRect
splitView_additionalEffectiveRectOfDividerAtIndex nsSplitViewController  splitView dividerIndex =
withObjCPtr splitView $ \raw_splitView ->
    sendMsgStret nsSplitViewController (mkSelector "splitView:additionalEffectiveRectOfDividerAtIndex:") retNSRect [argPtr (castPtr raw_splitView :: Ptr ()), argCLong (fromIntegral dividerIndex)]

-- | Animatedly collapses or uncollapses the first sidebar split view item in the receiver. Does nothing if the receiver does not contain any sidebars.
--
-- ObjC selector: @- toggleSidebar:@
toggleSidebar :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> RawId -> IO ()
toggleSidebar nsSplitViewController  sender =
  sendMsg nsSplitViewController (mkSelector "toggleSidebar:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Animatedly collapses or uncollapses the first inspector split view item in the receiver. Does nothing if the receiver does not contain any inspectors.
--
-- ObjC selector: @- toggleInspector:@
toggleInspector :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> RawId -> IO ()
toggleInspector nsSplitViewController  sender =
  sendMsg nsSplitViewController (mkSelector "toggleInspector:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | The split view managed by the SplitViewController. This can be used to customize view properties such as the dividerStyle, vertical, and autosaveName. It is not guaranteed to be the same view as the receivers 'view' property. The default created splitView is vertical with a dividerStyle of @NSSplitViewDividerStyleThin.@ To provide a custom NSSplitView, set the splitView property anytime before self.viewLoaded is YES.
--
-- ObjC selector: @- splitView@
splitView :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO (Id NSSplitView)
splitView nsSplitViewController  =
  sendMsg nsSplitViewController (mkSelector "splitView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The split view managed by the SplitViewController. This can be used to customize view properties such as the dividerStyle, vertical, and autosaveName. It is not guaranteed to be the same view as the receivers 'view' property. The default created splitView is vertical with a dividerStyle of @NSSplitViewDividerStyleThin.@ To provide a custom NSSplitView, set the splitView property anytime before self.viewLoaded is YES.
--
-- ObjC selector: @- setSplitView:@
setSplitView :: (IsNSSplitViewController nsSplitViewController, IsNSSplitView value) => nsSplitViewController -> value -> IO ()
setSplitView nsSplitViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSplitViewController (mkSelector "setSplitView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The array of SplitViewItems that correspond to the current child view controllers. After a child view controller is added to the receiving splitViewController, a NSSplitViewItem with the default values will be created for it. Once the child is removed, its corresponding splitViewItem will be removed from the splitViewItems array. Setting this will call through to @-insertSplitViewItem:atIndex@ and @-removeSplitViewItem:@ for items that are new or need removal.
--
-- ObjC selector: @- splitViewItems@
splitViewItems :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO (Id NSArray)
splitViewItems nsSplitViewController  =
  sendMsg nsSplitViewController (mkSelector "splitViewItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The array of SplitViewItems that correspond to the current child view controllers. After a child view controller is added to the receiving splitViewController, a NSSplitViewItem with the default values will be created for it. Once the child is removed, its corresponding splitViewItem will be removed from the splitViewItems array. Setting this will call through to @-insertSplitViewItem:atIndex@ and @-removeSplitViewItem:@ for items that are new or need removal.
--
-- ObjC selector: @- setSplitViewItems:@
setSplitViewItems :: (IsNSSplitViewController nsSplitViewController, IsNSArray value) => nsSplitViewController -> value -> IO ()
setSplitViewItems nsSplitViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSplitViewController (mkSelector "setSplitViewItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The minimum thickness in the primary axis of split view (width for "vertical", height otherwise) before sidebar items will automatically collapse. If reshown in fullscreen, they will overlay over the other split items. Auto-collapsed sidebars will automatically uncollapse if the thickness is increased back to or past the minimum thickness. Defaults to @NSSplitViewControllerAutomaticDimension,@ which will use the effective minimum sizes of the split view item views as described by constraints in the window to determine the minimum size for inline sidebars. Once constraints establishing the minimum size can't be satisfied for all non-collapsed split panes, all sidebars will auto-collapse. When fullscreen, if a sidebar tries to uncollapse in this state, it will overlay.
--
-- ObjC selector: @- minimumThicknessForInlineSidebars@
minimumThicknessForInlineSidebars :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> IO CDouble
minimumThicknessForInlineSidebars nsSplitViewController  =
  sendMsg nsSplitViewController (mkSelector "minimumThicknessForInlineSidebars") retCDouble []

-- | The minimum thickness in the primary axis of split view (width for "vertical", height otherwise) before sidebar items will automatically collapse. If reshown in fullscreen, they will overlay over the other split items. Auto-collapsed sidebars will automatically uncollapse if the thickness is increased back to or past the minimum thickness. Defaults to @NSSplitViewControllerAutomaticDimension,@ which will use the effective minimum sizes of the split view item views as described by constraints in the window to determine the minimum size for inline sidebars. Once constraints establishing the minimum size can't be satisfied for all non-collapsed split panes, all sidebars will auto-collapse. When fullscreen, if a sidebar tries to uncollapse in this state, it will overlay.
--
-- ObjC selector: @- setMinimumThicknessForInlineSidebars:@
setMinimumThicknessForInlineSidebars :: IsNSSplitViewController nsSplitViewController => nsSplitViewController -> CDouble -> IO ()
setMinimumThicknessForInlineSidebars nsSplitViewController  value =
  sendMsg nsSplitViewController (mkSelector "setMinimumThicknessForInlineSidebars:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSplitViewItem:@
addSplitViewItemSelector :: Selector
addSplitViewItemSelector = mkSelector "addSplitViewItem:"

-- | @Selector@ for @insertSplitViewItem:atIndex:@
insertSplitViewItem_atIndexSelector :: Selector
insertSplitViewItem_atIndexSelector = mkSelector "insertSplitViewItem:atIndex:"

-- | @Selector@ for @removeSplitViewItem:@
removeSplitViewItemSelector :: Selector
removeSplitViewItemSelector = mkSelector "removeSplitViewItem:"

-- | @Selector@ for @splitViewItemForViewController:@
splitViewItemForViewControllerSelector :: Selector
splitViewItemForViewControllerSelector = mkSelector "splitViewItemForViewController:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @viewDidLoad@
viewDidLoadSelector :: Selector
viewDidLoadSelector = mkSelector "viewDidLoad"

-- | @Selector@ for @splitView:canCollapseSubview:@
splitView_canCollapseSubviewSelector :: Selector
splitView_canCollapseSubviewSelector = mkSelector "splitView:canCollapseSubview:"

-- | @Selector@ for @splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:@
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector :: Selector
splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndexSelector = mkSelector "splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:"

-- | @Selector@ for @splitView:shouldHideDividerAtIndex:@
splitView_shouldHideDividerAtIndexSelector :: Selector
splitView_shouldHideDividerAtIndexSelector = mkSelector "splitView:shouldHideDividerAtIndex:"

-- | @Selector@ for @splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:@
splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector :: Selector
splitView_effectiveRect_forDrawnRect_ofDividerAtIndexSelector = mkSelector "splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:"

-- | @Selector@ for @splitView:additionalEffectiveRectOfDividerAtIndex:@
splitView_additionalEffectiveRectOfDividerAtIndexSelector :: Selector
splitView_additionalEffectiveRectOfDividerAtIndexSelector = mkSelector "splitView:additionalEffectiveRectOfDividerAtIndex:"

-- | @Selector@ for @toggleSidebar:@
toggleSidebarSelector :: Selector
toggleSidebarSelector = mkSelector "toggleSidebar:"

-- | @Selector@ for @toggleInspector:@
toggleInspectorSelector :: Selector
toggleInspectorSelector = mkSelector "toggleInspector:"

-- | @Selector@ for @splitView@
splitViewSelector :: Selector
splitViewSelector = mkSelector "splitView"

-- | @Selector@ for @setSplitView:@
setSplitViewSelector :: Selector
setSplitViewSelector = mkSelector "setSplitView:"

-- | @Selector@ for @splitViewItems@
splitViewItemsSelector :: Selector
splitViewItemsSelector = mkSelector "splitViewItems"

-- | @Selector@ for @setSplitViewItems:@
setSplitViewItemsSelector :: Selector
setSplitViewItemsSelector = mkSelector "setSplitViewItems:"

-- | @Selector@ for @minimumThicknessForInlineSidebars@
minimumThicknessForInlineSidebarsSelector :: Selector
minimumThicknessForInlineSidebarsSelector = mkSelector "minimumThicknessForInlineSidebars"

-- | @Selector@ for @setMinimumThicknessForInlineSidebars:@
setMinimumThicknessForInlineSidebarsSelector :: Selector
setMinimumThicknessForInlineSidebarsSelector = mkSelector "setMinimumThicknessForInlineSidebars:"

