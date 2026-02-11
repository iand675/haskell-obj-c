{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSTabViewController is a container view controller that displays a single child view controller at a time from its @childViewControllers.@ It provides standard tab-style UI for user selection of tabs, or allows custom UI to be easily created by providing targets for bindings. ChildViewControllers’ views are lazily loaded; they are only loaded once their tab is selected and visible. The NSTabViewController is set as the delegate of its managed NSTabView. Any overrides of NSTabViewDelegate methods must call super. Properties of the TabView such as the tabStyle can be directly manipulated, but calling methods that add and remove tabViewItems or changing the delegate is not allowed. NSViewController's methods @-addChildViewController:,@ @-insertViewController:atIndex:,@ and @-removeChildViewControllerAtIndex:@ can all be used as convience methods to add children; default TabViewItems will be appropriately created or destroyed. The default NSTabViewItem created with with +[NSTabViewItem tabViewItemForViewController:].
--
-- Generated bindings for @NSTabViewController@.
module ObjC.AppKit.NSTabViewController
  ( NSTabViewController
  , IsNSTabViewController(..)
  , addTabViewItem
  , insertTabViewItem_atIndex
  , removeTabViewItem
  , tabViewItemForViewController
  , viewDidLoad
  , tabView_willSelectTabViewItem
  , tabView_didSelectTabViewItem
  , tabView_shouldSelectTabViewItem
  , toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar
  , toolbarDefaultItemIdentifiers
  , toolbarAllowedItemIdentifiers
  , toolbarSelectableItemIdentifiers
  , tabStyle
  , setTabStyle
  , tabView
  , setTabView
  , transitionOptions
  , setTransitionOptions
  , canPropagateSelectedChildViewControllerTitle
  , setCanPropagateSelectedChildViewControllerTitle
  , tabViewItems
  , setTabViewItems
  , selectedTabViewItemIndex
  , setSelectedTabViewItemIndex
  , addTabViewItemSelector
  , insertTabViewItem_atIndexSelector
  , removeTabViewItemSelector
  , tabViewItemForViewControllerSelector
  , viewDidLoadSelector
  , tabView_willSelectTabViewItemSelector
  , tabView_didSelectTabViewItemSelector
  , tabView_shouldSelectTabViewItemSelector
  , toolbar_itemForItemIdentifier_willBeInsertedIntoToolbarSelector
  , toolbarDefaultItemIdentifiersSelector
  , toolbarAllowedItemIdentifiersSelector
  , toolbarSelectableItemIdentifiersSelector
  , tabStyleSelector
  , setTabStyleSelector
  , tabViewSelector
  , setTabViewSelector
  , transitionOptionsSelector
  , setTransitionOptionsSelector
  , canPropagateSelectedChildViewControllerTitleSelector
  , setCanPropagateSelectedChildViewControllerTitleSelector
  , tabViewItemsSelector
  , setTabViewItemsSelector
  , selectedTabViewItemIndexSelector
  , setSelectedTabViewItemIndexSelector

  -- * Enum types
  , NSTabViewControllerTabStyle(NSTabViewControllerTabStyle)
  , pattern NSTabViewControllerTabStyleSegmentedControlOnTop
  , pattern NSTabViewControllerTabStyleSegmentedControlOnBottom
  , pattern NSTabViewControllerTabStyleToolbar
  , pattern NSTabViewControllerTabStyleUnspecified
  , NSViewControllerTransitionOptions(NSViewControllerTransitionOptions)
  , pattern NSViewControllerTransitionNone
  , pattern NSViewControllerTransitionCrossfade
  , pattern NSViewControllerTransitionSlideUp
  , pattern NSViewControllerTransitionSlideDown
  , pattern NSViewControllerTransitionSlideLeft
  , pattern NSViewControllerTransitionSlideRight
  , pattern NSViewControllerTransitionSlideForward
  , pattern NSViewControllerTransitionSlideBackward
  , pattern NSViewControllerTransitionAllowUserInteraction

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

-- | Adds a TabViewItem to the end of the TabViewController. The tabViewItem’s viewController’s view will only be loaded once its tab is selected.
--
-- @tabViewItem@ — The TabViewItem to add. It must have a @viewController@ set by the time it is added or an exception will be thrown. An exception will also be thrown if tabViewItem is nil.
--
-- ObjC selector: @- addTabViewItem:@
addTabViewItem :: (IsNSTabViewController nsTabViewController, IsNSTabViewItem tabViewItem) => nsTabViewController -> tabViewItem -> IO ()
addTabViewItem nsTabViewController  tabViewItem =
withObjCPtr tabViewItem $ \raw_tabViewItem ->
    sendMsg nsTabViewController (mkSelector "addTabViewItem:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | Adds a TabViewItem to a given index in the TabViewController. The tabViewItem’s viewController’s view will only be loaded once its tab is selected. @-selectedTabViewItemIndex@ will be adjusted if the insertion index is before it. Subclasses must call through @-insertTabViewItem:atIndex:@ to add a TabViewItem.
--
-- @tabViewItem@ — The TabViewItem to add. It must have a @viewController@ set by the time it is added or an exception will be thrown. An exception will also be thrown if tabViewItem is nil.
--
-- @index@ — The index to add the TabViewItem at. Will throw an exception if @index@ < 0 or @index@ > @tabViewItems.count@
--
-- ObjC selector: @- insertTabViewItem:atIndex:@
insertTabViewItem_atIndex :: (IsNSTabViewController nsTabViewController, IsNSTabViewItem tabViewItem) => nsTabViewController -> tabViewItem -> CLong -> IO ()
insertTabViewItem_atIndex nsTabViewController  tabViewItem index =
withObjCPtr tabViewItem $ \raw_tabViewItem ->
    sendMsg nsTabViewController (mkSelector "insertTabViewItem:atIndex:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ()), argCLong (fromIntegral index)]

-- | Removes a TabViewItem from the receiver. If the removed tabViewItem currently selected, the next (or previous, if there is no next) tabViewItem will become selected. If this is the only tabViewItem in the TabViewController, the selectedTabViewItemIndex will become @-1.@ Subclasses must call through @-removeTabViewItem:@ to remove a TabViewItem.
--
-- @tabViewItem@ — The TabViewItem to remove. An exception will be thrown if @tabViewItem@ is not in the NSTabViewController or if it is nil.
--
-- ObjC selector: @- removeTabViewItem:@
removeTabViewItem :: (IsNSTabViewController nsTabViewController, IsNSTabViewItem tabViewItem) => nsTabViewController -> tabViewItem -> IO ()
removeTabViewItem nsTabViewController  tabViewItem =
withObjCPtr tabViewItem $ \raw_tabViewItem ->
    sendMsg nsTabViewController (mkSelector "removeTabViewItem:") retVoid [argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | Convenience method for getting the associated tab view item for a particular childViewController.
--
-- @viewController@ — The ViewController to look up.
--
-- Returns: The corresponding TabViewItem. Returns nil if @viewController@ is not a child of the TabViewController.
--
-- ObjC selector: @- tabViewItemForViewController:@
tabViewItemForViewController :: (IsNSTabViewController nsTabViewController, IsNSViewController viewController) => nsTabViewController -> viewController -> IO (Id NSTabViewItem)
tabViewItemForViewController nsTabViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsTabViewController (mkSelector "tabViewItemForViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

-- | @- viewDidLoad@
viewDidLoad :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO ()
viewDidLoad nsTabViewController  =
  sendMsg nsTabViewController (mkSelector "viewDidLoad") retVoid []

-- | @- tabView:willSelectTabViewItem:@
tabView_willSelectTabViewItem :: (IsNSTabViewController nsTabViewController, IsNSTabView tabView, IsNSTabViewItem tabViewItem) => nsTabViewController -> tabView -> tabViewItem -> IO ()
tabView_willSelectTabViewItem nsTabViewController  tabView tabViewItem =
withObjCPtr tabView $ \raw_tabView ->
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabViewController (mkSelector "tabView:willSelectTabViewItem:") retVoid [argPtr (castPtr raw_tabView :: Ptr ()), argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- tabView:didSelectTabViewItem:@
tabView_didSelectTabViewItem :: (IsNSTabViewController nsTabViewController, IsNSTabView tabView, IsNSTabViewItem tabViewItem) => nsTabViewController -> tabView -> tabViewItem -> IO ()
tabView_didSelectTabViewItem nsTabViewController  tabView tabViewItem =
withObjCPtr tabView $ \raw_tabView ->
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      sendMsg nsTabViewController (mkSelector "tabView:didSelectTabViewItem:") retVoid [argPtr (castPtr raw_tabView :: Ptr ()), argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- tabView:shouldSelectTabViewItem:@
tabView_shouldSelectTabViewItem :: (IsNSTabViewController nsTabViewController, IsNSTabView tabView, IsNSTabViewItem tabViewItem) => nsTabViewController -> tabView -> tabViewItem -> IO Bool
tabView_shouldSelectTabViewItem nsTabViewController  tabView tabViewItem =
withObjCPtr tabView $ \raw_tabView ->
  withObjCPtr tabViewItem $ \raw_tabViewItem ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTabViewController (mkSelector "tabView:shouldSelectTabViewItem:") retCULong [argPtr (castPtr raw_tabView :: Ptr ()), argPtr (castPtr raw_tabViewItem :: Ptr ())]

-- | @- toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:@
toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar :: (IsNSTabViewController nsTabViewController, IsNSToolbar toolbar, IsNSString itemIdentifier) => nsTabViewController -> toolbar -> itemIdentifier -> Bool -> IO (Id NSToolbarItem)
toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar nsTabViewController  toolbar itemIdentifier flag =
withObjCPtr toolbar $ \raw_toolbar ->
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsTabViewController (mkSelector "toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:") (retPtr retVoid) [argPtr (castPtr raw_toolbar :: Ptr ()), argPtr (castPtr raw_itemIdentifier :: Ptr ()), argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @- toolbarDefaultItemIdentifiers:@
toolbarDefaultItemIdentifiers :: (IsNSTabViewController nsTabViewController, IsNSToolbar toolbar) => nsTabViewController -> toolbar -> IO (Id NSArray)
toolbarDefaultItemIdentifiers nsTabViewController  toolbar =
withObjCPtr toolbar $ \raw_toolbar ->
    sendMsg nsTabViewController (mkSelector "toolbarDefaultItemIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_toolbar :: Ptr ())] >>= retainedObject . castPtr

-- | @- toolbarAllowedItemIdentifiers:@
toolbarAllowedItemIdentifiers :: (IsNSTabViewController nsTabViewController, IsNSToolbar toolbar) => nsTabViewController -> toolbar -> IO (Id NSArray)
toolbarAllowedItemIdentifiers nsTabViewController  toolbar =
withObjCPtr toolbar $ \raw_toolbar ->
    sendMsg nsTabViewController (mkSelector "toolbarAllowedItemIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_toolbar :: Ptr ())] >>= retainedObject . castPtr

-- | @- toolbarSelectableItemIdentifiers:@
toolbarSelectableItemIdentifiers :: (IsNSTabViewController nsTabViewController, IsNSToolbar toolbar) => nsTabViewController -> toolbar -> IO (Id NSArray)
toolbarSelectableItemIdentifiers nsTabViewController  toolbar =
withObjCPtr toolbar $ \raw_toolbar ->
    sendMsg nsTabViewController (mkSelector "toolbarSelectableItemIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_toolbar :: Ptr ())] >>= retainedObject . castPtr

-- | The style that this NSTabViewController displays its UI as. Defaults to @NSTabViewControllerTabStyleSegmentedControlOnTop.@
--
-- ObjC selector: @- tabStyle@
tabStyle :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO NSTabViewControllerTabStyle
tabStyle nsTabViewController  =
  fmap (coerce :: CLong -> NSTabViewControllerTabStyle) $ sendMsg nsTabViewController (mkSelector "tabStyle") retCLong []

-- | The style that this NSTabViewController displays its UI as. Defaults to @NSTabViewControllerTabStyleSegmentedControlOnTop.@
--
-- ObjC selector: @- setTabStyle:@
setTabStyle :: IsNSTabViewController nsTabViewController => nsTabViewController -> NSTabViewControllerTabStyle -> IO ()
setTabStyle nsTabViewController  value =
  sendMsg nsTabViewController (mkSelector "setTabStyle:") retVoid [argCLong (coerce value)]

-- | Access to the tab view that the controller is controlling. To provide a custom NSTabView, assign the value anytime before @self.viewLoaded@ is @YES.@ Querying the value will create it on-demand, if needed. Check @self.viewLoaded@ before querying the value to avoid prematurely creating the view. Note that the @-tabView@ may not be equal to the @viewController.view.@ Properties such as the tabStyle can be directly manipulated, but calling methods that add and remove tabViewItems or changing the delegate is not allowed. The NSTabViewController will be made the delegate of the NSTabView. Internally, the NSTabView is always used to switch between displayed childViewControllers, regardless of the style displayed.
--
-- ObjC selector: @- tabView@
tabView :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO (Id NSTabView)
tabView nsTabViewController  =
  sendMsg nsTabViewController (mkSelector "tabView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Access to the tab view that the controller is controlling. To provide a custom NSTabView, assign the value anytime before @self.viewLoaded@ is @YES.@ Querying the value will create it on-demand, if needed. Check @self.viewLoaded@ before querying the value to avoid prematurely creating the view. Note that the @-tabView@ may not be equal to the @viewController.view.@ Properties such as the tabStyle can be directly manipulated, but calling methods that add and remove tabViewItems or changing the delegate is not allowed. The NSTabViewController will be made the delegate of the NSTabView. Internally, the NSTabView is always used to switch between displayed childViewControllers, regardless of the style displayed.
--
-- ObjC selector: @- setTabView:@
setTabView :: (IsNSTabViewController nsTabViewController, IsNSTabView value) => nsTabViewController -> value -> IO ()
setTabView nsTabViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTabViewController (mkSelector "setTabView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This defines how NSTabViewController transitions from one view to another. Transitions go through [self transitionFromViewController:toViewController:options:completionHandler:]. The default value is @NSViewControllerTransitionCrossfade|NSViewControllerTransitionAllowUserInteraction.@
--
-- ObjC selector: @- transitionOptions@
transitionOptions :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO NSViewControllerTransitionOptions
transitionOptions nsTabViewController  =
  fmap (coerce :: CULong -> NSViewControllerTransitionOptions) $ sendMsg nsTabViewController (mkSelector "transitionOptions") retCULong []

-- | This defines how NSTabViewController transitions from one view to another. Transitions go through [self transitionFromViewController:toViewController:options:completionHandler:]. The default value is @NSViewControllerTransitionCrossfade|NSViewControllerTransitionAllowUserInteraction.@
--
-- ObjC selector: @- setTransitionOptions:@
setTransitionOptions :: IsNSTabViewController nsTabViewController => nsTabViewController -> NSViewControllerTransitionOptions -> IO ()
setTransitionOptions nsTabViewController  value =
  sendMsg nsTabViewController (mkSelector "setTransitionOptions:") retVoid [argCULong (coerce value)]

-- | If YES and the receiving NSTabViewController has a nil title, @-title@ will return its selected child ViewController's title. If NO, it will continue to return nil. The default value is @YES.@
--
-- ObjC selector: @- canPropagateSelectedChildViewControllerTitle@
canPropagateSelectedChildViewControllerTitle :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO Bool
canPropagateSelectedChildViewControllerTitle nsTabViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTabViewController (mkSelector "canPropagateSelectedChildViewControllerTitle") retCULong []

-- | If YES and the receiving NSTabViewController has a nil title, @-title@ will return its selected child ViewController's title. If NO, it will continue to return nil. The default value is @YES.@
--
-- ObjC selector: @- setCanPropagateSelectedChildViewControllerTitle:@
setCanPropagateSelectedChildViewControllerTitle :: IsNSTabViewController nsTabViewController => nsTabViewController -> Bool -> IO ()
setCanPropagateSelectedChildViewControllerTitle nsTabViewController  value =
  sendMsg nsTabViewController (mkSelector "setCanPropagateSelectedChildViewControllerTitle:") retVoid [argCULong (if value then 1 else 0)]

-- | The array of tab view items that correspond to the current child view controllers. After a child view controller is added to the receiving TabViewController, a NSTabViewItem with the default values will be created for it. Once the child is removed, its corresponding tabViewItem will be removed from the tabViewItems array.
--
-- ObjC selector: @- tabViewItems@
tabViewItems :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO (Id NSArray)
tabViewItems nsTabViewController  =
  sendMsg nsTabViewController (mkSelector "tabViewItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The array of tab view items that correspond to the current child view controllers. After a child view controller is added to the receiving TabViewController, a NSTabViewItem with the default values will be created for it. Once the child is removed, its corresponding tabViewItem will be removed from the tabViewItems array.
--
-- ObjC selector: @- setTabViewItems:@
setTabViewItems :: (IsNSTabViewController nsTabViewController, IsNSArray value) => nsTabViewController -> value -> IO ()
setTabViewItems nsTabViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTabViewController (mkSelector "setTabViewItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Read and write the current selected TabViewItem that is being shown. This value is KVC compliant and can be the target of a binding. For instance, a NSSegmentedControl's selection can be bound to this value with:
--
-- [segmentedControl bind:NSSelectedIndexBinding toObject:tabViewController withKeyPath:@“selectedTabViewItemIndex" options:nil];
--
-- ObjC selector: @- selectedTabViewItemIndex@
selectedTabViewItemIndex :: IsNSTabViewController nsTabViewController => nsTabViewController -> IO CLong
selectedTabViewItemIndex nsTabViewController  =
  sendMsg nsTabViewController (mkSelector "selectedTabViewItemIndex") retCLong []

-- | Read and write the current selected TabViewItem that is being shown. This value is KVC compliant and can be the target of a binding. For instance, a NSSegmentedControl's selection can be bound to this value with:
--
-- [segmentedControl bind:NSSelectedIndexBinding toObject:tabViewController withKeyPath:@“selectedTabViewItemIndex" options:nil];
--
-- ObjC selector: @- setSelectedTabViewItemIndex:@
setSelectedTabViewItemIndex :: IsNSTabViewController nsTabViewController => nsTabViewController -> CLong -> IO ()
setSelectedTabViewItemIndex nsTabViewController  value =
  sendMsg nsTabViewController (mkSelector "setSelectedTabViewItemIndex:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTabViewItem:@
addTabViewItemSelector :: Selector
addTabViewItemSelector = mkSelector "addTabViewItem:"

-- | @Selector@ for @insertTabViewItem:atIndex:@
insertTabViewItem_atIndexSelector :: Selector
insertTabViewItem_atIndexSelector = mkSelector "insertTabViewItem:atIndex:"

-- | @Selector@ for @removeTabViewItem:@
removeTabViewItemSelector :: Selector
removeTabViewItemSelector = mkSelector "removeTabViewItem:"

-- | @Selector@ for @tabViewItemForViewController:@
tabViewItemForViewControllerSelector :: Selector
tabViewItemForViewControllerSelector = mkSelector "tabViewItemForViewController:"

-- | @Selector@ for @viewDidLoad@
viewDidLoadSelector :: Selector
viewDidLoadSelector = mkSelector "viewDidLoad"

-- | @Selector@ for @tabView:willSelectTabViewItem:@
tabView_willSelectTabViewItemSelector :: Selector
tabView_willSelectTabViewItemSelector = mkSelector "tabView:willSelectTabViewItem:"

-- | @Selector@ for @tabView:didSelectTabViewItem:@
tabView_didSelectTabViewItemSelector :: Selector
tabView_didSelectTabViewItemSelector = mkSelector "tabView:didSelectTabViewItem:"

-- | @Selector@ for @tabView:shouldSelectTabViewItem:@
tabView_shouldSelectTabViewItemSelector :: Selector
tabView_shouldSelectTabViewItemSelector = mkSelector "tabView:shouldSelectTabViewItem:"

-- | @Selector@ for @toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:@
toolbar_itemForItemIdentifier_willBeInsertedIntoToolbarSelector :: Selector
toolbar_itemForItemIdentifier_willBeInsertedIntoToolbarSelector = mkSelector "toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:"

-- | @Selector@ for @toolbarDefaultItemIdentifiers:@
toolbarDefaultItemIdentifiersSelector :: Selector
toolbarDefaultItemIdentifiersSelector = mkSelector "toolbarDefaultItemIdentifiers:"

-- | @Selector@ for @toolbarAllowedItemIdentifiers:@
toolbarAllowedItemIdentifiersSelector :: Selector
toolbarAllowedItemIdentifiersSelector = mkSelector "toolbarAllowedItemIdentifiers:"

-- | @Selector@ for @toolbarSelectableItemIdentifiers:@
toolbarSelectableItemIdentifiersSelector :: Selector
toolbarSelectableItemIdentifiersSelector = mkSelector "toolbarSelectableItemIdentifiers:"

-- | @Selector@ for @tabStyle@
tabStyleSelector :: Selector
tabStyleSelector = mkSelector "tabStyle"

-- | @Selector@ for @setTabStyle:@
setTabStyleSelector :: Selector
setTabStyleSelector = mkSelector "setTabStyle:"

-- | @Selector@ for @tabView@
tabViewSelector :: Selector
tabViewSelector = mkSelector "tabView"

-- | @Selector@ for @setTabView:@
setTabViewSelector :: Selector
setTabViewSelector = mkSelector "setTabView:"

-- | @Selector@ for @transitionOptions@
transitionOptionsSelector :: Selector
transitionOptionsSelector = mkSelector "transitionOptions"

-- | @Selector@ for @setTransitionOptions:@
setTransitionOptionsSelector :: Selector
setTransitionOptionsSelector = mkSelector "setTransitionOptions:"

-- | @Selector@ for @canPropagateSelectedChildViewControllerTitle@
canPropagateSelectedChildViewControllerTitleSelector :: Selector
canPropagateSelectedChildViewControllerTitleSelector = mkSelector "canPropagateSelectedChildViewControllerTitle"

-- | @Selector@ for @setCanPropagateSelectedChildViewControllerTitle:@
setCanPropagateSelectedChildViewControllerTitleSelector :: Selector
setCanPropagateSelectedChildViewControllerTitleSelector = mkSelector "setCanPropagateSelectedChildViewControllerTitle:"

-- | @Selector@ for @tabViewItems@
tabViewItemsSelector :: Selector
tabViewItemsSelector = mkSelector "tabViewItems"

-- | @Selector@ for @setTabViewItems:@
setTabViewItemsSelector :: Selector
setTabViewItemsSelector = mkSelector "setTabViewItems:"

-- | @Selector@ for @selectedTabViewItemIndex@
selectedTabViewItemIndexSelector :: Selector
selectedTabViewItemIndexSelector = mkSelector "selectedTabViewItemIndex"

-- | @Selector@ for @setSelectedTabViewItemIndex:@
setSelectedTabViewItemIndexSelector :: Selector
setSelectedTabViewItemIndexSelector = mkSelector "setSelectedTabViewItemIndex:"

