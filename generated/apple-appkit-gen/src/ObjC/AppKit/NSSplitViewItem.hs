{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSSplitViewItem implements the items used in an NSSplitViewController. The item describes a child ViewController's state in a SplitViewController, e.g. its collapsibility, holding priority and other metrics, and collapsed state.
--
-- Generated bindings for @NSSplitViewItem@.
module ObjC.AppKit.NSSplitViewItem
  ( NSSplitViewItem
  , IsNSSplitViewItem(..)
  , splitViewItemWithViewController
  , sidebarWithViewController
  , contentListWithViewController
  , inspectorWithViewController
  , addTopAlignedAccessoryViewController
  , insertTopAlignedAccessoryViewController_atIndex
  , removeTopAlignedAccessoryViewControllerAtIndex
  , addBottomAlignedAccessoryViewController
  , insertBottomAlignedAccessoryViewController_atIndex
  , removeBottomAlignedAccessoryViewControllerAtIndex
  , behavior
  , viewController
  , setViewController
  , collapsed
  , setCollapsed
  , canCollapse
  , setCanCollapse
  , collapseBehavior
  , setCollapseBehavior
  , minimumThickness
  , setMinimumThickness
  , maximumThickness
  , setMaximumThickness
  , preferredThicknessFraction
  , setPreferredThicknessFraction
  , holdingPriority
  , setHoldingPriority
  , automaticMaximumThickness
  , setAutomaticMaximumThickness
  , springLoaded
  , setSpringLoaded
  , canCollapseFromWindowResize
  , setCanCollapseFromWindowResize
  , allowsFullHeightLayout
  , setAllowsFullHeightLayout
  , titlebarSeparatorStyle
  , setTitlebarSeparatorStyle
  , automaticallyAdjustsSafeAreaInsets
  , setAutomaticallyAdjustsSafeAreaInsets
  , topAlignedAccessoryViewControllers
  , setTopAlignedAccessoryViewControllers
  , bottomAlignedAccessoryViewControllers
  , setBottomAlignedAccessoryViewControllers
  , addBottomAlignedAccessoryViewControllerSelector
  , addTopAlignedAccessoryViewControllerSelector
  , allowsFullHeightLayoutSelector
  , automaticMaximumThicknessSelector
  , automaticallyAdjustsSafeAreaInsetsSelector
  , behaviorSelector
  , bottomAlignedAccessoryViewControllersSelector
  , canCollapseFromWindowResizeSelector
  , canCollapseSelector
  , collapseBehaviorSelector
  , collapsedSelector
  , contentListWithViewControllerSelector
  , holdingPrioritySelector
  , insertBottomAlignedAccessoryViewController_atIndexSelector
  , insertTopAlignedAccessoryViewController_atIndexSelector
  , inspectorWithViewControllerSelector
  , maximumThicknessSelector
  , minimumThicknessSelector
  , preferredThicknessFractionSelector
  , removeBottomAlignedAccessoryViewControllerAtIndexSelector
  , removeTopAlignedAccessoryViewControllerAtIndexSelector
  , setAllowsFullHeightLayoutSelector
  , setAutomaticMaximumThicknessSelector
  , setAutomaticallyAdjustsSafeAreaInsetsSelector
  , setBottomAlignedAccessoryViewControllersSelector
  , setCanCollapseFromWindowResizeSelector
  , setCanCollapseSelector
  , setCollapseBehaviorSelector
  , setCollapsedSelector
  , setHoldingPrioritySelector
  , setMaximumThicknessSelector
  , setMinimumThicknessSelector
  , setPreferredThicknessFractionSelector
  , setSpringLoadedSelector
  , setTitlebarSeparatorStyleSelector
  , setTopAlignedAccessoryViewControllersSelector
  , setViewControllerSelector
  , sidebarWithViewControllerSelector
  , splitViewItemWithViewControllerSelector
  , springLoadedSelector
  , titlebarSeparatorStyleSelector
  , topAlignedAccessoryViewControllersSelector
  , viewControllerSelector

  -- * Enum types
  , NSSplitViewItemBehavior(NSSplitViewItemBehavior)
  , pattern NSSplitViewItemBehaviorDefault
  , pattern NSSplitViewItemBehaviorSidebar
  , pattern NSSplitViewItemBehaviorContentList
  , pattern NSSplitViewItemBehaviorInspector
  , NSSplitViewItemCollapseBehavior(NSSplitViewItemCollapseBehavior)
  , pattern NSSplitViewItemCollapseBehaviorDefault
  , pattern NSSplitViewItemCollapseBehaviorPreferResizingSplitViewWithFixedSiblings
  , pattern NSSplitViewItemCollapseBehaviorPreferResizingSiblingsWithFixedSplitView
  , pattern NSSplitViewItemCollapseBehaviorUseConstraints
  , NSTitlebarSeparatorStyle(NSTitlebarSeparatorStyle)
  , pattern NSTitlebarSeparatorStyleAutomatic
  , pattern NSTitlebarSeparatorStyleNone
  , pattern NSTitlebarSeparatorStyleLine
  , pattern NSTitlebarSeparatorStyleShadow

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

-- | Creates an autoreleased SplitViewItem that represents the provided ViewController. All other properties are left at their default.
--
-- @viewController@ — The view controller used to set the viewController property
--
-- Returns: An autoreleased SplitViewItem.
--
-- ObjC selector: @+ splitViewItemWithViewController:@
splitViewItemWithViewController :: IsNSViewController viewController => viewController -> IO (Id NSSplitViewItem)
splitViewItemWithViewController viewController =
  do
    cls' <- getRequiredClass "NSSplitViewItem"
    sendClassMessage cls' splitViewItemWithViewControllerSelector (toNSViewController viewController)

-- | Creates a split view item representing a sidebar for the provided ViewController. Sidebars have standard system behavior, specifically:  - Translucent material background  - The ability to collapse/uncollapse on split view size changes  - The ability to overlay at small split view sizes when in fullscreen  - canCollapse is set to YES  - minimumThickness and maximumThickness are set to the standard minimum and maximum sidebar size  - preferredThicknessFraction is set to the standard fraction for sidebars (0.15)  - springLoaded is set to YES
--
-- @viewController@ — The view controller used to set the viewController property
--
-- Returns: An autoreleased SplitViewItem that acts as a sidebar.
--
-- ObjC selector: @+ sidebarWithViewController:@
sidebarWithViewController :: IsNSViewController viewController => viewController -> IO (Id NSSplitViewItem)
sidebarWithViewController viewController =
  do
    cls' <- getRequiredClass "NSSplitViewItem"
    sendClassMessage cls' sidebarWithViewControllerSelector (toNSViewController viewController)

-- | Creates a split view item representing a content list for the provided ViewController, akin to Mail's message list, Note's note list. Content lists have system standard defaults, specifically:  - minimumThickness and maximumThickness are set to the system standard for content lists  - automaticMaximumThickness is set to the system standard for content lists  - preferredThicknessFraction is set to the standard fraction for content lists (0.28 when a neighbor sidebar is visible, 0.33 if not)
--
-- @viewController@ — The view controller used to set the viewController property
--
-- Returns: An autoreleased SplitViewItem that acts as a content list.
--
-- ObjC selector: @+ contentListWithViewController:@
contentListWithViewController :: IsNSViewController viewController => viewController -> IO (Id NSSplitViewItem)
contentListWithViewController viewController =
  do
    cls' <- getRequiredClass "NSSplitViewItem"
    sendClassMessage cls' contentListWithViewControllerSelector (toNSViewController viewController)

-- | Creates a split view item representing an inspector for the provided ViewController. On macOS 14.0 and above inspectors have the following standard system behavior:  - canCollapse is set to YES  - minimumThickness and maximumThickness are set to the standard inspector size (270) and are not resizable by default
--
-- @viewController@ — The view controller used to set the viewController property
--
-- Returns: An autoreleased SplitViewItem that acts as an inspector.
--
-- ObjC selector: @+ inspectorWithViewController:@
inspectorWithViewController :: IsNSViewController viewController => viewController -> IO (Id NSSplitViewItem)
inspectorWithViewController viewController =
  do
    cls' <- getRequiredClass "NSSplitViewItem"
    sendClassMessage cls' inspectorWithViewControllerSelector (toNSViewController viewController)

-- | @- addTopAlignedAccessoryViewController:@
addTopAlignedAccessoryViewController :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> IO ()
addTopAlignedAccessoryViewController nsSplitViewItem childViewController =
  sendMessage nsSplitViewItem addTopAlignedAccessoryViewControllerSelector (toNSSplitViewItemAccessoryViewController childViewController)

-- | @- insertTopAlignedAccessoryViewController:atIndex:@
insertTopAlignedAccessoryViewController_atIndex :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> CLong -> IO ()
insertTopAlignedAccessoryViewController_atIndex nsSplitViewItem childViewController index =
  sendMessage nsSplitViewItem insertTopAlignedAccessoryViewController_atIndexSelector (toNSSplitViewItemAccessoryViewController childViewController) index

-- | NOTE: you can use this method, or @-removeFromParentViewController:@, whichever is easier.
--
-- ObjC selector: @- removeTopAlignedAccessoryViewControllerAtIndex:@
removeTopAlignedAccessoryViewControllerAtIndex :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CLong -> IO ()
removeTopAlignedAccessoryViewControllerAtIndex nsSplitViewItem index =
  sendMessage nsSplitViewItem removeTopAlignedAccessoryViewControllerAtIndexSelector index

-- | @- addBottomAlignedAccessoryViewController:@
addBottomAlignedAccessoryViewController :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> IO ()
addBottomAlignedAccessoryViewController nsSplitViewItem childViewController =
  sendMessage nsSplitViewItem addBottomAlignedAccessoryViewControllerSelector (toNSSplitViewItemAccessoryViewController childViewController)

-- | @- insertBottomAlignedAccessoryViewController:atIndex:@
insertBottomAlignedAccessoryViewController_atIndex :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> CLong -> IO ()
insertBottomAlignedAccessoryViewController_atIndex nsSplitViewItem childViewController index =
  sendMessage nsSplitViewItem insertBottomAlignedAccessoryViewController_atIndexSelector (toNSSplitViewItemAccessoryViewController childViewController) index

-- | NOTE: you can use this method, or @-removeFromParentViewController:@, whichever is easier.
--
-- ObjC selector: @- removeBottomAlignedAccessoryViewControllerAtIndex:@
removeBottomAlignedAccessoryViewControllerAtIndex :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CLong -> IO ()
removeBottomAlignedAccessoryViewControllerAtIndex nsSplitViewItem index =
  sendMessage nsSplitViewItem removeBottomAlignedAccessoryViewControllerAtIndexSelector index

-- | The standard behavior type of the receiver. See initializers for descriptions of each behavior.
--
-- ObjC selector: @- behavior@
behavior :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO NSSplitViewItemBehavior
behavior nsSplitViewItem =
  sendMessage nsSplitViewItem behaviorSelector

-- | The view controller represented by the SplitViewItem. An exception will be thrown if a new viewController is set while the receiving SplitViewItem is added to a SplitViewController.
--
-- ObjC selector: @- viewController@
viewController :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO (Id NSViewController)
viewController nsSplitViewItem =
  sendMessage nsSplitViewItem viewControllerSelector

-- | The view controller represented by the SplitViewItem. An exception will be thrown if a new viewController is set while the receiving SplitViewItem is added to a SplitViewController.
--
-- ObjC selector: @- setViewController:@
setViewController :: (IsNSSplitViewItem nsSplitViewItem, IsNSViewController value) => nsSplitViewItem -> value -> IO ()
setViewController nsSplitViewItem value =
  sendMessage nsSplitViewItem setViewControllerSelector (toNSViewController value)

-- | Whether or not the child ViewController corresponding to the SplitViewItem is collapsed in the SplitViewController. The default is @NO.@ This can be set with the animator proxy to animate the collapse or uncollapse. The exact animation used can be customized by setting it in the -animations dictionary with a key of "collapsed". If this is set to YES before it is added to the SplitViewController, it will be initially collapsed and the SplitViewController will not cause the view to be loaded until it is uncollapsed. This is KVC/KVO compliant and will be updated if the value changes from user interaction.
--
-- ObjC selector: @- collapsed@
collapsed :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
collapsed nsSplitViewItem =
  sendMessage nsSplitViewItem collapsedSelector

-- | Whether or not the child ViewController corresponding to the SplitViewItem is collapsed in the SplitViewController. The default is @NO.@ This can be set with the animator proxy to animate the collapse or uncollapse. The exact animation used can be customized by setting it in the -animations dictionary with a key of "collapsed". If this is set to YES before it is added to the SplitViewController, it will be initially collapsed and the SplitViewController will not cause the view to be loaded until it is uncollapsed. This is KVC/KVO compliant and will be updated if the value changes from user interaction.
--
-- ObjC selector: @- setCollapsed:@
setCollapsed :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setCollapsed nsSplitViewItem value =
  sendMessage nsSplitViewItem setCollapsedSelector value

-- | Whether or not the child view controller is collapsible from user interaction - whether by dragging or double clicking a divider. The default is @NO.@
--
-- ObjC selector: @- canCollapse@
canCollapse :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
canCollapse nsSplitViewItem =
  sendMessage nsSplitViewItem canCollapseSelector

-- | Whether or not the child view controller is collapsible from user interaction - whether by dragging or double clicking a divider. The default is @NO.@
--
-- ObjC selector: @- setCanCollapse:@
setCanCollapse :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setCanCollapse nsSplitViewItem value =
  sendMessage nsSplitViewItem setCanCollapseSelector value

-- | The resize behavior when the receiver toggles its @collapsed@ state programmatically, both animatedly and not. Defaults to @.Default@.
--
-- ObjC selector: @- collapseBehavior@
collapseBehavior :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO NSSplitViewItemCollapseBehavior
collapseBehavior nsSplitViewItem =
  sendMessage nsSplitViewItem collapseBehaviorSelector

-- | The resize behavior when the receiver toggles its @collapsed@ state programmatically, both animatedly and not. Defaults to @.Default@.
--
-- ObjC selector: @- setCollapseBehavior:@
setCollapseBehavior :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> NSSplitViewItemCollapseBehavior -> IO ()
setCollapseBehavior nsSplitViewItem value =
  sendMessage nsSplitViewItem setCollapseBehaviorSelector value

-- | A convenience to set the minimum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no minimum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some minimum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- minimumThickness@
minimumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
minimumThickness nsSplitViewItem =
  sendMessage nsSplitViewItem minimumThicknessSelector

-- | A convenience to set the minimum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no minimum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some minimum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- setMinimumThickness:@
setMinimumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setMinimumThickness nsSplitViewItem value =
  sendMessage nsSplitViewItem setMinimumThicknessSelector value

-- | A convenience to set the maximum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no maximum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some maximum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- maximumThickness@
maximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
maximumThickness nsSplitViewItem =
  sendMessage nsSplitViewItem maximumThicknessSelector

-- | A convenience to set the maximum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no maximum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some maximum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- setMaximumThickness:@
setMaximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setMaximumThickness nsSplitViewItem value =
  sendMessage nsSplitViewItem setMaximumThicknessSelector value

-- | The percentage of the contained NSSplitView that the NSSplitViewItem prefers to encompass. This is used when double-clicking on a neighbor divider to return to that standard ratio. As well as after entering fullscreen to determine the initial size of the receiver. Defaults to NSSplitViewItemUnspecifiedDimension, which means no resize will occur on double-clicks, and the absolute size is preserved when entering fullscreen.
--
-- ObjC selector: @- preferredThicknessFraction@
preferredThicknessFraction :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
preferredThicknessFraction nsSplitViewItem =
  sendMessage nsSplitViewItem preferredThicknessFractionSelector

-- | The percentage of the contained NSSplitView that the NSSplitViewItem prefers to encompass. This is used when double-clicking on a neighbor divider to return to that standard ratio. As well as after entering fullscreen to determine the initial size of the receiver. Defaults to NSSplitViewItemUnspecifiedDimension, which means no resize will occur on double-clicks, and the absolute size is preserved when entering fullscreen.
--
-- ObjC selector: @- setPreferredThicknessFraction:@
setPreferredThicknessFraction :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setPreferredThicknessFraction nsSplitViewItem value =
  sendMessage nsSplitViewItem setPreferredThicknessFractionSelector value

-- | Sets the priority under which a SplitViewItem will hold its width (for a vertical split view) or height (for a horizontal split view). The view with the lowest priority will be the first to take on additional width if the split view grows or shrinks. The default is @NSLayoutPriorityDefaultLow.@
--
-- ObjC selector: @- holdingPriority@
holdingPriority :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CFloat
holdingPriority nsSplitViewItem =
  sendMessage nsSplitViewItem holdingPrioritySelector

-- | Sets the priority under which a SplitViewItem will hold its width (for a vertical split view) or height (for a horizontal split view). The view with the lowest priority will be the first to take on additional width if the split view grows or shrinks. The default is @NSLayoutPriorityDefaultLow.@
--
-- ObjC selector: @- setHoldingPriority:@
setHoldingPriority :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CFloat -> IO ()
setHoldingPriority nsSplitViewItem value =
  sendMessage nsSplitViewItem setHoldingPrioritySelector value

-- | The maximum thickness of the split view item when resizing due to automatic sizing, such as entering fullscreen with a set preferredThicknessFraction or proportional sizing. The user can still resize up to the absolute maximum size by dragging the divider or otherwise. If NSSplitViewItemUnspecifiedDimension, no automatic maximum is enforced. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- automaticMaximumThickness@
automaticMaximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
automaticMaximumThickness nsSplitViewItem =
  sendMessage nsSplitViewItem automaticMaximumThicknessSelector

-- | The maximum thickness of the split view item when resizing due to automatic sizing, such as entering fullscreen with a set preferredThicknessFraction or proportional sizing. The user can still resize up to the absolute maximum size by dragging the divider or otherwise. If NSSplitViewItemUnspecifiedDimension, no automatic maximum is enforced. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- setAutomaticMaximumThickness:@
setAutomaticMaximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setAutomaticMaximumThickness nsSplitViewItem value =
  sendMessage nsSplitViewItem setAutomaticMaximumThicknessSelector value

-- | If YES, the split view item can be temporarily uncollapsed during a drag by hovering or deep clicking on its neighboring divider. Defaults to NO.
--
-- ObjC selector: @- springLoaded@
springLoaded :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
springLoaded nsSplitViewItem =
  sendMessage nsSplitViewItem springLoadedSelector

-- | If YES, the split view item can be temporarily uncollapsed during a drag by hovering or deep clicking on its neighboring divider. Defaults to NO.
--
-- ObjC selector: @- setSpringLoaded:@
setSpringLoaded :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setSpringLoaded nsSplitViewItem value =
  sendMessage nsSplitViewItem setSpringLoadedSelector value

-- | If YES, the item can be collapsed from a window resize. This can differ from @canCollapse@, to allow divider collapsing but not window resize collapsing or vice versa. Defaults to YES for Sidebars and NO for Inspectors. - Note: Setting @canCollapse@ for sidebars will reset this value to that new value.
--
-- ObjC selector: @- canCollapseFromWindowResize@
canCollapseFromWindowResize :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
canCollapseFromWindowResize nsSplitViewItem =
  sendMessage nsSplitViewItem canCollapseFromWindowResizeSelector

-- | If YES, the item can be collapsed from a window resize. This can differ from @canCollapse@, to allow divider collapsing but not window resize collapsing or vice versa. Defaults to YES for Sidebars and NO for Inspectors. - Note: Setting @canCollapse@ for sidebars will reset this value to that new value.
--
-- ObjC selector: @- setCanCollapseFromWindowResize:@
setCanCollapseFromWindowResize :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setCanCollapseFromWindowResize nsSplitViewItem value =
  sendMessage nsSplitViewItem setCanCollapseFromWindowResizeSelector value

-- | Whether or not a sidebar or inspector is allowed to be full height in the window when the @NSFullSizeContentViewWindowMask@ style mask is also set. Only applies to NSSplitViewItemBehaviorSidebar and NSSplitViewItemBehaviorInspector. Defaults to YES.
--
-- ObjC selector: @- allowsFullHeightLayout@
allowsFullHeightLayout :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
allowsFullHeightLayout nsSplitViewItem =
  sendMessage nsSplitViewItem allowsFullHeightLayoutSelector

-- | Whether or not a sidebar or inspector is allowed to be full height in the window when the @NSFullSizeContentViewWindowMask@ style mask is also set. Only applies to NSSplitViewItemBehaviorSidebar and NSSplitViewItemBehaviorInspector. Defaults to YES.
--
-- ObjC selector: @- setAllowsFullHeightLayout:@
setAllowsFullHeightLayout :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setAllowsFullHeightLayout nsSplitViewItem value =
  sendMessage nsSplitViewItem setAllowsFullHeightLayoutSelector value

-- | Specifies a preference for the style of separator displayed between the titlebar and the content of the split view item.
--
-- For this value to be applicable, the item's view must be associated with its own titlebar section (see @NSTrackingSeparatorToolbarItem@ for more info). The default value is NSTitlebarSeparatorStyleAutomatic. This value is subject to the containing window's preference and can be overridden.
--
-- ObjC selector: @- titlebarSeparatorStyle@
titlebarSeparatorStyle :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO NSTitlebarSeparatorStyle
titlebarSeparatorStyle nsSplitViewItem =
  sendMessage nsSplitViewItem titlebarSeparatorStyleSelector

-- | Specifies a preference for the style of separator displayed between the titlebar and the content of the split view item.
--
-- For this value to be applicable, the item's view must be associated with its own titlebar section (see @NSTrackingSeparatorToolbarItem@ for more info). The default value is NSTitlebarSeparatorStyleAutomatic. This value is subject to the containing window's preference and can be overridden.
--
-- ObjC selector: @- setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyle :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> NSTitlebarSeparatorStyle -> IO ()
setTitlebarSeparatorStyle nsSplitViewItem value =
  sendMessage nsSplitViewItem setTitlebarSeparatorStyleSelector value

-- | When YES, other items such as sidebars or inspectors may appear overlaid on top of this item's @viewController@ and this item's @safeAreaInsets@ will be adjusted with respect to overlaid content. Defaults to @NO@.
--
-- ObjC selector: @- automaticallyAdjustsSafeAreaInsets@
automaticallyAdjustsSafeAreaInsets :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
automaticallyAdjustsSafeAreaInsets nsSplitViewItem =
  sendMessage nsSplitViewItem automaticallyAdjustsSafeAreaInsetsSelector

-- | When YES, other items such as sidebars or inspectors may appear overlaid on top of this item's @viewController@ and this item's @safeAreaInsets@ will be adjusted with respect to overlaid content. Defaults to @NO@.
--
-- ObjC selector: @- setAutomaticallyAdjustsSafeAreaInsets:@
setAutomaticallyAdjustsSafeAreaInsets :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setAutomaticallyAdjustsSafeAreaInsets nsSplitViewItem value =
  sendMessage nsSplitViewItem setAutomaticallyAdjustsSafeAreaInsetsSelector value

-- | The following methods allow you to add accessory views to the top/bottom of this splitViewItem. See @NSSplitViewItemAccessoryViewController@ for more details.
--
-- ObjC selector: @- topAlignedAccessoryViewControllers@
topAlignedAccessoryViewControllers :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO (Id NSArray)
topAlignedAccessoryViewControllers nsSplitViewItem =
  sendMessage nsSplitViewItem topAlignedAccessoryViewControllersSelector

-- | The following methods allow you to add accessory views to the top/bottom of this splitViewItem. See @NSSplitViewItemAccessoryViewController@ for more details.
--
-- ObjC selector: @- setTopAlignedAccessoryViewControllers:@
setTopAlignedAccessoryViewControllers :: (IsNSSplitViewItem nsSplitViewItem, IsNSArray value) => nsSplitViewItem -> value -> IO ()
setTopAlignedAccessoryViewControllers nsSplitViewItem value =
  sendMessage nsSplitViewItem setTopAlignedAccessoryViewControllersSelector (toNSArray value)

-- | @- bottomAlignedAccessoryViewControllers@
bottomAlignedAccessoryViewControllers :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO (Id NSArray)
bottomAlignedAccessoryViewControllers nsSplitViewItem =
  sendMessage nsSplitViewItem bottomAlignedAccessoryViewControllersSelector

-- | @- setBottomAlignedAccessoryViewControllers:@
setBottomAlignedAccessoryViewControllers :: (IsNSSplitViewItem nsSplitViewItem, IsNSArray value) => nsSplitViewItem -> value -> IO ()
setBottomAlignedAccessoryViewControllers nsSplitViewItem value =
  sendMessage nsSplitViewItem setBottomAlignedAccessoryViewControllersSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @splitViewItemWithViewController:@
splitViewItemWithViewControllerSelector :: Selector '[Id NSViewController] (Id NSSplitViewItem)
splitViewItemWithViewControllerSelector = mkSelector "splitViewItemWithViewController:"

-- | @Selector@ for @sidebarWithViewController:@
sidebarWithViewControllerSelector :: Selector '[Id NSViewController] (Id NSSplitViewItem)
sidebarWithViewControllerSelector = mkSelector "sidebarWithViewController:"

-- | @Selector@ for @contentListWithViewController:@
contentListWithViewControllerSelector :: Selector '[Id NSViewController] (Id NSSplitViewItem)
contentListWithViewControllerSelector = mkSelector "contentListWithViewController:"

-- | @Selector@ for @inspectorWithViewController:@
inspectorWithViewControllerSelector :: Selector '[Id NSViewController] (Id NSSplitViewItem)
inspectorWithViewControllerSelector = mkSelector "inspectorWithViewController:"

-- | @Selector@ for @addTopAlignedAccessoryViewController:@
addTopAlignedAccessoryViewControllerSelector :: Selector '[Id NSSplitViewItemAccessoryViewController] ()
addTopAlignedAccessoryViewControllerSelector = mkSelector "addTopAlignedAccessoryViewController:"

-- | @Selector@ for @insertTopAlignedAccessoryViewController:atIndex:@
insertTopAlignedAccessoryViewController_atIndexSelector :: Selector '[Id NSSplitViewItemAccessoryViewController, CLong] ()
insertTopAlignedAccessoryViewController_atIndexSelector = mkSelector "insertTopAlignedAccessoryViewController:atIndex:"

-- | @Selector@ for @removeTopAlignedAccessoryViewControllerAtIndex:@
removeTopAlignedAccessoryViewControllerAtIndexSelector :: Selector '[CLong] ()
removeTopAlignedAccessoryViewControllerAtIndexSelector = mkSelector "removeTopAlignedAccessoryViewControllerAtIndex:"

-- | @Selector@ for @addBottomAlignedAccessoryViewController:@
addBottomAlignedAccessoryViewControllerSelector :: Selector '[Id NSSplitViewItemAccessoryViewController] ()
addBottomAlignedAccessoryViewControllerSelector = mkSelector "addBottomAlignedAccessoryViewController:"

-- | @Selector@ for @insertBottomAlignedAccessoryViewController:atIndex:@
insertBottomAlignedAccessoryViewController_atIndexSelector :: Selector '[Id NSSplitViewItemAccessoryViewController, CLong] ()
insertBottomAlignedAccessoryViewController_atIndexSelector = mkSelector "insertBottomAlignedAccessoryViewController:atIndex:"

-- | @Selector@ for @removeBottomAlignedAccessoryViewControllerAtIndex:@
removeBottomAlignedAccessoryViewControllerAtIndexSelector :: Selector '[CLong] ()
removeBottomAlignedAccessoryViewControllerAtIndexSelector = mkSelector "removeBottomAlignedAccessoryViewControllerAtIndex:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector '[] NSSplitViewItemBehavior
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector '[] (Id NSViewController)
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @setViewController:@
setViewControllerSelector :: Selector '[Id NSViewController] ()
setViewControllerSelector = mkSelector "setViewController:"

-- | @Selector@ for @collapsed@
collapsedSelector :: Selector '[] Bool
collapsedSelector = mkSelector "collapsed"

-- | @Selector@ for @setCollapsed:@
setCollapsedSelector :: Selector '[Bool] ()
setCollapsedSelector = mkSelector "setCollapsed:"

-- | @Selector@ for @canCollapse@
canCollapseSelector :: Selector '[] Bool
canCollapseSelector = mkSelector "canCollapse"

-- | @Selector@ for @setCanCollapse:@
setCanCollapseSelector :: Selector '[Bool] ()
setCanCollapseSelector = mkSelector "setCanCollapse:"

-- | @Selector@ for @collapseBehavior@
collapseBehaviorSelector :: Selector '[] NSSplitViewItemCollapseBehavior
collapseBehaviorSelector = mkSelector "collapseBehavior"

-- | @Selector@ for @setCollapseBehavior:@
setCollapseBehaviorSelector :: Selector '[NSSplitViewItemCollapseBehavior] ()
setCollapseBehaviorSelector = mkSelector "setCollapseBehavior:"

-- | @Selector@ for @minimumThickness@
minimumThicknessSelector :: Selector '[] CDouble
minimumThicknessSelector = mkSelector "minimumThickness"

-- | @Selector@ for @setMinimumThickness:@
setMinimumThicknessSelector :: Selector '[CDouble] ()
setMinimumThicknessSelector = mkSelector "setMinimumThickness:"

-- | @Selector@ for @maximumThickness@
maximumThicknessSelector :: Selector '[] CDouble
maximumThicknessSelector = mkSelector "maximumThickness"

-- | @Selector@ for @setMaximumThickness:@
setMaximumThicknessSelector :: Selector '[CDouble] ()
setMaximumThicknessSelector = mkSelector "setMaximumThickness:"

-- | @Selector@ for @preferredThicknessFraction@
preferredThicknessFractionSelector :: Selector '[] CDouble
preferredThicknessFractionSelector = mkSelector "preferredThicknessFraction"

-- | @Selector@ for @setPreferredThicknessFraction:@
setPreferredThicknessFractionSelector :: Selector '[CDouble] ()
setPreferredThicknessFractionSelector = mkSelector "setPreferredThicknessFraction:"

-- | @Selector@ for @holdingPriority@
holdingPrioritySelector :: Selector '[] CFloat
holdingPrioritySelector = mkSelector "holdingPriority"

-- | @Selector@ for @setHoldingPriority:@
setHoldingPrioritySelector :: Selector '[CFloat] ()
setHoldingPrioritySelector = mkSelector "setHoldingPriority:"

-- | @Selector@ for @automaticMaximumThickness@
automaticMaximumThicknessSelector :: Selector '[] CDouble
automaticMaximumThicknessSelector = mkSelector "automaticMaximumThickness"

-- | @Selector@ for @setAutomaticMaximumThickness:@
setAutomaticMaximumThicknessSelector :: Selector '[CDouble] ()
setAutomaticMaximumThicknessSelector = mkSelector "setAutomaticMaximumThickness:"

-- | @Selector@ for @springLoaded@
springLoadedSelector :: Selector '[] Bool
springLoadedSelector = mkSelector "springLoaded"

-- | @Selector@ for @setSpringLoaded:@
setSpringLoadedSelector :: Selector '[Bool] ()
setSpringLoadedSelector = mkSelector "setSpringLoaded:"

-- | @Selector@ for @canCollapseFromWindowResize@
canCollapseFromWindowResizeSelector :: Selector '[] Bool
canCollapseFromWindowResizeSelector = mkSelector "canCollapseFromWindowResize"

-- | @Selector@ for @setCanCollapseFromWindowResize:@
setCanCollapseFromWindowResizeSelector :: Selector '[Bool] ()
setCanCollapseFromWindowResizeSelector = mkSelector "setCanCollapseFromWindowResize:"

-- | @Selector@ for @allowsFullHeightLayout@
allowsFullHeightLayoutSelector :: Selector '[] Bool
allowsFullHeightLayoutSelector = mkSelector "allowsFullHeightLayout"

-- | @Selector@ for @setAllowsFullHeightLayout:@
setAllowsFullHeightLayoutSelector :: Selector '[Bool] ()
setAllowsFullHeightLayoutSelector = mkSelector "setAllowsFullHeightLayout:"

-- | @Selector@ for @titlebarSeparatorStyle@
titlebarSeparatorStyleSelector :: Selector '[] NSTitlebarSeparatorStyle
titlebarSeparatorStyleSelector = mkSelector "titlebarSeparatorStyle"

-- | @Selector@ for @setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyleSelector :: Selector '[NSTitlebarSeparatorStyle] ()
setTitlebarSeparatorStyleSelector = mkSelector "setTitlebarSeparatorStyle:"

-- | @Selector@ for @automaticallyAdjustsSafeAreaInsets@
automaticallyAdjustsSafeAreaInsetsSelector :: Selector '[] Bool
automaticallyAdjustsSafeAreaInsetsSelector = mkSelector "automaticallyAdjustsSafeAreaInsets"

-- | @Selector@ for @setAutomaticallyAdjustsSafeAreaInsets:@
setAutomaticallyAdjustsSafeAreaInsetsSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsSafeAreaInsetsSelector = mkSelector "setAutomaticallyAdjustsSafeAreaInsets:"

-- | @Selector@ for @topAlignedAccessoryViewControllers@
topAlignedAccessoryViewControllersSelector :: Selector '[] (Id NSArray)
topAlignedAccessoryViewControllersSelector = mkSelector "topAlignedAccessoryViewControllers"

-- | @Selector@ for @setTopAlignedAccessoryViewControllers:@
setTopAlignedAccessoryViewControllersSelector :: Selector '[Id NSArray] ()
setTopAlignedAccessoryViewControllersSelector = mkSelector "setTopAlignedAccessoryViewControllers:"

-- | @Selector@ for @bottomAlignedAccessoryViewControllers@
bottomAlignedAccessoryViewControllersSelector :: Selector '[] (Id NSArray)
bottomAlignedAccessoryViewControllersSelector = mkSelector "bottomAlignedAccessoryViewControllers"

-- | @Selector@ for @setBottomAlignedAccessoryViewControllers:@
setBottomAlignedAccessoryViewControllersSelector :: Selector '[Id NSArray] ()
setBottomAlignedAccessoryViewControllersSelector = mkSelector "setBottomAlignedAccessoryViewControllers:"

