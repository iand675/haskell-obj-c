{-# LANGUAGE PatternSynonyms #-}
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
  , splitViewItemWithViewControllerSelector
  , sidebarWithViewControllerSelector
  , contentListWithViewControllerSelector
  , inspectorWithViewControllerSelector
  , addTopAlignedAccessoryViewControllerSelector
  , insertTopAlignedAccessoryViewController_atIndexSelector
  , removeTopAlignedAccessoryViewControllerAtIndexSelector
  , addBottomAlignedAccessoryViewControllerSelector
  , insertBottomAlignedAccessoryViewController_atIndexSelector
  , removeBottomAlignedAccessoryViewControllerAtIndexSelector
  , behaviorSelector
  , viewControllerSelector
  , setViewControllerSelector
  , collapsedSelector
  , setCollapsedSelector
  , canCollapseSelector
  , setCanCollapseSelector
  , collapseBehaviorSelector
  , setCollapseBehaviorSelector
  , minimumThicknessSelector
  , setMinimumThicknessSelector
  , maximumThicknessSelector
  , setMaximumThicknessSelector
  , preferredThicknessFractionSelector
  , setPreferredThicknessFractionSelector
  , holdingPrioritySelector
  , setHoldingPrioritySelector
  , automaticMaximumThicknessSelector
  , setAutomaticMaximumThicknessSelector
  , springLoadedSelector
  , setSpringLoadedSelector
  , canCollapseFromWindowResizeSelector
  , setCanCollapseFromWindowResizeSelector
  , allowsFullHeightLayoutSelector
  , setAllowsFullHeightLayoutSelector
  , titlebarSeparatorStyleSelector
  , setTitlebarSeparatorStyleSelector
  , automaticallyAdjustsSafeAreaInsetsSelector
  , setAutomaticallyAdjustsSafeAreaInsetsSelector

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
    withObjCPtr viewController $ \raw_viewController ->
      sendClassMsg cls' (mkSelector "splitViewItemWithViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr viewController $ \raw_viewController ->
      sendClassMsg cls' (mkSelector "sidebarWithViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr viewController $ \raw_viewController ->
      sendClassMsg cls' (mkSelector "contentListWithViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr viewController $ \raw_viewController ->
      sendClassMsg cls' (mkSelector "inspectorWithViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

-- | @- addTopAlignedAccessoryViewController:@
addTopAlignedAccessoryViewController :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> IO ()
addTopAlignedAccessoryViewController nsSplitViewItem  childViewController =
withObjCPtr childViewController $ \raw_childViewController ->
    sendMsg nsSplitViewItem (mkSelector "addTopAlignedAccessoryViewController:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ())]

-- | @- insertTopAlignedAccessoryViewController:atIndex:@
insertTopAlignedAccessoryViewController_atIndex :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> CLong -> IO ()
insertTopAlignedAccessoryViewController_atIndex nsSplitViewItem  childViewController index =
withObjCPtr childViewController $ \raw_childViewController ->
    sendMsg nsSplitViewItem (mkSelector "insertTopAlignedAccessoryViewController:atIndex:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ()), argCLong (fromIntegral index)]

-- | NOTE: you can use this method, or @-removeFromParentViewController:@, whichever is easier.
--
-- ObjC selector: @- removeTopAlignedAccessoryViewControllerAtIndex:@
removeTopAlignedAccessoryViewControllerAtIndex :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CLong -> IO ()
removeTopAlignedAccessoryViewControllerAtIndex nsSplitViewItem  index =
  sendMsg nsSplitViewItem (mkSelector "removeTopAlignedAccessoryViewControllerAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- addBottomAlignedAccessoryViewController:@
addBottomAlignedAccessoryViewController :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> IO ()
addBottomAlignedAccessoryViewController nsSplitViewItem  childViewController =
withObjCPtr childViewController $ \raw_childViewController ->
    sendMsg nsSplitViewItem (mkSelector "addBottomAlignedAccessoryViewController:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ())]

-- | @- insertBottomAlignedAccessoryViewController:atIndex:@
insertBottomAlignedAccessoryViewController_atIndex :: (IsNSSplitViewItem nsSplitViewItem, IsNSSplitViewItemAccessoryViewController childViewController) => nsSplitViewItem -> childViewController -> CLong -> IO ()
insertBottomAlignedAccessoryViewController_atIndex nsSplitViewItem  childViewController index =
withObjCPtr childViewController $ \raw_childViewController ->
    sendMsg nsSplitViewItem (mkSelector "insertBottomAlignedAccessoryViewController:atIndex:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ()), argCLong (fromIntegral index)]

-- | NOTE: you can use this method, or @-removeFromParentViewController:@, whichever is easier.
--
-- ObjC selector: @- removeBottomAlignedAccessoryViewControllerAtIndex:@
removeBottomAlignedAccessoryViewControllerAtIndex :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CLong -> IO ()
removeBottomAlignedAccessoryViewControllerAtIndex nsSplitViewItem  index =
  sendMsg nsSplitViewItem (mkSelector "removeBottomAlignedAccessoryViewControllerAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | The standard behavior type of the receiver. See initializers for descriptions of each behavior.
--
-- ObjC selector: @- behavior@
behavior :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO NSSplitViewItemBehavior
behavior nsSplitViewItem  =
  fmap (coerce :: CLong -> NSSplitViewItemBehavior) $ sendMsg nsSplitViewItem (mkSelector "behavior") retCLong []

-- | The view controller represented by the SplitViewItem. An exception will be thrown if a new viewController is set while the receiving SplitViewItem is added to a SplitViewController.
--
-- ObjC selector: @- viewController@
viewController :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO (Id NSViewController)
viewController nsSplitViewItem  =
  sendMsg nsSplitViewItem (mkSelector "viewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view controller represented by the SplitViewItem. An exception will be thrown if a new viewController is set while the receiving SplitViewItem is added to a SplitViewController.
--
-- ObjC selector: @- setViewController:@
setViewController :: (IsNSSplitViewItem nsSplitViewItem, IsNSViewController value) => nsSplitViewItem -> value -> IO ()
setViewController nsSplitViewItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSplitViewItem (mkSelector "setViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether or not the child ViewController corresponding to the SplitViewItem is collapsed in the SplitViewController. The default is @NO.@ This can be set with the animator proxy to animate the collapse or uncollapse. The exact animation used can be customized by setting it in the -animations dictionary with a key of "collapsed". If this is set to YES before it is added to the SplitViewController, it will be initially collapsed and the SplitViewController will not cause the view to be loaded until it is uncollapsed. This is KVC/KVO compliant and will be updated if the value changes from user interaction.
--
-- ObjC selector: @- collapsed@
collapsed :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
collapsed nsSplitViewItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItem (mkSelector "collapsed") retCULong []

-- | Whether or not the child ViewController corresponding to the SplitViewItem is collapsed in the SplitViewController. The default is @NO.@ This can be set with the animator proxy to animate the collapse or uncollapse. The exact animation used can be customized by setting it in the -animations dictionary with a key of "collapsed". If this is set to YES before it is added to the SplitViewController, it will be initially collapsed and the SplitViewController will not cause the view to be loaded until it is uncollapsed. This is KVC/KVO compliant and will be updated if the value changes from user interaction.
--
-- ObjC selector: @- setCollapsed:@
setCollapsed :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setCollapsed nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setCollapsed:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether or not the child view controller is collapsible from user interaction - whether by dragging or double clicking a divider. The default is @NO.@
--
-- ObjC selector: @- canCollapse@
canCollapse :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
canCollapse nsSplitViewItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItem (mkSelector "canCollapse") retCULong []

-- | Whether or not the child view controller is collapsible from user interaction - whether by dragging or double clicking a divider. The default is @NO.@
--
-- ObjC selector: @- setCanCollapse:@
setCanCollapse :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setCanCollapse nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setCanCollapse:") retVoid [argCULong (if value then 1 else 0)]

-- | The resize behavior when the receiver toggles its @collapsed@ state programmatically, both animatedly and not. Defaults to @.Default@.
--
-- ObjC selector: @- collapseBehavior@
collapseBehavior :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO NSSplitViewItemCollapseBehavior
collapseBehavior nsSplitViewItem  =
  fmap (coerce :: CLong -> NSSplitViewItemCollapseBehavior) $ sendMsg nsSplitViewItem (mkSelector "collapseBehavior") retCLong []

-- | The resize behavior when the receiver toggles its @collapsed@ state programmatically, both animatedly and not. Defaults to @.Default@.
--
-- ObjC selector: @- setCollapseBehavior:@
setCollapseBehavior :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> NSSplitViewItemCollapseBehavior -> IO ()
setCollapseBehavior nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setCollapseBehavior:") retVoid [argCLong (coerce value)]

-- | A convenience to set the minimum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no minimum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some minimum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- minimumThickness@
minimumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
minimumThickness nsSplitViewItem  =
  sendMsg nsSplitViewItem (mkSelector "minimumThickness") retCDouble []

-- | A convenience to set the minimum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no minimum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some minimum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- setMinimumThickness:@
setMinimumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setMinimumThickness nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setMinimumThickness:") retVoid [argCDouble (fromIntegral value)]

-- | A convenience to set the maximum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no maximum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some maximum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- maximumThickness@
maximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
maximumThickness nsSplitViewItem  =
  sendMsg nsSplitViewItem (mkSelector "maximumThickness") retCDouble []

-- | A convenience to set the maximum thickness of the split view item -- width for "vertical" split views, height otherwise. If NSSplitViewItemUnspecifiedDimension, no maximum size is enforced by the SplitViewItem, although constraints in the contained view hierarchy might have constraints specify some maximum size on their own. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- setMaximumThickness:@
setMaximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setMaximumThickness nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setMaximumThickness:") retVoid [argCDouble (fromIntegral value)]

-- | The percentage of the contained NSSplitView that the NSSplitViewItem prefers to encompass. This is used when double-clicking on a neighbor divider to return to that standard ratio. As well as after entering fullscreen to determine the initial size of the receiver. Defaults to NSSplitViewItemUnspecifiedDimension, which means no resize will occur on double-clicks, and the absolute size is preserved when entering fullscreen.
--
-- ObjC selector: @- preferredThicknessFraction@
preferredThicknessFraction :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
preferredThicknessFraction nsSplitViewItem  =
  sendMsg nsSplitViewItem (mkSelector "preferredThicknessFraction") retCDouble []

-- | The percentage of the contained NSSplitView that the NSSplitViewItem prefers to encompass. This is used when double-clicking on a neighbor divider to return to that standard ratio. As well as after entering fullscreen to determine the initial size of the receiver. Defaults to NSSplitViewItemUnspecifiedDimension, which means no resize will occur on double-clicks, and the absolute size is preserved when entering fullscreen.
--
-- ObjC selector: @- setPreferredThicknessFraction:@
setPreferredThicknessFraction :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setPreferredThicknessFraction nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setPreferredThicknessFraction:") retVoid [argCDouble (fromIntegral value)]

-- | Sets the priority under which a SplitViewItem will hold its width (for a vertical split view) or height (for a horizontal split view). The view with the lowest priority will be the first to take on additional width if the split view grows or shrinks. The default is @NSLayoutPriorityDefaultLow.@
--
-- ObjC selector: @- holdingPriority@
holdingPriority :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CFloat
holdingPriority nsSplitViewItem  =
  sendMsg nsSplitViewItem (mkSelector "holdingPriority") retCFloat []

-- | Sets the priority under which a SplitViewItem will hold its width (for a vertical split view) or height (for a horizontal split view). The view with the lowest priority will be the first to take on additional width if the split view grows or shrinks. The default is @NSLayoutPriorityDefaultLow.@
--
-- ObjC selector: @- setHoldingPriority:@
setHoldingPriority :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CFloat -> IO ()
setHoldingPriority nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setHoldingPriority:") retVoid [argCFloat (fromIntegral value)]

-- | The maximum thickness of the split view item when resizing due to automatic sizing, such as entering fullscreen with a set preferredThicknessFraction or proportional sizing. The user can still resize up to the absolute maximum size by dragging the divider or otherwise. If NSSplitViewItemUnspecifiedDimension, no automatic maximum is enforced. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- automaticMaximumThickness@
automaticMaximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO CDouble
automaticMaximumThickness nsSplitViewItem  =
  sendMsg nsSplitViewItem (mkSelector "automaticMaximumThickness") retCDouble []

-- | The maximum thickness of the split view item when resizing due to automatic sizing, such as entering fullscreen with a set preferredThicknessFraction or proportional sizing. The user can still resize up to the absolute maximum size by dragging the divider or otherwise. If NSSplitViewItemUnspecifiedDimension, no automatic maximum is enforced. Defaults to NSSplitViewItemUnspecifiedDimension.
--
-- ObjC selector: @- setAutomaticMaximumThickness:@
setAutomaticMaximumThickness :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> CDouble -> IO ()
setAutomaticMaximumThickness nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setAutomaticMaximumThickness:") retVoid [argCDouble (fromIntegral value)]

-- | If YES, the split view item can be temporarily uncollapsed during a drag by hovering or deep clicking on its neighboring divider. Defaults to NO.
--
-- ObjC selector: @- springLoaded@
springLoaded :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
springLoaded nsSplitViewItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItem (mkSelector "springLoaded") retCULong []

-- | If YES, the split view item can be temporarily uncollapsed during a drag by hovering or deep clicking on its neighboring divider. Defaults to NO.
--
-- ObjC selector: @- setSpringLoaded:@
setSpringLoaded :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setSpringLoaded nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setSpringLoaded:") retVoid [argCULong (if value then 1 else 0)]

-- | If YES, the item can be collapsed from a window resize. This can differ from @canCollapse@, to allow divider collapsing but not window resize collapsing or vice versa. Defaults to YES for Sidebars and NO for Inspectors. - Note: Setting @canCollapse@ for sidebars will reset this value to that new value.
--
-- ObjC selector: @- canCollapseFromWindowResize@
canCollapseFromWindowResize :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
canCollapseFromWindowResize nsSplitViewItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItem (mkSelector "canCollapseFromWindowResize") retCULong []

-- | If YES, the item can be collapsed from a window resize. This can differ from @canCollapse@, to allow divider collapsing but not window resize collapsing or vice versa. Defaults to YES for Sidebars and NO for Inspectors. - Note: Setting @canCollapse@ for sidebars will reset this value to that new value.
--
-- ObjC selector: @- setCanCollapseFromWindowResize:@
setCanCollapseFromWindowResize :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setCanCollapseFromWindowResize nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setCanCollapseFromWindowResize:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether or not a sidebar or inspector is allowed to be full height in the window when the @NSFullSizeContentViewWindowMask@ style mask is also set. Only applies to NSSplitViewItemBehaviorSidebar and NSSplitViewItemBehaviorInspector. Defaults to YES.
--
-- ObjC selector: @- allowsFullHeightLayout@
allowsFullHeightLayout :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
allowsFullHeightLayout nsSplitViewItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItem (mkSelector "allowsFullHeightLayout") retCULong []

-- | Whether or not a sidebar or inspector is allowed to be full height in the window when the @NSFullSizeContentViewWindowMask@ style mask is also set. Only applies to NSSplitViewItemBehaviorSidebar and NSSplitViewItemBehaviorInspector. Defaults to YES.
--
-- ObjC selector: @- setAllowsFullHeightLayout:@
setAllowsFullHeightLayout :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setAllowsFullHeightLayout nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setAllowsFullHeightLayout:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies a preference for the style of separator displayed between the titlebar and the content of the split view item.
--
-- For this value to be applicable, the item's view must be associated with its own titlebar section (see @NSTrackingSeparatorToolbarItem@ for more info). The default value is NSTitlebarSeparatorStyleAutomatic. This value is subject to the containing window's preference and can be overridden.
--
-- ObjC selector: @- titlebarSeparatorStyle@
titlebarSeparatorStyle :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO NSTitlebarSeparatorStyle
titlebarSeparatorStyle nsSplitViewItem  =
  fmap (coerce :: CLong -> NSTitlebarSeparatorStyle) $ sendMsg nsSplitViewItem (mkSelector "titlebarSeparatorStyle") retCLong []

-- | Specifies a preference for the style of separator displayed between the titlebar and the content of the split view item.
--
-- For this value to be applicable, the item's view must be associated with its own titlebar section (see @NSTrackingSeparatorToolbarItem@ for more info). The default value is NSTitlebarSeparatorStyleAutomatic. This value is subject to the containing window's preference and can be overridden.
--
-- ObjC selector: @- setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyle :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> NSTitlebarSeparatorStyle -> IO ()
setTitlebarSeparatorStyle nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setTitlebarSeparatorStyle:") retVoid [argCLong (coerce value)]

-- | When YES, other items such as sidebars or inspectors may appear overlaid on top of this item's @viewController@ and this item's @safeAreaInsets@ will be adjusted with respect to overlaid content. Defaults to @NO@.
--
-- ObjC selector: @- automaticallyAdjustsSafeAreaInsets@
automaticallyAdjustsSafeAreaInsets :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> IO Bool
automaticallyAdjustsSafeAreaInsets nsSplitViewItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItem (mkSelector "automaticallyAdjustsSafeAreaInsets") retCULong []

-- | When YES, other items such as sidebars or inspectors may appear overlaid on top of this item's @viewController@ and this item's @safeAreaInsets@ will be adjusted with respect to overlaid content. Defaults to @NO@.
--
-- ObjC selector: @- setAutomaticallyAdjustsSafeAreaInsets:@
setAutomaticallyAdjustsSafeAreaInsets :: IsNSSplitViewItem nsSplitViewItem => nsSplitViewItem -> Bool -> IO ()
setAutomaticallyAdjustsSafeAreaInsets nsSplitViewItem  value =
  sendMsg nsSplitViewItem (mkSelector "setAutomaticallyAdjustsSafeAreaInsets:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @splitViewItemWithViewController:@
splitViewItemWithViewControllerSelector :: Selector
splitViewItemWithViewControllerSelector = mkSelector "splitViewItemWithViewController:"

-- | @Selector@ for @sidebarWithViewController:@
sidebarWithViewControllerSelector :: Selector
sidebarWithViewControllerSelector = mkSelector "sidebarWithViewController:"

-- | @Selector@ for @contentListWithViewController:@
contentListWithViewControllerSelector :: Selector
contentListWithViewControllerSelector = mkSelector "contentListWithViewController:"

-- | @Selector@ for @inspectorWithViewController:@
inspectorWithViewControllerSelector :: Selector
inspectorWithViewControllerSelector = mkSelector "inspectorWithViewController:"

-- | @Selector@ for @addTopAlignedAccessoryViewController:@
addTopAlignedAccessoryViewControllerSelector :: Selector
addTopAlignedAccessoryViewControllerSelector = mkSelector "addTopAlignedAccessoryViewController:"

-- | @Selector@ for @insertTopAlignedAccessoryViewController:atIndex:@
insertTopAlignedAccessoryViewController_atIndexSelector :: Selector
insertTopAlignedAccessoryViewController_atIndexSelector = mkSelector "insertTopAlignedAccessoryViewController:atIndex:"

-- | @Selector@ for @removeTopAlignedAccessoryViewControllerAtIndex:@
removeTopAlignedAccessoryViewControllerAtIndexSelector :: Selector
removeTopAlignedAccessoryViewControllerAtIndexSelector = mkSelector "removeTopAlignedAccessoryViewControllerAtIndex:"

-- | @Selector@ for @addBottomAlignedAccessoryViewController:@
addBottomAlignedAccessoryViewControllerSelector :: Selector
addBottomAlignedAccessoryViewControllerSelector = mkSelector "addBottomAlignedAccessoryViewController:"

-- | @Selector@ for @insertBottomAlignedAccessoryViewController:atIndex:@
insertBottomAlignedAccessoryViewController_atIndexSelector :: Selector
insertBottomAlignedAccessoryViewController_atIndexSelector = mkSelector "insertBottomAlignedAccessoryViewController:atIndex:"

-- | @Selector@ for @removeBottomAlignedAccessoryViewControllerAtIndex:@
removeBottomAlignedAccessoryViewControllerAtIndexSelector :: Selector
removeBottomAlignedAccessoryViewControllerAtIndexSelector = mkSelector "removeBottomAlignedAccessoryViewControllerAtIndex:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @setViewController:@
setViewControllerSelector :: Selector
setViewControllerSelector = mkSelector "setViewController:"

-- | @Selector@ for @collapsed@
collapsedSelector :: Selector
collapsedSelector = mkSelector "collapsed"

-- | @Selector@ for @setCollapsed:@
setCollapsedSelector :: Selector
setCollapsedSelector = mkSelector "setCollapsed:"

-- | @Selector@ for @canCollapse@
canCollapseSelector :: Selector
canCollapseSelector = mkSelector "canCollapse"

-- | @Selector@ for @setCanCollapse:@
setCanCollapseSelector :: Selector
setCanCollapseSelector = mkSelector "setCanCollapse:"

-- | @Selector@ for @collapseBehavior@
collapseBehaviorSelector :: Selector
collapseBehaviorSelector = mkSelector "collapseBehavior"

-- | @Selector@ for @setCollapseBehavior:@
setCollapseBehaviorSelector :: Selector
setCollapseBehaviorSelector = mkSelector "setCollapseBehavior:"

-- | @Selector@ for @minimumThickness@
minimumThicknessSelector :: Selector
minimumThicknessSelector = mkSelector "minimumThickness"

-- | @Selector@ for @setMinimumThickness:@
setMinimumThicknessSelector :: Selector
setMinimumThicknessSelector = mkSelector "setMinimumThickness:"

-- | @Selector@ for @maximumThickness@
maximumThicknessSelector :: Selector
maximumThicknessSelector = mkSelector "maximumThickness"

-- | @Selector@ for @setMaximumThickness:@
setMaximumThicknessSelector :: Selector
setMaximumThicknessSelector = mkSelector "setMaximumThickness:"

-- | @Selector@ for @preferredThicknessFraction@
preferredThicknessFractionSelector :: Selector
preferredThicknessFractionSelector = mkSelector "preferredThicknessFraction"

-- | @Selector@ for @setPreferredThicknessFraction:@
setPreferredThicknessFractionSelector :: Selector
setPreferredThicknessFractionSelector = mkSelector "setPreferredThicknessFraction:"

-- | @Selector@ for @holdingPriority@
holdingPrioritySelector :: Selector
holdingPrioritySelector = mkSelector "holdingPriority"

-- | @Selector@ for @setHoldingPriority:@
setHoldingPrioritySelector :: Selector
setHoldingPrioritySelector = mkSelector "setHoldingPriority:"

-- | @Selector@ for @automaticMaximumThickness@
automaticMaximumThicknessSelector :: Selector
automaticMaximumThicknessSelector = mkSelector "automaticMaximumThickness"

-- | @Selector@ for @setAutomaticMaximumThickness:@
setAutomaticMaximumThicknessSelector :: Selector
setAutomaticMaximumThicknessSelector = mkSelector "setAutomaticMaximumThickness:"

-- | @Selector@ for @springLoaded@
springLoadedSelector :: Selector
springLoadedSelector = mkSelector "springLoaded"

-- | @Selector@ for @setSpringLoaded:@
setSpringLoadedSelector :: Selector
setSpringLoadedSelector = mkSelector "setSpringLoaded:"

-- | @Selector@ for @canCollapseFromWindowResize@
canCollapseFromWindowResizeSelector :: Selector
canCollapseFromWindowResizeSelector = mkSelector "canCollapseFromWindowResize"

-- | @Selector@ for @setCanCollapseFromWindowResize:@
setCanCollapseFromWindowResizeSelector :: Selector
setCanCollapseFromWindowResizeSelector = mkSelector "setCanCollapseFromWindowResize:"

-- | @Selector@ for @allowsFullHeightLayout@
allowsFullHeightLayoutSelector :: Selector
allowsFullHeightLayoutSelector = mkSelector "allowsFullHeightLayout"

-- | @Selector@ for @setAllowsFullHeightLayout:@
setAllowsFullHeightLayoutSelector :: Selector
setAllowsFullHeightLayoutSelector = mkSelector "setAllowsFullHeightLayout:"

-- | @Selector@ for @titlebarSeparatorStyle@
titlebarSeparatorStyleSelector :: Selector
titlebarSeparatorStyleSelector = mkSelector "titlebarSeparatorStyle"

-- | @Selector@ for @setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyleSelector :: Selector
setTitlebarSeparatorStyleSelector = mkSelector "setTitlebarSeparatorStyle:"

-- | @Selector@ for @automaticallyAdjustsSafeAreaInsets@
automaticallyAdjustsSafeAreaInsetsSelector :: Selector
automaticallyAdjustsSafeAreaInsetsSelector = mkSelector "automaticallyAdjustsSafeAreaInsets"

-- | @Selector@ for @setAutomaticallyAdjustsSafeAreaInsets:@
setAutomaticallyAdjustsSafeAreaInsetsSelector :: Selector
setAutomaticallyAdjustsSafeAreaInsetsSelector = mkSelector "setAutomaticallyAdjustsSafeAreaInsets:"

