{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStackView@.
module ObjC.AppKit.NSStackView
  ( NSStackView
  , IsNSStackView(..)
  , stackViewWithViews
  , setCustomSpacing_afterView
  , customSpacingAfterView
  , addArrangedSubview
  , insertArrangedSubview_atIndex
  , removeArrangedSubview
  , setVisibilityPriority_forView
  , visibilityPriorityForView
  , clippingResistancePriorityForOrientation
  , setClippingResistancePriority_forOrientation
  , huggingPriorityForOrientation
  , setHuggingPriority_forOrientation
  , addView_inGravity
  , insertView_atIndex_inGravity
  , removeView
  , viewsInGravity
  , setViews_inGravity
  , orientation
  , setOrientation
  , alignment
  , setAlignment
  , edgeInsets
  , setEdgeInsets
  , distribution
  , setDistribution
  , spacing
  , setSpacing
  , detachesHiddenViews
  , setDetachesHiddenViews
  , detachedViews
  , hasEqualSpacing
  , setHasEqualSpacing
  , views
  , stackViewWithViewsSelector
  , setCustomSpacing_afterViewSelector
  , customSpacingAfterViewSelector
  , addArrangedSubviewSelector
  , insertArrangedSubview_atIndexSelector
  , removeArrangedSubviewSelector
  , setVisibilityPriority_forViewSelector
  , visibilityPriorityForViewSelector
  , clippingResistancePriorityForOrientationSelector
  , setClippingResistancePriority_forOrientationSelector
  , huggingPriorityForOrientationSelector
  , setHuggingPriority_forOrientationSelector
  , addView_inGravitySelector
  , insertView_atIndex_inGravitySelector
  , removeViewSelector
  , viewsInGravitySelector
  , setViews_inGravitySelector
  , orientationSelector
  , setOrientationSelector
  , alignmentSelector
  , setAlignmentSelector
  , edgeInsetsSelector
  , setEdgeInsetsSelector
  , distributionSelector
  , setDistributionSelector
  , spacingSelector
  , setSpacingSelector
  , detachesHiddenViewsSelector
  , setDetachesHiddenViewsSelector
  , detachedViewsSelector
  , hasEqualSpacingSelector
  , setHasEqualSpacingSelector
  , viewsSelector

  -- * Enum types
  , NSLayoutAttribute(NSLayoutAttribute)
  , pattern NSLayoutAttributeLeft
  , pattern NSLayoutAttributeRight
  , pattern NSLayoutAttributeTop
  , pattern NSLayoutAttributeBottom
  , pattern NSLayoutAttributeLeading
  , pattern NSLayoutAttributeTrailing
  , pattern NSLayoutAttributeWidth
  , pattern NSLayoutAttributeHeight
  , pattern NSLayoutAttributeCenterX
  , pattern NSLayoutAttributeCenterY
  , pattern NSLayoutAttributeLastBaseline
  , pattern NSLayoutAttributeBaseline
  , pattern NSLayoutAttributeFirstBaseline
  , pattern NSLayoutAttributeNotAnAttribute
  , NSLayoutConstraintOrientation(NSLayoutConstraintOrientation)
  , pattern NSLayoutConstraintOrientationHorizontal
  , pattern NSLayoutConstraintOrientationVertical
  , NSStackViewDistribution(NSStackViewDistribution)
  , pattern NSStackViewDistributionGravityAreas
  , pattern NSStackViewDistributionFill
  , pattern NSStackViewDistributionFillEqually
  , pattern NSStackViewDistributionFillProportionally
  , pattern NSStackViewDistributionEqualSpacing
  , pattern NSStackViewDistributionEqualCentering
  , NSStackViewGravity(NSStackViewGravity)
  , pattern NSStackViewGravityTop
  , pattern NSStackViewGravityLeading
  , pattern NSStackViewGravityCenter
  , pattern NSStackViewGravityBottom
  , pattern NSStackViewGravityTrailing
  , NSUserInterfaceLayoutOrientation(NSUserInterfaceLayoutOrientation)
  , pattern NSUserInterfaceLayoutOrientationHorizontal
  , pattern NSUserInterfaceLayoutOrientationVertical

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

-- | @+ stackViewWithViews:@
stackViewWithViews :: IsNSArray views => views -> IO (Id NSStackView)
stackViewWithViews views =
  do
    cls' <- getRequiredClass "NSStackView"
    withObjCPtr views $ \raw_views ->
      sendClassMsg cls' (mkSelector "stackViewWithViews:") (retPtr retVoid) [argPtr (castPtr raw_views :: Ptr ())] >>= retainedObject . castPtr

-- | @- setCustomSpacing:afterView:@
setCustomSpacing_afterView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> CDouble -> view -> IO ()
setCustomSpacing_afterView nsStackView  spacing view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "setCustomSpacing:afterView:") retVoid [argCDouble (fromIntegral spacing), argPtr (castPtr raw_view :: Ptr ())]

-- | @- customSpacingAfterView:@
customSpacingAfterView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO CDouble
customSpacingAfterView nsStackView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "customSpacingAfterView:") retCDouble [argPtr (castPtr raw_view :: Ptr ())]

-- | Adds a view to the end of the arrangedSubviews list. If the view is not a subview of the receiver, it will be added as one.
--
-- ObjC selector: @- addArrangedSubview:@
addArrangedSubview :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO ()
addArrangedSubview nsStackView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "addArrangedSubview:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | Adds a view to the arrangedSubviews list at a specific index. If the view is already in the arrangedSubviews list, it will move the view to the specified index (but not change the subview index). If the view is not a subview of the receiver, it will be added as one (not necessarily at the same index).
--
-- ObjC selector: @- insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndex :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> CLong -> IO ()
insertArrangedSubview_atIndex nsStackView  view index =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "insertArrangedSubview:atIndex:") retVoid [argPtr (castPtr raw_view :: Ptr ()), argCLong (fromIntegral index)]

-- | Removes a subview from the list of arranged subviews without removing it as a subview of the receiver. Removing the view as a subview (either by -[view removeFromSuperview] or setting the receiver's subviews) will automatically remove it as an arranged subview.
--
-- ObjC selector: @- removeArrangedSubview:@
removeArrangedSubview :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO ()
removeArrangedSubview nsStackView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "removeArrangedSubview:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- setVisibilityPriority:forView:@
setVisibilityPriority_forView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> CFloat -> view -> IO ()
setVisibilityPriority_forView nsStackView  priority view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "setVisibilityPriority:forView:") retVoid [argCFloat (fromIntegral priority), argPtr (castPtr raw_view :: Ptr ())]

-- | @- visibilityPriorityForView:@
visibilityPriorityForView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO CFloat
visibilityPriorityForView nsStackView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "visibilityPriorityForView:") retCFloat [argPtr (castPtr raw_view :: Ptr ())]

-- | @- clippingResistancePriorityForOrientation:@
clippingResistancePriorityForOrientation :: IsNSStackView nsStackView => nsStackView -> NSLayoutConstraintOrientation -> IO CFloat
clippingResistancePriorityForOrientation nsStackView  orientation =
  sendMsg nsStackView (mkSelector "clippingResistancePriorityForOrientation:") retCFloat [argCLong (coerce orientation)]

-- | @- setClippingResistancePriority:forOrientation:@
setClippingResistancePriority_forOrientation :: IsNSStackView nsStackView => nsStackView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setClippingResistancePriority_forOrientation nsStackView  clippingResistancePriority orientation =
  sendMsg nsStackView (mkSelector "setClippingResistancePriority:forOrientation:") retVoid [argCFloat (fromIntegral clippingResistancePriority), argCLong (coerce orientation)]

-- | @- huggingPriorityForOrientation:@
huggingPriorityForOrientation :: IsNSStackView nsStackView => nsStackView -> NSLayoutConstraintOrientation -> IO CFloat
huggingPriorityForOrientation nsStackView  orientation =
  sendMsg nsStackView (mkSelector "huggingPriorityForOrientation:") retCFloat [argCLong (coerce orientation)]

-- | @- setHuggingPriority:forOrientation:@
setHuggingPriority_forOrientation :: IsNSStackView nsStackView => nsStackView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setHuggingPriority_forOrientation nsStackView  huggingPriority orientation =
  sendMsg nsStackView (mkSelector "setHuggingPriority:forOrientation:") retVoid [argCFloat (fromIntegral huggingPriority), argCLong (coerce orientation)]

-- | @- addView:inGravity:@
addView_inGravity :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> NSStackViewGravity -> IO ()
addView_inGravity nsStackView  view gravity =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "addView:inGravity:") retVoid [argPtr (castPtr raw_view :: Ptr ()), argCLong (coerce gravity)]

-- | @- insertView:atIndex:inGravity:@
insertView_atIndex_inGravity :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> CULong -> NSStackViewGravity -> IO ()
insertView_atIndex_inGravity nsStackView  view index gravity =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "insertView:atIndex:inGravity:") retVoid [argPtr (castPtr raw_view :: Ptr ()), argCULong (fromIntegral index), argCLong (coerce gravity)]

-- | @- removeView:@
removeView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO ()
removeView nsStackView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsStackView (mkSelector "removeView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- viewsInGravity:@
viewsInGravity :: IsNSStackView nsStackView => nsStackView -> NSStackViewGravity -> IO (Id NSArray)
viewsInGravity nsStackView  gravity =
  sendMsg nsStackView (mkSelector "viewsInGravity:") (retPtr retVoid) [argCLong (coerce gravity)] >>= retainedObject . castPtr

-- | @- setViews:inGravity:@
setViews_inGravity :: (IsNSStackView nsStackView, IsNSArray views) => nsStackView -> views -> NSStackViewGravity -> IO ()
setViews_inGravity nsStackView  views gravity =
withObjCPtr views $ \raw_views ->
    sendMsg nsStackView (mkSelector "setViews:inGravity:") retVoid [argPtr (castPtr raw_views :: Ptr ()), argCLong (coerce gravity)]

-- | Orientation of the StackView, defaults to NSUserInterfaceLayoutOrientationHorizontal
--
-- ObjC selector: @- orientation@
orientation :: IsNSStackView nsStackView => nsStackView -> IO NSUserInterfaceLayoutOrientation
orientation nsStackView  =
  fmap (coerce :: CLong -> NSUserInterfaceLayoutOrientation) $ sendMsg nsStackView (mkSelector "orientation") retCLong []

-- | Orientation of the StackView, defaults to NSUserInterfaceLayoutOrientationHorizontal
--
-- ObjC selector: @- setOrientation:@
setOrientation :: IsNSStackView nsStackView => nsStackView -> NSUserInterfaceLayoutOrientation -> IO ()
setOrientation nsStackView  value =
  sendMsg nsStackView (mkSelector "setOrientation:") retVoid [argCLong (coerce value)]

-- | Describes how subviews are aligned within the StackView, defaults to @NSLayoutAttributeCenterY@ for horizontal stacks, @NSLayoutAttributeCenterX@ for vertical stacks. Setting @NSLayoutAttributeNotAnAttribute@ will cause the internal alignment constraints to not be created, and could result in an ambiguous layout. Setting an inapplicable attribute for the set orientation will result in the alignment being ignored (similar to its handling with NSLayoutAttributeNotAnAttribute). The alignment constraints are established at a priority of @NSLayoutPriorityDefaultLow@ and are overridable for individual views using external constraints.
--
-- ObjC selector: @- alignment@
alignment :: IsNSStackView nsStackView => nsStackView -> IO NSLayoutAttribute
alignment nsStackView  =
  fmap (coerce :: CLong -> NSLayoutAttribute) $ sendMsg nsStackView (mkSelector "alignment") retCLong []

-- | Describes how subviews are aligned within the StackView, defaults to @NSLayoutAttributeCenterY@ for horizontal stacks, @NSLayoutAttributeCenterX@ for vertical stacks. Setting @NSLayoutAttributeNotAnAttribute@ will cause the internal alignment constraints to not be created, and could result in an ambiguous layout. Setting an inapplicable attribute for the set orientation will result in the alignment being ignored (similar to its handling with NSLayoutAttributeNotAnAttribute). The alignment constraints are established at a priority of @NSLayoutPriorityDefaultLow@ and are overridable for individual views using external constraints.
--
-- ObjC selector: @- setAlignment:@
setAlignment :: IsNSStackView nsStackView => nsStackView -> NSLayoutAttribute -> IO ()
setAlignment nsStackView  value =
  sendMsg nsStackView (mkSelector "setAlignment:") retVoid [argCLong (coerce value)]

-- | Default padding inside the StackView, around all of the subviews.
--
-- ObjC selector: @- edgeInsets@
edgeInsets :: IsNSStackView nsStackView => nsStackView -> IO NSEdgeInsets
edgeInsets nsStackView  =
  sendMsgStret nsStackView (mkSelector "edgeInsets") retNSEdgeInsets []

-- | Default padding inside the StackView, around all of the subviews.
--
-- ObjC selector: @- setEdgeInsets:@
setEdgeInsets :: IsNSStackView nsStackView => nsStackView -> NSEdgeInsets -> IO ()
setEdgeInsets nsStackView  value =
  sendMsg nsStackView (mkSelector "setEdgeInsets:") retVoid [argNSEdgeInsets value]

-- | The spacing and sizing distribution of stacked views along the primary axis. Defaults to GravityAreas.
--
-- ObjC selector: @- distribution@
distribution :: IsNSStackView nsStackView => nsStackView -> IO NSStackViewDistribution
distribution nsStackView  =
  fmap (coerce :: CLong -> NSStackViewDistribution) $ sendMsg nsStackView (mkSelector "distribution") retCLong []

-- | The spacing and sizing distribution of stacked views along the primary axis. Defaults to GravityAreas.
--
-- ObjC selector: @- setDistribution:@
setDistribution :: IsNSStackView nsStackView => nsStackView -> NSStackViewDistribution -> IO ()
setDistribution nsStackView  value =
  sendMsg nsStackView (mkSelector "setDistribution:") retVoid [argCLong (coerce value)]

-- | Default (minimum) spacing between each view
--
-- ObjC selector: @- spacing@
spacing :: IsNSStackView nsStackView => nsStackView -> IO CDouble
spacing nsStackView  =
  sendMsg nsStackView (mkSelector "spacing") retCDouble []

-- | Default (minimum) spacing between each view
--
-- ObjC selector: @- setSpacing:@
setSpacing :: IsNSStackView nsStackView => nsStackView -> CDouble -> IO ()
setSpacing nsStackView  value =
  sendMsg nsStackView (mkSelector "setSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | If YES, when a stacked view's @hidden@ property is set to YES, the view will be detached from the stack and reattached when set to NO. Similarly, if the view has a lowered visibility priority and is detached from the stack view, it will be set as @hidden@ rather than removed from the view hierarchy. Defaults to YES for apps linked on the 10.11 SDK or later.
--
-- ObjC selector: @- detachesHiddenViews@
detachesHiddenViews :: IsNSStackView nsStackView => nsStackView -> IO Bool
detachesHiddenViews nsStackView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStackView (mkSelector "detachesHiddenViews") retCULong []

-- | If YES, when a stacked view's @hidden@ property is set to YES, the view will be detached from the stack and reattached when set to NO. Similarly, if the view has a lowered visibility priority and is detached from the stack view, it will be set as @hidden@ rather than removed from the view hierarchy. Defaults to YES for apps linked on the 10.11 SDK or later.
--
-- ObjC selector: @- setDetachesHiddenViews:@
setDetachesHiddenViews :: IsNSStackView nsStackView => nsStackView -> Bool -> IO ()
setDetachesHiddenViews nsStackView  value =
  sendMsg nsStackView (mkSelector "setDetachesHiddenViews:") retVoid [argCULong (if value then 1 else 0)]

-- | The arrangedSubviews that are currently detached/hidden.
--
-- ObjC selector: @- detachedViews@
detachedViews :: IsNSStackView nsStackView => nsStackView -> IO (Id NSArray)
detachedViews nsStackView  =
  sendMsg nsStackView (mkSelector "detachedViews") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasEqualSpacing@
hasEqualSpacing :: IsNSStackView nsStackView => nsStackView -> IO Bool
hasEqualSpacing nsStackView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStackView (mkSelector "hasEqualSpacing") retCULong []

-- | @- setHasEqualSpacing:@
setHasEqualSpacing :: IsNSStackView nsStackView => nsStackView -> Bool -> IO ()
setHasEqualSpacing nsStackView  value =
  sendMsg nsStackView (mkSelector "setHasEqualSpacing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- views@
views :: IsNSStackView nsStackView => nsStackView -> IO (Id NSArray)
views nsStackView  =
  sendMsg nsStackView (mkSelector "views") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stackViewWithViews:@
stackViewWithViewsSelector :: Selector
stackViewWithViewsSelector = mkSelector "stackViewWithViews:"

-- | @Selector@ for @setCustomSpacing:afterView:@
setCustomSpacing_afterViewSelector :: Selector
setCustomSpacing_afterViewSelector = mkSelector "setCustomSpacing:afterView:"

-- | @Selector@ for @customSpacingAfterView:@
customSpacingAfterViewSelector :: Selector
customSpacingAfterViewSelector = mkSelector "customSpacingAfterView:"

-- | @Selector@ for @addArrangedSubview:@
addArrangedSubviewSelector :: Selector
addArrangedSubviewSelector = mkSelector "addArrangedSubview:"

-- | @Selector@ for @insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndexSelector :: Selector
insertArrangedSubview_atIndexSelector = mkSelector "insertArrangedSubview:atIndex:"

-- | @Selector@ for @removeArrangedSubview:@
removeArrangedSubviewSelector :: Selector
removeArrangedSubviewSelector = mkSelector "removeArrangedSubview:"

-- | @Selector@ for @setVisibilityPriority:forView:@
setVisibilityPriority_forViewSelector :: Selector
setVisibilityPriority_forViewSelector = mkSelector "setVisibilityPriority:forView:"

-- | @Selector@ for @visibilityPriorityForView:@
visibilityPriorityForViewSelector :: Selector
visibilityPriorityForViewSelector = mkSelector "visibilityPriorityForView:"

-- | @Selector@ for @clippingResistancePriorityForOrientation:@
clippingResistancePriorityForOrientationSelector :: Selector
clippingResistancePriorityForOrientationSelector = mkSelector "clippingResistancePriorityForOrientation:"

-- | @Selector@ for @setClippingResistancePriority:forOrientation:@
setClippingResistancePriority_forOrientationSelector :: Selector
setClippingResistancePriority_forOrientationSelector = mkSelector "setClippingResistancePriority:forOrientation:"

-- | @Selector@ for @huggingPriorityForOrientation:@
huggingPriorityForOrientationSelector :: Selector
huggingPriorityForOrientationSelector = mkSelector "huggingPriorityForOrientation:"

-- | @Selector@ for @setHuggingPriority:forOrientation:@
setHuggingPriority_forOrientationSelector :: Selector
setHuggingPriority_forOrientationSelector = mkSelector "setHuggingPriority:forOrientation:"

-- | @Selector@ for @addView:inGravity:@
addView_inGravitySelector :: Selector
addView_inGravitySelector = mkSelector "addView:inGravity:"

-- | @Selector@ for @insertView:atIndex:inGravity:@
insertView_atIndex_inGravitySelector :: Selector
insertView_atIndex_inGravitySelector = mkSelector "insertView:atIndex:inGravity:"

-- | @Selector@ for @removeView:@
removeViewSelector :: Selector
removeViewSelector = mkSelector "removeView:"

-- | @Selector@ for @viewsInGravity:@
viewsInGravitySelector :: Selector
viewsInGravitySelector = mkSelector "viewsInGravity:"

-- | @Selector@ for @setViews:inGravity:@
setViews_inGravitySelector :: Selector
setViews_inGravitySelector = mkSelector "setViews:inGravity:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @edgeInsets@
edgeInsetsSelector :: Selector
edgeInsetsSelector = mkSelector "edgeInsets"

-- | @Selector@ for @setEdgeInsets:@
setEdgeInsetsSelector :: Selector
setEdgeInsetsSelector = mkSelector "setEdgeInsets:"

-- | @Selector@ for @distribution@
distributionSelector :: Selector
distributionSelector = mkSelector "distribution"

-- | @Selector@ for @setDistribution:@
setDistributionSelector :: Selector
setDistributionSelector = mkSelector "setDistribution:"

-- | @Selector@ for @spacing@
spacingSelector :: Selector
spacingSelector = mkSelector "spacing"

-- | @Selector@ for @setSpacing:@
setSpacingSelector :: Selector
setSpacingSelector = mkSelector "setSpacing:"

-- | @Selector@ for @detachesHiddenViews@
detachesHiddenViewsSelector :: Selector
detachesHiddenViewsSelector = mkSelector "detachesHiddenViews"

-- | @Selector@ for @setDetachesHiddenViews:@
setDetachesHiddenViewsSelector :: Selector
setDetachesHiddenViewsSelector = mkSelector "setDetachesHiddenViews:"

-- | @Selector@ for @detachedViews@
detachedViewsSelector :: Selector
detachedViewsSelector = mkSelector "detachedViews"

-- | @Selector@ for @hasEqualSpacing@
hasEqualSpacingSelector :: Selector
hasEqualSpacingSelector = mkSelector "hasEqualSpacing"

-- | @Selector@ for @setHasEqualSpacing:@
setHasEqualSpacingSelector :: Selector
setHasEqualSpacingSelector = mkSelector "setHasEqualSpacing:"

-- | @Selector@ for @views@
viewsSelector :: Selector
viewsSelector = mkSelector "views"

