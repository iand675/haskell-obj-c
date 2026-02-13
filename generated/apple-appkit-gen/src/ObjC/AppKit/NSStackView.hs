{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , arrangedSubviews
  , detachedViews
  , hasEqualSpacing
  , setHasEqualSpacing
  , views
  , addArrangedSubviewSelector
  , addView_inGravitySelector
  , alignmentSelector
  , arrangedSubviewsSelector
  , clippingResistancePriorityForOrientationSelector
  , customSpacingAfterViewSelector
  , delegateSelector
  , detachedViewsSelector
  , detachesHiddenViewsSelector
  , distributionSelector
  , edgeInsetsSelector
  , hasEqualSpacingSelector
  , huggingPriorityForOrientationSelector
  , insertArrangedSubview_atIndexSelector
  , insertView_atIndex_inGravitySelector
  , orientationSelector
  , removeArrangedSubviewSelector
  , removeViewSelector
  , setAlignmentSelector
  , setClippingResistancePriority_forOrientationSelector
  , setCustomSpacing_afterViewSelector
  , setDelegateSelector
  , setDetachesHiddenViewsSelector
  , setDistributionSelector
  , setEdgeInsetsSelector
  , setHasEqualSpacingSelector
  , setHuggingPriority_forOrientationSelector
  , setOrientationSelector
  , setSpacingSelector
  , setViews_inGravitySelector
  , setVisibilityPriority_forViewSelector
  , spacingSelector
  , stackViewWithViewsSelector
  , viewsInGravitySelector
  , viewsSelector
  , visibilityPriorityForViewSelector

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

-- | @+ stackViewWithViews:@
stackViewWithViews :: IsNSArray views => views -> IO (Id NSStackView)
stackViewWithViews views =
  do
    cls' <- getRequiredClass "NSStackView"
    sendClassMessage cls' stackViewWithViewsSelector (toNSArray views)

-- | @- setCustomSpacing:afterView:@
setCustomSpacing_afterView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> CDouble -> view -> IO ()
setCustomSpacing_afterView nsStackView spacing view =
  sendMessage nsStackView setCustomSpacing_afterViewSelector spacing (toNSView view)

-- | @- customSpacingAfterView:@
customSpacingAfterView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO CDouble
customSpacingAfterView nsStackView view =
  sendMessage nsStackView customSpacingAfterViewSelector (toNSView view)

-- | Adds a view to the end of the arrangedSubviews list. If the view is not a subview of the receiver, it will be added as one.
--
-- ObjC selector: @- addArrangedSubview:@
addArrangedSubview :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO ()
addArrangedSubview nsStackView view =
  sendMessage nsStackView addArrangedSubviewSelector (toNSView view)

-- | Adds a view to the arrangedSubviews list at a specific index. If the view is already in the arrangedSubviews list, it will move the view to the specified index (but not change the subview index). If the view is not a subview of the receiver, it will be added as one (not necessarily at the same index).
--
-- ObjC selector: @- insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndex :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> CLong -> IO ()
insertArrangedSubview_atIndex nsStackView view index =
  sendMessage nsStackView insertArrangedSubview_atIndexSelector (toNSView view) index

-- | Removes a subview from the list of arranged subviews without removing it as a subview of the receiver. Removing the view as a subview (either by -[view removeFromSuperview] or setting the receiver's subviews) will automatically remove it as an arranged subview.
--
-- ObjC selector: @- removeArrangedSubview:@
removeArrangedSubview :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO ()
removeArrangedSubview nsStackView view =
  sendMessage nsStackView removeArrangedSubviewSelector (toNSView view)

-- | @- setVisibilityPriority:forView:@
setVisibilityPriority_forView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> CFloat -> view -> IO ()
setVisibilityPriority_forView nsStackView priority view =
  sendMessage nsStackView setVisibilityPriority_forViewSelector priority (toNSView view)

-- | @- visibilityPriorityForView:@
visibilityPriorityForView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO CFloat
visibilityPriorityForView nsStackView view =
  sendMessage nsStackView visibilityPriorityForViewSelector (toNSView view)

-- | @- clippingResistancePriorityForOrientation:@
clippingResistancePriorityForOrientation :: IsNSStackView nsStackView => nsStackView -> NSLayoutConstraintOrientation -> IO CFloat
clippingResistancePriorityForOrientation nsStackView orientation =
  sendMessage nsStackView clippingResistancePriorityForOrientationSelector orientation

-- | @- setClippingResistancePriority:forOrientation:@
setClippingResistancePriority_forOrientation :: IsNSStackView nsStackView => nsStackView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setClippingResistancePriority_forOrientation nsStackView clippingResistancePriority orientation =
  sendMessage nsStackView setClippingResistancePriority_forOrientationSelector clippingResistancePriority orientation

-- | @- huggingPriorityForOrientation:@
huggingPriorityForOrientation :: IsNSStackView nsStackView => nsStackView -> NSLayoutConstraintOrientation -> IO CFloat
huggingPriorityForOrientation nsStackView orientation =
  sendMessage nsStackView huggingPriorityForOrientationSelector orientation

-- | @- setHuggingPriority:forOrientation:@
setHuggingPriority_forOrientation :: IsNSStackView nsStackView => nsStackView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setHuggingPriority_forOrientation nsStackView huggingPriority orientation =
  sendMessage nsStackView setHuggingPriority_forOrientationSelector huggingPriority orientation

-- | @- addView:inGravity:@
addView_inGravity :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> NSStackViewGravity -> IO ()
addView_inGravity nsStackView view gravity =
  sendMessage nsStackView addView_inGravitySelector (toNSView view) gravity

-- | @- insertView:atIndex:inGravity:@
insertView_atIndex_inGravity :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> CULong -> NSStackViewGravity -> IO ()
insertView_atIndex_inGravity nsStackView view index gravity =
  sendMessage nsStackView insertView_atIndex_inGravitySelector (toNSView view) index gravity

-- | @- removeView:@
removeView :: (IsNSStackView nsStackView, IsNSView view) => nsStackView -> view -> IO ()
removeView nsStackView view =
  sendMessage nsStackView removeViewSelector (toNSView view)

-- | @- viewsInGravity:@
viewsInGravity :: IsNSStackView nsStackView => nsStackView -> NSStackViewGravity -> IO (Id NSArray)
viewsInGravity nsStackView gravity =
  sendMessage nsStackView viewsInGravitySelector gravity

-- | @- setViews:inGravity:@
setViews_inGravity :: (IsNSStackView nsStackView, IsNSArray views) => nsStackView -> views -> NSStackViewGravity -> IO ()
setViews_inGravity nsStackView views gravity =
  sendMessage nsStackView setViews_inGravitySelector (toNSArray views) gravity

-- | @- delegate@
delegate :: IsNSStackView nsStackView => nsStackView -> IO RawId
delegate nsStackView =
  sendMessage nsStackView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSStackView nsStackView => nsStackView -> RawId -> IO ()
setDelegate nsStackView value =
  sendMessage nsStackView setDelegateSelector value

-- | Orientation of the StackView, defaults to NSUserInterfaceLayoutOrientationHorizontal
--
-- ObjC selector: @- orientation@
orientation :: IsNSStackView nsStackView => nsStackView -> IO NSUserInterfaceLayoutOrientation
orientation nsStackView =
  sendMessage nsStackView orientationSelector

-- | Orientation of the StackView, defaults to NSUserInterfaceLayoutOrientationHorizontal
--
-- ObjC selector: @- setOrientation:@
setOrientation :: IsNSStackView nsStackView => nsStackView -> NSUserInterfaceLayoutOrientation -> IO ()
setOrientation nsStackView value =
  sendMessage nsStackView setOrientationSelector value

-- | Describes how subviews are aligned within the StackView, defaults to @NSLayoutAttributeCenterY@ for horizontal stacks, @NSLayoutAttributeCenterX@ for vertical stacks. Setting @NSLayoutAttributeNotAnAttribute@ will cause the internal alignment constraints to not be created, and could result in an ambiguous layout. Setting an inapplicable attribute for the set orientation will result in the alignment being ignored (similar to its handling with NSLayoutAttributeNotAnAttribute). The alignment constraints are established at a priority of @NSLayoutPriorityDefaultLow@ and are overridable for individual views using external constraints.
--
-- ObjC selector: @- alignment@
alignment :: IsNSStackView nsStackView => nsStackView -> IO NSLayoutAttribute
alignment nsStackView =
  sendMessage nsStackView alignmentSelector

-- | Describes how subviews are aligned within the StackView, defaults to @NSLayoutAttributeCenterY@ for horizontal stacks, @NSLayoutAttributeCenterX@ for vertical stacks. Setting @NSLayoutAttributeNotAnAttribute@ will cause the internal alignment constraints to not be created, and could result in an ambiguous layout. Setting an inapplicable attribute for the set orientation will result in the alignment being ignored (similar to its handling with NSLayoutAttributeNotAnAttribute). The alignment constraints are established at a priority of @NSLayoutPriorityDefaultLow@ and are overridable for individual views using external constraints.
--
-- ObjC selector: @- setAlignment:@
setAlignment :: IsNSStackView nsStackView => nsStackView -> NSLayoutAttribute -> IO ()
setAlignment nsStackView value =
  sendMessage nsStackView setAlignmentSelector value

-- | Default padding inside the StackView, around all of the subviews.
--
-- ObjC selector: @- edgeInsets@
edgeInsets :: IsNSStackView nsStackView => nsStackView -> IO NSEdgeInsets
edgeInsets nsStackView =
  sendMessage nsStackView edgeInsetsSelector

-- | Default padding inside the StackView, around all of the subviews.
--
-- ObjC selector: @- setEdgeInsets:@
setEdgeInsets :: IsNSStackView nsStackView => nsStackView -> NSEdgeInsets -> IO ()
setEdgeInsets nsStackView value =
  sendMessage nsStackView setEdgeInsetsSelector value

-- | The spacing and sizing distribution of stacked views along the primary axis. Defaults to GravityAreas.
--
-- ObjC selector: @- distribution@
distribution :: IsNSStackView nsStackView => nsStackView -> IO NSStackViewDistribution
distribution nsStackView =
  sendMessage nsStackView distributionSelector

-- | The spacing and sizing distribution of stacked views along the primary axis. Defaults to GravityAreas.
--
-- ObjC selector: @- setDistribution:@
setDistribution :: IsNSStackView nsStackView => nsStackView -> NSStackViewDistribution -> IO ()
setDistribution nsStackView value =
  sendMessage nsStackView setDistributionSelector value

-- | Default (minimum) spacing between each view
--
-- ObjC selector: @- spacing@
spacing :: IsNSStackView nsStackView => nsStackView -> IO CDouble
spacing nsStackView =
  sendMessage nsStackView spacingSelector

-- | Default (minimum) spacing between each view
--
-- ObjC selector: @- setSpacing:@
setSpacing :: IsNSStackView nsStackView => nsStackView -> CDouble -> IO ()
setSpacing nsStackView value =
  sendMessage nsStackView setSpacingSelector value

-- | If YES, when a stacked view's @hidden@ property is set to YES, the view will be detached from the stack and reattached when set to NO. Similarly, if the view has a lowered visibility priority and is detached from the stack view, it will be set as @hidden@ rather than removed from the view hierarchy. Defaults to YES for apps linked on the 10.11 SDK or later.
--
-- ObjC selector: @- detachesHiddenViews@
detachesHiddenViews :: IsNSStackView nsStackView => nsStackView -> IO Bool
detachesHiddenViews nsStackView =
  sendMessage nsStackView detachesHiddenViewsSelector

-- | If YES, when a stacked view's @hidden@ property is set to YES, the view will be detached from the stack and reattached when set to NO. Similarly, if the view has a lowered visibility priority and is detached from the stack view, it will be set as @hidden@ rather than removed from the view hierarchy. Defaults to YES for apps linked on the 10.11 SDK or later.
--
-- ObjC selector: @- setDetachesHiddenViews:@
setDetachesHiddenViews :: IsNSStackView nsStackView => nsStackView -> Bool -> IO ()
setDetachesHiddenViews nsStackView value =
  sendMessage nsStackView setDetachesHiddenViewsSelector value

-- | The list of views that are arranged in a stack by the receiver. They are a subset of @-subviews,@ with potential difference in ordering.
--
-- ObjC selector: @- arrangedSubviews@
arrangedSubviews :: IsNSStackView nsStackView => nsStackView -> IO (Id NSArray)
arrangedSubviews nsStackView =
  sendMessage nsStackView arrangedSubviewsSelector

-- | The arrangedSubviews that are currently detached/hidden.
--
-- ObjC selector: @- detachedViews@
detachedViews :: IsNSStackView nsStackView => nsStackView -> IO (Id NSArray)
detachedViews nsStackView =
  sendMessage nsStackView detachedViewsSelector

-- | @- hasEqualSpacing@
hasEqualSpacing :: IsNSStackView nsStackView => nsStackView -> IO Bool
hasEqualSpacing nsStackView =
  sendMessage nsStackView hasEqualSpacingSelector

-- | @- setHasEqualSpacing:@
setHasEqualSpacing :: IsNSStackView nsStackView => nsStackView -> Bool -> IO ()
setHasEqualSpacing nsStackView value =
  sendMessage nsStackView setHasEqualSpacingSelector value

-- | @- views@
views :: IsNSStackView nsStackView => nsStackView -> IO (Id NSArray)
views nsStackView =
  sendMessage nsStackView viewsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stackViewWithViews:@
stackViewWithViewsSelector :: Selector '[Id NSArray] (Id NSStackView)
stackViewWithViewsSelector = mkSelector "stackViewWithViews:"

-- | @Selector@ for @setCustomSpacing:afterView:@
setCustomSpacing_afterViewSelector :: Selector '[CDouble, Id NSView] ()
setCustomSpacing_afterViewSelector = mkSelector "setCustomSpacing:afterView:"

-- | @Selector@ for @customSpacingAfterView:@
customSpacingAfterViewSelector :: Selector '[Id NSView] CDouble
customSpacingAfterViewSelector = mkSelector "customSpacingAfterView:"

-- | @Selector@ for @addArrangedSubview:@
addArrangedSubviewSelector :: Selector '[Id NSView] ()
addArrangedSubviewSelector = mkSelector "addArrangedSubview:"

-- | @Selector@ for @insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndexSelector :: Selector '[Id NSView, CLong] ()
insertArrangedSubview_atIndexSelector = mkSelector "insertArrangedSubview:atIndex:"

-- | @Selector@ for @removeArrangedSubview:@
removeArrangedSubviewSelector :: Selector '[Id NSView] ()
removeArrangedSubviewSelector = mkSelector "removeArrangedSubview:"

-- | @Selector@ for @setVisibilityPriority:forView:@
setVisibilityPriority_forViewSelector :: Selector '[CFloat, Id NSView] ()
setVisibilityPriority_forViewSelector = mkSelector "setVisibilityPriority:forView:"

-- | @Selector@ for @visibilityPriorityForView:@
visibilityPriorityForViewSelector :: Selector '[Id NSView] CFloat
visibilityPriorityForViewSelector = mkSelector "visibilityPriorityForView:"

-- | @Selector@ for @clippingResistancePriorityForOrientation:@
clippingResistancePriorityForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] CFloat
clippingResistancePriorityForOrientationSelector = mkSelector "clippingResistancePriorityForOrientation:"

-- | @Selector@ for @setClippingResistancePriority:forOrientation:@
setClippingResistancePriority_forOrientationSelector :: Selector '[CFloat, NSLayoutConstraintOrientation] ()
setClippingResistancePriority_forOrientationSelector = mkSelector "setClippingResistancePriority:forOrientation:"

-- | @Selector@ for @huggingPriorityForOrientation:@
huggingPriorityForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] CFloat
huggingPriorityForOrientationSelector = mkSelector "huggingPriorityForOrientation:"

-- | @Selector@ for @setHuggingPriority:forOrientation:@
setHuggingPriority_forOrientationSelector :: Selector '[CFloat, NSLayoutConstraintOrientation] ()
setHuggingPriority_forOrientationSelector = mkSelector "setHuggingPriority:forOrientation:"

-- | @Selector@ for @addView:inGravity:@
addView_inGravitySelector :: Selector '[Id NSView, NSStackViewGravity] ()
addView_inGravitySelector = mkSelector "addView:inGravity:"

-- | @Selector@ for @insertView:atIndex:inGravity:@
insertView_atIndex_inGravitySelector :: Selector '[Id NSView, CULong, NSStackViewGravity] ()
insertView_atIndex_inGravitySelector = mkSelector "insertView:atIndex:inGravity:"

-- | @Selector@ for @removeView:@
removeViewSelector :: Selector '[Id NSView] ()
removeViewSelector = mkSelector "removeView:"

-- | @Selector@ for @viewsInGravity:@
viewsInGravitySelector :: Selector '[NSStackViewGravity] (Id NSArray)
viewsInGravitySelector = mkSelector "viewsInGravity:"

-- | @Selector@ for @setViews:inGravity:@
setViews_inGravitySelector :: Selector '[Id NSArray, NSStackViewGravity] ()
setViews_inGravitySelector = mkSelector "setViews:inGravity:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] NSUserInterfaceLayoutOrientation
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[NSUserInterfaceLayoutOrientation] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSLayoutAttribute
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSLayoutAttribute] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @edgeInsets@
edgeInsetsSelector :: Selector '[] NSEdgeInsets
edgeInsetsSelector = mkSelector "edgeInsets"

-- | @Selector@ for @setEdgeInsets:@
setEdgeInsetsSelector :: Selector '[NSEdgeInsets] ()
setEdgeInsetsSelector = mkSelector "setEdgeInsets:"

-- | @Selector@ for @distribution@
distributionSelector :: Selector '[] NSStackViewDistribution
distributionSelector = mkSelector "distribution"

-- | @Selector@ for @setDistribution:@
setDistributionSelector :: Selector '[NSStackViewDistribution] ()
setDistributionSelector = mkSelector "setDistribution:"

-- | @Selector@ for @spacing@
spacingSelector :: Selector '[] CDouble
spacingSelector = mkSelector "spacing"

-- | @Selector@ for @setSpacing:@
setSpacingSelector :: Selector '[CDouble] ()
setSpacingSelector = mkSelector "setSpacing:"

-- | @Selector@ for @detachesHiddenViews@
detachesHiddenViewsSelector :: Selector '[] Bool
detachesHiddenViewsSelector = mkSelector "detachesHiddenViews"

-- | @Selector@ for @setDetachesHiddenViews:@
setDetachesHiddenViewsSelector :: Selector '[Bool] ()
setDetachesHiddenViewsSelector = mkSelector "setDetachesHiddenViews:"

-- | @Selector@ for @arrangedSubviews@
arrangedSubviewsSelector :: Selector '[] (Id NSArray)
arrangedSubviewsSelector = mkSelector "arrangedSubviews"

-- | @Selector@ for @detachedViews@
detachedViewsSelector :: Selector '[] (Id NSArray)
detachedViewsSelector = mkSelector "detachedViews"

-- | @Selector@ for @hasEqualSpacing@
hasEqualSpacingSelector :: Selector '[] Bool
hasEqualSpacingSelector = mkSelector "hasEqualSpacing"

-- | @Selector@ for @setHasEqualSpacing:@
setHasEqualSpacingSelector :: Selector '[Bool] ()
setHasEqualSpacingSelector = mkSelector "setHasEqualSpacing:"

-- | @Selector@ for @views@
viewsSelector :: Selector '[] (Id NSArray)
viewsSelector = mkSelector "views"

