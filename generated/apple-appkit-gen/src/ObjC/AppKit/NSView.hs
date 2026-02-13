{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSView@.
module ObjC.AppKit.NSView
  ( NSView
  , IsNSView(..)
  , initWithFrame
  , initWithCoder
  , isDescendantOf
  , ancestorSharedWithView
  , getRectsBeingDrawn_count
  , needsToDrawRect
  , viewDidHide
  , viewDidUnhide
  , addSubview
  , addSubview_positioned_relativeTo
  , sortSubviewsUsingFunction_context
  , viewWillMoveToWindow
  , viewDidMoveToWindow
  , viewWillMoveToSuperview
  , viewDidMoveToSuperview
  , didAddSubview
  , willRemoveSubview
  , removeFromSuperview
  , replaceSubview_with
  , removeFromSuperviewWithoutNeedingDisplay
  , viewDidChangeBackingProperties
  , resizeSubviewsWithOldSize
  , resizeWithOldSuperviewSize
  , setFrameOrigin
  , setFrameSize
  , setBoundsOrigin
  , setBoundsSize
  , translateOriginToPoint
  , scaleUnitSquareToSize
  , rotateByAngle
  , convertPoint_fromView
  , convertPoint_toView
  , convertSize_fromView
  , convertSize_toView
  , convertRect_fromView
  , convertRect_toView
  , backingAlignedRect_options
  , centerScanRect
  , convertPointToBacking
  , convertPointFromBacking
  , convertSizeToBacking
  , convertSizeFromBacking
  , convertRectToBacking
  , convertRectFromBacking
  , convertPointToLayer
  , convertPointFromLayer
  , convertSizeToLayer
  , convertSizeFromLayer
  , convertRectToLayer
  , convertRectFromLayer
  , setNeedsDisplayInRect
  , lockFocus
  , unlockFocus
  , lockFocusIfCanDraw
  , lockFocusIfCanDrawInContext
  , display
  , displayIfNeeded
  , displayIfNeededIgnoringOpacity
  , displayRect
  , displayIfNeededInRect
  , displayRectIgnoringOpacity
  , displayIfNeededInRectIgnoringOpacity
  , drawRect
  , displayRectIgnoringOpacity_inContext
  , bitmapImageRepForCachingDisplayInRect
  , cacheDisplayInRect_toBitmapImageRep
  , viewWillDraw
  , scrollPoint
  , scrollRectToVisible
  , autoscroll
  , adjustScroll
  , scrollRect_by
  , translateRectsNeedingDisplayInRect_by
  , hitTest
  , mouse_inRect
  , viewWithTag
  , performKeyEquivalent
  , acceptsFirstMouse
  , shouldDelayWindowOrderingForEvent
  , makeBackingLayer
  , updateLayer
  , layoutSubtreeIfNeeded
  , layout
  , menuForEvent
  , willOpenMenu_withEvent
  , didCloseMenu_withEvent
  , addToolTipRect_owner_userData
  , removeToolTip
  , removeAllToolTips
  , viewWillStartLiveResize
  , viewDidEndLiveResize
  , getRectsExposedDuringLiveResize_count
  , rectForSmartMagnificationAtPoint_inRect
  , prepareForReuse
  , prepareContentInRect
  , viewDidChangeEffectiveAppearance
  , rulerView_shouldMoveMarker
  , rulerView_willMoveMarker_toLocation
  , rulerView_didMoveMarker
  , rulerView_shouldRemoveMarker
  , rulerView_didRemoveMarker
  , rulerView_shouldAddMarker
  , rulerView_willAddMarker_atLocation
  , rulerView_didAddMarker
  , rulerView_handleMouseDown
  , rulerView_willSetClientView
  , rulerView_locationForPoint
  , rulerView_pointForLocation
  , layoutGuideForLayoutRegion
  , edgeInsetsForLayoutRegion
  , rectForLayoutRegion
  , addLayoutGuide
  , removeLayoutGuide
  , constraintsAffectingLayoutForOrientation
  , exerciseAmbiguityInLayout
  , alignmentRectForFrame
  , frameForAlignmentRect
  , invalidateIntrinsicContentSize
  , contentHuggingPriorityForOrientation
  , setContentHuggingPriority_forOrientation
  , contentCompressionResistancePriorityForOrientation
  , setContentCompressionResistancePriority_forOrientation
  , updateConstraintsForSubtreeIfNeeded
  , updateConstraints
  , addConstraint
  , addConstraints
  , removeConstraint
  , removeConstraints
  , reflectScrolledClipView
  , scrollClipView_toPoint
  , dragImage_at_offset_event_pasteboard_source_slideBack
  , dragFile_fromRect_slideBack_event
  , dragPromisedFilesOfTypes_fromRect_source_slideBack_event
  , convertPointToBase
  , convertPointFromBase
  , convertSizeToBase
  , convertSizeFromBase
  , convertRectToBase
  , convertRectFromBase
  , performMnemonic
  , shouldDrawColor
  , gState
  , allocateGState
  , releaseGState
  , setUpGState
  , renewGState
  , displayLinkWithTarget_selector
  , addTrackingArea
  , removeTrackingArea
  , updateTrackingAreas
  , addCursorRect_cursor
  , removeCursorRect_cursor
  , discardCursorRects
  , resetCursorRects
  , addTrackingRect_owner_userData_assumeInside
  , removeTrackingRect
  , addGestureRecognizer
  , removeGestureRecognizer
  , showDefinitionForAttributedString_atPoint
  , showDefinitionForAttributedString_range_options_baselineOriginProvider
  , enterFullScreenMode_withOptions
  , exitFullScreenModeWithOptions
  , beginDraggingSessionWithItems_event_source
  , registerForDraggedTypes
  , unregisterDraggedTypes
  , writeEPSInsideRect_toPasteboard
  , dataWithEPSInsideRect
  , writePDFInsideRect_toPasteboard
  , dataWithPDFInsideRect
  , print_
  , knowsPageRange
  , adjustPageWidthNew_left_right_limit
  , adjustPageHeightNew_top_bottom_limit
  , rectForPage
  , locationOfPrintRect
  , drawPageBorderWithSize
  , drawSheetBorderWithSize
  , beginDocument
  , endDocument
  , beginPageInRect_atPlacement
  , endPage
  , setKeyboardFocusRingNeedsDisplayInRect
  , drawFocusRingMask
  , noteFocusRingMaskChanged
  , window
  , superview
  , subviews
  , setSubviews
  , opaqueAncestor
  , hidden
  , setHidden
  , hiddenOrHasHiddenAncestor
  , wantsDefaultClipping
  , postsFrameChangedNotifications
  , setPostsFrameChangedNotifications
  , autoresizesSubviews
  , setAutoresizesSubviews
  , autoresizingMask
  , setAutoresizingMask
  , frame
  , setFrame
  , frameRotation
  , setFrameRotation
  , frameCenterRotation
  , setFrameCenterRotation
  , boundsRotation
  , setBoundsRotation
  , bounds
  , setBounds
  , flipped
  , rotatedFromBase
  , rotatedOrScaledFromBase
  , opaque
  , canDrawConcurrently
  , setCanDrawConcurrently
  , canDraw
  , needsDisplay
  , setNeedsDisplay
  , focusView
  , visibleRect
  , tag
  , needsPanelToBecomeKey
  , mouseDownCanMoveWindow
  , acceptsTouchEvents
  , setAcceptsTouchEvents
  , wantsRestingTouches
  , setWantsRestingTouches
  , layerContentsRedrawPolicy
  , setLayerContentsRedrawPolicy
  , layerContentsPlacement
  , setLayerContentsPlacement
  , wantsLayer
  , setWantsLayer
  , layer
  , setLayer
  , wantsUpdateLayer
  , canDrawSubviewsIntoLayer
  , setCanDrawSubviewsIntoLayer
  , needsLayout
  , setNeedsLayout
  , alphaValue
  , setAlphaValue
  , layerUsesCoreImageFilters
  , setLayerUsesCoreImageFilters
  , backgroundFilters
  , setBackgroundFilters
  , compositingFilter
  , setCompositingFilter
  , contentFilters
  , setContentFilters
  , shadow
  , setShadow
  , clipsToBounds
  , setClipsToBounds
  , postsBoundsChangedNotifications
  , setPostsBoundsChangedNotifications
  , enclosingScrollView
  , defaultMenu
  , toolTip
  , setToolTip
  , inLiveResize
  , preservesContentDuringLiveResize
  , rectPreservedDuringLiveResize
  , inputContext
  , userInterfaceLayoutDirection
  , setUserInterfaceLayoutDirection
  , compatibleWithResponsiveScrolling
  , preparedContentRect
  , setPreparedContentRect
  , allowsVibrancy
  , pressureConfiguration
  , setPressureConfiguration
  , wantsExtendedDynamicRangeOpenGLSurface
  , setWantsExtendedDynamicRangeOpenGLSurface
  , wantsBestResolutionOpenGLSurface
  , setWantsBestResolutionOpenGLSurface
  , layoutGuides
  , hasAmbiguousLayout
  , fittingSize
  , alignmentRectInsets
  , firstBaselineOffsetFromTop
  , lastBaselineOffsetFromBottom
  , baselineOffsetFromBottom
  , intrinsicContentSize
  , horizontalContentSizeConstraintActive
  , setHorizontalContentSizeConstraintActive
  , verticalContentSizeConstraintActive
  , setVerticalContentSizeConstraintActive
  , translatesAutoresizingMaskIntoConstraints
  , setTranslatesAutoresizingMaskIntoConstraints
  , requiresConstraintBasedLayout
  , needsUpdateConstraints
  , setNeedsUpdateConstraints
  , leadingAnchor
  , trailingAnchor
  , leftAnchor
  , rightAnchor
  , topAnchor
  , bottomAnchor
  , widthAnchor
  , heightAnchor
  , centerXAnchor
  , centerYAnchor
  , firstBaselineAnchor
  , lastBaselineAnchor
  , constraints
  , candidateListTouchBarItem
  , enclosingMenuItem
  , writingToolsCoordinator
  , setWritingToolsCoordinator
  , trackingAreas
  , prefersCompactControlSizeMetrics
  , setPrefersCompactControlSizeMetrics
  , safeAreaInsets
  , additionalSafeAreaInsets
  , setAdditionalSafeAreaInsets
  , safeAreaLayoutGuide
  , safeAreaRect
  , layoutMarginsGuide
  , allowedTouchTypes
  , setAllowedTouchTypes
  , gestureRecognizers
  , setGestureRecognizers
  , drawingFindIndicator
  , inFullScreenMode
  , registeredDraggedTypes
  , heightAdjustLimit
  , widthAdjustLimit
  , pageHeader
  , pageFooter
  , printJobTitle
  , nextKeyView
  , setNextKeyView
  , previousKeyView
  , nextValidKeyView
  , previousValidKeyView
  , canBecomeKeyView
  , focusRingType
  , setFocusRingType
  , defaultFocusRingType
  , focusRingMaskBounds
  , acceptsFirstMouseSelector
  , acceptsTouchEventsSelector
  , addConstraintSelector
  , addConstraintsSelector
  , addCursorRect_cursorSelector
  , addGestureRecognizerSelector
  , addLayoutGuideSelector
  , addSubviewSelector
  , addSubview_positioned_relativeToSelector
  , addToolTipRect_owner_userDataSelector
  , addTrackingAreaSelector
  , addTrackingRect_owner_userData_assumeInsideSelector
  , additionalSafeAreaInsetsSelector
  , adjustPageHeightNew_top_bottom_limitSelector
  , adjustPageWidthNew_left_right_limitSelector
  , adjustScrollSelector
  , alignmentRectForFrameSelector
  , alignmentRectInsetsSelector
  , allocateGStateSelector
  , allowedTouchTypesSelector
  , allowsVibrancySelector
  , alphaValueSelector
  , ancestorSharedWithViewSelector
  , autoresizesSubviewsSelector
  , autoresizingMaskSelector
  , autoscrollSelector
  , backgroundFiltersSelector
  , backingAlignedRect_optionsSelector
  , baselineOffsetFromBottomSelector
  , beginDocumentSelector
  , beginDraggingSessionWithItems_event_sourceSelector
  , beginPageInRect_atPlacementSelector
  , bitmapImageRepForCachingDisplayInRectSelector
  , bottomAnchorSelector
  , boundsRotationSelector
  , boundsSelector
  , cacheDisplayInRect_toBitmapImageRepSelector
  , canBecomeKeyViewSelector
  , canDrawConcurrentlySelector
  , canDrawSelector
  , canDrawSubviewsIntoLayerSelector
  , candidateListTouchBarItemSelector
  , centerScanRectSelector
  , centerXAnchorSelector
  , centerYAnchorSelector
  , clipsToBoundsSelector
  , compatibleWithResponsiveScrollingSelector
  , compositingFilterSelector
  , constraintsAffectingLayoutForOrientationSelector
  , constraintsSelector
  , contentCompressionResistancePriorityForOrientationSelector
  , contentFiltersSelector
  , contentHuggingPriorityForOrientationSelector
  , convertPointFromBackingSelector
  , convertPointFromBaseSelector
  , convertPointFromLayerSelector
  , convertPointToBackingSelector
  , convertPointToBaseSelector
  , convertPointToLayerSelector
  , convertPoint_fromViewSelector
  , convertPoint_toViewSelector
  , convertRectFromBackingSelector
  , convertRectFromBaseSelector
  , convertRectFromLayerSelector
  , convertRectToBackingSelector
  , convertRectToBaseSelector
  , convertRectToLayerSelector
  , convertRect_fromViewSelector
  , convertRect_toViewSelector
  , convertSizeFromBackingSelector
  , convertSizeFromBaseSelector
  , convertSizeFromLayerSelector
  , convertSizeToBackingSelector
  , convertSizeToBaseSelector
  , convertSizeToLayerSelector
  , convertSize_fromViewSelector
  , convertSize_toViewSelector
  , dataWithEPSInsideRectSelector
  , dataWithPDFInsideRectSelector
  , defaultFocusRingTypeSelector
  , defaultMenuSelector
  , didAddSubviewSelector
  , didCloseMenu_withEventSelector
  , discardCursorRectsSelector
  , displayIfNeededIgnoringOpacitySelector
  , displayIfNeededInRectIgnoringOpacitySelector
  , displayIfNeededInRectSelector
  , displayIfNeededSelector
  , displayLinkWithTarget_selectorSelector
  , displayRectIgnoringOpacitySelector
  , displayRectIgnoringOpacity_inContextSelector
  , displayRectSelector
  , displaySelector
  , dragFile_fromRect_slideBack_eventSelector
  , dragImage_at_offset_event_pasteboard_source_slideBackSelector
  , dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector
  , drawFocusRingMaskSelector
  , drawPageBorderWithSizeSelector
  , drawRectSelector
  , drawSheetBorderWithSizeSelector
  , drawingFindIndicatorSelector
  , edgeInsetsForLayoutRegionSelector
  , enclosingMenuItemSelector
  , enclosingScrollViewSelector
  , endDocumentSelector
  , endPageSelector
  , enterFullScreenMode_withOptionsSelector
  , exerciseAmbiguityInLayoutSelector
  , exitFullScreenModeWithOptionsSelector
  , firstBaselineAnchorSelector
  , firstBaselineOffsetFromTopSelector
  , fittingSizeSelector
  , flippedSelector
  , focusRingMaskBoundsSelector
  , focusRingTypeSelector
  , focusViewSelector
  , frameCenterRotationSelector
  , frameForAlignmentRectSelector
  , frameRotationSelector
  , frameSelector
  , gStateSelector
  , gestureRecognizersSelector
  , getRectsBeingDrawn_countSelector
  , getRectsExposedDuringLiveResize_countSelector
  , hasAmbiguousLayoutSelector
  , heightAdjustLimitSelector
  , heightAnchorSelector
  , hiddenOrHasHiddenAncestorSelector
  , hiddenSelector
  , hitTestSelector
  , horizontalContentSizeConstraintActiveSelector
  , inFullScreenModeSelector
  , inLiveResizeSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , inputContextSelector
  , intrinsicContentSizeSelector
  , invalidateIntrinsicContentSizeSelector
  , isDescendantOfSelector
  , knowsPageRangeSelector
  , lastBaselineAnchorSelector
  , lastBaselineOffsetFromBottomSelector
  , layerContentsPlacementSelector
  , layerContentsRedrawPolicySelector
  , layerSelector
  , layerUsesCoreImageFiltersSelector
  , layoutGuideForLayoutRegionSelector
  , layoutGuidesSelector
  , layoutMarginsGuideSelector
  , layoutSelector
  , layoutSubtreeIfNeededSelector
  , leadingAnchorSelector
  , leftAnchorSelector
  , locationOfPrintRectSelector
  , lockFocusIfCanDrawInContextSelector
  , lockFocusIfCanDrawSelector
  , lockFocusSelector
  , makeBackingLayerSelector
  , menuForEventSelector
  , mouseDownCanMoveWindowSelector
  , mouse_inRectSelector
  , needsDisplaySelector
  , needsLayoutSelector
  , needsPanelToBecomeKeySelector
  , needsToDrawRectSelector
  , needsUpdateConstraintsSelector
  , nextKeyViewSelector
  , nextValidKeyViewSelector
  , noteFocusRingMaskChangedSelector
  , opaqueAncestorSelector
  , opaqueSelector
  , pageFooterSelector
  , pageHeaderSelector
  , performKeyEquivalentSelector
  , performMnemonicSelector
  , postsBoundsChangedNotificationsSelector
  , postsFrameChangedNotificationsSelector
  , prefersCompactControlSizeMetricsSelector
  , prepareContentInRectSelector
  , prepareForReuseSelector
  , preparedContentRectSelector
  , preservesContentDuringLiveResizeSelector
  , pressureConfigurationSelector
  , previousKeyViewSelector
  , previousValidKeyViewSelector
  , printJobTitleSelector
  , printSelector
  , rectForLayoutRegionSelector
  , rectForPageSelector
  , rectForSmartMagnificationAtPoint_inRectSelector
  , rectPreservedDuringLiveResizeSelector
  , reflectScrolledClipViewSelector
  , registerForDraggedTypesSelector
  , registeredDraggedTypesSelector
  , releaseGStateSelector
  , removeAllToolTipsSelector
  , removeConstraintSelector
  , removeConstraintsSelector
  , removeCursorRect_cursorSelector
  , removeFromSuperviewSelector
  , removeFromSuperviewWithoutNeedingDisplaySelector
  , removeGestureRecognizerSelector
  , removeLayoutGuideSelector
  , removeToolTipSelector
  , removeTrackingAreaSelector
  , removeTrackingRectSelector
  , renewGStateSelector
  , replaceSubview_withSelector
  , requiresConstraintBasedLayoutSelector
  , resetCursorRectsSelector
  , resizeSubviewsWithOldSizeSelector
  , resizeWithOldSuperviewSizeSelector
  , rightAnchorSelector
  , rotateByAngleSelector
  , rotatedFromBaseSelector
  , rotatedOrScaledFromBaseSelector
  , rulerView_didAddMarkerSelector
  , rulerView_didMoveMarkerSelector
  , rulerView_didRemoveMarkerSelector
  , rulerView_handleMouseDownSelector
  , rulerView_locationForPointSelector
  , rulerView_pointForLocationSelector
  , rulerView_shouldAddMarkerSelector
  , rulerView_shouldMoveMarkerSelector
  , rulerView_shouldRemoveMarkerSelector
  , rulerView_willAddMarker_atLocationSelector
  , rulerView_willMoveMarker_toLocationSelector
  , rulerView_willSetClientViewSelector
  , safeAreaInsetsSelector
  , safeAreaLayoutGuideSelector
  , safeAreaRectSelector
  , scaleUnitSquareToSizeSelector
  , scrollClipView_toPointSelector
  , scrollPointSelector
  , scrollRectToVisibleSelector
  , scrollRect_bySelector
  , setAcceptsTouchEventsSelector
  , setAdditionalSafeAreaInsetsSelector
  , setAllowedTouchTypesSelector
  , setAlphaValueSelector
  , setAutoresizesSubviewsSelector
  , setAutoresizingMaskSelector
  , setBackgroundFiltersSelector
  , setBoundsOriginSelector
  , setBoundsRotationSelector
  , setBoundsSelector
  , setBoundsSizeSelector
  , setCanDrawConcurrentlySelector
  , setCanDrawSubviewsIntoLayerSelector
  , setClipsToBoundsSelector
  , setCompositingFilterSelector
  , setContentCompressionResistancePriority_forOrientationSelector
  , setContentFiltersSelector
  , setContentHuggingPriority_forOrientationSelector
  , setFocusRingTypeSelector
  , setFrameCenterRotationSelector
  , setFrameOriginSelector
  , setFrameRotationSelector
  , setFrameSelector
  , setFrameSizeSelector
  , setGestureRecognizersSelector
  , setHiddenSelector
  , setHorizontalContentSizeConstraintActiveSelector
  , setKeyboardFocusRingNeedsDisplayInRectSelector
  , setLayerContentsPlacementSelector
  , setLayerContentsRedrawPolicySelector
  , setLayerSelector
  , setLayerUsesCoreImageFiltersSelector
  , setNeedsDisplayInRectSelector
  , setNeedsDisplaySelector
  , setNeedsLayoutSelector
  , setNeedsUpdateConstraintsSelector
  , setNextKeyViewSelector
  , setPostsBoundsChangedNotificationsSelector
  , setPostsFrameChangedNotificationsSelector
  , setPrefersCompactControlSizeMetricsSelector
  , setPreparedContentRectSelector
  , setPressureConfigurationSelector
  , setShadowSelector
  , setSubviewsSelector
  , setToolTipSelector
  , setTranslatesAutoresizingMaskIntoConstraintsSelector
  , setUpGStateSelector
  , setUserInterfaceLayoutDirectionSelector
  , setVerticalContentSizeConstraintActiveSelector
  , setWantsBestResolutionOpenGLSurfaceSelector
  , setWantsExtendedDynamicRangeOpenGLSurfaceSelector
  , setWantsLayerSelector
  , setWantsRestingTouchesSelector
  , setWritingToolsCoordinatorSelector
  , shadowSelector
  , shouldDelayWindowOrderingForEventSelector
  , shouldDrawColorSelector
  , showDefinitionForAttributedString_atPointSelector
  , showDefinitionForAttributedString_range_options_baselineOriginProviderSelector
  , sortSubviewsUsingFunction_contextSelector
  , subviewsSelector
  , superviewSelector
  , tagSelector
  , toolTipSelector
  , topAnchorSelector
  , trackingAreasSelector
  , trailingAnchorSelector
  , translateOriginToPointSelector
  , translateRectsNeedingDisplayInRect_bySelector
  , translatesAutoresizingMaskIntoConstraintsSelector
  , unlockFocusSelector
  , unregisterDraggedTypesSelector
  , updateConstraintsForSubtreeIfNeededSelector
  , updateConstraintsSelector
  , updateLayerSelector
  , updateTrackingAreasSelector
  , userInterfaceLayoutDirectionSelector
  , verticalContentSizeConstraintActiveSelector
  , viewDidChangeBackingPropertiesSelector
  , viewDidChangeEffectiveAppearanceSelector
  , viewDidEndLiveResizeSelector
  , viewDidHideSelector
  , viewDidMoveToSuperviewSelector
  , viewDidMoveToWindowSelector
  , viewDidUnhideSelector
  , viewWillDrawSelector
  , viewWillMoveToSuperviewSelector
  , viewWillMoveToWindowSelector
  , viewWillStartLiveResizeSelector
  , viewWithTagSelector
  , visibleRectSelector
  , wantsBestResolutionOpenGLSurfaceSelector
  , wantsDefaultClippingSelector
  , wantsExtendedDynamicRangeOpenGLSurfaceSelector
  , wantsLayerSelector
  , wantsRestingTouchesSelector
  , wantsUpdateLayerSelector
  , widthAdjustLimitSelector
  , widthAnchorSelector
  , willOpenMenu_withEventSelector
  , willRemoveSubviewSelector
  , windowSelector
  , writeEPSInsideRect_toPasteboardSelector
  , writePDFInsideRect_toPasteboardSelector
  , writingToolsCoordinatorSelector

  -- * Enum types
  , NSAlignmentOptions(NSAlignmentOptions)
  , pattern NSAlignMinXInward
  , pattern NSAlignMinYInward
  , pattern NSAlignMaxXInward
  , pattern NSAlignMaxYInward
  , pattern NSAlignWidthInward
  , pattern NSAlignHeightInward
  , pattern NSAlignMinXOutward
  , pattern NSAlignMinYOutward
  , pattern NSAlignMaxXOutward
  , pattern NSAlignMaxYOutward
  , pattern NSAlignWidthOutward
  , pattern NSAlignHeightOutward
  , pattern NSAlignMinXNearest
  , pattern NSAlignMinYNearest
  , pattern NSAlignMaxXNearest
  , pattern NSAlignMaxYNearest
  , pattern NSAlignWidthNearest
  , pattern NSAlignHeightNearest
  , pattern NSAlignRectFlipped
  , pattern NSAlignAllEdgesInward
  , pattern NSAlignAllEdgesOutward
  , pattern NSAlignAllEdgesNearest
  , NSAutoresizingMaskOptions(NSAutoresizingMaskOptions)
  , pattern NSViewNotSizable
  , pattern NSViewMinXMargin
  , pattern NSViewWidthSizable
  , pattern NSViewMaxXMargin
  , pattern NSViewMinYMargin
  , pattern NSViewHeightSizable
  , pattern NSViewMaxYMargin
  , NSFocusRingType(NSFocusRingType)
  , pattern NSFocusRingTypeDefault
  , pattern NSFocusRingTypeNone
  , pattern NSFocusRingTypeExterior
  , NSLayoutConstraintOrientation(NSLayoutConstraintOrientation)
  , pattern NSLayoutConstraintOrientationHorizontal
  , pattern NSLayoutConstraintOrientationVertical
  , NSTouchTypeMask(NSTouchTypeMask)
  , pattern NSTouchTypeMaskDirect
  , pattern NSTouchTypeMaskIndirect
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft
  , NSViewLayerContentsPlacement(NSViewLayerContentsPlacement)
  , pattern NSViewLayerContentsPlacementScaleAxesIndependently
  , pattern NSViewLayerContentsPlacementScaleProportionallyToFit
  , pattern NSViewLayerContentsPlacementScaleProportionallyToFill
  , pattern NSViewLayerContentsPlacementCenter
  , pattern NSViewLayerContentsPlacementTop
  , pattern NSViewLayerContentsPlacementTopRight
  , pattern NSViewLayerContentsPlacementRight
  , pattern NSViewLayerContentsPlacementBottomRight
  , pattern NSViewLayerContentsPlacementBottom
  , pattern NSViewLayerContentsPlacementBottomLeft
  , pattern NSViewLayerContentsPlacementLeft
  , pattern NSViewLayerContentsPlacementTopLeft
  , NSViewLayerContentsRedrawPolicy(NSViewLayerContentsRedrawPolicy)
  , pattern NSViewLayerContentsRedrawNever
  , pattern NSViewLayerContentsRedrawOnSetNeedsDisplay
  , pattern NSViewLayerContentsRedrawDuringViewResize
  , pattern NSViewLayerContentsRedrawBeforeViewResize
  , pattern NSViewLayerContentsRedrawCrossfade
  , NSWindowOrderingMode(NSWindowOrderingMode)
  , pattern NSWindowAbove
  , pattern NSWindowBelow
  , pattern NSWindowOut

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSView nsView => nsView -> NSRect -> IO (Id NSView)
initWithFrame nsView frameRect =
  sendOwnedMessage nsView initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSView nsView, IsNSCoder coder) => nsView -> coder -> IO (Id NSView)
initWithCoder nsView coder =
  sendOwnedMessage nsView initWithCoderSelector (toNSCoder coder)

-- | @- isDescendantOf:@
isDescendantOf :: (IsNSView nsView, IsNSView view) => nsView -> view -> IO Bool
isDescendantOf nsView view =
  sendMessage nsView isDescendantOfSelector (toNSView view)

-- | @- ancestorSharedWithView:@
ancestorSharedWithView :: (IsNSView nsView, IsNSView view) => nsView -> view -> IO (Id NSView)
ancestorSharedWithView nsView view =
  sendMessage nsView ancestorSharedWithViewSelector (toNSView view)

-- | @- getRectsBeingDrawn:count:@
getRectsBeingDrawn_count :: IsNSView nsView => nsView -> Const (Ptr NSRect) -> Ptr CLong -> IO ()
getRectsBeingDrawn_count nsView rects count =
  sendMessage nsView getRectsBeingDrawn_countSelector rects count

-- | @- needsToDrawRect:@
needsToDrawRect :: IsNSView nsView => nsView -> NSRect -> IO Bool
needsToDrawRect nsView rect =
  sendMessage nsView needsToDrawRectSelector rect

-- | @- viewDidHide@
viewDidHide :: IsNSView nsView => nsView -> IO ()
viewDidHide nsView =
  sendMessage nsView viewDidHideSelector

-- | @- viewDidUnhide@
viewDidUnhide :: IsNSView nsView => nsView -> IO ()
viewDidUnhide nsView =
  sendMessage nsView viewDidUnhideSelector

-- | @- addSubview:@
addSubview :: (IsNSView nsView, IsNSView view) => nsView -> view -> IO ()
addSubview nsView view =
  sendMessage nsView addSubviewSelector (toNSView view)

-- | @- addSubview:positioned:relativeTo:@
addSubview_positioned_relativeTo :: (IsNSView nsView, IsNSView view, IsNSView otherView) => nsView -> view -> NSWindowOrderingMode -> otherView -> IO ()
addSubview_positioned_relativeTo nsView view place otherView =
  sendMessage nsView addSubview_positioned_relativeToSelector (toNSView view) place (toNSView otherView)

-- | @- sortSubviewsUsingFunction:context:@
sortSubviewsUsingFunction_context :: IsNSView nsView => nsView -> Ptr () -> Ptr () -> IO ()
sortSubviewsUsingFunction_context nsView compare_ context =
  sendMessage nsView sortSubviewsUsingFunction_contextSelector compare_ context

-- | @- viewWillMoveToWindow:@
viewWillMoveToWindow :: (IsNSView nsView, IsNSWindow newWindow) => nsView -> newWindow -> IO ()
viewWillMoveToWindow nsView newWindow =
  sendMessage nsView viewWillMoveToWindowSelector (toNSWindow newWindow)

-- | @- viewDidMoveToWindow@
viewDidMoveToWindow :: IsNSView nsView => nsView -> IO ()
viewDidMoveToWindow nsView =
  sendMessage nsView viewDidMoveToWindowSelector

-- | @- viewWillMoveToSuperview:@
viewWillMoveToSuperview :: (IsNSView nsView, IsNSView newSuperview) => nsView -> newSuperview -> IO ()
viewWillMoveToSuperview nsView newSuperview =
  sendMessage nsView viewWillMoveToSuperviewSelector (toNSView newSuperview)

-- | @- viewDidMoveToSuperview@
viewDidMoveToSuperview :: IsNSView nsView => nsView -> IO ()
viewDidMoveToSuperview nsView =
  sendMessage nsView viewDidMoveToSuperviewSelector

-- | @- didAddSubview:@
didAddSubview :: (IsNSView nsView, IsNSView subview) => nsView -> subview -> IO ()
didAddSubview nsView subview =
  sendMessage nsView didAddSubviewSelector (toNSView subview)

-- | @- willRemoveSubview:@
willRemoveSubview :: (IsNSView nsView, IsNSView subview) => nsView -> subview -> IO ()
willRemoveSubview nsView subview =
  sendMessage nsView willRemoveSubviewSelector (toNSView subview)

-- | @- removeFromSuperview@
removeFromSuperview :: IsNSView nsView => nsView -> IO ()
removeFromSuperview nsView =
  sendMessage nsView removeFromSuperviewSelector

-- | @- replaceSubview:with:@
replaceSubview_with :: (IsNSView nsView, IsNSView oldView, IsNSView newView) => nsView -> oldView -> newView -> IO ()
replaceSubview_with nsView oldView newView =
  sendMessage nsView replaceSubview_withSelector (toNSView oldView) (toNSView newView)

-- | @- removeFromSuperviewWithoutNeedingDisplay@
removeFromSuperviewWithoutNeedingDisplay :: IsNSView nsView => nsView -> IO ()
removeFromSuperviewWithoutNeedingDisplay nsView =
  sendMessage nsView removeFromSuperviewWithoutNeedingDisplaySelector

-- | @- viewDidChangeBackingProperties@
viewDidChangeBackingProperties :: IsNSView nsView => nsView -> IO ()
viewDidChangeBackingProperties nsView =
  sendMessage nsView viewDidChangeBackingPropertiesSelector

-- | @- resizeSubviewsWithOldSize:@
resizeSubviewsWithOldSize :: IsNSView nsView => nsView -> NSSize -> IO ()
resizeSubviewsWithOldSize nsView oldSize =
  sendMessage nsView resizeSubviewsWithOldSizeSelector oldSize

-- | @- resizeWithOldSuperviewSize:@
resizeWithOldSuperviewSize :: IsNSView nsView => nsView -> NSSize -> IO ()
resizeWithOldSuperviewSize nsView oldSize =
  sendMessage nsView resizeWithOldSuperviewSizeSelector oldSize

-- | @- setFrameOrigin:@
setFrameOrigin :: IsNSView nsView => nsView -> NSPoint -> IO ()
setFrameOrigin nsView newOrigin =
  sendMessage nsView setFrameOriginSelector newOrigin

-- | @- setFrameSize:@
setFrameSize :: IsNSView nsView => nsView -> NSSize -> IO ()
setFrameSize nsView newSize =
  sendMessage nsView setFrameSizeSelector newSize

-- | @- setBoundsOrigin:@
setBoundsOrigin :: IsNSView nsView => nsView -> NSPoint -> IO ()
setBoundsOrigin nsView newOrigin =
  sendMessage nsView setBoundsOriginSelector newOrigin

-- | @- setBoundsSize:@
setBoundsSize :: IsNSView nsView => nsView -> NSSize -> IO ()
setBoundsSize nsView newSize =
  sendMessage nsView setBoundsSizeSelector newSize

-- | @- translateOriginToPoint:@
translateOriginToPoint :: IsNSView nsView => nsView -> NSPoint -> IO ()
translateOriginToPoint nsView translation =
  sendMessage nsView translateOriginToPointSelector translation

-- | @- scaleUnitSquareToSize:@
scaleUnitSquareToSize :: IsNSView nsView => nsView -> NSSize -> IO ()
scaleUnitSquareToSize nsView newUnitSize =
  sendMessage nsView scaleUnitSquareToSizeSelector newUnitSize

-- | @- rotateByAngle:@
rotateByAngle :: IsNSView nsView => nsView -> CDouble -> IO ()
rotateByAngle nsView angle =
  sendMessage nsView rotateByAngleSelector angle

-- | @- convertPoint:fromView:@
convertPoint_fromView :: (IsNSView nsView, IsNSView view) => nsView -> NSPoint -> view -> IO NSPoint
convertPoint_fromView nsView point view =
  sendMessage nsView convertPoint_fromViewSelector point (toNSView view)

-- | @- convertPoint:toView:@
convertPoint_toView :: (IsNSView nsView, IsNSView view) => nsView -> NSPoint -> view -> IO NSPoint
convertPoint_toView nsView point view =
  sendMessage nsView convertPoint_toViewSelector point (toNSView view)

-- | @- convertSize:fromView:@
convertSize_fromView :: (IsNSView nsView, IsNSView view) => nsView -> NSSize -> view -> IO NSSize
convertSize_fromView nsView size view =
  sendMessage nsView convertSize_fromViewSelector size (toNSView view)

-- | @- convertSize:toView:@
convertSize_toView :: (IsNSView nsView, IsNSView view) => nsView -> NSSize -> view -> IO NSSize
convertSize_toView nsView size view =
  sendMessage nsView convertSize_toViewSelector size (toNSView view)

-- | @- convertRect:fromView:@
convertRect_fromView :: (IsNSView nsView, IsNSView view) => nsView -> NSRect -> view -> IO NSRect
convertRect_fromView nsView rect view =
  sendMessage nsView convertRect_fromViewSelector rect (toNSView view)

-- | @- convertRect:toView:@
convertRect_toView :: (IsNSView nsView, IsNSView view) => nsView -> NSRect -> view -> IO NSRect
convertRect_toView nsView rect view =
  sendMessage nsView convertRect_toViewSelector rect (toNSView view)

-- | @- backingAlignedRect:options:@
backingAlignedRect_options :: IsNSView nsView => nsView -> NSRect -> NSAlignmentOptions -> IO NSRect
backingAlignedRect_options nsView rect options =
  sendMessage nsView backingAlignedRect_optionsSelector rect options

-- | @- centerScanRect:@
centerScanRect :: IsNSView nsView => nsView -> NSRect -> IO NSRect
centerScanRect nsView rect =
  sendMessage nsView centerScanRectSelector rect

-- | @- convertPointToBacking:@
convertPointToBacking :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointToBacking nsView point =
  sendMessage nsView convertPointToBackingSelector point

-- | @- convertPointFromBacking:@
convertPointFromBacking :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointFromBacking nsView point =
  sendMessage nsView convertPointFromBackingSelector point

-- | @- convertSizeToBacking:@
convertSizeToBacking :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeToBacking nsView size =
  sendMessage nsView convertSizeToBackingSelector size

-- | @- convertSizeFromBacking:@
convertSizeFromBacking :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeFromBacking nsView size =
  sendMessage nsView convertSizeFromBackingSelector size

-- | @- convertRectToBacking:@
convertRectToBacking :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectToBacking nsView rect =
  sendMessage nsView convertRectToBackingSelector rect

-- | @- convertRectFromBacking:@
convertRectFromBacking :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectFromBacking nsView rect =
  sendMessage nsView convertRectFromBackingSelector rect

-- | @- convertPointToLayer:@
convertPointToLayer :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointToLayer nsView point =
  sendMessage nsView convertPointToLayerSelector point

-- | @- convertPointFromLayer:@
convertPointFromLayer :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointFromLayer nsView point =
  sendMessage nsView convertPointFromLayerSelector point

-- | @- convertSizeToLayer:@
convertSizeToLayer :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeToLayer nsView size =
  sendMessage nsView convertSizeToLayerSelector size

-- | @- convertSizeFromLayer:@
convertSizeFromLayer :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeFromLayer nsView size =
  sendMessage nsView convertSizeFromLayerSelector size

-- | @- convertRectToLayer:@
convertRectToLayer :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectToLayer nsView rect =
  sendMessage nsView convertRectToLayerSelector rect

-- | @- convertRectFromLayer:@
convertRectFromLayer :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectFromLayer nsView rect =
  sendMessage nsView convertRectFromLayerSelector rect

-- | @- setNeedsDisplayInRect:@
setNeedsDisplayInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
setNeedsDisplayInRect nsView invalidRect =
  sendMessage nsView setNeedsDisplayInRectSelector invalidRect

-- | @- lockFocus@
lockFocus :: IsNSView nsView => nsView -> IO ()
lockFocus nsView =
  sendMessage nsView lockFocusSelector

-- | @- unlockFocus@
unlockFocus :: IsNSView nsView => nsView -> IO ()
unlockFocus nsView =
  sendMessage nsView unlockFocusSelector

-- | @- lockFocusIfCanDraw@
lockFocusIfCanDraw :: IsNSView nsView => nsView -> IO Bool
lockFocusIfCanDraw nsView =
  sendMessage nsView lockFocusIfCanDrawSelector

-- | @- lockFocusIfCanDrawInContext:@
lockFocusIfCanDrawInContext :: (IsNSView nsView, IsNSGraphicsContext context) => nsView -> context -> IO Bool
lockFocusIfCanDrawInContext nsView context =
  sendMessage nsView lockFocusIfCanDrawInContextSelector (toNSGraphicsContext context)

-- | @- display@
display :: IsNSView nsView => nsView -> IO ()
display nsView =
  sendMessage nsView displaySelector

-- | @- displayIfNeeded@
displayIfNeeded :: IsNSView nsView => nsView -> IO ()
displayIfNeeded nsView =
  sendMessage nsView displayIfNeededSelector

-- | @- displayIfNeededIgnoringOpacity@
displayIfNeededIgnoringOpacity :: IsNSView nsView => nsView -> IO ()
displayIfNeededIgnoringOpacity nsView =
  sendMessage nsView displayIfNeededIgnoringOpacitySelector

-- | @- displayRect:@
displayRect :: IsNSView nsView => nsView -> NSRect -> IO ()
displayRect nsView rect =
  sendMessage nsView displayRectSelector rect

-- | @- displayIfNeededInRect:@
displayIfNeededInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
displayIfNeededInRect nsView rect =
  sendMessage nsView displayIfNeededInRectSelector rect

-- | @- displayRectIgnoringOpacity:@
displayRectIgnoringOpacity :: IsNSView nsView => nsView -> NSRect -> IO ()
displayRectIgnoringOpacity nsView rect =
  sendMessage nsView displayRectIgnoringOpacitySelector rect

-- | @- displayIfNeededInRectIgnoringOpacity:@
displayIfNeededInRectIgnoringOpacity :: IsNSView nsView => nsView -> NSRect -> IO ()
displayIfNeededInRectIgnoringOpacity nsView rect =
  sendMessage nsView displayIfNeededInRectIgnoringOpacitySelector rect

-- | @- drawRect:@
drawRect :: IsNSView nsView => nsView -> NSRect -> IO ()
drawRect nsView dirtyRect =
  sendMessage nsView drawRectSelector dirtyRect

-- | @- displayRectIgnoringOpacity:inContext:@
displayRectIgnoringOpacity_inContext :: (IsNSView nsView, IsNSGraphicsContext context) => nsView -> NSRect -> context -> IO ()
displayRectIgnoringOpacity_inContext nsView rect context =
  sendMessage nsView displayRectIgnoringOpacity_inContextSelector rect (toNSGraphicsContext context)

-- | @- bitmapImageRepForCachingDisplayInRect:@
bitmapImageRepForCachingDisplayInRect :: IsNSView nsView => nsView -> NSRect -> IO (Id NSBitmapImageRep)
bitmapImageRepForCachingDisplayInRect nsView rect =
  sendMessage nsView bitmapImageRepForCachingDisplayInRectSelector rect

-- | @- cacheDisplayInRect:toBitmapImageRep:@
cacheDisplayInRect_toBitmapImageRep :: (IsNSView nsView, IsNSBitmapImageRep bitmapImageRep) => nsView -> NSRect -> bitmapImageRep -> IO ()
cacheDisplayInRect_toBitmapImageRep nsView rect bitmapImageRep =
  sendMessage nsView cacheDisplayInRect_toBitmapImageRepSelector rect (toNSBitmapImageRep bitmapImageRep)

-- | @- viewWillDraw@
viewWillDraw :: IsNSView nsView => nsView -> IO ()
viewWillDraw nsView =
  sendMessage nsView viewWillDrawSelector

-- | @- scrollPoint:@
scrollPoint :: IsNSView nsView => nsView -> NSPoint -> IO ()
scrollPoint nsView point =
  sendMessage nsView scrollPointSelector point

-- | @- scrollRectToVisible:@
scrollRectToVisible :: IsNSView nsView => nsView -> NSRect -> IO Bool
scrollRectToVisible nsView rect =
  sendMessage nsView scrollRectToVisibleSelector rect

-- | @- autoscroll:@
autoscroll :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
autoscroll nsView event =
  sendMessage nsView autoscrollSelector (toNSEvent event)

-- | @- adjustScroll:@
adjustScroll :: IsNSView nsView => nsView -> NSRect -> IO NSRect
adjustScroll nsView newVisible =
  sendMessage nsView adjustScrollSelector newVisible

-- | @- scrollRect:by:@
scrollRect_by :: IsNSView nsView => nsView -> NSRect -> NSSize -> IO ()
scrollRect_by nsView rect delta =
  sendMessage nsView scrollRect_bySelector rect delta

-- | @- translateRectsNeedingDisplayInRect:by:@
translateRectsNeedingDisplayInRect_by :: IsNSView nsView => nsView -> NSRect -> NSSize -> IO ()
translateRectsNeedingDisplayInRect_by nsView clipRect delta =
  sendMessage nsView translateRectsNeedingDisplayInRect_bySelector clipRect delta

-- | @- hitTest:@
hitTest :: IsNSView nsView => nsView -> NSPoint -> IO (Id NSView)
hitTest nsView point =
  sendMessage nsView hitTestSelector point

-- | @- mouse:inRect:@
mouse_inRect :: IsNSView nsView => nsView -> NSPoint -> NSRect -> IO Bool
mouse_inRect nsView point rect =
  sendMessage nsView mouse_inRectSelector point rect

-- | @- viewWithTag:@
viewWithTag :: IsNSView nsView => nsView -> CLong -> IO (Id NSView)
viewWithTag nsView tag =
  sendMessage nsView viewWithTagSelector tag

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
performKeyEquivalent nsView event =
  sendMessage nsView performKeyEquivalentSelector (toNSEvent event)

-- | @- acceptsFirstMouse:@
acceptsFirstMouse :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
acceptsFirstMouse nsView event =
  sendMessage nsView acceptsFirstMouseSelector (toNSEvent event)

-- | @- shouldDelayWindowOrderingForEvent:@
shouldDelayWindowOrderingForEvent :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
shouldDelayWindowOrderingForEvent nsView event =
  sendMessage nsView shouldDelayWindowOrderingForEventSelector (toNSEvent event)

-- | @- makeBackingLayer@
makeBackingLayer :: IsNSView nsView => nsView -> IO (Id CALayer)
makeBackingLayer nsView =
  sendMessage nsView makeBackingLayerSelector

-- | @- updateLayer@
updateLayer :: IsNSView nsView => nsView -> IO ()
updateLayer nsView =
  sendMessage nsView updateLayerSelector

-- | @- layoutSubtreeIfNeeded@
layoutSubtreeIfNeeded :: IsNSView nsView => nsView -> IO ()
layoutSubtreeIfNeeded nsView =
  sendMessage nsView layoutSubtreeIfNeededSelector

-- | @- layout@
layout :: IsNSView nsView => nsView -> IO ()
layout nsView =
  sendMessage nsView layoutSelector

-- | @- menuForEvent:@
menuForEvent :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO (Id NSMenu)
menuForEvent nsView event =
  sendMessage nsView menuForEventSelector (toNSEvent event)

-- | A contextual menu is being opened from the receiving view. The view should update any visual state in response — such as making a selection.
--
-- @menu@ — The contextual menu that is being opened on the view
--
-- @event@ — The event that caused the menu to open.
--
-- ObjC selector: @- willOpenMenu:withEvent:@
willOpenMenu_withEvent :: (IsNSView nsView, IsNSMenu menu, IsNSEvent event) => nsView -> menu -> event -> IO ()
willOpenMenu_withEvent nsView menu event =
  sendMessage nsView willOpenMenu_withEventSelector (toNSMenu menu) (toNSEvent event)

-- | A contextual menu shown from the receiving view has been closed. This is only called if the menu had been opened and the view previously received @-willOpenMenu:withEvent:.@ The view should update any visual state in response — such as removing a temporary selection.
--
-- @menu@ — The contextual menu that was open on the view
--
-- @event@ — The event that caused the menu to close. This may be nil if there is no specific event that triggered the closing.
--
-- ObjC selector: @- didCloseMenu:withEvent:@
didCloseMenu_withEvent :: (IsNSView nsView, IsNSMenu menu, IsNSEvent event) => nsView -> menu -> event -> IO ()
didCloseMenu_withEvent nsView menu event =
  sendMessage nsView didCloseMenu_withEventSelector (toNSMenu menu) (toNSEvent event)

-- | @- addToolTipRect:owner:userData:@
addToolTipRect_owner_userData :: IsNSView nsView => nsView -> NSRect -> RawId -> Ptr () -> IO CLong
addToolTipRect_owner_userData nsView rect owner data_ =
  sendMessage nsView addToolTipRect_owner_userDataSelector rect owner data_

-- | @- removeToolTip:@
removeToolTip :: IsNSView nsView => nsView -> CLong -> IO ()
removeToolTip nsView tag =
  sendMessage nsView removeToolTipSelector tag

-- | @- removeAllToolTips@
removeAllToolTips :: IsNSView nsView => nsView -> IO ()
removeAllToolTips nsView =
  sendMessage nsView removeAllToolTipsSelector

-- | @- viewWillStartLiveResize@
viewWillStartLiveResize :: IsNSView nsView => nsView -> IO ()
viewWillStartLiveResize nsView =
  sendMessage nsView viewWillStartLiveResizeSelector

-- | @- viewDidEndLiveResize@
viewDidEndLiveResize :: IsNSView nsView => nsView -> IO ()
viewDidEndLiveResize nsView =
  sendMessage nsView viewDidEndLiveResizeSelector

-- | @- getRectsExposedDuringLiveResize:count:@
getRectsExposedDuringLiveResize_count :: IsNSView nsView => nsView -> Ptr NSRect -> Ptr CLong -> IO ()
getRectsExposedDuringLiveResize_count nsView exposedRects count =
  sendMessage nsView getRectsExposedDuringLiveResize_countSelector exposedRects count

-- | @- rectForSmartMagnificationAtPoint:inRect:@
rectForSmartMagnificationAtPoint_inRect :: IsNSView nsView => nsView -> NSPoint -> NSRect -> IO NSRect
rectForSmartMagnificationAtPoint_inRect nsView location visibleRect =
  sendMessage nsView rectForSmartMagnificationAtPoint_inRectSelector location visibleRect

-- | @- prepareForReuse@
prepareForReuse :: IsNSView nsView => nsView -> IO ()
prepareForReuse nsView =
  sendMessage nsView prepareForReuseSelector

-- | @- prepareContentInRect:@
prepareContentInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
prepareContentInRect nsView rect =
  sendMessage nsView prepareContentInRectSelector rect

-- | Override point for reacting to the effective appearance of the receiver changing. At this point @effectiveAppearance@ property reflects the new appearance.
--
-- ObjC selector: @- viewDidChangeEffectiveAppearance@
viewDidChangeEffectiveAppearance :: IsNSView nsView => nsView -> IO ()
viewDidChangeEffectiveAppearance nsView =
  sendMessage nsView viewDidChangeEffectiveAppearanceSelector

-- | @- rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO Bool
rulerView_shouldMoveMarker nsView ruler marker =
  sendMessage nsView rulerView_shouldMoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocation :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willMoveMarker_toLocation nsView ruler marker location =
  sendMessage nsView rulerView_willMoveMarker_toLocationSelector (toNSRulerView ruler) (toNSRulerMarker marker) location

-- | @- rulerView:didMoveMarker:@
rulerView_didMoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO ()
rulerView_didMoveMarker nsView ruler marker =
  sendMessage nsView rulerView_didMoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO Bool
rulerView_shouldRemoveMarker nsView ruler marker =
  sendMessage nsView rulerView_shouldRemoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:didRemoveMarker:@
rulerView_didRemoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO ()
rulerView_didRemoveMarker nsView ruler marker =
  sendMessage nsView rulerView_didRemoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:shouldAddMarker:@
rulerView_shouldAddMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO Bool
rulerView_shouldAddMarker nsView ruler marker =
  sendMessage nsView rulerView_shouldAddMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocation :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willAddMarker_atLocation nsView ruler marker location =
  sendMessage nsView rulerView_willAddMarker_atLocationSelector (toNSRulerView ruler) (toNSRulerMarker marker) location

-- | @- rulerView:didAddMarker:@
rulerView_didAddMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO ()
rulerView_didAddMarker nsView ruler marker =
  sendMessage nsView rulerView_didAddMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:handleMouseDown:@
rulerView_handleMouseDown :: (IsNSView nsView, IsNSRulerView ruler, IsNSEvent event) => nsView -> ruler -> event -> IO ()
rulerView_handleMouseDown nsView ruler event =
  sendMessage nsView rulerView_handleMouseDownSelector (toNSRulerView ruler) (toNSEvent event)

-- | @- rulerView:willSetClientView:@
rulerView_willSetClientView :: (IsNSView nsView, IsNSRulerView ruler, IsNSView newClient) => nsView -> ruler -> newClient -> IO ()
rulerView_willSetClientView nsView ruler newClient =
  sendMessage nsView rulerView_willSetClientViewSelector (toNSRulerView ruler) (toNSView newClient)

-- | @- rulerView:locationForPoint:@
rulerView_locationForPoint :: (IsNSView nsView, IsNSRulerView ruler) => nsView -> ruler -> NSPoint -> IO CDouble
rulerView_locationForPoint nsView ruler point =
  sendMessage nsView rulerView_locationForPointSelector (toNSRulerView ruler) point

-- | @- rulerView:pointForLocation:@
rulerView_pointForLocation :: (IsNSView nsView, IsNSRulerView ruler) => nsView -> ruler -> CDouble -> IO NSPoint
rulerView_pointForLocation nsView ruler point =
  sendMessage nsView rulerView_pointForLocationSelector (toNSRulerView ruler) point

-- | @- layoutGuideForLayoutRegion:@
layoutGuideForLayoutRegion :: (IsNSView nsView, IsNSViewLayoutRegion layoutRegion) => nsView -> layoutRegion -> IO (Id NSLayoutGuide)
layoutGuideForLayoutRegion nsView layoutRegion =
  sendMessage nsView layoutGuideForLayoutRegionSelector (toNSViewLayoutRegion layoutRegion)

-- | @- edgeInsetsForLayoutRegion:@
edgeInsetsForLayoutRegion :: (IsNSView nsView, IsNSViewLayoutRegion layoutRegion) => nsView -> layoutRegion -> IO NSEdgeInsets
edgeInsetsForLayoutRegion nsView layoutRegion =
  sendMessage nsView edgeInsetsForLayoutRegionSelector (toNSViewLayoutRegion layoutRegion)

-- | @- rectForLayoutRegion:@
rectForLayoutRegion :: (IsNSView nsView, IsNSViewLayoutRegion layoutRegion) => nsView -> layoutRegion -> IO NSRect
rectForLayoutRegion nsView layoutRegion =
  sendMessage nsView rectForLayoutRegionSelector (toNSViewLayoutRegion layoutRegion)

-- | @- addLayoutGuide:@
addLayoutGuide :: (IsNSView nsView, IsNSLayoutGuide guide) => nsView -> guide -> IO ()
addLayoutGuide nsView guide =
  sendMessage nsView addLayoutGuideSelector (toNSLayoutGuide guide)

-- | @- removeLayoutGuide:@
removeLayoutGuide :: (IsNSView nsView, IsNSLayoutGuide guide) => nsView -> guide -> IO ()
removeLayoutGuide nsView guide =
  sendMessage nsView removeLayoutGuideSelector (toNSLayoutGuide guide)

-- | @- constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientation :: IsNSView nsView => nsView -> NSLayoutConstraintOrientation -> IO (Id NSArray)
constraintsAffectingLayoutForOrientation nsView orientation =
  sendMessage nsView constraintsAffectingLayoutForOrientationSelector orientation

-- | @- exerciseAmbiguityInLayout@
exerciseAmbiguityInLayout :: IsNSView nsView => nsView -> IO ()
exerciseAmbiguityInLayout nsView =
  sendMessage nsView exerciseAmbiguityInLayoutSelector

-- | @- alignmentRectForFrame:@
alignmentRectForFrame :: IsNSView nsView => nsView -> NSRect -> IO NSRect
alignmentRectForFrame nsView frame =
  sendMessage nsView alignmentRectForFrameSelector frame

-- | @- frameForAlignmentRect:@
frameForAlignmentRect :: IsNSView nsView => nsView -> NSRect -> IO NSRect
frameForAlignmentRect nsView alignmentRect =
  sendMessage nsView frameForAlignmentRectSelector alignmentRect

-- | @- invalidateIntrinsicContentSize@
invalidateIntrinsicContentSize :: IsNSView nsView => nsView -> IO ()
invalidateIntrinsicContentSize nsView =
  sendMessage nsView invalidateIntrinsicContentSizeSelector

-- | @- contentHuggingPriorityForOrientation:@
contentHuggingPriorityForOrientation :: IsNSView nsView => nsView -> NSLayoutConstraintOrientation -> IO CFloat
contentHuggingPriorityForOrientation nsView orientation =
  sendMessage nsView contentHuggingPriorityForOrientationSelector orientation

-- | @- setContentHuggingPriority:forOrientation:@
setContentHuggingPriority_forOrientation :: IsNSView nsView => nsView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setContentHuggingPriority_forOrientation nsView priority orientation =
  sendMessage nsView setContentHuggingPriority_forOrientationSelector priority orientation

-- | @- contentCompressionResistancePriorityForOrientation:@
contentCompressionResistancePriorityForOrientation :: IsNSView nsView => nsView -> NSLayoutConstraintOrientation -> IO CFloat
contentCompressionResistancePriorityForOrientation nsView orientation =
  sendMessage nsView contentCompressionResistancePriorityForOrientationSelector orientation

-- | @- setContentCompressionResistancePriority:forOrientation:@
setContentCompressionResistancePriority_forOrientation :: IsNSView nsView => nsView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setContentCompressionResistancePriority_forOrientation nsView priority orientation =
  sendMessage nsView setContentCompressionResistancePriority_forOrientationSelector priority orientation

-- | @- updateConstraintsForSubtreeIfNeeded@
updateConstraintsForSubtreeIfNeeded :: IsNSView nsView => nsView -> IO ()
updateConstraintsForSubtreeIfNeeded nsView =
  sendMessage nsView updateConstraintsForSubtreeIfNeededSelector

-- | @- updateConstraints@
updateConstraints :: IsNSView nsView => nsView -> IO ()
updateConstraints nsView =
  sendMessage nsView updateConstraintsSelector

-- | @- addConstraint:@
addConstraint :: (IsNSView nsView, IsNSLayoutConstraint constraint) => nsView -> constraint -> IO ()
addConstraint nsView constraint =
  sendMessage nsView addConstraintSelector (toNSLayoutConstraint constraint)

-- | @- addConstraints:@
addConstraints :: (IsNSView nsView, IsNSArray constraints) => nsView -> constraints -> IO ()
addConstraints nsView constraints =
  sendMessage nsView addConstraintsSelector (toNSArray constraints)

-- | @- removeConstraint:@
removeConstraint :: (IsNSView nsView, IsNSLayoutConstraint constraint) => nsView -> constraint -> IO ()
removeConstraint nsView constraint =
  sendMessage nsView removeConstraintSelector (toNSLayoutConstraint constraint)

-- | @- removeConstraints:@
removeConstraints :: (IsNSView nsView, IsNSArray constraints) => nsView -> constraints -> IO ()
removeConstraints nsView constraints =
  sendMessage nsView removeConstraintsSelector (toNSArray constraints)

-- | @- reflectScrolledClipView:@
reflectScrolledClipView :: (IsNSView nsView, IsNSClipView clipView) => nsView -> clipView -> IO ()
reflectScrolledClipView nsView clipView =
  sendMessage nsView reflectScrolledClipViewSelector (toNSClipView clipView)

-- | @- scrollClipView:toPoint:@
scrollClipView_toPoint :: (IsNSView nsView, IsNSClipView clipView) => nsView -> clipView -> NSPoint -> IO ()
scrollClipView_toPoint nsView clipView point =
  sendMessage nsView scrollClipView_toPointSelector (toNSClipView clipView) point

-- | @- dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBack :: (IsNSView nsView, IsNSImage image, IsNSEvent event, IsNSPasteboard pboard) => nsView -> image -> NSPoint -> NSSize -> event -> pboard -> RawId -> Bool -> IO ()
dragImage_at_offset_event_pasteboard_source_slideBack nsView image viewLocation initialOffset event pboard sourceObj slideFlag =
  sendMessage nsView dragImage_at_offset_event_pasteboard_source_slideBackSelector (toNSImage image) viewLocation initialOffset (toNSEvent event) (toNSPasteboard pboard) sourceObj slideFlag

-- | @- dragFile:fromRect:slideBack:event:@
dragFile_fromRect_slideBack_event :: (IsNSView nsView, IsNSString filename, IsNSEvent event) => nsView -> filename -> NSRect -> Bool -> event -> IO Bool
dragFile_fromRect_slideBack_event nsView filename rect flag event =
  sendMessage nsView dragFile_fromRect_slideBack_eventSelector (toNSString filename) rect flag (toNSEvent event)

-- | @- dragPromisedFilesOfTypes:fromRect:source:slideBack:event:@
dragPromisedFilesOfTypes_fromRect_source_slideBack_event :: (IsNSView nsView, IsNSArray typeArray, IsNSEvent event) => nsView -> typeArray -> NSRect -> RawId -> Bool -> event -> IO Bool
dragPromisedFilesOfTypes_fromRect_source_slideBack_event nsView typeArray rect sourceObject flag event =
  sendMessage nsView dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector (toNSArray typeArray) rect sourceObject flag (toNSEvent event)

-- | @- convertPointToBase:@
convertPointToBase :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointToBase nsView point =
  sendMessage nsView convertPointToBaseSelector point

-- | @- convertPointFromBase:@
convertPointFromBase :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointFromBase nsView point =
  sendMessage nsView convertPointFromBaseSelector point

-- | @- convertSizeToBase:@
convertSizeToBase :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeToBase nsView size =
  sendMessage nsView convertSizeToBaseSelector size

-- | @- convertSizeFromBase:@
convertSizeFromBase :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeFromBase nsView size =
  sendMessage nsView convertSizeFromBaseSelector size

-- | @- convertRectToBase:@
convertRectToBase :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectToBase nsView rect =
  sendMessage nsView convertRectToBaseSelector rect

-- | @- convertRectFromBase:@
convertRectFromBase :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectFromBase nsView rect =
  sendMessage nsView convertRectFromBaseSelector rect

-- | @- performMnemonic:@
performMnemonic :: (IsNSView nsView, IsNSString string) => nsView -> string -> IO Bool
performMnemonic nsView string =
  sendMessage nsView performMnemonicSelector (toNSString string)

-- | @- shouldDrawColor@
shouldDrawColor :: IsNSView nsView => nsView -> IO Bool
shouldDrawColor nsView =
  sendMessage nsView shouldDrawColorSelector

-- | @- gState@
gState :: IsNSView nsView => nsView -> IO CLong
gState nsView =
  sendMessage nsView gStateSelector

-- | @- allocateGState@
allocateGState :: IsNSView nsView => nsView -> IO ()
allocateGState nsView =
  sendOwnedMessage nsView allocateGStateSelector

-- | @- releaseGState@
releaseGState :: IsNSView nsView => nsView -> IO ()
releaseGState nsView =
  sendMessage nsView releaseGStateSelector

-- | @- setUpGState@
setUpGState :: IsNSView nsView => nsView -> IO ()
setUpGState nsView =
  sendMessage nsView setUpGStateSelector

-- | @- renewGState@
renewGState :: IsNSView nsView => nsView -> IO ()
renewGState nsView =
  sendMessage nsView renewGStateSelector

-- | @- displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: IsNSView nsView => nsView -> RawId -> Sel -> IO (Id CADisplayLink)
displayLinkWithTarget_selector nsView target selector =
  sendMessage nsView displayLinkWithTarget_selectorSelector target selector

-- | @- addTrackingArea:@
addTrackingArea :: (IsNSView nsView, IsNSTrackingArea trackingArea) => nsView -> trackingArea -> IO ()
addTrackingArea nsView trackingArea =
  sendMessage nsView addTrackingAreaSelector (toNSTrackingArea trackingArea)

-- | @- removeTrackingArea:@
removeTrackingArea :: (IsNSView nsView, IsNSTrackingArea trackingArea) => nsView -> trackingArea -> IO ()
removeTrackingArea nsView trackingArea =
  sendMessage nsView removeTrackingAreaSelector (toNSTrackingArea trackingArea)

-- | @- updateTrackingAreas@
updateTrackingAreas :: IsNSView nsView => nsView -> IO ()
updateTrackingAreas nsView =
  sendMessage nsView updateTrackingAreasSelector

-- | @- addCursorRect:cursor:@
addCursorRect_cursor :: (IsNSView nsView, IsNSCursor object) => nsView -> NSRect -> object -> IO ()
addCursorRect_cursor nsView rect object =
  sendMessage nsView addCursorRect_cursorSelector rect (toNSCursor object)

-- | @- removeCursorRect:cursor:@
removeCursorRect_cursor :: (IsNSView nsView, IsNSCursor object) => nsView -> NSRect -> object -> IO ()
removeCursorRect_cursor nsView rect object =
  sendMessage nsView removeCursorRect_cursorSelector rect (toNSCursor object)

-- | @- discardCursorRects@
discardCursorRects :: IsNSView nsView => nsView -> IO ()
discardCursorRects nsView =
  sendMessage nsView discardCursorRectsSelector

-- | @- resetCursorRects@
resetCursorRects :: IsNSView nsView => nsView -> IO ()
resetCursorRects nsView =
  sendMessage nsView resetCursorRectsSelector

-- | @- addTrackingRect:owner:userData:assumeInside:@
addTrackingRect_owner_userData_assumeInside :: IsNSView nsView => nsView -> NSRect -> RawId -> Ptr () -> Bool -> IO CLong
addTrackingRect_owner_userData_assumeInside nsView rect owner data_ flag =
  sendMessage nsView addTrackingRect_owner_userData_assumeInsideSelector rect owner data_ flag

-- | @- removeTrackingRect:@
removeTrackingRect :: IsNSView nsView => nsView -> CLong -> IO ()
removeTrackingRect nsView tag =
  sendMessage nsView removeTrackingRectSelector tag

-- | @- addGestureRecognizer:@
addGestureRecognizer :: (IsNSView nsView, IsNSGestureRecognizer gestureRecognizer) => nsView -> gestureRecognizer -> IO ()
addGestureRecognizer nsView gestureRecognizer =
  sendMessage nsView addGestureRecognizerSelector (toNSGestureRecognizer gestureRecognizer)

-- | @- removeGestureRecognizer:@
removeGestureRecognizer :: (IsNSView nsView, IsNSGestureRecognizer gestureRecognizer) => nsView -> gestureRecognizer -> IO ()
removeGestureRecognizer nsView gestureRecognizer =
  sendMessage nsView removeGestureRecognizerSelector (toNSGestureRecognizer gestureRecognizer)

-- | @- showDefinitionForAttributedString:atPoint:@
showDefinitionForAttributedString_atPoint :: (IsNSView nsView, IsNSAttributedString attrString) => nsView -> attrString -> NSPoint -> IO ()
showDefinitionForAttributedString_atPoint nsView attrString textBaselineOrigin =
  sendMessage nsView showDefinitionForAttributedString_atPointSelector (toNSAttributedString attrString) textBaselineOrigin

-- | @- showDefinitionForAttributedString:range:options:baselineOriginProvider:@
showDefinitionForAttributedString_range_options_baselineOriginProvider :: (IsNSView nsView, IsNSAttributedString attrString, IsNSDictionary options) => nsView -> attrString -> NSRange -> options -> Ptr () -> IO ()
showDefinitionForAttributedString_range_options_baselineOriginProvider nsView attrString targetRange options originProvider =
  sendMessage nsView showDefinitionForAttributedString_range_options_baselineOriginProviderSelector (toNSAttributedString attrString) targetRange (toNSDictionary options) originProvider

-- | @- enterFullScreenMode:withOptions:@
enterFullScreenMode_withOptions :: (IsNSView nsView, IsNSScreen screen, IsNSDictionary options) => nsView -> screen -> options -> IO Bool
enterFullScreenMode_withOptions nsView screen options =
  sendMessage nsView enterFullScreenMode_withOptionsSelector (toNSScreen screen) (toNSDictionary options)

-- | @- exitFullScreenModeWithOptions:@
exitFullScreenModeWithOptions :: (IsNSView nsView, IsNSDictionary options) => nsView -> options -> IO ()
exitFullScreenModeWithOptions nsView options =
  sendMessage nsView exitFullScreenModeWithOptionsSelector (toNSDictionary options)

-- | @- beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_source :: (IsNSView nsView, IsNSArray items, IsNSEvent event) => nsView -> items -> event -> RawId -> IO (Id NSDraggingSession)
beginDraggingSessionWithItems_event_source nsView items event source =
  sendMessage nsView beginDraggingSessionWithItems_event_sourceSelector (toNSArray items) (toNSEvent event) source

-- | @- registerForDraggedTypes:@
registerForDraggedTypes :: (IsNSView nsView, IsNSArray newTypes) => nsView -> newTypes -> IO ()
registerForDraggedTypes nsView newTypes =
  sendMessage nsView registerForDraggedTypesSelector (toNSArray newTypes)

-- | @- unregisterDraggedTypes@
unregisterDraggedTypes :: IsNSView nsView => nsView -> IO ()
unregisterDraggedTypes nsView =
  sendMessage nsView unregisterDraggedTypesSelector

-- | @- writeEPSInsideRect:toPasteboard:@
writeEPSInsideRect_toPasteboard :: (IsNSView nsView, IsNSPasteboard pasteboard) => nsView -> NSRect -> pasteboard -> IO ()
writeEPSInsideRect_toPasteboard nsView rect pasteboard =
  sendMessage nsView writeEPSInsideRect_toPasteboardSelector rect (toNSPasteboard pasteboard)

-- | @- dataWithEPSInsideRect:@
dataWithEPSInsideRect :: IsNSView nsView => nsView -> NSRect -> IO (Id NSData)
dataWithEPSInsideRect nsView rect =
  sendMessage nsView dataWithEPSInsideRectSelector rect

-- | @- writePDFInsideRect:toPasteboard:@
writePDFInsideRect_toPasteboard :: (IsNSView nsView, IsNSPasteboard pasteboard) => nsView -> NSRect -> pasteboard -> IO ()
writePDFInsideRect_toPasteboard nsView rect pasteboard =
  sendMessage nsView writePDFInsideRect_toPasteboardSelector rect (toNSPasteboard pasteboard)

-- | @- dataWithPDFInsideRect:@
dataWithPDFInsideRect :: IsNSView nsView => nsView -> NSRect -> IO (Id NSData)
dataWithPDFInsideRect nsView rect =
  sendMessage nsView dataWithPDFInsideRectSelector rect

-- | @- print:@
print_ :: IsNSView nsView => nsView -> RawId -> IO ()
print_ nsView sender =
  sendMessage nsView printSelector sender

-- | @- knowsPageRange:@
knowsPageRange :: IsNSView nsView => nsView -> Ptr NSRange -> IO Bool
knowsPageRange nsView range =
  sendMessage nsView knowsPageRangeSelector range

-- | @- adjustPageWidthNew:left:right:limit:@
adjustPageWidthNew_left_right_limit :: IsNSView nsView => nsView -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> IO ()
adjustPageWidthNew_left_right_limit nsView newRight oldLeft oldRight rightLimit =
  sendMessage nsView adjustPageWidthNew_left_right_limitSelector newRight oldLeft oldRight rightLimit

-- | @- adjustPageHeightNew:top:bottom:limit:@
adjustPageHeightNew_top_bottom_limit :: IsNSView nsView => nsView -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> IO ()
adjustPageHeightNew_top_bottom_limit nsView newBottom oldTop oldBottom bottomLimit =
  sendMessage nsView adjustPageHeightNew_top_bottom_limitSelector newBottom oldTop oldBottom bottomLimit

-- | @- rectForPage:@
rectForPage :: IsNSView nsView => nsView -> CLong -> IO NSRect
rectForPage nsView page =
  sendMessage nsView rectForPageSelector page

-- | @- locationOfPrintRect:@
locationOfPrintRect :: IsNSView nsView => nsView -> NSRect -> IO NSPoint
locationOfPrintRect nsView rect =
  sendMessage nsView locationOfPrintRectSelector rect

-- | @- drawPageBorderWithSize:@
drawPageBorderWithSize :: IsNSView nsView => nsView -> NSSize -> IO ()
drawPageBorderWithSize nsView borderSize =
  sendMessage nsView drawPageBorderWithSizeSelector borderSize

-- | * This method is obsolete.  It will never be invoked from within AppKit, and NSView's implementation of it does nothing. **
--
-- ObjC selector: @- drawSheetBorderWithSize:@
drawSheetBorderWithSize :: IsNSView nsView => nsView -> NSSize -> IO ()
drawSheetBorderWithSize nsView borderSize =
  sendMessage nsView drawSheetBorderWithSizeSelector borderSize

-- | @- beginDocument@
beginDocument :: IsNSView nsView => nsView -> IO ()
beginDocument nsView =
  sendMessage nsView beginDocumentSelector

-- | @- endDocument@
endDocument :: IsNSView nsView => nsView -> IO ()
endDocument nsView =
  sendMessage nsView endDocumentSelector

-- | @- beginPageInRect:atPlacement:@
beginPageInRect_atPlacement :: IsNSView nsView => nsView -> NSRect -> NSPoint -> IO ()
beginPageInRect_atPlacement nsView rect location =
  sendMessage nsView beginPageInRect_atPlacementSelector rect location

-- | @- endPage@
endPage :: IsNSView nsView => nsView -> IO ()
endPage nsView =
  sendMessage nsView endPageSelector

-- | @- setKeyboardFocusRingNeedsDisplayInRect:@
setKeyboardFocusRingNeedsDisplayInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
setKeyboardFocusRingNeedsDisplayInRect nsView rect =
  sendMessage nsView setKeyboardFocusRingNeedsDisplayInRectSelector rect

-- | @- drawFocusRingMask@
drawFocusRingMask :: IsNSView nsView => nsView -> IO ()
drawFocusRingMask nsView =
  sendMessage nsView drawFocusRingMaskSelector

-- | @- noteFocusRingMaskChanged@
noteFocusRingMaskChanged :: IsNSView nsView => nsView -> IO ()
noteFocusRingMaskChanged nsView =
  sendMessage nsView noteFocusRingMaskChangedSelector

-- | @- window@
window :: IsNSView nsView => nsView -> IO (Id NSWindow)
window nsView =
  sendMessage nsView windowSelector

-- | @- superview@
superview :: IsNSView nsView => nsView -> IO (Id NSView)
superview nsView =
  sendMessage nsView superviewSelector

-- | @- subviews@
subviews :: IsNSView nsView => nsView -> IO (Id NSArray)
subviews nsView =
  sendMessage nsView subviewsSelector

-- | @- setSubviews:@
setSubviews :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setSubviews nsView value =
  sendMessage nsView setSubviewsSelector (toNSArray value)

-- | @- opaqueAncestor@
opaqueAncestor :: IsNSView nsView => nsView -> IO (Id NSView)
opaqueAncestor nsView =
  sendMessage nsView opaqueAncestorSelector

-- | @- hidden@
hidden :: IsNSView nsView => nsView -> IO Bool
hidden nsView =
  sendMessage nsView hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSView nsView => nsView -> Bool -> IO ()
setHidden nsView value =
  sendMessage nsView setHiddenSelector value

-- | @- hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestor :: IsNSView nsView => nsView -> IO Bool
hiddenOrHasHiddenAncestor nsView =
  sendMessage nsView hiddenOrHasHiddenAncestorSelector

-- | @- wantsDefaultClipping@
wantsDefaultClipping :: IsNSView nsView => nsView -> IO Bool
wantsDefaultClipping nsView =
  sendMessage nsView wantsDefaultClippingSelector

-- | @- postsFrameChangedNotifications@
postsFrameChangedNotifications :: IsNSView nsView => nsView -> IO Bool
postsFrameChangedNotifications nsView =
  sendMessage nsView postsFrameChangedNotificationsSelector

-- | @- setPostsFrameChangedNotifications:@
setPostsFrameChangedNotifications :: IsNSView nsView => nsView -> Bool -> IO ()
setPostsFrameChangedNotifications nsView value =
  sendMessage nsView setPostsFrameChangedNotificationsSelector value

-- | @- autoresizesSubviews@
autoresizesSubviews :: IsNSView nsView => nsView -> IO Bool
autoresizesSubviews nsView =
  sendMessage nsView autoresizesSubviewsSelector

-- | @- setAutoresizesSubviews:@
setAutoresizesSubviews :: IsNSView nsView => nsView -> Bool -> IO ()
setAutoresizesSubviews nsView value =
  sendMessage nsView setAutoresizesSubviewsSelector value

-- | @- autoresizingMask@
autoresizingMask :: IsNSView nsView => nsView -> IO NSAutoresizingMaskOptions
autoresizingMask nsView =
  sendMessage nsView autoresizingMaskSelector

-- | @- setAutoresizingMask:@
setAutoresizingMask :: IsNSView nsView => nsView -> NSAutoresizingMaskOptions -> IO ()
setAutoresizingMask nsView value =
  sendMessage nsView setAutoresizingMaskSelector value

-- | @- frame@
frame :: IsNSView nsView => nsView -> IO NSRect
frame nsView =
  sendMessage nsView frameSelector

-- | @- setFrame:@
setFrame :: IsNSView nsView => nsView -> NSRect -> IO ()
setFrame nsView value =
  sendMessage nsView setFrameSelector value

-- | @- frameRotation@
frameRotation :: IsNSView nsView => nsView -> IO CDouble
frameRotation nsView =
  sendMessage nsView frameRotationSelector

-- | @- setFrameRotation:@
setFrameRotation :: IsNSView nsView => nsView -> CDouble -> IO ()
setFrameRotation nsView value =
  sendMessage nsView setFrameRotationSelector value

-- | @- frameCenterRotation@
frameCenterRotation :: IsNSView nsView => nsView -> IO CDouble
frameCenterRotation nsView =
  sendMessage nsView frameCenterRotationSelector

-- | @- setFrameCenterRotation:@
setFrameCenterRotation :: IsNSView nsView => nsView -> CDouble -> IO ()
setFrameCenterRotation nsView value =
  sendMessage nsView setFrameCenterRotationSelector value

-- | @- boundsRotation@
boundsRotation :: IsNSView nsView => nsView -> IO CDouble
boundsRotation nsView =
  sendMessage nsView boundsRotationSelector

-- | @- setBoundsRotation:@
setBoundsRotation :: IsNSView nsView => nsView -> CDouble -> IO ()
setBoundsRotation nsView value =
  sendMessage nsView setBoundsRotationSelector value

-- | @- bounds@
bounds :: IsNSView nsView => nsView -> IO NSRect
bounds nsView =
  sendMessage nsView boundsSelector

-- | @- setBounds:@
setBounds :: IsNSView nsView => nsView -> NSRect -> IO ()
setBounds nsView value =
  sendMessage nsView setBoundsSelector value

-- | @- flipped@
flipped :: IsNSView nsView => nsView -> IO Bool
flipped nsView =
  sendMessage nsView flippedSelector

-- | @- rotatedFromBase@
rotatedFromBase :: IsNSView nsView => nsView -> IO Bool
rotatedFromBase nsView =
  sendMessage nsView rotatedFromBaseSelector

-- | @- rotatedOrScaledFromBase@
rotatedOrScaledFromBase :: IsNSView nsView => nsView -> IO Bool
rotatedOrScaledFromBase nsView =
  sendMessage nsView rotatedOrScaledFromBaseSelector

-- | @- opaque@
opaque :: IsNSView nsView => nsView -> IO Bool
opaque nsView =
  sendMessage nsView opaqueSelector

-- | @- canDrawConcurrently@
canDrawConcurrently :: IsNSView nsView => nsView -> IO Bool
canDrawConcurrently nsView =
  sendMessage nsView canDrawConcurrentlySelector

-- | @- setCanDrawConcurrently:@
setCanDrawConcurrently :: IsNSView nsView => nsView -> Bool -> IO ()
setCanDrawConcurrently nsView value =
  sendMessage nsView setCanDrawConcurrentlySelector value

-- | @- canDraw@
canDraw :: IsNSView nsView => nsView -> IO Bool
canDraw nsView =
  sendMessage nsView canDrawSelector

-- | @- needsDisplay@
needsDisplay :: IsNSView nsView => nsView -> IO Bool
needsDisplay nsView =
  sendMessage nsView needsDisplaySelector

-- | @- setNeedsDisplay:@
setNeedsDisplay :: IsNSView nsView => nsView -> Bool -> IO ()
setNeedsDisplay nsView value =
  sendMessage nsView setNeedsDisplaySelector value

-- | @+ focusView@
focusView :: IO (Id NSView)
focusView  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMessage cls' focusViewSelector

-- | The portion of the view that isn’t clipped by its superviews.
--
-- Visibility, as reflected by this property, doesn’t account for whether other view or window objects overlap the current view or whether the current view is installed in a window at all. This value of this property is @NSZeroRect@ if the current view is effectively hidden.
--
-- During a printing operation, the visible rectangle is further clipped to the page being imaged.
--
-- ObjC selector: @- visibleRect@
visibleRect :: IsNSView nsView => nsView -> IO NSRect
visibleRect nsView =
  sendMessage nsView visibleRectSelector

-- | @- tag@
tag :: IsNSView nsView => nsView -> IO CLong
tag nsView =
  sendMessage nsView tagSelector

-- | @- needsPanelToBecomeKey@
needsPanelToBecomeKey :: IsNSView nsView => nsView -> IO Bool
needsPanelToBecomeKey nsView =
  sendMessage nsView needsPanelToBecomeKeySelector

-- | @- mouseDownCanMoveWindow@
mouseDownCanMoveWindow :: IsNSView nsView => nsView -> IO Bool
mouseDownCanMoveWindow nsView =
  sendMessage nsView mouseDownCanMoveWindowSelector

-- | @- acceptsTouchEvents@
acceptsTouchEvents :: IsNSView nsView => nsView -> IO Bool
acceptsTouchEvents nsView =
  sendMessage nsView acceptsTouchEventsSelector

-- | @- setAcceptsTouchEvents:@
setAcceptsTouchEvents :: IsNSView nsView => nsView -> Bool -> IO ()
setAcceptsTouchEvents nsView value =
  sendMessage nsView setAcceptsTouchEventsSelector value

-- | @- wantsRestingTouches@
wantsRestingTouches :: IsNSView nsView => nsView -> IO Bool
wantsRestingTouches nsView =
  sendMessage nsView wantsRestingTouchesSelector

-- | @- setWantsRestingTouches:@
setWantsRestingTouches :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsRestingTouches nsView value =
  sendMessage nsView setWantsRestingTouchesSelector value

-- | @- layerContentsRedrawPolicy@
layerContentsRedrawPolicy :: IsNSView nsView => nsView -> IO NSViewLayerContentsRedrawPolicy
layerContentsRedrawPolicy nsView =
  sendMessage nsView layerContentsRedrawPolicySelector

-- | @- setLayerContentsRedrawPolicy:@
setLayerContentsRedrawPolicy :: IsNSView nsView => nsView -> NSViewLayerContentsRedrawPolicy -> IO ()
setLayerContentsRedrawPolicy nsView value =
  sendMessage nsView setLayerContentsRedrawPolicySelector value

-- | @- layerContentsPlacement@
layerContentsPlacement :: IsNSView nsView => nsView -> IO NSViewLayerContentsPlacement
layerContentsPlacement nsView =
  sendMessage nsView layerContentsPlacementSelector

-- | @- setLayerContentsPlacement:@
setLayerContentsPlacement :: IsNSView nsView => nsView -> NSViewLayerContentsPlacement -> IO ()
setLayerContentsPlacement nsView value =
  sendMessage nsView setLayerContentsPlacementSelector value

-- | @- wantsLayer@
wantsLayer :: IsNSView nsView => nsView -> IO Bool
wantsLayer nsView =
  sendMessage nsView wantsLayerSelector

-- | @- setWantsLayer:@
setWantsLayer :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsLayer nsView value =
  sendMessage nsView setWantsLayerSelector value

-- | @- layer@
layer :: IsNSView nsView => nsView -> IO (Id CALayer)
layer nsView =
  sendMessage nsView layerSelector

-- | @- setLayer:@
setLayer :: (IsNSView nsView, IsCALayer value) => nsView -> value -> IO ()
setLayer nsView value =
  sendMessage nsView setLayerSelector (toCALayer value)

-- | @- wantsUpdateLayer@
wantsUpdateLayer :: IsNSView nsView => nsView -> IO Bool
wantsUpdateLayer nsView =
  sendMessage nsView wantsUpdateLayerSelector

-- | @- canDrawSubviewsIntoLayer@
canDrawSubviewsIntoLayer :: IsNSView nsView => nsView -> IO Bool
canDrawSubviewsIntoLayer nsView =
  sendMessage nsView canDrawSubviewsIntoLayerSelector

-- | @- setCanDrawSubviewsIntoLayer:@
setCanDrawSubviewsIntoLayer :: IsNSView nsView => nsView -> Bool -> IO ()
setCanDrawSubviewsIntoLayer nsView value =
  sendMessage nsView setCanDrawSubviewsIntoLayerSelector value

-- | @- needsLayout@
needsLayout :: IsNSView nsView => nsView -> IO Bool
needsLayout nsView =
  sendMessage nsView needsLayoutSelector

-- | @- setNeedsLayout:@
setNeedsLayout :: IsNSView nsView => nsView -> Bool -> IO ()
setNeedsLayout nsView value =
  sendMessage nsView setNeedsLayoutSelector value

-- | @- alphaValue@
alphaValue :: IsNSView nsView => nsView -> IO CDouble
alphaValue nsView =
  sendMessage nsView alphaValueSelector

-- | @- setAlphaValue:@
setAlphaValue :: IsNSView nsView => nsView -> CDouble -> IO ()
setAlphaValue nsView value =
  sendMessage nsView setAlphaValueSelector value

-- | @- layerUsesCoreImageFilters@
layerUsesCoreImageFilters :: IsNSView nsView => nsView -> IO Bool
layerUsesCoreImageFilters nsView =
  sendMessage nsView layerUsesCoreImageFiltersSelector

-- | @- setLayerUsesCoreImageFilters:@
setLayerUsesCoreImageFilters :: IsNSView nsView => nsView -> Bool -> IO ()
setLayerUsesCoreImageFilters nsView value =
  sendMessage nsView setLayerUsesCoreImageFiltersSelector value

-- | @- backgroundFilters@
backgroundFilters :: IsNSView nsView => nsView -> IO (Id NSArray)
backgroundFilters nsView =
  sendMessage nsView backgroundFiltersSelector

-- | @- setBackgroundFilters:@
setBackgroundFilters :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setBackgroundFilters nsView value =
  sendMessage nsView setBackgroundFiltersSelector (toNSArray value)

-- | @- compositingFilter@
compositingFilter :: IsNSView nsView => nsView -> IO (Id CIFilter)
compositingFilter nsView =
  sendMessage nsView compositingFilterSelector

-- | @- setCompositingFilter:@
setCompositingFilter :: (IsNSView nsView, IsCIFilter value) => nsView -> value -> IO ()
setCompositingFilter nsView value =
  sendMessage nsView setCompositingFilterSelector (toCIFilter value)

-- | @- contentFilters@
contentFilters :: IsNSView nsView => nsView -> IO (Id NSArray)
contentFilters nsView =
  sendMessage nsView contentFiltersSelector

-- | @- setContentFilters:@
setContentFilters :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setContentFilters nsView value =
  sendMessage nsView setContentFiltersSelector (toNSArray value)

-- | @- shadow@
shadow :: IsNSView nsView => nsView -> IO (Id NSShadow)
shadow nsView =
  sendMessage nsView shadowSelector

-- | @- setShadow:@
setShadow :: (IsNSView nsView, IsNSShadow value) => nsView -> value -> IO ()
setShadow nsView value =
  sendMessage nsView setShadowSelector (toNSShadow value)

-- | @- clipsToBounds@
clipsToBounds :: IsNSView nsView => nsView -> IO Bool
clipsToBounds nsView =
  sendMessage nsView clipsToBoundsSelector

-- | @- setClipsToBounds:@
setClipsToBounds :: IsNSView nsView => nsView -> Bool -> IO ()
setClipsToBounds nsView value =
  sendMessage nsView setClipsToBoundsSelector value

-- | @- postsBoundsChangedNotifications@
postsBoundsChangedNotifications :: IsNSView nsView => nsView -> IO Bool
postsBoundsChangedNotifications nsView =
  sendMessage nsView postsBoundsChangedNotificationsSelector

-- | @- setPostsBoundsChangedNotifications:@
setPostsBoundsChangedNotifications :: IsNSView nsView => nsView -> Bool -> IO ()
setPostsBoundsChangedNotifications nsView value =
  sendMessage nsView setPostsBoundsChangedNotificationsSelector value

-- | @- enclosingScrollView@
enclosingScrollView :: IsNSView nsView => nsView -> IO (Id NSScrollView)
enclosingScrollView nsView =
  sendMessage nsView enclosingScrollViewSelector

-- | @+ defaultMenu@
defaultMenu :: IO (Id NSMenu)
defaultMenu  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMessage cls' defaultMenuSelector

-- | @- toolTip@
toolTip :: IsNSView nsView => nsView -> IO (Id NSString)
toolTip nsView =
  sendMessage nsView toolTipSelector

-- | @- setToolTip:@
setToolTip :: (IsNSView nsView, IsNSString value) => nsView -> value -> IO ()
setToolTip nsView value =
  sendMessage nsView setToolTipSelector (toNSString value)

-- | @- inLiveResize@
inLiveResize :: IsNSView nsView => nsView -> IO Bool
inLiveResize nsView =
  sendMessage nsView inLiveResizeSelector

-- | @- preservesContentDuringLiveResize@
preservesContentDuringLiveResize :: IsNSView nsView => nsView -> IO Bool
preservesContentDuringLiveResize nsView =
  sendMessage nsView preservesContentDuringLiveResizeSelector

-- | @- rectPreservedDuringLiveResize@
rectPreservedDuringLiveResize :: IsNSView nsView => nsView -> IO NSRect
rectPreservedDuringLiveResize nsView =
  sendMessage nsView rectPreservedDuringLiveResizeSelector

-- | @- inputContext@
inputContext :: IsNSView nsView => nsView -> IO (Id NSTextInputContext)
inputContext nsView =
  sendMessage nsView inputContextSelector

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSView nsView => nsView -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsView =
  sendMessage nsView userInterfaceLayoutDirectionSelector

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSView nsView => nsView -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsView value =
  sendMessage nsView setUserInterfaceLayoutDirectionSelector value

-- | @+ compatibleWithResponsiveScrolling@
compatibleWithResponsiveScrolling :: IO Bool
compatibleWithResponsiveScrolling  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMessage cls' compatibleWithResponsiveScrollingSelector

-- | @- preparedContentRect@
preparedContentRect :: IsNSView nsView => nsView -> IO NSRect
preparedContentRect nsView =
  sendMessage nsView preparedContentRectSelector

-- | @- setPreparedContentRect:@
setPreparedContentRect :: IsNSView nsView => nsView -> NSRect -> IO ()
setPreparedContentRect nsView value =
  sendMessage nsView setPreparedContentRectSelector value

-- | @- allowsVibrancy@
allowsVibrancy :: IsNSView nsView => nsView -> IO Bool
allowsVibrancy nsView =
  sendMessage nsView allowsVibrancySelector

-- | @- pressureConfiguration@
pressureConfiguration :: IsNSView nsView => nsView -> IO (Id NSPressureConfiguration)
pressureConfiguration nsView =
  sendMessage nsView pressureConfigurationSelector

-- | @- setPressureConfiguration:@
setPressureConfiguration :: (IsNSView nsView, IsNSPressureConfiguration value) => nsView -> value -> IO ()
setPressureConfiguration nsView value =
  sendMessage nsView setPressureConfigurationSelector (toNSPressureConfiguration value)

-- | @- wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurface :: IsNSView nsView => nsView -> IO Bool
wantsExtendedDynamicRangeOpenGLSurface nsView =
  sendMessage nsView wantsExtendedDynamicRangeOpenGLSurfaceSelector

-- | @- setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurface :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsExtendedDynamicRangeOpenGLSurface nsView value =
  sendMessage nsView setWantsExtendedDynamicRangeOpenGLSurfaceSelector value

-- | @- wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurface :: IsNSView nsView => nsView -> IO Bool
wantsBestResolutionOpenGLSurface nsView =
  sendMessage nsView wantsBestResolutionOpenGLSurfaceSelector

-- | @- setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurface :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsBestResolutionOpenGLSurface nsView value =
  sendMessage nsView setWantsBestResolutionOpenGLSurfaceSelector value

-- | @- layoutGuides@
layoutGuides :: IsNSView nsView => nsView -> IO (Id NSArray)
layoutGuides nsView =
  sendMessage nsView layoutGuidesSelector

-- | @- hasAmbiguousLayout@
hasAmbiguousLayout :: IsNSView nsView => nsView -> IO Bool
hasAmbiguousLayout nsView =
  sendMessage nsView hasAmbiguousLayoutSelector

-- | @- fittingSize@
fittingSize :: IsNSView nsView => nsView -> IO NSSize
fittingSize nsView =
  sendMessage nsView fittingSizeSelector

-- | @- alignmentRectInsets@
alignmentRectInsets :: IsNSView nsView => nsView -> IO NSEdgeInsets
alignmentRectInsets nsView =
  sendMessage nsView alignmentRectInsetsSelector

-- | @- firstBaselineOffsetFromTop@
firstBaselineOffsetFromTop :: IsNSView nsView => nsView -> IO CDouble
firstBaselineOffsetFromTop nsView =
  sendMessage nsView firstBaselineOffsetFromTopSelector

-- | @- lastBaselineOffsetFromBottom@
lastBaselineOffsetFromBottom :: IsNSView nsView => nsView -> IO CDouble
lastBaselineOffsetFromBottom nsView =
  sendMessage nsView lastBaselineOffsetFromBottomSelector

-- | @- baselineOffsetFromBottom@
baselineOffsetFromBottom :: IsNSView nsView => nsView -> IO CDouble
baselineOffsetFromBottom nsView =
  sendMessage nsView baselineOffsetFromBottomSelector

-- | @- intrinsicContentSize@
intrinsicContentSize :: IsNSView nsView => nsView -> IO NSSize
intrinsicContentSize nsView =
  sendMessage nsView intrinsicContentSizeSelector

-- | @- horizontalContentSizeConstraintActive@
horizontalContentSizeConstraintActive :: IsNSView nsView => nsView -> IO Bool
horizontalContentSizeConstraintActive nsView =
  sendMessage nsView horizontalContentSizeConstraintActiveSelector

-- | @- setHorizontalContentSizeConstraintActive:@
setHorizontalContentSizeConstraintActive :: IsNSView nsView => nsView -> Bool -> IO ()
setHorizontalContentSizeConstraintActive nsView value =
  sendMessage nsView setHorizontalContentSizeConstraintActiveSelector value

-- | @- verticalContentSizeConstraintActive@
verticalContentSizeConstraintActive :: IsNSView nsView => nsView -> IO Bool
verticalContentSizeConstraintActive nsView =
  sendMessage nsView verticalContentSizeConstraintActiveSelector

-- | @- setVerticalContentSizeConstraintActive:@
setVerticalContentSizeConstraintActive :: IsNSView nsView => nsView -> Bool -> IO ()
setVerticalContentSizeConstraintActive nsView value =
  sendMessage nsView setVerticalContentSizeConstraintActiveSelector value

-- | @- translatesAutoresizingMaskIntoConstraints@
translatesAutoresizingMaskIntoConstraints :: IsNSView nsView => nsView -> IO Bool
translatesAutoresizingMaskIntoConstraints nsView =
  sendMessage nsView translatesAutoresizingMaskIntoConstraintsSelector

-- | @- setTranslatesAutoresizingMaskIntoConstraints:@
setTranslatesAutoresizingMaskIntoConstraints :: IsNSView nsView => nsView -> Bool -> IO ()
setTranslatesAutoresizingMaskIntoConstraints nsView value =
  sendMessage nsView setTranslatesAutoresizingMaskIntoConstraintsSelector value

-- | @+ requiresConstraintBasedLayout@
requiresConstraintBasedLayout :: IO Bool
requiresConstraintBasedLayout  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMessage cls' requiresConstraintBasedLayoutSelector

-- | @- needsUpdateConstraints@
needsUpdateConstraints :: IsNSView nsView => nsView -> IO Bool
needsUpdateConstraints nsView =
  sendMessage nsView needsUpdateConstraintsSelector

-- | @- setNeedsUpdateConstraints:@
setNeedsUpdateConstraints :: IsNSView nsView => nsView -> Bool -> IO ()
setNeedsUpdateConstraints nsView value =
  sendMessage nsView setNeedsUpdateConstraintsSelector value

-- | @- leadingAnchor@
leadingAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
leadingAnchor nsView =
  sendMessage nsView leadingAnchorSelector

-- | @- trailingAnchor@
trailingAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
trailingAnchor nsView =
  sendMessage nsView trailingAnchorSelector

-- | @- leftAnchor@
leftAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
leftAnchor nsView =
  sendMessage nsView leftAnchorSelector

-- | @- rightAnchor@
rightAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
rightAnchor nsView =
  sendMessage nsView rightAnchorSelector

-- | @- topAnchor@
topAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
topAnchor nsView =
  sendMessage nsView topAnchorSelector

-- | @- bottomAnchor@
bottomAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
bottomAnchor nsView =
  sendMessage nsView bottomAnchorSelector

-- | @- widthAnchor@
widthAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutDimension)
widthAnchor nsView =
  sendMessage nsView widthAnchorSelector

-- | @- heightAnchor@
heightAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutDimension)
heightAnchor nsView =
  sendMessage nsView heightAnchorSelector

-- | @- centerXAnchor@
centerXAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
centerXAnchor nsView =
  sendMessage nsView centerXAnchorSelector

-- | @- centerYAnchor@
centerYAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
centerYAnchor nsView =
  sendMessage nsView centerYAnchorSelector

-- | @- firstBaselineAnchor@
firstBaselineAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
firstBaselineAnchor nsView =
  sendMessage nsView firstBaselineAnchorSelector

-- | @- lastBaselineAnchor@
lastBaselineAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
lastBaselineAnchor nsView =
  sendMessage nsView lastBaselineAnchorSelector

-- | @- constraints@
constraints :: IsNSView nsView => nsView -> IO (Id NSArray)
constraints nsView =
  sendMessage nsView constraintsSelector

-- | @- candidateListTouchBarItem@
candidateListTouchBarItem :: IsNSView nsView => nsView -> IO (Id NSCandidateListTouchBarItem)
candidateListTouchBarItem nsView =
  sendMessage nsView candidateListTouchBarItemSelector

-- | @- enclosingMenuItem@
enclosingMenuItem :: IsNSView nsView => nsView -> IO (Id NSMenuItem)
enclosingMenuItem nsView =
  sendMessage nsView enclosingMenuItemSelector

-- | @- writingToolsCoordinator@
writingToolsCoordinator :: IsNSView nsView => nsView -> IO (Id NSWritingToolsCoordinator)
writingToolsCoordinator nsView =
  sendMessage nsView writingToolsCoordinatorSelector

-- | @- setWritingToolsCoordinator:@
setWritingToolsCoordinator :: (IsNSView nsView, IsNSWritingToolsCoordinator value) => nsView -> value -> IO ()
setWritingToolsCoordinator nsView value =
  sendMessage nsView setWritingToolsCoordinatorSelector (toNSWritingToolsCoordinator value)

-- | @- trackingAreas@
trackingAreas :: IsNSView nsView => nsView -> IO (Id NSArray)
trackingAreas nsView =
  sendMessage nsView trackingAreasSelector

-- | When this property is true, any NSControls in the view or its descendants will be sized with compact metrics compatible with macOS 15 and earlier. Defaults to false
--
-- ObjC selector: @- prefersCompactControlSizeMetrics@
prefersCompactControlSizeMetrics :: IsNSView nsView => nsView -> IO Bool
prefersCompactControlSizeMetrics nsView =
  sendMessage nsView prefersCompactControlSizeMetricsSelector

-- | When this property is true, any NSControls in the view or its descendants will be sized with compact metrics compatible with macOS 15 and earlier. Defaults to false
--
-- ObjC selector: @- setPrefersCompactControlSizeMetrics:@
setPrefersCompactControlSizeMetrics :: IsNSView nsView => nsView -> Bool -> IO ()
setPrefersCompactControlSizeMetrics nsView value =
  sendMessage nsView setPrefersCompactControlSizeMetricsSelector value

-- | @- safeAreaInsets@
safeAreaInsets :: IsNSView nsView => nsView -> IO NSEdgeInsets
safeAreaInsets nsView =
  sendMessage nsView safeAreaInsetsSelector

-- | @- additionalSafeAreaInsets@
additionalSafeAreaInsets :: IsNSView nsView => nsView -> IO NSEdgeInsets
additionalSafeAreaInsets nsView =
  sendMessage nsView additionalSafeAreaInsetsSelector

-- | @- setAdditionalSafeAreaInsets:@
setAdditionalSafeAreaInsets :: IsNSView nsView => nsView -> NSEdgeInsets -> IO ()
setAdditionalSafeAreaInsets nsView value =
  sendMessage nsView setAdditionalSafeAreaInsetsSelector value

-- | @- safeAreaLayoutGuide@
safeAreaLayoutGuide :: IsNSView nsView => nsView -> IO (Id NSLayoutGuide)
safeAreaLayoutGuide nsView =
  sendMessage nsView safeAreaLayoutGuideSelector

-- | @- safeAreaRect@
safeAreaRect :: IsNSView nsView => nsView -> IO NSRect
safeAreaRect nsView =
  sendMessage nsView safeAreaRectSelector

-- | @- layoutMarginsGuide@
layoutMarginsGuide :: IsNSView nsView => nsView -> IO (Id NSLayoutGuide)
layoutMarginsGuide nsView =
  sendMessage nsView layoutMarginsGuideSelector

-- | @- allowedTouchTypes@
allowedTouchTypes :: IsNSView nsView => nsView -> IO NSTouchTypeMask
allowedTouchTypes nsView =
  sendMessage nsView allowedTouchTypesSelector

-- | @- setAllowedTouchTypes:@
setAllowedTouchTypes :: IsNSView nsView => nsView -> NSTouchTypeMask -> IO ()
setAllowedTouchTypes nsView value =
  sendMessage nsView setAllowedTouchTypesSelector value

-- | @- gestureRecognizers@
gestureRecognizers :: IsNSView nsView => nsView -> IO (Id NSArray)
gestureRecognizers nsView =
  sendMessage nsView gestureRecognizersSelector

-- | @- setGestureRecognizers:@
setGestureRecognizers :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setGestureRecognizers nsView value =
  sendMessage nsView setGestureRecognizersSelector (toNSArray value)

-- | @- drawingFindIndicator@
drawingFindIndicator :: IsNSView nsView => nsView -> IO Bool
drawingFindIndicator nsView =
  sendMessage nsView drawingFindIndicatorSelector

-- | @- inFullScreenMode@
inFullScreenMode :: IsNSView nsView => nsView -> IO Bool
inFullScreenMode nsView =
  sendMessage nsView inFullScreenModeSelector

-- | @- registeredDraggedTypes@
registeredDraggedTypes :: IsNSView nsView => nsView -> IO (Id NSArray)
registeredDraggedTypes nsView =
  sendMessage nsView registeredDraggedTypesSelector

-- | @- heightAdjustLimit@
heightAdjustLimit :: IsNSView nsView => nsView -> IO CDouble
heightAdjustLimit nsView =
  sendMessage nsView heightAdjustLimitSelector

-- | @- widthAdjustLimit@
widthAdjustLimit :: IsNSView nsView => nsView -> IO CDouble
widthAdjustLimit nsView =
  sendMessage nsView widthAdjustLimitSelector

-- | @- pageHeader@
pageHeader :: IsNSView nsView => nsView -> IO (Id NSAttributedString)
pageHeader nsView =
  sendMessage nsView pageHeaderSelector

-- | @- pageFooter@
pageFooter :: IsNSView nsView => nsView -> IO (Id NSAttributedString)
pageFooter nsView =
  sendMessage nsView pageFooterSelector

-- | @- printJobTitle@
printJobTitle :: IsNSView nsView => nsView -> IO (Id NSString)
printJobTitle nsView =
  sendMessage nsView printJobTitleSelector

-- | @- nextKeyView@
nextKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
nextKeyView nsView =
  sendMessage nsView nextKeyViewSelector

-- | @- setNextKeyView:@
setNextKeyView :: (IsNSView nsView, IsNSView value) => nsView -> value -> IO ()
setNextKeyView nsView value =
  sendMessage nsView setNextKeyViewSelector (toNSView value)

-- | @- previousKeyView@
previousKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
previousKeyView nsView =
  sendMessage nsView previousKeyViewSelector

-- | @- nextValidKeyView@
nextValidKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
nextValidKeyView nsView =
  sendMessage nsView nextValidKeyViewSelector

-- | @- previousValidKeyView@
previousValidKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
previousValidKeyView nsView =
  sendMessage nsView previousValidKeyViewSelector

-- | @- canBecomeKeyView@
canBecomeKeyView :: IsNSView nsView => nsView -> IO Bool
canBecomeKeyView nsView =
  sendMessage nsView canBecomeKeyViewSelector

-- | @- focusRingType@
focusRingType :: IsNSView nsView => nsView -> IO NSFocusRingType
focusRingType nsView =
  sendMessage nsView focusRingTypeSelector

-- | @- setFocusRingType:@
setFocusRingType :: IsNSView nsView => nsView -> NSFocusRingType -> IO ()
setFocusRingType nsView value =
  sendMessage nsView setFocusRingTypeSelector value

-- | @+ defaultFocusRingType@
defaultFocusRingType :: IO NSFocusRingType
defaultFocusRingType  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMessage cls' defaultFocusRingTypeSelector

-- | @- focusRingMaskBounds@
focusRingMaskBounds :: IsNSView nsView => nsView -> IO NSRect
focusRingMaskBounds nsView =
  sendMessage nsView focusRingMaskBoundsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSView)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @isDescendantOf:@
isDescendantOfSelector :: Selector '[Id NSView] Bool
isDescendantOfSelector = mkSelector "isDescendantOf:"

-- | @Selector@ for @ancestorSharedWithView:@
ancestorSharedWithViewSelector :: Selector '[Id NSView] (Id NSView)
ancestorSharedWithViewSelector = mkSelector "ancestorSharedWithView:"

-- | @Selector@ for @getRectsBeingDrawn:count:@
getRectsBeingDrawn_countSelector :: Selector '[Const (Ptr NSRect), Ptr CLong] ()
getRectsBeingDrawn_countSelector = mkSelector "getRectsBeingDrawn:count:"

-- | @Selector@ for @needsToDrawRect:@
needsToDrawRectSelector :: Selector '[NSRect] Bool
needsToDrawRectSelector = mkSelector "needsToDrawRect:"

-- | @Selector@ for @viewDidHide@
viewDidHideSelector :: Selector '[] ()
viewDidHideSelector = mkSelector "viewDidHide"

-- | @Selector@ for @viewDidUnhide@
viewDidUnhideSelector :: Selector '[] ()
viewDidUnhideSelector = mkSelector "viewDidUnhide"

-- | @Selector@ for @addSubview:@
addSubviewSelector :: Selector '[Id NSView] ()
addSubviewSelector = mkSelector "addSubview:"

-- | @Selector@ for @addSubview:positioned:relativeTo:@
addSubview_positioned_relativeToSelector :: Selector '[Id NSView, NSWindowOrderingMode, Id NSView] ()
addSubview_positioned_relativeToSelector = mkSelector "addSubview:positioned:relativeTo:"

-- | @Selector@ for @sortSubviewsUsingFunction:context:@
sortSubviewsUsingFunction_contextSelector :: Selector '[Ptr (), Ptr ()] ()
sortSubviewsUsingFunction_contextSelector = mkSelector "sortSubviewsUsingFunction:context:"

-- | @Selector@ for @viewWillMoveToWindow:@
viewWillMoveToWindowSelector :: Selector '[Id NSWindow] ()
viewWillMoveToWindowSelector = mkSelector "viewWillMoveToWindow:"

-- | @Selector@ for @viewDidMoveToWindow@
viewDidMoveToWindowSelector :: Selector '[] ()
viewDidMoveToWindowSelector = mkSelector "viewDidMoveToWindow"

-- | @Selector@ for @viewWillMoveToSuperview:@
viewWillMoveToSuperviewSelector :: Selector '[Id NSView] ()
viewWillMoveToSuperviewSelector = mkSelector "viewWillMoveToSuperview:"

-- | @Selector@ for @viewDidMoveToSuperview@
viewDidMoveToSuperviewSelector :: Selector '[] ()
viewDidMoveToSuperviewSelector = mkSelector "viewDidMoveToSuperview"

-- | @Selector@ for @didAddSubview:@
didAddSubviewSelector :: Selector '[Id NSView] ()
didAddSubviewSelector = mkSelector "didAddSubview:"

-- | @Selector@ for @willRemoveSubview:@
willRemoveSubviewSelector :: Selector '[Id NSView] ()
willRemoveSubviewSelector = mkSelector "willRemoveSubview:"

-- | @Selector@ for @removeFromSuperview@
removeFromSuperviewSelector :: Selector '[] ()
removeFromSuperviewSelector = mkSelector "removeFromSuperview"

-- | @Selector@ for @replaceSubview:with:@
replaceSubview_withSelector :: Selector '[Id NSView, Id NSView] ()
replaceSubview_withSelector = mkSelector "replaceSubview:with:"

-- | @Selector@ for @removeFromSuperviewWithoutNeedingDisplay@
removeFromSuperviewWithoutNeedingDisplaySelector :: Selector '[] ()
removeFromSuperviewWithoutNeedingDisplaySelector = mkSelector "removeFromSuperviewWithoutNeedingDisplay"

-- | @Selector@ for @viewDidChangeBackingProperties@
viewDidChangeBackingPropertiesSelector :: Selector '[] ()
viewDidChangeBackingPropertiesSelector = mkSelector "viewDidChangeBackingProperties"

-- | @Selector@ for @resizeSubviewsWithOldSize:@
resizeSubviewsWithOldSizeSelector :: Selector '[NSSize] ()
resizeSubviewsWithOldSizeSelector = mkSelector "resizeSubviewsWithOldSize:"

-- | @Selector@ for @resizeWithOldSuperviewSize:@
resizeWithOldSuperviewSizeSelector :: Selector '[NSSize] ()
resizeWithOldSuperviewSizeSelector = mkSelector "resizeWithOldSuperviewSize:"

-- | @Selector@ for @setFrameOrigin:@
setFrameOriginSelector :: Selector '[NSPoint] ()
setFrameOriginSelector = mkSelector "setFrameOrigin:"

-- | @Selector@ for @setFrameSize:@
setFrameSizeSelector :: Selector '[NSSize] ()
setFrameSizeSelector = mkSelector "setFrameSize:"

-- | @Selector@ for @setBoundsOrigin:@
setBoundsOriginSelector :: Selector '[NSPoint] ()
setBoundsOriginSelector = mkSelector "setBoundsOrigin:"

-- | @Selector@ for @setBoundsSize:@
setBoundsSizeSelector :: Selector '[NSSize] ()
setBoundsSizeSelector = mkSelector "setBoundsSize:"

-- | @Selector@ for @translateOriginToPoint:@
translateOriginToPointSelector :: Selector '[NSPoint] ()
translateOriginToPointSelector = mkSelector "translateOriginToPoint:"

-- | @Selector@ for @scaleUnitSquareToSize:@
scaleUnitSquareToSizeSelector :: Selector '[NSSize] ()
scaleUnitSquareToSizeSelector = mkSelector "scaleUnitSquareToSize:"

-- | @Selector@ for @rotateByAngle:@
rotateByAngleSelector :: Selector '[CDouble] ()
rotateByAngleSelector = mkSelector "rotateByAngle:"

-- | @Selector@ for @convertPoint:fromView:@
convertPoint_fromViewSelector :: Selector '[NSPoint, Id NSView] NSPoint
convertPoint_fromViewSelector = mkSelector "convertPoint:fromView:"

-- | @Selector@ for @convertPoint:toView:@
convertPoint_toViewSelector :: Selector '[NSPoint, Id NSView] NSPoint
convertPoint_toViewSelector = mkSelector "convertPoint:toView:"

-- | @Selector@ for @convertSize:fromView:@
convertSize_fromViewSelector :: Selector '[NSSize, Id NSView] NSSize
convertSize_fromViewSelector = mkSelector "convertSize:fromView:"

-- | @Selector@ for @convertSize:toView:@
convertSize_toViewSelector :: Selector '[NSSize, Id NSView] NSSize
convertSize_toViewSelector = mkSelector "convertSize:toView:"

-- | @Selector@ for @convertRect:fromView:@
convertRect_fromViewSelector :: Selector '[NSRect, Id NSView] NSRect
convertRect_fromViewSelector = mkSelector "convertRect:fromView:"

-- | @Selector@ for @convertRect:toView:@
convertRect_toViewSelector :: Selector '[NSRect, Id NSView] NSRect
convertRect_toViewSelector = mkSelector "convertRect:toView:"

-- | @Selector@ for @backingAlignedRect:options:@
backingAlignedRect_optionsSelector :: Selector '[NSRect, NSAlignmentOptions] NSRect
backingAlignedRect_optionsSelector = mkSelector "backingAlignedRect:options:"

-- | @Selector@ for @centerScanRect:@
centerScanRectSelector :: Selector '[NSRect] NSRect
centerScanRectSelector = mkSelector "centerScanRect:"

-- | @Selector@ for @convertPointToBacking:@
convertPointToBackingSelector :: Selector '[NSPoint] NSPoint
convertPointToBackingSelector = mkSelector "convertPointToBacking:"

-- | @Selector@ for @convertPointFromBacking:@
convertPointFromBackingSelector :: Selector '[NSPoint] NSPoint
convertPointFromBackingSelector = mkSelector "convertPointFromBacking:"

-- | @Selector@ for @convertSizeToBacking:@
convertSizeToBackingSelector :: Selector '[NSSize] NSSize
convertSizeToBackingSelector = mkSelector "convertSizeToBacking:"

-- | @Selector@ for @convertSizeFromBacking:@
convertSizeFromBackingSelector :: Selector '[NSSize] NSSize
convertSizeFromBackingSelector = mkSelector "convertSizeFromBacking:"

-- | @Selector@ for @convertRectToBacking:@
convertRectToBackingSelector :: Selector '[NSRect] NSRect
convertRectToBackingSelector = mkSelector "convertRectToBacking:"

-- | @Selector@ for @convertRectFromBacking:@
convertRectFromBackingSelector :: Selector '[NSRect] NSRect
convertRectFromBackingSelector = mkSelector "convertRectFromBacking:"

-- | @Selector@ for @convertPointToLayer:@
convertPointToLayerSelector :: Selector '[NSPoint] NSPoint
convertPointToLayerSelector = mkSelector "convertPointToLayer:"

-- | @Selector@ for @convertPointFromLayer:@
convertPointFromLayerSelector :: Selector '[NSPoint] NSPoint
convertPointFromLayerSelector = mkSelector "convertPointFromLayer:"

-- | @Selector@ for @convertSizeToLayer:@
convertSizeToLayerSelector :: Selector '[NSSize] NSSize
convertSizeToLayerSelector = mkSelector "convertSizeToLayer:"

-- | @Selector@ for @convertSizeFromLayer:@
convertSizeFromLayerSelector :: Selector '[NSSize] NSSize
convertSizeFromLayerSelector = mkSelector "convertSizeFromLayer:"

-- | @Selector@ for @convertRectToLayer:@
convertRectToLayerSelector :: Selector '[NSRect] NSRect
convertRectToLayerSelector = mkSelector "convertRectToLayer:"

-- | @Selector@ for @convertRectFromLayer:@
convertRectFromLayerSelector :: Selector '[NSRect] NSRect
convertRectFromLayerSelector = mkSelector "convertRectFromLayer:"

-- | @Selector@ for @setNeedsDisplayInRect:@
setNeedsDisplayInRectSelector :: Selector '[NSRect] ()
setNeedsDisplayInRectSelector = mkSelector "setNeedsDisplayInRect:"

-- | @Selector@ for @lockFocus@
lockFocusSelector :: Selector '[] ()
lockFocusSelector = mkSelector "lockFocus"

-- | @Selector@ for @unlockFocus@
unlockFocusSelector :: Selector '[] ()
unlockFocusSelector = mkSelector "unlockFocus"

-- | @Selector@ for @lockFocusIfCanDraw@
lockFocusIfCanDrawSelector :: Selector '[] Bool
lockFocusIfCanDrawSelector = mkSelector "lockFocusIfCanDraw"

-- | @Selector@ for @lockFocusIfCanDrawInContext:@
lockFocusIfCanDrawInContextSelector :: Selector '[Id NSGraphicsContext] Bool
lockFocusIfCanDrawInContextSelector = mkSelector "lockFocusIfCanDrawInContext:"

-- | @Selector@ for @display@
displaySelector :: Selector '[] ()
displaySelector = mkSelector "display"

-- | @Selector@ for @displayIfNeeded@
displayIfNeededSelector :: Selector '[] ()
displayIfNeededSelector = mkSelector "displayIfNeeded"

-- | @Selector@ for @displayIfNeededIgnoringOpacity@
displayIfNeededIgnoringOpacitySelector :: Selector '[] ()
displayIfNeededIgnoringOpacitySelector = mkSelector "displayIfNeededIgnoringOpacity"

-- | @Selector@ for @displayRect:@
displayRectSelector :: Selector '[NSRect] ()
displayRectSelector = mkSelector "displayRect:"

-- | @Selector@ for @displayIfNeededInRect:@
displayIfNeededInRectSelector :: Selector '[NSRect] ()
displayIfNeededInRectSelector = mkSelector "displayIfNeededInRect:"

-- | @Selector@ for @displayRectIgnoringOpacity:@
displayRectIgnoringOpacitySelector :: Selector '[NSRect] ()
displayRectIgnoringOpacitySelector = mkSelector "displayRectIgnoringOpacity:"

-- | @Selector@ for @displayIfNeededInRectIgnoringOpacity:@
displayIfNeededInRectIgnoringOpacitySelector :: Selector '[NSRect] ()
displayIfNeededInRectIgnoringOpacitySelector = mkSelector "displayIfNeededInRectIgnoringOpacity:"

-- | @Selector@ for @drawRect:@
drawRectSelector :: Selector '[NSRect] ()
drawRectSelector = mkSelector "drawRect:"

-- | @Selector@ for @displayRectIgnoringOpacity:inContext:@
displayRectIgnoringOpacity_inContextSelector :: Selector '[NSRect, Id NSGraphicsContext] ()
displayRectIgnoringOpacity_inContextSelector = mkSelector "displayRectIgnoringOpacity:inContext:"

-- | @Selector@ for @bitmapImageRepForCachingDisplayInRect:@
bitmapImageRepForCachingDisplayInRectSelector :: Selector '[NSRect] (Id NSBitmapImageRep)
bitmapImageRepForCachingDisplayInRectSelector = mkSelector "bitmapImageRepForCachingDisplayInRect:"

-- | @Selector@ for @cacheDisplayInRect:toBitmapImageRep:@
cacheDisplayInRect_toBitmapImageRepSelector :: Selector '[NSRect, Id NSBitmapImageRep] ()
cacheDisplayInRect_toBitmapImageRepSelector = mkSelector "cacheDisplayInRect:toBitmapImageRep:"

-- | @Selector@ for @viewWillDraw@
viewWillDrawSelector :: Selector '[] ()
viewWillDrawSelector = mkSelector "viewWillDraw"

-- | @Selector@ for @scrollPoint:@
scrollPointSelector :: Selector '[NSPoint] ()
scrollPointSelector = mkSelector "scrollPoint:"

-- | @Selector@ for @scrollRectToVisible:@
scrollRectToVisibleSelector :: Selector '[NSRect] Bool
scrollRectToVisibleSelector = mkSelector "scrollRectToVisible:"

-- | @Selector@ for @autoscroll:@
autoscrollSelector :: Selector '[Id NSEvent] Bool
autoscrollSelector = mkSelector "autoscroll:"

-- | @Selector@ for @adjustScroll:@
adjustScrollSelector :: Selector '[NSRect] NSRect
adjustScrollSelector = mkSelector "adjustScroll:"

-- | @Selector@ for @scrollRect:by:@
scrollRect_bySelector :: Selector '[NSRect, NSSize] ()
scrollRect_bySelector = mkSelector "scrollRect:by:"

-- | @Selector@ for @translateRectsNeedingDisplayInRect:by:@
translateRectsNeedingDisplayInRect_bySelector :: Selector '[NSRect, NSSize] ()
translateRectsNeedingDisplayInRect_bySelector = mkSelector "translateRectsNeedingDisplayInRect:by:"

-- | @Selector@ for @hitTest:@
hitTestSelector :: Selector '[NSPoint] (Id NSView)
hitTestSelector = mkSelector "hitTest:"

-- | @Selector@ for @mouse:inRect:@
mouse_inRectSelector :: Selector '[NSPoint, NSRect] Bool
mouse_inRectSelector = mkSelector "mouse:inRect:"

-- | @Selector@ for @viewWithTag:@
viewWithTagSelector :: Selector '[CLong] (Id NSView)
viewWithTagSelector = mkSelector "viewWithTag:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector '[Id NSEvent] Bool
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @acceptsFirstMouse:@
acceptsFirstMouseSelector :: Selector '[Id NSEvent] Bool
acceptsFirstMouseSelector = mkSelector "acceptsFirstMouse:"

-- | @Selector@ for @shouldDelayWindowOrderingForEvent:@
shouldDelayWindowOrderingForEventSelector :: Selector '[Id NSEvent] Bool
shouldDelayWindowOrderingForEventSelector = mkSelector "shouldDelayWindowOrderingForEvent:"

-- | @Selector@ for @makeBackingLayer@
makeBackingLayerSelector :: Selector '[] (Id CALayer)
makeBackingLayerSelector = mkSelector "makeBackingLayer"

-- | @Selector@ for @updateLayer@
updateLayerSelector :: Selector '[] ()
updateLayerSelector = mkSelector "updateLayer"

-- | @Selector@ for @layoutSubtreeIfNeeded@
layoutSubtreeIfNeededSelector :: Selector '[] ()
layoutSubtreeIfNeededSelector = mkSelector "layoutSubtreeIfNeeded"

-- | @Selector@ for @layout@
layoutSelector :: Selector '[] ()
layoutSelector = mkSelector "layout"

-- | @Selector@ for @menuForEvent:@
menuForEventSelector :: Selector '[Id NSEvent] (Id NSMenu)
menuForEventSelector = mkSelector "menuForEvent:"

-- | @Selector@ for @willOpenMenu:withEvent:@
willOpenMenu_withEventSelector :: Selector '[Id NSMenu, Id NSEvent] ()
willOpenMenu_withEventSelector = mkSelector "willOpenMenu:withEvent:"

-- | @Selector@ for @didCloseMenu:withEvent:@
didCloseMenu_withEventSelector :: Selector '[Id NSMenu, Id NSEvent] ()
didCloseMenu_withEventSelector = mkSelector "didCloseMenu:withEvent:"

-- | @Selector@ for @addToolTipRect:owner:userData:@
addToolTipRect_owner_userDataSelector :: Selector '[NSRect, RawId, Ptr ()] CLong
addToolTipRect_owner_userDataSelector = mkSelector "addToolTipRect:owner:userData:"

-- | @Selector@ for @removeToolTip:@
removeToolTipSelector :: Selector '[CLong] ()
removeToolTipSelector = mkSelector "removeToolTip:"

-- | @Selector@ for @removeAllToolTips@
removeAllToolTipsSelector :: Selector '[] ()
removeAllToolTipsSelector = mkSelector "removeAllToolTips"

-- | @Selector@ for @viewWillStartLiveResize@
viewWillStartLiveResizeSelector :: Selector '[] ()
viewWillStartLiveResizeSelector = mkSelector "viewWillStartLiveResize"

-- | @Selector@ for @viewDidEndLiveResize@
viewDidEndLiveResizeSelector :: Selector '[] ()
viewDidEndLiveResizeSelector = mkSelector "viewDidEndLiveResize"

-- | @Selector@ for @getRectsExposedDuringLiveResize:count:@
getRectsExposedDuringLiveResize_countSelector :: Selector '[Ptr NSRect, Ptr CLong] ()
getRectsExposedDuringLiveResize_countSelector = mkSelector "getRectsExposedDuringLiveResize:count:"

-- | @Selector@ for @rectForSmartMagnificationAtPoint:inRect:@
rectForSmartMagnificationAtPoint_inRectSelector :: Selector '[NSPoint, NSRect] NSRect
rectForSmartMagnificationAtPoint_inRectSelector = mkSelector "rectForSmartMagnificationAtPoint:inRect:"

-- | @Selector@ for @prepareForReuse@
prepareForReuseSelector :: Selector '[] ()
prepareForReuseSelector = mkSelector "prepareForReuse"

-- | @Selector@ for @prepareContentInRect:@
prepareContentInRectSelector :: Selector '[NSRect] ()
prepareContentInRectSelector = mkSelector "prepareContentInRect:"

-- | @Selector@ for @viewDidChangeEffectiveAppearance@
viewDidChangeEffectiveAppearanceSelector :: Selector '[] ()
viewDidChangeEffectiveAppearanceSelector = mkSelector "viewDidChangeEffectiveAppearance"

-- | @Selector@ for @rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] Bool
rulerView_shouldMoveMarkerSelector = mkSelector "rulerView:shouldMoveMarker:"

-- | @Selector@ for @rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocationSelector :: Selector '[Id NSRulerView, Id NSRulerMarker, CDouble] CDouble
rulerView_willMoveMarker_toLocationSelector = mkSelector "rulerView:willMoveMarker:toLocation:"

-- | @Selector@ for @rulerView:didMoveMarker:@
rulerView_didMoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] ()
rulerView_didMoveMarkerSelector = mkSelector "rulerView:didMoveMarker:"

-- | @Selector@ for @rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] Bool
rulerView_shouldRemoveMarkerSelector = mkSelector "rulerView:shouldRemoveMarker:"

-- | @Selector@ for @rulerView:didRemoveMarker:@
rulerView_didRemoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] ()
rulerView_didRemoveMarkerSelector = mkSelector "rulerView:didRemoveMarker:"

-- | @Selector@ for @rulerView:shouldAddMarker:@
rulerView_shouldAddMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] Bool
rulerView_shouldAddMarkerSelector = mkSelector "rulerView:shouldAddMarker:"

-- | @Selector@ for @rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocationSelector :: Selector '[Id NSRulerView, Id NSRulerMarker, CDouble] CDouble
rulerView_willAddMarker_atLocationSelector = mkSelector "rulerView:willAddMarker:atLocation:"

-- | @Selector@ for @rulerView:didAddMarker:@
rulerView_didAddMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] ()
rulerView_didAddMarkerSelector = mkSelector "rulerView:didAddMarker:"

-- | @Selector@ for @rulerView:handleMouseDown:@
rulerView_handleMouseDownSelector :: Selector '[Id NSRulerView, Id NSEvent] ()
rulerView_handleMouseDownSelector = mkSelector "rulerView:handleMouseDown:"

-- | @Selector@ for @rulerView:willSetClientView:@
rulerView_willSetClientViewSelector :: Selector '[Id NSRulerView, Id NSView] ()
rulerView_willSetClientViewSelector = mkSelector "rulerView:willSetClientView:"

-- | @Selector@ for @rulerView:locationForPoint:@
rulerView_locationForPointSelector :: Selector '[Id NSRulerView, NSPoint] CDouble
rulerView_locationForPointSelector = mkSelector "rulerView:locationForPoint:"

-- | @Selector@ for @rulerView:pointForLocation:@
rulerView_pointForLocationSelector :: Selector '[Id NSRulerView, CDouble] NSPoint
rulerView_pointForLocationSelector = mkSelector "rulerView:pointForLocation:"

-- | @Selector@ for @layoutGuideForLayoutRegion:@
layoutGuideForLayoutRegionSelector :: Selector '[Id NSViewLayoutRegion] (Id NSLayoutGuide)
layoutGuideForLayoutRegionSelector = mkSelector "layoutGuideForLayoutRegion:"

-- | @Selector@ for @edgeInsetsForLayoutRegion:@
edgeInsetsForLayoutRegionSelector :: Selector '[Id NSViewLayoutRegion] NSEdgeInsets
edgeInsetsForLayoutRegionSelector = mkSelector "edgeInsetsForLayoutRegion:"

-- | @Selector@ for @rectForLayoutRegion:@
rectForLayoutRegionSelector :: Selector '[Id NSViewLayoutRegion] NSRect
rectForLayoutRegionSelector = mkSelector "rectForLayoutRegion:"

-- | @Selector@ for @addLayoutGuide:@
addLayoutGuideSelector :: Selector '[Id NSLayoutGuide] ()
addLayoutGuideSelector = mkSelector "addLayoutGuide:"

-- | @Selector@ for @removeLayoutGuide:@
removeLayoutGuideSelector :: Selector '[Id NSLayoutGuide] ()
removeLayoutGuideSelector = mkSelector "removeLayoutGuide:"

-- | @Selector@ for @constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] (Id NSArray)
constraintsAffectingLayoutForOrientationSelector = mkSelector "constraintsAffectingLayoutForOrientation:"

-- | @Selector@ for @exerciseAmbiguityInLayout@
exerciseAmbiguityInLayoutSelector :: Selector '[] ()
exerciseAmbiguityInLayoutSelector = mkSelector "exerciseAmbiguityInLayout"

-- | @Selector@ for @alignmentRectForFrame:@
alignmentRectForFrameSelector :: Selector '[NSRect] NSRect
alignmentRectForFrameSelector = mkSelector "alignmentRectForFrame:"

-- | @Selector@ for @frameForAlignmentRect:@
frameForAlignmentRectSelector :: Selector '[NSRect] NSRect
frameForAlignmentRectSelector = mkSelector "frameForAlignmentRect:"

-- | @Selector@ for @invalidateIntrinsicContentSize@
invalidateIntrinsicContentSizeSelector :: Selector '[] ()
invalidateIntrinsicContentSizeSelector = mkSelector "invalidateIntrinsicContentSize"

-- | @Selector@ for @contentHuggingPriorityForOrientation:@
contentHuggingPriorityForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] CFloat
contentHuggingPriorityForOrientationSelector = mkSelector "contentHuggingPriorityForOrientation:"

-- | @Selector@ for @setContentHuggingPriority:forOrientation:@
setContentHuggingPriority_forOrientationSelector :: Selector '[CFloat, NSLayoutConstraintOrientation] ()
setContentHuggingPriority_forOrientationSelector = mkSelector "setContentHuggingPriority:forOrientation:"

-- | @Selector@ for @contentCompressionResistancePriorityForOrientation:@
contentCompressionResistancePriorityForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] CFloat
contentCompressionResistancePriorityForOrientationSelector = mkSelector "contentCompressionResistancePriorityForOrientation:"

-- | @Selector@ for @setContentCompressionResistancePriority:forOrientation:@
setContentCompressionResistancePriority_forOrientationSelector :: Selector '[CFloat, NSLayoutConstraintOrientation] ()
setContentCompressionResistancePriority_forOrientationSelector = mkSelector "setContentCompressionResistancePriority:forOrientation:"

-- | @Selector@ for @updateConstraintsForSubtreeIfNeeded@
updateConstraintsForSubtreeIfNeededSelector :: Selector '[] ()
updateConstraintsForSubtreeIfNeededSelector = mkSelector "updateConstraintsForSubtreeIfNeeded"

-- | @Selector@ for @updateConstraints@
updateConstraintsSelector :: Selector '[] ()
updateConstraintsSelector = mkSelector "updateConstraints"

-- | @Selector@ for @addConstraint:@
addConstraintSelector :: Selector '[Id NSLayoutConstraint] ()
addConstraintSelector = mkSelector "addConstraint:"

-- | @Selector@ for @addConstraints:@
addConstraintsSelector :: Selector '[Id NSArray] ()
addConstraintsSelector = mkSelector "addConstraints:"

-- | @Selector@ for @removeConstraint:@
removeConstraintSelector :: Selector '[Id NSLayoutConstraint] ()
removeConstraintSelector = mkSelector "removeConstraint:"

-- | @Selector@ for @removeConstraints:@
removeConstraintsSelector :: Selector '[Id NSArray] ()
removeConstraintsSelector = mkSelector "removeConstraints:"

-- | @Selector@ for @reflectScrolledClipView:@
reflectScrolledClipViewSelector :: Selector '[Id NSClipView] ()
reflectScrolledClipViewSelector = mkSelector "reflectScrolledClipView:"

-- | @Selector@ for @scrollClipView:toPoint:@
scrollClipView_toPointSelector :: Selector '[Id NSClipView, NSPoint] ()
scrollClipView_toPointSelector = mkSelector "scrollClipView:toPoint:"

-- | @Selector@ for @dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBackSelector :: Selector '[Id NSImage, NSPoint, NSSize, Id NSEvent, Id NSPasteboard, RawId, Bool] ()
dragImage_at_offset_event_pasteboard_source_slideBackSelector = mkSelector "dragImage:at:offset:event:pasteboard:source:slideBack:"

-- | @Selector@ for @dragFile:fromRect:slideBack:event:@
dragFile_fromRect_slideBack_eventSelector :: Selector '[Id NSString, NSRect, Bool, Id NSEvent] Bool
dragFile_fromRect_slideBack_eventSelector = mkSelector "dragFile:fromRect:slideBack:event:"

-- | @Selector@ for @dragPromisedFilesOfTypes:fromRect:source:slideBack:event:@
dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector :: Selector '[Id NSArray, NSRect, RawId, Bool, Id NSEvent] Bool
dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector = mkSelector "dragPromisedFilesOfTypes:fromRect:source:slideBack:event:"

-- | @Selector@ for @convertPointToBase:@
convertPointToBaseSelector :: Selector '[NSPoint] NSPoint
convertPointToBaseSelector = mkSelector "convertPointToBase:"

-- | @Selector@ for @convertPointFromBase:@
convertPointFromBaseSelector :: Selector '[NSPoint] NSPoint
convertPointFromBaseSelector = mkSelector "convertPointFromBase:"

-- | @Selector@ for @convertSizeToBase:@
convertSizeToBaseSelector :: Selector '[NSSize] NSSize
convertSizeToBaseSelector = mkSelector "convertSizeToBase:"

-- | @Selector@ for @convertSizeFromBase:@
convertSizeFromBaseSelector :: Selector '[NSSize] NSSize
convertSizeFromBaseSelector = mkSelector "convertSizeFromBase:"

-- | @Selector@ for @convertRectToBase:@
convertRectToBaseSelector :: Selector '[NSRect] NSRect
convertRectToBaseSelector = mkSelector "convertRectToBase:"

-- | @Selector@ for @convertRectFromBase:@
convertRectFromBaseSelector :: Selector '[NSRect] NSRect
convertRectFromBaseSelector = mkSelector "convertRectFromBase:"

-- | @Selector@ for @performMnemonic:@
performMnemonicSelector :: Selector '[Id NSString] Bool
performMnemonicSelector = mkSelector "performMnemonic:"

-- | @Selector@ for @shouldDrawColor@
shouldDrawColorSelector :: Selector '[] Bool
shouldDrawColorSelector = mkSelector "shouldDrawColor"

-- | @Selector@ for @gState@
gStateSelector :: Selector '[] CLong
gStateSelector = mkSelector "gState"

-- | @Selector@ for @allocateGState@
allocateGStateSelector :: Selector '[] ()
allocateGStateSelector = mkSelector "allocateGState"

-- | @Selector@ for @releaseGState@
releaseGStateSelector :: Selector '[] ()
releaseGStateSelector = mkSelector "releaseGState"

-- | @Selector@ for @setUpGState@
setUpGStateSelector :: Selector '[] ()
setUpGStateSelector = mkSelector "setUpGState"

-- | @Selector@ for @renewGState@
renewGStateSelector :: Selector '[] ()
renewGStateSelector = mkSelector "renewGState"

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector '[RawId, Sel] (Id CADisplayLink)
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @addTrackingArea:@
addTrackingAreaSelector :: Selector '[Id NSTrackingArea] ()
addTrackingAreaSelector = mkSelector "addTrackingArea:"

-- | @Selector@ for @removeTrackingArea:@
removeTrackingAreaSelector :: Selector '[Id NSTrackingArea] ()
removeTrackingAreaSelector = mkSelector "removeTrackingArea:"

-- | @Selector@ for @updateTrackingAreas@
updateTrackingAreasSelector :: Selector '[] ()
updateTrackingAreasSelector = mkSelector "updateTrackingAreas"

-- | @Selector@ for @addCursorRect:cursor:@
addCursorRect_cursorSelector :: Selector '[NSRect, Id NSCursor] ()
addCursorRect_cursorSelector = mkSelector "addCursorRect:cursor:"

-- | @Selector@ for @removeCursorRect:cursor:@
removeCursorRect_cursorSelector :: Selector '[NSRect, Id NSCursor] ()
removeCursorRect_cursorSelector = mkSelector "removeCursorRect:cursor:"

-- | @Selector@ for @discardCursorRects@
discardCursorRectsSelector :: Selector '[] ()
discardCursorRectsSelector = mkSelector "discardCursorRects"

-- | @Selector@ for @resetCursorRects@
resetCursorRectsSelector :: Selector '[] ()
resetCursorRectsSelector = mkSelector "resetCursorRects"

-- | @Selector@ for @addTrackingRect:owner:userData:assumeInside:@
addTrackingRect_owner_userData_assumeInsideSelector :: Selector '[NSRect, RawId, Ptr (), Bool] CLong
addTrackingRect_owner_userData_assumeInsideSelector = mkSelector "addTrackingRect:owner:userData:assumeInside:"

-- | @Selector@ for @removeTrackingRect:@
removeTrackingRectSelector :: Selector '[CLong] ()
removeTrackingRectSelector = mkSelector "removeTrackingRect:"

-- | @Selector@ for @addGestureRecognizer:@
addGestureRecognizerSelector :: Selector '[Id NSGestureRecognizer] ()
addGestureRecognizerSelector = mkSelector "addGestureRecognizer:"

-- | @Selector@ for @removeGestureRecognizer:@
removeGestureRecognizerSelector :: Selector '[Id NSGestureRecognizer] ()
removeGestureRecognizerSelector = mkSelector "removeGestureRecognizer:"

-- | @Selector@ for @showDefinitionForAttributedString:atPoint:@
showDefinitionForAttributedString_atPointSelector :: Selector '[Id NSAttributedString, NSPoint] ()
showDefinitionForAttributedString_atPointSelector = mkSelector "showDefinitionForAttributedString:atPoint:"

-- | @Selector@ for @showDefinitionForAttributedString:range:options:baselineOriginProvider:@
showDefinitionForAttributedString_range_options_baselineOriginProviderSelector :: Selector '[Id NSAttributedString, NSRange, Id NSDictionary, Ptr ()] ()
showDefinitionForAttributedString_range_options_baselineOriginProviderSelector = mkSelector "showDefinitionForAttributedString:range:options:baselineOriginProvider:"

-- | @Selector@ for @enterFullScreenMode:withOptions:@
enterFullScreenMode_withOptionsSelector :: Selector '[Id NSScreen, Id NSDictionary] Bool
enterFullScreenMode_withOptionsSelector = mkSelector "enterFullScreenMode:withOptions:"

-- | @Selector@ for @exitFullScreenModeWithOptions:@
exitFullScreenModeWithOptionsSelector :: Selector '[Id NSDictionary] ()
exitFullScreenModeWithOptionsSelector = mkSelector "exitFullScreenModeWithOptions:"

-- | @Selector@ for @beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_sourceSelector :: Selector '[Id NSArray, Id NSEvent, RawId] (Id NSDraggingSession)
beginDraggingSessionWithItems_event_sourceSelector = mkSelector "beginDraggingSessionWithItems:event:source:"

-- | @Selector@ for @registerForDraggedTypes:@
registerForDraggedTypesSelector :: Selector '[Id NSArray] ()
registerForDraggedTypesSelector = mkSelector "registerForDraggedTypes:"

-- | @Selector@ for @unregisterDraggedTypes@
unregisterDraggedTypesSelector :: Selector '[] ()
unregisterDraggedTypesSelector = mkSelector "unregisterDraggedTypes"

-- | @Selector@ for @writeEPSInsideRect:toPasteboard:@
writeEPSInsideRect_toPasteboardSelector :: Selector '[NSRect, Id NSPasteboard] ()
writeEPSInsideRect_toPasteboardSelector = mkSelector "writeEPSInsideRect:toPasteboard:"

-- | @Selector@ for @dataWithEPSInsideRect:@
dataWithEPSInsideRectSelector :: Selector '[NSRect] (Id NSData)
dataWithEPSInsideRectSelector = mkSelector "dataWithEPSInsideRect:"

-- | @Selector@ for @writePDFInsideRect:toPasteboard:@
writePDFInsideRect_toPasteboardSelector :: Selector '[NSRect, Id NSPasteboard] ()
writePDFInsideRect_toPasteboardSelector = mkSelector "writePDFInsideRect:toPasteboard:"

-- | @Selector@ for @dataWithPDFInsideRect:@
dataWithPDFInsideRectSelector :: Selector '[NSRect] (Id NSData)
dataWithPDFInsideRectSelector = mkSelector "dataWithPDFInsideRect:"

-- | @Selector@ for @print:@
printSelector :: Selector '[RawId] ()
printSelector = mkSelector "print:"

-- | @Selector@ for @knowsPageRange:@
knowsPageRangeSelector :: Selector '[Ptr NSRange] Bool
knowsPageRangeSelector = mkSelector "knowsPageRange:"

-- | @Selector@ for @adjustPageWidthNew:left:right:limit:@
adjustPageWidthNew_left_right_limitSelector :: Selector '[Ptr CDouble, CDouble, CDouble, CDouble] ()
adjustPageWidthNew_left_right_limitSelector = mkSelector "adjustPageWidthNew:left:right:limit:"

-- | @Selector@ for @adjustPageHeightNew:top:bottom:limit:@
adjustPageHeightNew_top_bottom_limitSelector :: Selector '[Ptr CDouble, CDouble, CDouble, CDouble] ()
adjustPageHeightNew_top_bottom_limitSelector = mkSelector "adjustPageHeightNew:top:bottom:limit:"

-- | @Selector@ for @rectForPage:@
rectForPageSelector :: Selector '[CLong] NSRect
rectForPageSelector = mkSelector "rectForPage:"

-- | @Selector@ for @locationOfPrintRect:@
locationOfPrintRectSelector :: Selector '[NSRect] NSPoint
locationOfPrintRectSelector = mkSelector "locationOfPrintRect:"

-- | @Selector@ for @drawPageBorderWithSize:@
drawPageBorderWithSizeSelector :: Selector '[NSSize] ()
drawPageBorderWithSizeSelector = mkSelector "drawPageBorderWithSize:"

-- | @Selector@ for @drawSheetBorderWithSize:@
drawSheetBorderWithSizeSelector :: Selector '[NSSize] ()
drawSheetBorderWithSizeSelector = mkSelector "drawSheetBorderWithSize:"

-- | @Selector@ for @beginDocument@
beginDocumentSelector :: Selector '[] ()
beginDocumentSelector = mkSelector "beginDocument"

-- | @Selector@ for @endDocument@
endDocumentSelector :: Selector '[] ()
endDocumentSelector = mkSelector "endDocument"

-- | @Selector@ for @beginPageInRect:atPlacement:@
beginPageInRect_atPlacementSelector :: Selector '[NSRect, NSPoint] ()
beginPageInRect_atPlacementSelector = mkSelector "beginPageInRect:atPlacement:"

-- | @Selector@ for @endPage@
endPageSelector :: Selector '[] ()
endPageSelector = mkSelector "endPage"

-- | @Selector@ for @setKeyboardFocusRingNeedsDisplayInRect:@
setKeyboardFocusRingNeedsDisplayInRectSelector :: Selector '[NSRect] ()
setKeyboardFocusRingNeedsDisplayInRectSelector = mkSelector "setKeyboardFocusRingNeedsDisplayInRect:"

-- | @Selector@ for @drawFocusRingMask@
drawFocusRingMaskSelector :: Selector '[] ()
drawFocusRingMaskSelector = mkSelector "drawFocusRingMask"

-- | @Selector@ for @noteFocusRingMaskChanged@
noteFocusRingMaskChangedSelector :: Selector '[] ()
noteFocusRingMaskChangedSelector = mkSelector "noteFocusRingMaskChanged"

-- | @Selector@ for @window@
windowSelector :: Selector '[] (Id NSWindow)
windowSelector = mkSelector "window"

-- | @Selector@ for @superview@
superviewSelector :: Selector '[] (Id NSView)
superviewSelector = mkSelector "superview"

-- | @Selector@ for @subviews@
subviewsSelector :: Selector '[] (Id NSArray)
subviewsSelector = mkSelector "subviews"

-- | @Selector@ for @setSubviews:@
setSubviewsSelector :: Selector '[Id NSArray] ()
setSubviewsSelector = mkSelector "setSubviews:"

-- | @Selector@ for @opaqueAncestor@
opaqueAncestorSelector :: Selector '[] (Id NSView)
opaqueAncestorSelector = mkSelector "opaqueAncestor"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestorSelector :: Selector '[] Bool
hiddenOrHasHiddenAncestorSelector = mkSelector "hiddenOrHasHiddenAncestor"

-- | @Selector@ for @wantsDefaultClipping@
wantsDefaultClippingSelector :: Selector '[] Bool
wantsDefaultClippingSelector = mkSelector "wantsDefaultClipping"

-- | @Selector@ for @postsFrameChangedNotifications@
postsFrameChangedNotificationsSelector :: Selector '[] Bool
postsFrameChangedNotificationsSelector = mkSelector "postsFrameChangedNotifications"

-- | @Selector@ for @setPostsFrameChangedNotifications:@
setPostsFrameChangedNotificationsSelector :: Selector '[Bool] ()
setPostsFrameChangedNotificationsSelector = mkSelector "setPostsFrameChangedNotifications:"

-- | @Selector@ for @autoresizesSubviews@
autoresizesSubviewsSelector :: Selector '[] Bool
autoresizesSubviewsSelector = mkSelector "autoresizesSubviews"

-- | @Selector@ for @setAutoresizesSubviews:@
setAutoresizesSubviewsSelector :: Selector '[Bool] ()
setAutoresizesSubviewsSelector = mkSelector "setAutoresizesSubviews:"

-- | @Selector@ for @autoresizingMask@
autoresizingMaskSelector :: Selector '[] NSAutoresizingMaskOptions
autoresizingMaskSelector = mkSelector "autoresizingMask"

-- | @Selector@ for @setAutoresizingMask:@
setAutoresizingMaskSelector :: Selector '[NSAutoresizingMaskOptions] ()
setAutoresizingMaskSelector = mkSelector "setAutoresizingMask:"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector '[NSRect] ()
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @frameRotation@
frameRotationSelector :: Selector '[] CDouble
frameRotationSelector = mkSelector "frameRotation"

-- | @Selector@ for @setFrameRotation:@
setFrameRotationSelector :: Selector '[CDouble] ()
setFrameRotationSelector = mkSelector "setFrameRotation:"

-- | @Selector@ for @frameCenterRotation@
frameCenterRotationSelector :: Selector '[] CDouble
frameCenterRotationSelector = mkSelector "frameCenterRotation"

-- | @Selector@ for @setFrameCenterRotation:@
setFrameCenterRotationSelector :: Selector '[CDouble] ()
setFrameCenterRotationSelector = mkSelector "setFrameCenterRotation:"

-- | @Selector@ for @boundsRotation@
boundsRotationSelector :: Selector '[] CDouble
boundsRotationSelector = mkSelector "boundsRotation"

-- | @Selector@ for @setBoundsRotation:@
setBoundsRotationSelector :: Selector '[CDouble] ()
setBoundsRotationSelector = mkSelector "setBoundsRotation:"

-- | @Selector@ for @bounds@
boundsSelector :: Selector '[] NSRect
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @setBounds:@
setBoundsSelector :: Selector '[NSRect] ()
setBoundsSelector = mkSelector "setBounds:"

-- | @Selector@ for @flipped@
flippedSelector :: Selector '[] Bool
flippedSelector = mkSelector "flipped"

-- | @Selector@ for @rotatedFromBase@
rotatedFromBaseSelector :: Selector '[] Bool
rotatedFromBaseSelector = mkSelector "rotatedFromBase"

-- | @Selector@ for @rotatedOrScaledFromBase@
rotatedOrScaledFromBaseSelector :: Selector '[] Bool
rotatedOrScaledFromBaseSelector = mkSelector "rotatedOrScaledFromBase"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @canDrawConcurrently@
canDrawConcurrentlySelector :: Selector '[] Bool
canDrawConcurrentlySelector = mkSelector "canDrawConcurrently"

-- | @Selector@ for @setCanDrawConcurrently:@
setCanDrawConcurrentlySelector :: Selector '[Bool] ()
setCanDrawConcurrentlySelector = mkSelector "setCanDrawConcurrently:"

-- | @Selector@ for @canDraw@
canDrawSelector :: Selector '[] Bool
canDrawSelector = mkSelector "canDraw"

-- | @Selector@ for @needsDisplay@
needsDisplaySelector :: Selector '[] Bool
needsDisplaySelector = mkSelector "needsDisplay"

-- | @Selector@ for @setNeedsDisplay:@
setNeedsDisplaySelector :: Selector '[Bool] ()
setNeedsDisplaySelector = mkSelector "setNeedsDisplay:"

-- | @Selector@ for @focusView@
focusViewSelector :: Selector '[] (Id NSView)
focusViewSelector = mkSelector "focusView"

-- | @Selector@ for @visibleRect@
visibleRectSelector :: Selector '[] NSRect
visibleRectSelector = mkSelector "visibleRect"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CLong
tagSelector = mkSelector "tag"

-- | @Selector@ for @needsPanelToBecomeKey@
needsPanelToBecomeKeySelector :: Selector '[] Bool
needsPanelToBecomeKeySelector = mkSelector "needsPanelToBecomeKey"

-- | @Selector@ for @mouseDownCanMoveWindow@
mouseDownCanMoveWindowSelector :: Selector '[] Bool
mouseDownCanMoveWindowSelector = mkSelector "mouseDownCanMoveWindow"

-- | @Selector@ for @acceptsTouchEvents@
acceptsTouchEventsSelector :: Selector '[] Bool
acceptsTouchEventsSelector = mkSelector "acceptsTouchEvents"

-- | @Selector@ for @setAcceptsTouchEvents:@
setAcceptsTouchEventsSelector :: Selector '[Bool] ()
setAcceptsTouchEventsSelector = mkSelector "setAcceptsTouchEvents:"

-- | @Selector@ for @wantsRestingTouches@
wantsRestingTouchesSelector :: Selector '[] Bool
wantsRestingTouchesSelector = mkSelector "wantsRestingTouches"

-- | @Selector@ for @setWantsRestingTouches:@
setWantsRestingTouchesSelector :: Selector '[Bool] ()
setWantsRestingTouchesSelector = mkSelector "setWantsRestingTouches:"

-- | @Selector@ for @layerContentsRedrawPolicy@
layerContentsRedrawPolicySelector :: Selector '[] NSViewLayerContentsRedrawPolicy
layerContentsRedrawPolicySelector = mkSelector "layerContentsRedrawPolicy"

-- | @Selector@ for @setLayerContentsRedrawPolicy:@
setLayerContentsRedrawPolicySelector :: Selector '[NSViewLayerContentsRedrawPolicy] ()
setLayerContentsRedrawPolicySelector = mkSelector "setLayerContentsRedrawPolicy:"

-- | @Selector@ for @layerContentsPlacement@
layerContentsPlacementSelector :: Selector '[] NSViewLayerContentsPlacement
layerContentsPlacementSelector = mkSelector "layerContentsPlacement"

-- | @Selector@ for @setLayerContentsPlacement:@
setLayerContentsPlacementSelector :: Selector '[NSViewLayerContentsPlacement] ()
setLayerContentsPlacementSelector = mkSelector "setLayerContentsPlacement:"

-- | @Selector@ for @wantsLayer@
wantsLayerSelector :: Selector '[] Bool
wantsLayerSelector = mkSelector "wantsLayer"

-- | @Selector@ for @setWantsLayer:@
setWantsLayerSelector :: Selector '[Bool] ()
setWantsLayerSelector = mkSelector "setWantsLayer:"

-- | @Selector@ for @layer@
layerSelector :: Selector '[] (Id CALayer)
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector '[Id CALayer] ()
setLayerSelector = mkSelector "setLayer:"

-- | @Selector@ for @wantsUpdateLayer@
wantsUpdateLayerSelector :: Selector '[] Bool
wantsUpdateLayerSelector = mkSelector "wantsUpdateLayer"

-- | @Selector@ for @canDrawSubviewsIntoLayer@
canDrawSubviewsIntoLayerSelector :: Selector '[] Bool
canDrawSubviewsIntoLayerSelector = mkSelector "canDrawSubviewsIntoLayer"

-- | @Selector@ for @setCanDrawSubviewsIntoLayer:@
setCanDrawSubviewsIntoLayerSelector :: Selector '[Bool] ()
setCanDrawSubviewsIntoLayerSelector = mkSelector "setCanDrawSubviewsIntoLayer:"

-- | @Selector@ for @needsLayout@
needsLayoutSelector :: Selector '[] Bool
needsLayoutSelector = mkSelector "needsLayout"

-- | @Selector@ for @setNeedsLayout:@
setNeedsLayoutSelector :: Selector '[Bool] ()
setNeedsLayoutSelector = mkSelector "setNeedsLayout:"

-- | @Selector@ for @alphaValue@
alphaValueSelector :: Selector '[] CDouble
alphaValueSelector = mkSelector "alphaValue"

-- | @Selector@ for @setAlphaValue:@
setAlphaValueSelector :: Selector '[CDouble] ()
setAlphaValueSelector = mkSelector "setAlphaValue:"

-- | @Selector@ for @layerUsesCoreImageFilters@
layerUsesCoreImageFiltersSelector :: Selector '[] Bool
layerUsesCoreImageFiltersSelector = mkSelector "layerUsesCoreImageFilters"

-- | @Selector@ for @setLayerUsesCoreImageFilters:@
setLayerUsesCoreImageFiltersSelector :: Selector '[Bool] ()
setLayerUsesCoreImageFiltersSelector = mkSelector "setLayerUsesCoreImageFilters:"

-- | @Selector@ for @backgroundFilters@
backgroundFiltersSelector :: Selector '[] (Id NSArray)
backgroundFiltersSelector = mkSelector "backgroundFilters"

-- | @Selector@ for @setBackgroundFilters:@
setBackgroundFiltersSelector :: Selector '[Id NSArray] ()
setBackgroundFiltersSelector = mkSelector "setBackgroundFilters:"

-- | @Selector@ for @compositingFilter@
compositingFilterSelector :: Selector '[] (Id CIFilter)
compositingFilterSelector = mkSelector "compositingFilter"

-- | @Selector@ for @setCompositingFilter:@
setCompositingFilterSelector :: Selector '[Id CIFilter] ()
setCompositingFilterSelector = mkSelector "setCompositingFilter:"

-- | @Selector@ for @contentFilters@
contentFiltersSelector :: Selector '[] (Id NSArray)
contentFiltersSelector = mkSelector "contentFilters"

-- | @Selector@ for @setContentFilters:@
setContentFiltersSelector :: Selector '[Id NSArray] ()
setContentFiltersSelector = mkSelector "setContentFilters:"

-- | @Selector@ for @shadow@
shadowSelector :: Selector '[] (Id NSShadow)
shadowSelector = mkSelector "shadow"

-- | @Selector@ for @setShadow:@
setShadowSelector :: Selector '[Id NSShadow] ()
setShadowSelector = mkSelector "setShadow:"

-- | @Selector@ for @clipsToBounds@
clipsToBoundsSelector :: Selector '[] Bool
clipsToBoundsSelector = mkSelector "clipsToBounds"

-- | @Selector@ for @setClipsToBounds:@
setClipsToBoundsSelector :: Selector '[Bool] ()
setClipsToBoundsSelector = mkSelector "setClipsToBounds:"

-- | @Selector@ for @postsBoundsChangedNotifications@
postsBoundsChangedNotificationsSelector :: Selector '[] Bool
postsBoundsChangedNotificationsSelector = mkSelector "postsBoundsChangedNotifications"

-- | @Selector@ for @setPostsBoundsChangedNotifications:@
setPostsBoundsChangedNotificationsSelector :: Selector '[Bool] ()
setPostsBoundsChangedNotificationsSelector = mkSelector "setPostsBoundsChangedNotifications:"

-- | @Selector@ for @enclosingScrollView@
enclosingScrollViewSelector :: Selector '[] (Id NSScrollView)
enclosingScrollViewSelector = mkSelector "enclosingScrollView"

-- | @Selector@ for @defaultMenu@
defaultMenuSelector :: Selector '[] (Id NSMenu)
defaultMenuSelector = mkSelector "defaultMenu"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] (Id NSString)
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector '[Id NSString] ()
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @inLiveResize@
inLiveResizeSelector :: Selector '[] Bool
inLiveResizeSelector = mkSelector "inLiveResize"

-- | @Selector@ for @preservesContentDuringLiveResize@
preservesContentDuringLiveResizeSelector :: Selector '[] Bool
preservesContentDuringLiveResizeSelector = mkSelector "preservesContentDuringLiveResize"

-- | @Selector@ for @rectPreservedDuringLiveResize@
rectPreservedDuringLiveResizeSelector :: Selector '[] NSRect
rectPreservedDuringLiveResizeSelector = mkSelector "rectPreservedDuringLiveResize"

-- | @Selector@ for @inputContext@
inputContextSelector :: Selector '[] (Id NSTextInputContext)
inputContextSelector = mkSelector "inputContext"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector '[NSUserInterfaceLayoutDirection] ()
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @compatibleWithResponsiveScrolling@
compatibleWithResponsiveScrollingSelector :: Selector '[] Bool
compatibleWithResponsiveScrollingSelector = mkSelector "compatibleWithResponsiveScrolling"

-- | @Selector@ for @preparedContentRect@
preparedContentRectSelector :: Selector '[] NSRect
preparedContentRectSelector = mkSelector "preparedContentRect"

-- | @Selector@ for @setPreparedContentRect:@
setPreparedContentRectSelector :: Selector '[NSRect] ()
setPreparedContentRectSelector = mkSelector "setPreparedContentRect:"

-- | @Selector@ for @allowsVibrancy@
allowsVibrancySelector :: Selector '[] Bool
allowsVibrancySelector = mkSelector "allowsVibrancy"

-- | @Selector@ for @pressureConfiguration@
pressureConfigurationSelector :: Selector '[] (Id NSPressureConfiguration)
pressureConfigurationSelector = mkSelector "pressureConfiguration"

-- | @Selector@ for @setPressureConfiguration:@
setPressureConfigurationSelector :: Selector '[Id NSPressureConfiguration] ()
setPressureConfigurationSelector = mkSelector "setPressureConfiguration:"

-- | @Selector@ for @wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector '[] Bool
wantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "wantsExtendedDynamicRangeOpenGLSurface"

-- | @Selector@ for @setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector '[Bool] ()
setWantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "setWantsExtendedDynamicRangeOpenGLSurface:"

-- | @Selector@ for @wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurfaceSelector :: Selector '[] Bool
wantsBestResolutionOpenGLSurfaceSelector = mkSelector "wantsBestResolutionOpenGLSurface"

-- | @Selector@ for @setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurfaceSelector :: Selector '[Bool] ()
setWantsBestResolutionOpenGLSurfaceSelector = mkSelector "setWantsBestResolutionOpenGLSurface:"

-- | @Selector@ for @layoutGuides@
layoutGuidesSelector :: Selector '[] (Id NSArray)
layoutGuidesSelector = mkSelector "layoutGuides"

-- | @Selector@ for @hasAmbiguousLayout@
hasAmbiguousLayoutSelector :: Selector '[] Bool
hasAmbiguousLayoutSelector = mkSelector "hasAmbiguousLayout"

-- | @Selector@ for @fittingSize@
fittingSizeSelector :: Selector '[] NSSize
fittingSizeSelector = mkSelector "fittingSize"

-- | @Selector@ for @alignmentRectInsets@
alignmentRectInsetsSelector :: Selector '[] NSEdgeInsets
alignmentRectInsetsSelector = mkSelector "alignmentRectInsets"

-- | @Selector@ for @firstBaselineOffsetFromTop@
firstBaselineOffsetFromTopSelector :: Selector '[] CDouble
firstBaselineOffsetFromTopSelector = mkSelector "firstBaselineOffsetFromTop"

-- | @Selector@ for @lastBaselineOffsetFromBottom@
lastBaselineOffsetFromBottomSelector :: Selector '[] CDouble
lastBaselineOffsetFromBottomSelector = mkSelector "lastBaselineOffsetFromBottom"

-- | @Selector@ for @baselineOffsetFromBottom@
baselineOffsetFromBottomSelector :: Selector '[] CDouble
baselineOffsetFromBottomSelector = mkSelector "baselineOffsetFromBottom"

-- | @Selector@ for @intrinsicContentSize@
intrinsicContentSizeSelector :: Selector '[] NSSize
intrinsicContentSizeSelector = mkSelector "intrinsicContentSize"

-- | @Selector@ for @horizontalContentSizeConstraintActive@
horizontalContentSizeConstraintActiveSelector :: Selector '[] Bool
horizontalContentSizeConstraintActiveSelector = mkSelector "horizontalContentSizeConstraintActive"

-- | @Selector@ for @setHorizontalContentSizeConstraintActive:@
setHorizontalContentSizeConstraintActiveSelector :: Selector '[Bool] ()
setHorizontalContentSizeConstraintActiveSelector = mkSelector "setHorizontalContentSizeConstraintActive:"

-- | @Selector@ for @verticalContentSizeConstraintActive@
verticalContentSizeConstraintActiveSelector :: Selector '[] Bool
verticalContentSizeConstraintActiveSelector = mkSelector "verticalContentSizeConstraintActive"

-- | @Selector@ for @setVerticalContentSizeConstraintActive:@
setVerticalContentSizeConstraintActiveSelector :: Selector '[Bool] ()
setVerticalContentSizeConstraintActiveSelector = mkSelector "setVerticalContentSizeConstraintActive:"

-- | @Selector@ for @translatesAutoresizingMaskIntoConstraints@
translatesAutoresizingMaskIntoConstraintsSelector :: Selector '[] Bool
translatesAutoresizingMaskIntoConstraintsSelector = mkSelector "translatesAutoresizingMaskIntoConstraints"

-- | @Selector@ for @setTranslatesAutoresizingMaskIntoConstraints:@
setTranslatesAutoresizingMaskIntoConstraintsSelector :: Selector '[Bool] ()
setTranslatesAutoresizingMaskIntoConstraintsSelector = mkSelector "setTranslatesAutoresizingMaskIntoConstraints:"

-- | @Selector@ for @requiresConstraintBasedLayout@
requiresConstraintBasedLayoutSelector :: Selector '[] Bool
requiresConstraintBasedLayoutSelector = mkSelector "requiresConstraintBasedLayout"

-- | @Selector@ for @needsUpdateConstraints@
needsUpdateConstraintsSelector :: Selector '[] Bool
needsUpdateConstraintsSelector = mkSelector "needsUpdateConstraints"

-- | @Selector@ for @setNeedsUpdateConstraints:@
setNeedsUpdateConstraintsSelector :: Selector '[Bool] ()
setNeedsUpdateConstraintsSelector = mkSelector "setNeedsUpdateConstraints:"

-- | @Selector@ for @leadingAnchor@
leadingAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
leadingAnchorSelector = mkSelector "leadingAnchor"

-- | @Selector@ for @trailingAnchor@
trailingAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
trailingAnchorSelector = mkSelector "trailingAnchor"

-- | @Selector@ for @leftAnchor@
leftAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
leftAnchorSelector = mkSelector "leftAnchor"

-- | @Selector@ for @rightAnchor@
rightAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
rightAnchorSelector = mkSelector "rightAnchor"

-- | @Selector@ for @topAnchor@
topAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
topAnchorSelector = mkSelector "topAnchor"

-- | @Selector@ for @bottomAnchor@
bottomAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
bottomAnchorSelector = mkSelector "bottomAnchor"

-- | @Selector@ for @widthAnchor@
widthAnchorSelector :: Selector '[] (Id NSLayoutDimension)
widthAnchorSelector = mkSelector "widthAnchor"

-- | @Selector@ for @heightAnchor@
heightAnchorSelector :: Selector '[] (Id NSLayoutDimension)
heightAnchorSelector = mkSelector "heightAnchor"

-- | @Selector@ for @centerXAnchor@
centerXAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
centerXAnchorSelector = mkSelector "centerXAnchor"

-- | @Selector@ for @centerYAnchor@
centerYAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
centerYAnchorSelector = mkSelector "centerYAnchor"

-- | @Selector@ for @firstBaselineAnchor@
firstBaselineAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
firstBaselineAnchorSelector = mkSelector "firstBaselineAnchor"

-- | @Selector@ for @lastBaselineAnchor@
lastBaselineAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
lastBaselineAnchorSelector = mkSelector "lastBaselineAnchor"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector '[] (Id NSArray)
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @candidateListTouchBarItem@
candidateListTouchBarItemSelector :: Selector '[] (Id NSCandidateListTouchBarItem)
candidateListTouchBarItemSelector = mkSelector "candidateListTouchBarItem"

-- | @Selector@ for @enclosingMenuItem@
enclosingMenuItemSelector :: Selector '[] (Id NSMenuItem)
enclosingMenuItemSelector = mkSelector "enclosingMenuItem"

-- | @Selector@ for @writingToolsCoordinator@
writingToolsCoordinatorSelector :: Selector '[] (Id NSWritingToolsCoordinator)
writingToolsCoordinatorSelector = mkSelector "writingToolsCoordinator"

-- | @Selector@ for @setWritingToolsCoordinator:@
setWritingToolsCoordinatorSelector :: Selector '[Id NSWritingToolsCoordinator] ()
setWritingToolsCoordinatorSelector = mkSelector "setWritingToolsCoordinator:"

-- | @Selector@ for @trackingAreas@
trackingAreasSelector :: Selector '[] (Id NSArray)
trackingAreasSelector = mkSelector "trackingAreas"

-- | @Selector@ for @prefersCompactControlSizeMetrics@
prefersCompactControlSizeMetricsSelector :: Selector '[] Bool
prefersCompactControlSizeMetricsSelector = mkSelector "prefersCompactControlSizeMetrics"

-- | @Selector@ for @setPrefersCompactControlSizeMetrics:@
setPrefersCompactControlSizeMetricsSelector :: Selector '[Bool] ()
setPrefersCompactControlSizeMetricsSelector = mkSelector "setPrefersCompactControlSizeMetrics:"

-- | @Selector@ for @safeAreaInsets@
safeAreaInsetsSelector :: Selector '[] NSEdgeInsets
safeAreaInsetsSelector = mkSelector "safeAreaInsets"

-- | @Selector@ for @additionalSafeAreaInsets@
additionalSafeAreaInsetsSelector :: Selector '[] NSEdgeInsets
additionalSafeAreaInsetsSelector = mkSelector "additionalSafeAreaInsets"

-- | @Selector@ for @setAdditionalSafeAreaInsets:@
setAdditionalSafeAreaInsetsSelector :: Selector '[NSEdgeInsets] ()
setAdditionalSafeAreaInsetsSelector = mkSelector "setAdditionalSafeAreaInsets:"

-- | @Selector@ for @safeAreaLayoutGuide@
safeAreaLayoutGuideSelector :: Selector '[] (Id NSLayoutGuide)
safeAreaLayoutGuideSelector = mkSelector "safeAreaLayoutGuide"

-- | @Selector@ for @safeAreaRect@
safeAreaRectSelector :: Selector '[] NSRect
safeAreaRectSelector = mkSelector "safeAreaRect"

-- | @Selector@ for @layoutMarginsGuide@
layoutMarginsGuideSelector :: Selector '[] (Id NSLayoutGuide)
layoutMarginsGuideSelector = mkSelector "layoutMarginsGuide"

-- | @Selector@ for @allowedTouchTypes@
allowedTouchTypesSelector :: Selector '[] NSTouchTypeMask
allowedTouchTypesSelector = mkSelector "allowedTouchTypes"

-- | @Selector@ for @setAllowedTouchTypes:@
setAllowedTouchTypesSelector :: Selector '[NSTouchTypeMask] ()
setAllowedTouchTypesSelector = mkSelector "setAllowedTouchTypes:"

-- | @Selector@ for @gestureRecognizers@
gestureRecognizersSelector :: Selector '[] (Id NSArray)
gestureRecognizersSelector = mkSelector "gestureRecognizers"

-- | @Selector@ for @setGestureRecognizers:@
setGestureRecognizersSelector :: Selector '[Id NSArray] ()
setGestureRecognizersSelector = mkSelector "setGestureRecognizers:"

-- | @Selector@ for @drawingFindIndicator@
drawingFindIndicatorSelector :: Selector '[] Bool
drawingFindIndicatorSelector = mkSelector "drawingFindIndicator"

-- | @Selector@ for @inFullScreenMode@
inFullScreenModeSelector :: Selector '[] Bool
inFullScreenModeSelector = mkSelector "inFullScreenMode"

-- | @Selector@ for @registeredDraggedTypes@
registeredDraggedTypesSelector :: Selector '[] (Id NSArray)
registeredDraggedTypesSelector = mkSelector "registeredDraggedTypes"

-- | @Selector@ for @heightAdjustLimit@
heightAdjustLimitSelector :: Selector '[] CDouble
heightAdjustLimitSelector = mkSelector "heightAdjustLimit"

-- | @Selector@ for @widthAdjustLimit@
widthAdjustLimitSelector :: Selector '[] CDouble
widthAdjustLimitSelector = mkSelector "widthAdjustLimit"

-- | @Selector@ for @pageHeader@
pageHeaderSelector :: Selector '[] (Id NSAttributedString)
pageHeaderSelector = mkSelector "pageHeader"

-- | @Selector@ for @pageFooter@
pageFooterSelector :: Selector '[] (Id NSAttributedString)
pageFooterSelector = mkSelector "pageFooter"

-- | @Selector@ for @printJobTitle@
printJobTitleSelector :: Selector '[] (Id NSString)
printJobTitleSelector = mkSelector "printJobTitle"

-- | @Selector@ for @nextKeyView@
nextKeyViewSelector :: Selector '[] (Id NSView)
nextKeyViewSelector = mkSelector "nextKeyView"

-- | @Selector@ for @setNextKeyView:@
setNextKeyViewSelector :: Selector '[Id NSView] ()
setNextKeyViewSelector = mkSelector "setNextKeyView:"

-- | @Selector@ for @previousKeyView@
previousKeyViewSelector :: Selector '[] (Id NSView)
previousKeyViewSelector = mkSelector "previousKeyView"

-- | @Selector@ for @nextValidKeyView@
nextValidKeyViewSelector :: Selector '[] (Id NSView)
nextValidKeyViewSelector = mkSelector "nextValidKeyView"

-- | @Selector@ for @previousValidKeyView@
previousValidKeyViewSelector :: Selector '[] (Id NSView)
previousValidKeyViewSelector = mkSelector "previousValidKeyView"

-- | @Selector@ for @canBecomeKeyView@
canBecomeKeyViewSelector :: Selector '[] Bool
canBecomeKeyViewSelector = mkSelector "canBecomeKeyView"

-- | @Selector@ for @focusRingType@
focusRingTypeSelector :: Selector '[] NSFocusRingType
focusRingTypeSelector = mkSelector "focusRingType"

-- | @Selector@ for @setFocusRingType:@
setFocusRingTypeSelector :: Selector '[NSFocusRingType] ()
setFocusRingTypeSelector = mkSelector "setFocusRingType:"

-- | @Selector@ for @defaultFocusRingType@
defaultFocusRingTypeSelector :: Selector '[] NSFocusRingType
defaultFocusRingTypeSelector = mkSelector "defaultFocusRingType"

-- | @Selector@ for @focusRingMaskBounds@
focusRingMaskBoundsSelector :: Selector '[] NSRect
focusRingMaskBoundsSelector = mkSelector "focusRingMaskBounds"

