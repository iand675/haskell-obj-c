{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameSelector
  , initWithCoderSelector
  , isDescendantOfSelector
  , ancestorSharedWithViewSelector
  , getRectsBeingDrawn_countSelector
  , needsToDrawRectSelector
  , viewDidHideSelector
  , viewDidUnhideSelector
  , addSubviewSelector
  , addSubview_positioned_relativeToSelector
  , sortSubviewsUsingFunction_contextSelector
  , viewWillMoveToWindowSelector
  , viewDidMoveToWindowSelector
  , viewWillMoveToSuperviewSelector
  , viewDidMoveToSuperviewSelector
  , didAddSubviewSelector
  , willRemoveSubviewSelector
  , removeFromSuperviewSelector
  , replaceSubview_withSelector
  , removeFromSuperviewWithoutNeedingDisplaySelector
  , viewDidChangeBackingPropertiesSelector
  , resizeSubviewsWithOldSizeSelector
  , resizeWithOldSuperviewSizeSelector
  , setFrameOriginSelector
  , setFrameSizeSelector
  , setBoundsOriginSelector
  , setBoundsSizeSelector
  , translateOriginToPointSelector
  , scaleUnitSquareToSizeSelector
  , rotateByAngleSelector
  , convertPoint_fromViewSelector
  , convertPoint_toViewSelector
  , convertSize_fromViewSelector
  , convertSize_toViewSelector
  , convertRect_fromViewSelector
  , convertRect_toViewSelector
  , backingAlignedRect_optionsSelector
  , centerScanRectSelector
  , convertPointToBackingSelector
  , convertPointFromBackingSelector
  , convertSizeToBackingSelector
  , convertSizeFromBackingSelector
  , convertRectToBackingSelector
  , convertRectFromBackingSelector
  , convertPointToLayerSelector
  , convertPointFromLayerSelector
  , convertSizeToLayerSelector
  , convertSizeFromLayerSelector
  , convertRectToLayerSelector
  , convertRectFromLayerSelector
  , setNeedsDisplayInRectSelector
  , lockFocusSelector
  , unlockFocusSelector
  , lockFocusIfCanDrawSelector
  , lockFocusIfCanDrawInContextSelector
  , displaySelector
  , displayIfNeededSelector
  , displayIfNeededIgnoringOpacitySelector
  , displayRectSelector
  , displayIfNeededInRectSelector
  , displayRectIgnoringOpacitySelector
  , displayIfNeededInRectIgnoringOpacitySelector
  , drawRectSelector
  , displayRectIgnoringOpacity_inContextSelector
  , bitmapImageRepForCachingDisplayInRectSelector
  , cacheDisplayInRect_toBitmapImageRepSelector
  , viewWillDrawSelector
  , scrollPointSelector
  , scrollRectToVisibleSelector
  , autoscrollSelector
  , adjustScrollSelector
  , scrollRect_bySelector
  , translateRectsNeedingDisplayInRect_bySelector
  , hitTestSelector
  , mouse_inRectSelector
  , viewWithTagSelector
  , performKeyEquivalentSelector
  , acceptsFirstMouseSelector
  , shouldDelayWindowOrderingForEventSelector
  , makeBackingLayerSelector
  , updateLayerSelector
  , layoutSubtreeIfNeededSelector
  , layoutSelector
  , menuForEventSelector
  , willOpenMenu_withEventSelector
  , didCloseMenu_withEventSelector
  , addToolTipRect_owner_userDataSelector
  , removeToolTipSelector
  , removeAllToolTipsSelector
  , viewWillStartLiveResizeSelector
  , viewDidEndLiveResizeSelector
  , getRectsExposedDuringLiveResize_countSelector
  , rectForSmartMagnificationAtPoint_inRectSelector
  , prepareForReuseSelector
  , prepareContentInRectSelector
  , viewDidChangeEffectiveAppearanceSelector
  , rulerView_shouldMoveMarkerSelector
  , rulerView_willMoveMarker_toLocationSelector
  , rulerView_didMoveMarkerSelector
  , rulerView_shouldRemoveMarkerSelector
  , rulerView_didRemoveMarkerSelector
  , rulerView_shouldAddMarkerSelector
  , rulerView_willAddMarker_atLocationSelector
  , rulerView_didAddMarkerSelector
  , rulerView_handleMouseDownSelector
  , rulerView_willSetClientViewSelector
  , rulerView_locationForPointSelector
  , rulerView_pointForLocationSelector
  , layoutGuideForLayoutRegionSelector
  , edgeInsetsForLayoutRegionSelector
  , rectForLayoutRegionSelector
  , addLayoutGuideSelector
  , removeLayoutGuideSelector
  , constraintsAffectingLayoutForOrientationSelector
  , exerciseAmbiguityInLayoutSelector
  , alignmentRectForFrameSelector
  , frameForAlignmentRectSelector
  , invalidateIntrinsicContentSizeSelector
  , contentHuggingPriorityForOrientationSelector
  , setContentHuggingPriority_forOrientationSelector
  , contentCompressionResistancePriorityForOrientationSelector
  , setContentCompressionResistancePriority_forOrientationSelector
  , updateConstraintsForSubtreeIfNeededSelector
  , updateConstraintsSelector
  , addConstraintSelector
  , addConstraintsSelector
  , removeConstraintSelector
  , removeConstraintsSelector
  , reflectScrolledClipViewSelector
  , scrollClipView_toPointSelector
  , dragImage_at_offset_event_pasteboard_source_slideBackSelector
  , dragFile_fromRect_slideBack_eventSelector
  , dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector
  , convertPointToBaseSelector
  , convertPointFromBaseSelector
  , convertSizeToBaseSelector
  , convertSizeFromBaseSelector
  , convertRectToBaseSelector
  , convertRectFromBaseSelector
  , performMnemonicSelector
  , shouldDrawColorSelector
  , gStateSelector
  , allocateGStateSelector
  , releaseGStateSelector
  , setUpGStateSelector
  , renewGStateSelector
  , displayLinkWithTarget_selectorSelector
  , addTrackingAreaSelector
  , removeTrackingAreaSelector
  , updateTrackingAreasSelector
  , addCursorRect_cursorSelector
  , removeCursorRect_cursorSelector
  , discardCursorRectsSelector
  , resetCursorRectsSelector
  , addTrackingRect_owner_userData_assumeInsideSelector
  , removeTrackingRectSelector
  , addGestureRecognizerSelector
  , removeGestureRecognizerSelector
  , showDefinitionForAttributedString_atPointSelector
  , showDefinitionForAttributedString_range_options_baselineOriginProviderSelector
  , enterFullScreenMode_withOptionsSelector
  , exitFullScreenModeWithOptionsSelector
  , beginDraggingSessionWithItems_event_sourceSelector
  , registerForDraggedTypesSelector
  , unregisterDraggedTypesSelector
  , writeEPSInsideRect_toPasteboardSelector
  , dataWithEPSInsideRectSelector
  , writePDFInsideRect_toPasteboardSelector
  , dataWithPDFInsideRectSelector
  , printSelector
  , knowsPageRangeSelector
  , adjustPageWidthNew_left_right_limitSelector
  , adjustPageHeightNew_top_bottom_limitSelector
  , rectForPageSelector
  , locationOfPrintRectSelector
  , drawPageBorderWithSizeSelector
  , drawSheetBorderWithSizeSelector
  , beginDocumentSelector
  , endDocumentSelector
  , beginPageInRect_atPlacementSelector
  , endPageSelector
  , setKeyboardFocusRingNeedsDisplayInRectSelector
  , drawFocusRingMaskSelector
  , noteFocusRingMaskChangedSelector
  , windowSelector
  , superviewSelector
  , subviewsSelector
  , setSubviewsSelector
  , opaqueAncestorSelector
  , hiddenSelector
  , setHiddenSelector
  , hiddenOrHasHiddenAncestorSelector
  , wantsDefaultClippingSelector
  , postsFrameChangedNotificationsSelector
  , setPostsFrameChangedNotificationsSelector
  , autoresizesSubviewsSelector
  , setAutoresizesSubviewsSelector
  , autoresizingMaskSelector
  , setAutoresizingMaskSelector
  , frameSelector
  , setFrameSelector
  , frameRotationSelector
  , setFrameRotationSelector
  , frameCenterRotationSelector
  , setFrameCenterRotationSelector
  , boundsRotationSelector
  , setBoundsRotationSelector
  , boundsSelector
  , setBoundsSelector
  , flippedSelector
  , rotatedFromBaseSelector
  , rotatedOrScaledFromBaseSelector
  , opaqueSelector
  , canDrawConcurrentlySelector
  , setCanDrawConcurrentlySelector
  , canDrawSelector
  , needsDisplaySelector
  , setNeedsDisplaySelector
  , focusViewSelector
  , visibleRectSelector
  , tagSelector
  , needsPanelToBecomeKeySelector
  , mouseDownCanMoveWindowSelector
  , acceptsTouchEventsSelector
  , setAcceptsTouchEventsSelector
  , wantsRestingTouchesSelector
  , setWantsRestingTouchesSelector
  , layerContentsRedrawPolicySelector
  , setLayerContentsRedrawPolicySelector
  , layerContentsPlacementSelector
  , setLayerContentsPlacementSelector
  , wantsLayerSelector
  , setWantsLayerSelector
  , layerSelector
  , setLayerSelector
  , wantsUpdateLayerSelector
  , canDrawSubviewsIntoLayerSelector
  , setCanDrawSubviewsIntoLayerSelector
  , needsLayoutSelector
  , setNeedsLayoutSelector
  , alphaValueSelector
  , setAlphaValueSelector
  , layerUsesCoreImageFiltersSelector
  , setLayerUsesCoreImageFiltersSelector
  , backgroundFiltersSelector
  , setBackgroundFiltersSelector
  , compositingFilterSelector
  , setCompositingFilterSelector
  , contentFiltersSelector
  , setContentFiltersSelector
  , shadowSelector
  , setShadowSelector
  , clipsToBoundsSelector
  , setClipsToBoundsSelector
  , postsBoundsChangedNotificationsSelector
  , setPostsBoundsChangedNotificationsSelector
  , enclosingScrollViewSelector
  , defaultMenuSelector
  , toolTipSelector
  , setToolTipSelector
  , inLiveResizeSelector
  , preservesContentDuringLiveResizeSelector
  , rectPreservedDuringLiveResizeSelector
  , inputContextSelector
  , userInterfaceLayoutDirectionSelector
  , setUserInterfaceLayoutDirectionSelector
  , compatibleWithResponsiveScrollingSelector
  , preparedContentRectSelector
  , setPreparedContentRectSelector
  , allowsVibrancySelector
  , pressureConfigurationSelector
  , setPressureConfigurationSelector
  , wantsExtendedDynamicRangeOpenGLSurfaceSelector
  , setWantsExtendedDynamicRangeOpenGLSurfaceSelector
  , wantsBestResolutionOpenGLSurfaceSelector
  , setWantsBestResolutionOpenGLSurfaceSelector
  , layoutGuidesSelector
  , hasAmbiguousLayoutSelector
  , fittingSizeSelector
  , alignmentRectInsetsSelector
  , firstBaselineOffsetFromTopSelector
  , lastBaselineOffsetFromBottomSelector
  , baselineOffsetFromBottomSelector
  , intrinsicContentSizeSelector
  , horizontalContentSizeConstraintActiveSelector
  , setHorizontalContentSizeConstraintActiveSelector
  , verticalContentSizeConstraintActiveSelector
  , setVerticalContentSizeConstraintActiveSelector
  , translatesAutoresizingMaskIntoConstraintsSelector
  , setTranslatesAutoresizingMaskIntoConstraintsSelector
  , requiresConstraintBasedLayoutSelector
  , needsUpdateConstraintsSelector
  , setNeedsUpdateConstraintsSelector
  , leadingAnchorSelector
  , trailingAnchorSelector
  , leftAnchorSelector
  , rightAnchorSelector
  , topAnchorSelector
  , bottomAnchorSelector
  , widthAnchorSelector
  , heightAnchorSelector
  , centerXAnchorSelector
  , centerYAnchorSelector
  , firstBaselineAnchorSelector
  , lastBaselineAnchorSelector
  , constraintsSelector
  , candidateListTouchBarItemSelector
  , enclosingMenuItemSelector
  , writingToolsCoordinatorSelector
  , setWritingToolsCoordinatorSelector
  , trackingAreasSelector
  , prefersCompactControlSizeMetricsSelector
  , setPrefersCompactControlSizeMetricsSelector
  , safeAreaInsetsSelector
  , additionalSafeAreaInsetsSelector
  , setAdditionalSafeAreaInsetsSelector
  , safeAreaLayoutGuideSelector
  , safeAreaRectSelector
  , layoutMarginsGuideSelector
  , allowedTouchTypesSelector
  , setAllowedTouchTypesSelector
  , gestureRecognizersSelector
  , setGestureRecognizersSelector
  , drawingFindIndicatorSelector
  , inFullScreenModeSelector
  , registeredDraggedTypesSelector
  , heightAdjustLimitSelector
  , widthAdjustLimitSelector
  , pageHeaderSelector
  , pageFooterSelector
  , printJobTitleSelector
  , nextKeyViewSelector
  , setNextKeyViewSelector
  , previousKeyViewSelector
  , nextValidKeyViewSelector
  , previousValidKeyViewSelector
  , canBecomeKeyViewSelector
  , focusRingTypeSelector
  , setFocusRingTypeSelector
  , defaultFocusRingTypeSelector
  , focusRingMaskBoundsSelector

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSView nsView => nsView -> NSRect -> IO (Id NSView)
initWithFrame nsView  frameRect =
    sendMsg nsView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSView nsView, IsNSCoder coder) => nsView -> coder -> IO (Id NSView)
initWithCoder nsView  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- isDescendantOf:@
isDescendantOf :: (IsNSView nsView, IsNSView view) => nsView -> view -> IO Bool
isDescendantOf nsView  view =
  withObjCPtr view $ \raw_view ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "isDescendantOf:") retCULong [argPtr (castPtr raw_view :: Ptr ())]

-- | @- ancestorSharedWithView:@
ancestorSharedWithView :: (IsNSView nsView, IsNSView view) => nsView -> view -> IO (Id NSView)
ancestorSharedWithView nsView  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsView (mkSelector "ancestorSharedWithView:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- getRectsBeingDrawn:count:@
getRectsBeingDrawn_count :: IsNSView nsView => nsView -> Const (Ptr NSRect) -> Ptr CLong -> IO ()
getRectsBeingDrawn_count nsView  rects count =
    sendMsg nsView (mkSelector "getRectsBeingDrawn:count:") retVoid [argPtr (unConst rects), argPtr count]

-- | @- needsToDrawRect:@
needsToDrawRect :: IsNSView nsView => nsView -> NSRect -> IO Bool
needsToDrawRect nsView  rect =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "needsToDrawRect:") retCULong [argNSRect rect]

-- | @- viewDidHide@
viewDidHide :: IsNSView nsView => nsView -> IO ()
viewDidHide nsView  =
    sendMsg nsView (mkSelector "viewDidHide") retVoid []

-- | @- viewDidUnhide@
viewDidUnhide :: IsNSView nsView => nsView -> IO ()
viewDidUnhide nsView  =
    sendMsg nsView (mkSelector "viewDidUnhide") retVoid []

-- | @- addSubview:@
addSubview :: (IsNSView nsView, IsNSView view) => nsView -> view -> IO ()
addSubview nsView  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsView (mkSelector "addSubview:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- addSubview:positioned:relativeTo:@
addSubview_positioned_relativeTo :: (IsNSView nsView, IsNSView view, IsNSView otherView) => nsView -> view -> NSWindowOrderingMode -> otherView -> IO ()
addSubview_positioned_relativeTo nsView  view place otherView =
  withObjCPtr view $ \raw_view ->
    withObjCPtr otherView $ \raw_otherView ->
        sendMsg nsView (mkSelector "addSubview:positioned:relativeTo:") retVoid [argPtr (castPtr raw_view :: Ptr ()), argCLong (coerce place), argPtr (castPtr raw_otherView :: Ptr ())]

-- | @- sortSubviewsUsingFunction:context:@
sortSubviewsUsingFunction_context :: IsNSView nsView => nsView -> Ptr () -> Ptr () -> IO ()
sortSubviewsUsingFunction_context nsView  compare_ context =
    sendMsg nsView (mkSelector "sortSubviewsUsingFunction:context:") retVoid [argPtr compare_, argPtr context]

-- | @- viewWillMoveToWindow:@
viewWillMoveToWindow :: (IsNSView nsView, IsNSWindow newWindow) => nsView -> newWindow -> IO ()
viewWillMoveToWindow nsView  newWindow =
  withObjCPtr newWindow $ \raw_newWindow ->
      sendMsg nsView (mkSelector "viewWillMoveToWindow:") retVoid [argPtr (castPtr raw_newWindow :: Ptr ())]

-- | @- viewDidMoveToWindow@
viewDidMoveToWindow :: IsNSView nsView => nsView -> IO ()
viewDidMoveToWindow nsView  =
    sendMsg nsView (mkSelector "viewDidMoveToWindow") retVoid []

-- | @- viewWillMoveToSuperview:@
viewWillMoveToSuperview :: (IsNSView nsView, IsNSView newSuperview) => nsView -> newSuperview -> IO ()
viewWillMoveToSuperview nsView  newSuperview =
  withObjCPtr newSuperview $ \raw_newSuperview ->
      sendMsg nsView (mkSelector "viewWillMoveToSuperview:") retVoid [argPtr (castPtr raw_newSuperview :: Ptr ())]

-- | @- viewDidMoveToSuperview@
viewDidMoveToSuperview :: IsNSView nsView => nsView -> IO ()
viewDidMoveToSuperview nsView  =
    sendMsg nsView (mkSelector "viewDidMoveToSuperview") retVoid []

-- | @- didAddSubview:@
didAddSubview :: (IsNSView nsView, IsNSView subview) => nsView -> subview -> IO ()
didAddSubview nsView  subview =
  withObjCPtr subview $ \raw_subview ->
      sendMsg nsView (mkSelector "didAddSubview:") retVoid [argPtr (castPtr raw_subview :: Ptr ())]

-- | @- willRemoveSubview:@
willRemoveSubview :: (IsNSView nsView, IsNSView subview) => nsView -> subview -> IO ()
willRemoveSubview nsView  subview =
  withObjCPtr subview $ \raw_subview ->
      sendMsg nsView (mkSelector "willRemoveSubview:") retVoid [argPtr (castPtr raw_subview :: Ptr ())]

-- | @- removeFromSuperview@
removeFromSuperview :: IsNSView nsView => nsView -> IO ()
removeFromSuperview nsView  =
    sendMsg nsView (mkSelector "removeFromSuperview") retVoid []

-- | @- replaceSubview:with:@
replaceSubview_with :: (IsNSView nsView, IsNSView oldView, IsNSView newView) => nsView -> oldView -> newView -> IO ()
replaceSubview_with nsView  oldView newView =
  withObjCPtr oldView $ \raw_oldView ->
    withObjCPtr newView $ \raw_newView ->
        sendMsg nsView (mkSelector "replaceSubview:with:") retVoid [argPtr (castPtr raw_oldView :: Ptr ()), argPtr (castPtr raw_newView :: Ptr ())]

-- | @- removeFromSuperviewWithoutNeedingDisplay@
removeFromSuperviewWithoutNeedingDisplay :: IsNSView nsView => nsView -> IO ()
removeFromSuperviewWithoutNeedingDisplay nsView  =
    sendMsg nsView (mkSelector "removeFromSuperviewWithoutNeedingDisplay") retVoid []

-- | @- viewDidChangeBackingProperties@
viewDidChangeBackingProperties :: IsNSView nsView => nsView -> IO ()
viewDidChangeBackingProperties nsView  =
    sendMsg nsView (mkSelector "viewDidChangeBackingProperties") retVoid []

-- | @- resizeSubviewsWithOldSize:@
resizeSubviewsWithOldSize :: IsNSView nsView => nsView -> NSSize -> IO ()
resizeSubviewsWithOldSize nsView  oldSize =
    sendMsg nsView (mkSelector "resizeSubviewsWithOldSize:") retVoid [argNSSize oldSize]

-- | @- resizeWithOldSuperviewSize:@
resizeWithOldSuperviewSize :: IsNSView nsView => nsView -> NSSize -> IO ()
resizeWithOldSuperviewSize nsView  oldSize =
    sendMsg nsView (mkSelector "resizeWithOldSuperviewSize:") retVoid [argNSSize oldSize]

-- | @- setFrameOrigin:@
setFrameOrigin :: IsNSView nsView => nsView -> NSPoint -> IO ()
setFrameOrigin nsView  newOrigin =
    sendMsg nsView (mkSelector "setFrameOrigin:") retVoid [argNSPoint newOrigin]

-- | @- setFrameSize:@
setFrameSize :: IsNSView nsView => nsView -> NSSize -> IO ()
setFrameSize nsView  newSize =
    sendMsg nsView (mkSelector "setFrameSize:") retVoid [argNSSize newSize]

-- | @- setBoundsOrigin:@
setBoundsOrigin :: IsNSView nsView => nsView -> NSPoint -> IO ()
setBoundsOrigin nsView  newOrigin =
    sendMsg nsView (mkSelector "setBoundsOrigin:") retVoid [argNSPoint newOrigin]

-- | @- setBoundsSize:@
setBoundsSize :: IsNSView nsView => nsView -> NSSize -> IO ()
setBoundsSize nsView  newSize =
    sendMsg nsView (mkSelector "setBoundsSize:") retVoid [argNSSize newSize]

-- | @- translateOriginToPoint:@
translateOriginToPoint :: IsNSView nsView => nsView -> NSPoint -> IO ()
translateOriginToPoint nsView  translation =
    sendMsg nsView (mkSelector "translateOriginToPoint:") retVoid [argNSPoint translation]

-- | @- scaleUnitSquareToSize:@
scaleUnitSquareToSize :: IsNSView nsView => nsView -> NSSize -> IO ()
scaleUnitSquareToSize nsView  newUnitSize =
    sendMsg nsView (mkSelector "scaleUnitSquareToSize:") retVoid [argNSSize newUnitSize]

-- | @- rotateByAngle:@
rotateByAngle :: IsNSView nsView => nsView -> CDouble -> IO ()
rotateByAngle nsView  angle =
    sendMsg nsView (mkSelector "rotateByAngle:") retVoid [argCDouble angle]

-- | @- convertPoint:fromView:@
convertPoint_fromView :: (IsNSView nsView, IsNSView view) => nsView -> NSPoint -> view -> IO NSPoint
convertPoint_fromView nsView  point view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsView (mkSelector "convertPoint:fromView:") retNSPoint [argNSPoint point, argPtr (castPtr raw_view :: Ptr ())]

-- | @- convertPoint:toView:@
convertPoint_toView :: (IsNSView nsView, IsNSView view) => nsView -> NSPoint -> view -> IO NSPoint
convertPoint_toView nsView  point view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsView (mkSelector "convertPoint:toView:") retNSPoint [argNSPoint point, argPtr (castPtr raw_view :: Ptr ())]

-- | @- convertSize:fromView:@
convertSize_fromView :: (IsNSView nsView, IsNSView view) => nsView -> NSSize -> view -> IO NSSize
convertSize_fromView nsView  size view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsView (mkSelector "convertSize:fromView:") retNSSize [argNSSize size, argPtr (castPtr raw_view :: Ptr ())]

-- | @- convertSize:toView:@
convertSize_toView :: (IsNSView nsView, IsNSView view) => nsView -> NSSize -> view -> IO NSSize
convertSize_toView nsView  size view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsView (mkSelector "convertSize:toView:") retNSSize [argNSSize size, argPtr (castPtr raw_view :: Ptr ())]

-- | @- convertRect:fromView:@
convertRect_fromView :: (IsNSView nsView, IsNSView view) => nsView -> NSRect -> view -> IO NSRect
convertRect_fromView nsView  rect view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsView (mkSelector "convertRect:fromView:") retNSRect [argNSRect rect, argPtr (castPtr raw_view :: Ptr ())]

-- | @- convertRect:toView:@
convertRect_toView :: (IsNSView nsView, IsNSView view) => nsView -> NSRect -> view -> IO NSRect
convertRect_toView nsView  rect view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsView (mkSelector "convertRect:toView:") retNSRect [argNSRect rect, argPtr (castPtr raw_view :: Ptr ())]

-- | @- backingAlignedRect:options:@
backingAlignedRect_options :: IsNSView nsView => nsView -> NSRect -> NSAlignmentOptions -> IO NSRect
backingAlignedRect_options nsView  rect options =
    sendMsgStret nsView (mkSelector "backingAlignedRect:options:") retNSRect [argNSRect rect, argCULong (coerce options)]

-- | @- centerScanRect:@
centerScanRect :: IsNSView nsView => nsView -> NSRect -> IO NSRect
centerScanRect nsView  rect =
    sendMsgStret nsView (mkSelector "centerScanRect:") retNSRect [argNSRect rect]

-- | @- convertPointToBacking:@
convertPointToBacking :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointToBacking nsView  point =
    sendMsgStret nsView (mkSelector "convertPointToBacking:") retNSPoint [argNSPoint point]

-- | @- convertPointFromBacking:@
convertPointFromBacking :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointFromBacking nsView  point =
    sendMsgStret nsView (mkSelector "convertPointFromBacking:") retNSPoint [argNSPoint point]

-- | @- convertSizeToBacking:@
convertSizeToBacking :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeToBacking nsView  size =
    sendMsgStret nsView (mkSelector "convertSizeToBacking:") retNSSize [argNSSize size]

-- | @- convertSizeFromBacking:@
convertSizeFromBacking :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeFromBacking nsView  size =
    sendMsgStret nsView (mkSelector "convertSizeFromBacking:") retNSSize [argNSSize size]

-- | @- convertRectToBacking:@
convertRectToBacking :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectToBacking nsView  rect =
    sendMsgStret nsView (mkSelector "convertRectToBacking:") retNSRect [argNSRect rect]

-- | @- convertRectFromBacking:@
convertRectFromBacking :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectFromBacking nsView  rect =
    sendMsgStret nsView (mkSelector "convertRectFromBacking:") retNSRect [argNSRect rect]

-- | @- convertPointToLayer:@
convertPointToLayer :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointToLayer nsView  point =
    sendMsgStret nsView (mkSelector "convertPointToLayer:") retNSPoint [argNSPoint point]

-- | @- convertPointFromLayer:@
convertPointFromLayer :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointFromLayer nsView  point =
    sendMsgStret nsView (mkSelector "convertPointFromLayer:") retNSPoint [argNSPoint point]

-- | @- convertSizeToLayer:@
convertSizeToLayer :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeToLayer nsView  size =
    sendMsgStret nsView (mkSelector "convertSizeToLayer:") retNSSize [argNSSize size]

-- | @- convertSizeFromLayer:@
convertSizeFromLayer :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeFromLayer nsView  size =
    sendMsgStret nsView (mkSelector "convertSizeFromLayer:") retNSSize [argNSSize size]

-- | @- convertRectToLayer:@
convertRectToLayer :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectToLayer nsView  rect =
    sendMsgStret nsView (mkSelector "convertRectToLayer:") retNSRect [argNSRect rect]

-- | @- convertRectFromLayer:@
convertRectFromLayer :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectFromLayer nsView  rect =
    sendMsgStret nsView (mkSelector "convertRectFromLayer:") retNSRect [argNSRect rect]

-- | @- setNeedsDisplayInRect:@
setNeedsDisplayInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
setNeedsDisplayInRect nsView  invalidRect =
    sendMsg nsView (mkSelector "setNeedsDisplayInRect:") retVoid [argNSRect invalidRect]

-- | @- lockFocus@
lockFocus :: IsNSView nsView => nsView -> IO ()
lockFocus nsView  =
    sendMsg nsView (mkSelector "lockFocus") retVoid []

-- | @- unlockFocus@
unlockFocus :: IsNSView nsView => nsView -> IO ()
unlockFocus nsView  =
    sendMsg nsView (mkSelector "unlockFocus") retVoid []

-- | @- lockFocusIfCanDraw@
lockFocusIfCanDraw :: IsNSView nsView => nsView -> IO Bool
lockFocusIfCanDraw nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "lockFocusIfCanDraw") retCULong []

-- | @- lockFocusIfCanDrawInContext:@
lockFocusIfCanDrawInContext :: (IsNSView nsView, IsNSGraphicsContext context) => nsView -> context -> IO Bool
lockFocusIfCanDrawInContext nsView  context =
  withObjCPtr context $ \raw_context ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "lockFocusIfCanDrawInContext:") retCULong [argPtr (castPtr raw_context :: Ptr ())]

-- | @- display@
display :: IsNSView nsView => nsView -> IO ()
display nsView  =
    sendMsg nsView (mkSelector "display") retVoid []

-- | @- displayIfNeeded@
displayIfNeeded :: IsNSView nsView => nsView -> IO ()
displayIfNeeded nsView  =
    sendMsg nsView (mkSelector "displayIfNeeded") retVoid []

-- | @- displayIfNeededIgnoringOpacity@
displayIfNeededIgnoringOpacity :: IsNSView nsView => nsView -> IO ()
displayIfNeededIgnoringOpacity nsView  =
    sendMsg nsView (mkSelector "displayIfNeededIgnoringOpacity") retVoid []

-- | @- displayRect:@
displayRect :: IsNSView nsView => nsView -> NSRect -> IO ()
displayRect nsView  rect =
    sendMsg nsView (mkSelector "displayRect:") retVoid [argNSRect rect]

-- | @- displayIfNeededInRect:@
displayIfNeededInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
displayIfNeededInRect nsView  rect =
    sendMsg nsView (mkSelector "displayIfNeededInRect:") retVoid [argNSRect rect]

-- | @- displayRectIgnoringOpacity:@
displayRectIgnoringOpacity :: IsNSView nsView => nsView -> NSRect -> IO ()
displayRectIgnoringOpacity nsView  rect =
    sendMsg nsView (mkSelector "displayRectIgnoringOpacity:") retVoid [argNSRect rect]

-- | @- displayIfNeededInRectIgnoringOpacity:@
displayIfNeededInRectIgnoringOpacity :: IsNSView nsView => nsView -> NSRect -> IO ()
displayIfNeededInRectIgnoringOpacity nsView  rect =
    sendMsg nsView (mkSelector "displayIfNeededInRectIgnoringOpacity:") retVoid [argNSRect rect]

-- | @- drawRect:@
drawRect :: IsNSView nsView => nsView -> NSRect -> IO ()
drawRect nsView  dirtyRect =
    sendMsg nsView (mkSelector "drawRect:") retVoid [argNSRect dirtyRect]

-- | @- displayRectIgnoringOpacity:inContext:@
displayRectIgnoringOpacity_inContext :: (IsNSView nsView, IsNSGraphicsContext context) => nsView -> NSRect -> context -> IO ()
displayRectIgnoringOpacity_inContext nsView  rect context =
  withObjCPtr context $ \raw_context ->
      sendMsg nsView (mkSelector "displayRectIgnoringOpacity:inContext:") retVoid [argNSRect rect, argPtr (castPtr raw_context :: Ptr ())]

-- | @- bitmapImageRepForCachingDisplayInRect:@
bitmapImageRepForCachingDisplayInRect :: IsNSView nsView => nsView -> NSRect -> IO (Id NSBitmapImageRep)
bitmapImageRepForCachingDisplayInRect nsView  rect =
    sendMsg nsView (mkSelector "bitmapImageRepForCachingDisplayInRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- cacheDisplayInRect:toBitmapImageRep:@
cacheDisplayInRect_toBitmapImageRep :: (IsNSView nsView, IsNSBitmapImageRep bitmapImageRep) => nsView -> NSRect -> bitmapImageRep -> IO ()
cacheDisplayInRect_toBitmapImageRep nsView  rect bitmapImageRep =
  withObjCPtr bitmapImageRep $ \raw_bitmapImageRep ->
      sendMsg nsView (mkSelector "cacheDisplayInRect:toBitmapImageRep:") retVoid [argNSRect rect, argPtr (castPtr raw_bitmapImageRep :: Ptr ())]

-- | @- viewWillDraw@
viewWillDraw :: IsNSView nsView => nsView -> IO ()
viewWillDraw nsView  =
    sendMsg nsView (mkSelector "viewWillDraw") retVoid []

-- | @- scrollPoint:@
scrollPoint :: IsNSView nsView => nsView -> NSPoint -> IO ()
scrollPoint nsView  point =
    sendMsg nsView (mkSelector "scrollPoint:") retVoid [argNSPoint point]

-- | @- scrollRectToVisible:@
scrollRectToVisible :: IsNSView nsView => nsView -> NSRect -> IO Bool
scrollRectToVisible nsView  rect =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "scrollRectToVisible:") retCULong [argNSRect rect]

-- | @- autoscroll:@
autoscroll :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
autoscroll nsView  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "autoscroll:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- adjustScroll:@
adjustScroll :: IsNSView nsView => nsView -> NSRect -> IO NSRect
adjustScroll nsView  newVisible =
    sendMsgStret nsView (mkSelector "adjustScroll:") retNSRect [argNSRect newVisible]

-- | @- scrollRect:by:@
scrollRect_by :: IsNSView nsView => nsView -> NSRect -> NSSize -> IO ()
scrollRect_by nsView  rect delta =
    sendMsg nsView (mkSelector "scrollRect:by:") retVoid [argNSRect rect, argNSSize delta]

-- | @- translateRectsNeedingDisplayInRect:by:@
translateRectsNeedingDisplayInRect_by :: IsNSView nsView => nsView -> NSRect -> NSSize -> IO ()
translateRectsNeedingDisplayInRect_by nsView  clipRect delta =
    sendMsg nsView (mkSelector "translateRectsNeedingDisplayInRect:by:") retVoid [argNSRect clipRect, argNSSize delta]

-- | @- hitTest:@
hitTest :: IsNSView nsView => nsView -> NSPoint -> IO (Id NSView)
hitTest nsView  point =
    sendMsg nsView (mkSelector "hitTest:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- mouse:inRect:@
mouse_inRect :: IsNSView nsView => nsView -> NSPoint -> NSRect -> IO Bool
mouse_inRect nsView  point rect =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "mouse:inRect:") retCULong [argNSPoint point, argNSRect rect]

-- | @- viewWithTag:@
viewWithTag :: IsNSView nsView => nsView -> CLong -> IO (Id NSView)
viewWithTag nsView  tag =
    sendMsg nsView (mkSelector "viewWithTag:") (retPtr retVoid) [argCLong tag] >>= retainedObject . castPtr

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
performKeyEquivalent nsView  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "performKeyEquivalent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- acceptsFirstMouse:@
acceptsFirstMouse :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
acceptsFirstMouse nsView  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "acceptsFirstMouse:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- shouldDelayWindowOrderingForEvent:@
shouldDelayWindowOrderingForEvent :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO Bool
shouldDelayWindowOrderingForEvent nsView  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "shouldDelayWindowOrderingForEvent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- makeBackingLayer@
makeBackingLayer :: IsNSView nsView => nsView -> IO (Id CALayer)
makeBackingLayer nsView  =
    sendMsg nsView (mkSelector "makeBackingLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updateLayer@
updateLayer :: IsNSView nsView => nsView -> IO ()
updateLayer nsView  =
    sendMsg nsView (mkSelector "updateLayer") retVoid []

-- | @- layoutSubtreeIfNeeded@
layoutSubtreeIfNeeded :: IsNSView nsView => nsView -> IO ()
layoutSubtreeIfNeeded nsView  =
    sendMsg nsView (mkSelector "layoutSubtreeIfNeeded") retVoid []

-- | @- layout@
layout :: IsNSView nsView => nsView -> IO ()
layout nsView  =
    sendMsg nsView (mkSelector "layout") retVoid []

-- | @- menuForEvent:@
menuForEvent :: (IsNSView nsView, IsNSEvent event) => nsView -> event -> IO (Id NSMenu)
menuForEvent nsView  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsView (mkSelector "menuForEvent:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ())] >>= retainedObject . castPtr

-- | A contextual menu is being opened from the receiving view. The view should update any visual state in response — such as making a selection.
--
-- @menu@ — The contextual menu that is being opened on the view
--
-- @event@ — The event that caused the menu to open.
--
-- ObjC selector: @- willOpenMenu:withEvent:@
willOpenMenu_withEvent :: (IsNSView nsView, IsNSMenu menu, IsNSEvent event) => nsView -> menu -> event -> IO ()
willOpenMenu_withEvent nsView  menu event =
  withObjCPtr menu $ \raw_menu ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsView (mkSelector "willOpenMenu:withEvent:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | A contextual menu shown from the receiving view has been closed. This is only called if the menu had been opened and the view previously received @-willOpenMenu:withEvent:.@ The view should update any visual state in response — such as removing a temporary selection.
--
-- @menu@ — The contextual menu that was open on the view
--
-- @event@ — The event that caused the menu to close. This may be nil if there is no specific event that triggered the closing.
--
-- ObjC selector: @- didCloseMenu:withEvent:@
didCloseMenu_withEvent :: (IsNSView nsView, IsNSMenu menu, IsNSEvent event) => nsView -> menu -> event -> IO ()
didCloseMenu_withEvent nsView  menu event =
  withObjCPtr menu $ \raw_menu ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsView (mkSelector "didCloseMenu:withEvent:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | @- addToolTipRect:owner:userData:@
addToolTipRect_owner_userData :: IsNSView nsView => nsView -> NSRect -> RawId -> Ptr () -> IO CLong
addToolTipRect_owner_userData nsView  rect owner data_ =
    sendMsg nsView (mkSelector "addToolTipRect:owner:userData:") retCLong [argNSRect rect, argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr data_]

-- | @- removeToolTip:@
removeToolTip :: IsNSView nsView => nsView -> CLong -> IO ()
removeToolTip nsView  tag =
    sendMsg nsView (mkSelector "removeToolTip:") retVoid [argCLong tag]

-- | @- removeAllToolTips@
removeAllToolTips :: IsNSView nsView => nsView -> IO ()
removeAllToolTips nsView  =
    sendMsg nsView (mkSelector "removeAllToolTips") retVoid []

-- | @- viewWillStartLiveResize@
viewWillStartLiveResize :: IsNSView nsView => nsView -> IO ()
viewWillStartLiveResize nsView  =
    sendMsg nsView (mkSelector "viewWillStartLiveResize") retVoid []

-- | @- viewDidEndLiveResize@
viewDidEndLiveResize :: IsNSView nsView => nsView -> IO ()
viewDidEndLiveResize nsView  =
    sendMsg nsView (mkSelector "viewDidEndLiveResize") retVoid []

-- | @- getRectsExposedDuringLiveResize:count:@
getRectsExposedDuringLiveResize_count :: IsNSView nsView => nsView -> Ptr NSRect -> Ptr CLong -> IO ()
getRectsExposedDuringLiveResize_count nsView  exposedRects count =
    sendMsg nsView (mkSelector "getRectsExposedDuringLiveResize:count:") retVoid [argPtr exposedRects, argPtr count]

-- | @- rectForSmartMagnificationAtPoint:inRect:@
rectForSmartMagnificationAtPoint_inRect :: IsNSView nsView => nsView -> NSPoint -> NSRect -> IO NSRect
rectForSmartMagnificationAtPoint_inRect nsView  location visibleRect =
    sendMsgStret nsView (mkSelector "rectForSmartMagnificationAtPoint:inRect:") retNSRect [argNSPoint location, argNSRect visibleRect]

-- | @- prepareForReuse@
prepareForReuse :: IsNSView nsView => nsView -> IO ()
prepareForReuse nsView  =
    sendMsg nsView (mkSelector "prepareForReuse") retVoid []

-- | @- prepareContentInRect:@
prepareContentInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
prepareContentInRect nsView  rect =
    sendMsg nsView (mkSelector "prepareContentInRect:") retVoid [argNSRect rect]

-- | Override point for reacting to the effective appearance of the receiver changing. At this point @effectiveAppearance@ property reflects the new appearance.
--
-- ObjC selector: @- viewDidChangeEffectiveAppearance@
viewDidChangeEffectiveAppearance :: IsNSView nsView => nsView -> IO ()
viewDidChangeEffectiveAppearance nsView  =
    sendMsg nsView (mkSelector "viewDidChangeEffectiveAppearance") retVoid []

-- | @- rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO Bool
rulerView_shouldMoveMarker nsView  ruler marker =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "rulerView:shouldMoveMarker:") retCULong [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocation :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willMoveMarker_toLocation nsView  ruler marker location =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        sendMsg nsView (mkSelector "rulerView:willMoveMarker:toLocation:") retCDouble [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ()), argCDouble location]

-- | @- rulerView:didMoveMarker:@
rulerView_didMoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO ()
rulerView_didMoveMarker nsView  ruler marker =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        sendMsg nsView (mkSelector "rulerView:didMoveMarker:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO Bool
rulerView_shouldRemoveMarker nsView  ruler marker =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "rulerView:shouldRemoveMarker:") retCULong [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:didRemoveMarker:@
rulerView_didRemoveMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO ()
rulerView_didRemoveMarker nsView  ruler marker =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        sendMsg nsView (mkSelector "rulerView:didRemoveMarker:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:shouldAddMarker:@
rulerView_shouldAddMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO Bool
rulerView_shouldAddMarker nsView  ruler marker =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "rulerView:shouldAddMarker:") retCULong [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocation :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willAddMarker_atLocation nsView  ruler marker location =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        sendMsg nsView (mkSelector "rulerView:willAddMarker:atLocation:") retCDouble [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ()), argCDouble location]

-- | @- rulerView:didAddMarker:@
rulerView_didAddMarker :: (IsNSView nsView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsView -> ruler -> marker -> IO ()
rulerView_didAddMarker nsView  ruler marker =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr marker $ \raw_marker ->
        sendMsg nsView (mkSelector "rulerView:didAddMarker:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:handleMouseDown:@
rulerView_handleMouseDown :: (IsNSView nsView, IsNSRulerView ruler, IsNSEvent event) => nsView -> ruler -> event -> IO ()
rulerView_handleMouseDown nsView  ruler event =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsView (mkSelector "rulerView:handleMouseDown:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | @- rulerView:willSetClientView:@
rulerView_willSetClientView :: (IsNSView nsView, IsNSRulerView ruler, IsNSView newClient) => nsView -> ruler -> newClient -> IO ()
rulerView_willSetClientView nsView  ruler newClient =
  withObjCPtr ruler $ \raw_ruler ->
    withObjCPtr newClient $ \raw_newClient ->
        sendMsg nsView (mkSelector "rulerView:willSetClientView:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_newClient :: Ptr ())]

-- | @- rulerView:locationForPoint:@
rulerView_locationForPoint :: (IsNSView nsView, IsNSRulerView ruler) => nsView -> ruler -> NSPoint -> IO CDouble
rulerView_locationForPoint nsView  ruler point =
  withObjCPtr ruler $ \raw_ruler ->
      sendMsg nsView (mkSelector "rulerView:locationForPoint:") retCDouble [argPtr (castPtr raw_ruler :: Ptr ()), argNSPoint point]

-- | @- rulerView:pointForLocation:@
rulerView_pointForLocation :: (IsNSView nsView, IsNSRulerView ruler) => nsView -> ruler -> CDouble -> IO NSPoint
rulerView_pointForLocation nsView  ruler point =
  withObjCPtr ruler $ \raw_ruler ->
      sendMsgStret nsView (mkSelector "rulerView:pointForLocation:") retNSPoint [argPtr (castPtr raw_ruler :: Ptr ()), argCDouble point]

-- | @- layoutGuideForLayoutRegion:@
layoutGuideForLayoutRegion :: (IsNSView nsView, IsNSViewLayoutRegion layoutRegion) => nsView -> layoutRegion -> IO (Id NSLayoutGuide)
layoutGuideForLayoutRegion nsView  layoutRegion =
  withObjCPtr layoutRegion $ \raw_layoutRegion ->
      sendMsg nsView (mkSelector "layoutGuideForLayoutRegion:") (retPtr retVoid) [argPtr (castPtr raw_layoutRegion :: Ptr ())] >>= retainedObject . castPtr

-- | @- edgeInsetsForLayoutRegion:@
edgeInsetsForLayoutRegion :: (IsNSView nsView, IsNSViewLayoutRegion layoutRegion) => nsView -> layoutRegion -> IO NSEdgeInsets
edgeInsetsForLayoutRegion nsView  layoutRegion =
  withObjCPtr layoutRegion $ \raw_layoutRegion ->
      sendMsgStret nsView (mkSelector "edgeInsetsForLayoutRegion:") retNSEdgeInsets [argPtr (castPtr raw_layoutRegion :: Ptr ())]

-- | @- rectForLayoutRegion:@
rectForLayoutRegion :: (IsNSView nsView, IsNSViewLayoutRegion layoutRegion) => nsView -> layoutRegion -> IO NSRect
rectForLayoutRegion nsView  layoutRegion =
  withObjCPtr layoutRegion $ \raw_layoutRegion ->
      sendMsgStret nsView (mkSelector "rectForLayoutRegion:") retNSRect [argPtr (castPtr raw_layoutRegion :: Ptr ())]

-- | @- addLayoutGuide:@
addLayoutGuide :: (IsNSView nsView, IsNSLayoutGuide guide) => nsView -> guide -> IO ()
addLayoutGuide nsView  guide =
  withObjCPtr guide $ \raw_guide ->
      sendMsg nsView (mkSelector "addLayoutGuide:") retVoid [argPtr (castPtr raw_guide :: Ptr ())]

-- | @- removeLayoutGuide:@
removeLayoutGuide :: (IsNSView nsView, IsNSLayoutGuide guide) => nsView -> guide -> IO ()
removeLayoutGuide nsView  guide =
  withObjCPtr guide $ \raw_guide ->
      sendMsg nsView (mkSelector "removeLayoutGuide:") retVoid [argPtr (castPtr raw_guide :: Ptr ())]

-- | @- constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientation :: IsNSView nsView => nsView -> NSLayoutConstraintOrientation -> IO (Id NSArray)
constraintsAffectingLayoutForOrientation nsView  orientation =
    sendMsg nsView (mkSelector "constraintsAffectingLayoutForOrientation:") (retPtr retVoid) [argCLong (coerce orientation)] >>= retainedObject . castPtr

-- | @- exerciseAmbiguityInLayout@
exerciseAmbiguityInLayout :: IsNSView nsView => nsView -> IO ()
exerciseAmbiguityInLayout nsView  =
    sendMsg nsView (mkSelector "exerciseAmbiguityInLayout") retVoid []

-- | @- alignmentRectForFrame:@
alignmentRectForFrame :: IsNSView nsView => nsView -> NSRect -> IO NSRect
alignmentRectForFrame nsView  frame =
    sendMsgStret nsView (mkSelector "alignmentRectForFrame:") retNSRect [argNSRect frame]

-- | @- frameForAlignmentRect:@
frameForAlignmentRect :: IsNSView nsView => nsView -> NSRect -> IO NSRect
frameForAlignmentRect nsView  alignmentRect =
    sendMsgStret nsView (mkSelector "frameForAlignmentRect:") retNSRect [argNSRect alignmentRect]

-- | @- invalidateIntrinsicContentSize@
invalidateIntrinsicContentSize :: IsNSView nsView => nsView -> IO ()
invalidateIntrinsicContentSize nsView  =
    sendMsg nsView (mkSelector "invalidateIntrinsicContentSize") retVoid []

-- | @- contentHuggingPriorityForOrientation:@
contentHuggingPriorityForOrientation :: IsNSView nsView => nsView -> NSLayoutConstraintOrientation -> IO CFloat
contentHuggingPriorityForOrientation nsView  orientation =
    sendMsg nsView (mkSelector "contentHuggingPriorityForOrientation:") retCFloat [argCLong (coerce orientation)]

-- | @- setContentHuggingPriority:forOrientation:@
setContentHuggingPriority_forOrientation :: IsNSView nsView => nsView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setContentHuggingPriority_forOrientation nsView  priority orientation =
    sendMsg nsView (mkSelector "setContentHuggingPriority:forOrientation:") retVoid [argCFloat priority, argCLong (coerce orientation)]

-- | @- contentCompressionResistancePriorityForOrientation:@
contentCompressionResistancePriorityForOrientation :: IsNSView nsView => nsView -> NSLayoutConstraintOrientation -> IO CFloat
contentCompressionResistancePriorityForOrientation nsView  orientation =
    sendMsg nsView (mkSelector "contentCompressionResistancePriorityForOrientation:") retCFloat [argCLong (coerce orientation)]

-- | @- setContentCompressionResistancePriority:forOrientation:@
setContentCompressionResistancePriority_forOrientation :: IsNSView nsView => nsView -> CFloat -> NSLayoutConstraintOrientation -> IO ()
setContentCompressionResistancePriority_forOrientation nsView  priority orientation =
    sendMsg nsView (mkSelector "setContentCompressionResistancePriority:forOrientation:") retVoid [argCFloat priority, argCLong (coerce orientation)]

-- | @- updateConstraintsForSubtreeIfNeeded@
updateConstraintsForSubtreeIfNeeded :: IsNSView nsView => nsView -> IO ()
updateConstraintsForSubtreeIfNeeded nsView  =
    sendMsg nsView (mkSelector "updateConstraintsForSubtreeIfNeeded") retVoid []

-- | @- updateConstraints@
updateConstraints :: IsNSView nsView => nsView -> IO ()
updateConstraints nsView  =
    sendMsg nsView (mkSelector "updateConstraints") retVoid []

-- | @- addConstraint:@
addConstraint :: (IsNSView nsView, IsNSLayoutConstraint constraint) => nsView -> constraint -> IO ()
addConstraint nsView  constraint =
  withObjCPtr constraint $ \raw_constraint ->
      sendMsg nsView (mkSelector "addConstraint:") retVoid [argPtr (castPtr raw_constraint :: Ptr ())]

-- | @- addConstraints:@
addConstraints :: (IsNSView nsView, IsNSArray constraints) => nsView -> constraints -> IO ()
addConstraints nsView  constraints =
  withObjCPtr constraints $ \raw_constraints ->
      sendMsg nsView (mkSelector "addConstraints:") retVoid [argPtr (castPtr raw_constraints :: Ptr ())]

-- | @- removeConstraint:@
removeConstraint :: (IsNSView nsView, IsNSLayoutConstraint constraint) => nsView -> constraint -> IO ()
removeConstraint nsView  constraint =
  withObjCPtr constraint $ \raw_constraint ->
      sendMsg nsView (mkSelector "removeConstraint:") retVoid [argPtr (castPtr raw_constraint :: Ptr ())]

-- | @- removeConstraints:@
removeConstraints :: (IsNSView nsView, IsNSArray constraints) => nsView -> constraints -> IO ()
removeConstraints nsView  constraints =
  withObjCPtr constraints $ \raw_constraints ->
      sendMsg nsView (mkSelector "removeConstraints:") retVoid [argPtr (castPtr raw_constraints :: Ptr ())]

-- | @- reflectScrolledClipView:@
reflectScrolledClipView :: (IsNSView nsView, IsNSClipView clipView) => nsView -> clipView -> IO ()
reflectScrolledClipView nsView  clipView =
  withObjCPtr clipView $ \raw_clipView ->
      sendMsg nsView (mkSelector "reflectScrolledClipView:") retVoid [argPtr (castPtr raw_clipView :: Ptr ())]

-- | @- scrollClipView:toPoint:@
scrollClipView_toPoint :: (IsNSView nsView, IsNSClipView clipView) => nsView -> clipView -> NSPoint -> IO ()
scrollClipView_toPoint nsView  clipView point =
  withObjCPtr clipView $ \raw_clipView ->
      sendMsg nsView (mkSelector "scrollClipView:toPoint:") retVoid [argPtr (castPtr raw_clipView :: Ptr ()), argNSPoint point]

-- | @- dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBack :: (IsNSView nsView, IsNSImage image, IsNSEvent event, IsNSPasteboard pboard) => nsView -> image -> NSPoint -> NSSize -> event -> pboard -> RawId -> Bool -> IO ()
dragImage_at_offset_event_pasteboard_source_slideBack nsView  image viewLocation initialOffset event pboard sourceObj slideFlag =
  withObjCPtr image $ \raw_image ->
    withObjCPtr event $ \raw_event ->
      withObjCPtr pboard $ \raw_pboard ->
          sendMsg nsView (mkSelector "dragImage:at:offset:event:pasteboard:source:slideBack:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint viewLocation, argNSSize initialOffset, argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_pboard :: Ptr ()), argPtr (castPtr (unRawId sourceObj) :: Ptr ()), argCULong (if slideFlag then 1 else 0)]

-- | @- dragFile:fromRect:slideBack:event:@
dragFile_fromRect_slideBack_event :: (IsNSView nsView, IsNSString filename, IsNSEvent event) => nsView -> filename -> NSRect -> Bool -> event -> IO Bool
dragFile_fromRect_slideBack_event nsView  filename rect flag event =
  withObjCPtr filename $ \raw_filename ->
    withObjCPtr event $ \raw_event ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "dragFile:fromRect:slideBack:event:") retCULong [argPtr (castPtr raw_filename :: Ptr ()), argNSRect rect, argCULong (if flag then 1 else 0), argPtr (castPtr raw_event :: Ptr ())]

-- | @- dragPromisedFilesOfTypes:fromRect:source:slideBack:event:@
dragPromisedFilesOfTypes_fromRect_source_slideBack_event :: (IsNSView nsView, IsNSArray typeArray, IsNSEvent event) => nsView -> typeArray -> NSRect -> RawId -> Bool -> event -> IO Bool
dragPromisedFilesOfTypes_fromRect_source_slideBack_event nsView  typeArray rect sourceObject flag event =
  withObjCPtr typeArray $ \raw_typeArray ->
    withObjCPtr event $ \raw_event ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "dragPromisedFilesOfTypes:fromRect:source:slideBack:event:") retCULong [argPtr (castPtr raw_typeArray :: Ptr ()), argNSRect rect, argPtr (castPtr (unRawId sourceObject) :: Ptr ()), argCULong (if flag then 1 else 0), argPtr (castPtr raw_event :: Ptr ())]

-- | @- convertPointToBase:@
convertPointToBase :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointToBase nsView  point =
    sendMsgStret nsView (mkSelector "convertPointToBase:") retNSPoint [argNSPoint point]

-- | @- convertPointFromBase:@
convertPointFromBase :: IsNSView nsView => nsView -> NSPoint -> IO NSPoint
convertPointFromBase nsView  point =
    sendMsgStret nsView (mkSelector "convertPointFromBase:") retNSPoint [argNSPoint point]

-- | @- convertSizeToBase:@
convertSizeToBase :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeToBase nsView  size =
    sendMsgStret nsView (mkSelector "convertSizeToBase:") retNSSize [argNSSize size]

-- | @- convertSizeFromBase:@
convertSizeFromBase :: IsNSView nsView => nsView -> NSSize -> IO NSSize
convertSizeFromBase nsView  size =
    sendMsgStret nsView (mkSelector "convertSizeFromBase:") retNSSize [argNSSize size]

-- | @- convertRectToBase:@
convertRectToBase :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectToBase nsView  rect =
    sendMsgStret nsView (mkSelector "convertRectToBase:") retNSRect [argNSRect rect]

-- | @- convertRectFromBase:@
convertRectFromBase :: IsNSView nsView => nsView -> NSRect -> IO NSRect
convertRectFromBase nsView  rect =
    sendMsgStret nsView (mkSelector "convertRectFromBase:") retNSRect [argNSRect rect]

-- | @- performMnemonic:@
performMnemonic :: (IsNSView nsView, IsNSString string) => nsView -> string -> IO Bool
performMnemonic nsView  string =
  withObjCPtr string $ \raw_string ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "performMnemonic:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- shouldDrawColor@
shouldDrawColor :: IsNSView nsView => nsView -> IO Bool
shouldDrawColor nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "shouldDrawColor") retCULong []

-- | @- gState@
gState :: IsNSView nsView => nsView -> IO CLong
gState nsView  =
    sendMsg nsView (mkSelector "gState") retCLong []

-- | @- allocateGState@
allocateGState :: IsNSView nsView => nsView -> IO ()
allocateGState nsView  =
    sendMsg nsView (mkSelector "allocateGState") retVoid []

-- | @- releaseGState@
releaseGState :: IsNSView nsView => nsView -> IO ()
releaseGState nsView  =
    sendMsg nsView (mkSelector "releaseGState") retVoid []

-- | @- setUpGState@
setUpGState :: IsNSView nsView => nsView -> IO ()
setUpGState nsView  =
    sendMsg nsView (mkSelector "setUpGState") retVoid []

-- | @- renewGState@
renewGState :: IsNSView nsView => nsView -> IO ()
renewGState nsView  =
    sendMsg nsView (mkSelector "renewGState") retVoid []

-- | @- displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: IsNSView nsView => nsView -> RawId -> Selector -> IO (Id CADisplayLink)
displayLinkWithTarget_selector nsView  target selector =
    sendMsg nsView (mkSelector "displayLinkWithTarget:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | @- addTrackingArea:@
addTrackingArea :: (IsNSView nsView, IsNSTrackingArea trackingArea) => nsView -> trackingArea -> IO ()
addTrackingArea nsView  trackingArea =
  withObjCPtr trackingArea $ \raw_trackingArea ->
      sendMsg nsView (mkSelector "addTrackingArea:") retVoid [argPtr (castPtr raw_trackingArea :: Ptr ())]

-- | @- removeTrackingArea:@
removeTrackingArea :: (IsNSView nsView, IsNSTrackingArea trackingArea) => nsView -> trackingArea -> IO ()
removeTrackingArea nsView  trackingArea =
  withObjCPtr trackingArea $ \raw_trackingArea ->
      sendMsg nsView (mkSelector "removeTrackingArea:") retVoid [argPtr (castPtr raw_trackingArea :: Ptr ())]

-- | @- updateTrackingAreas@
updateTrackingAreas :: IsNSView nsView => nsView -> IO ()
updateTrackingAreas nsView  =
    sendMsg nsView (mkSelector "updateTrackingAreas") retVoid []

-- | @- addCursorRect:cursor:@
addCursorRect_cursor :: (IsNSView nsView, IsNSCursor object) => nsView -> NSRect -> object -> IO ()
addCursorRect_cursor nsView  rect object =
  withObjCPtr object $ \raw_object ->
      sendMsg nsView (mkSelector "addCursorRect:cursor:") retVoid [argNSRect rect, argPtr (castPtr raw_object :: Ptr ())]

-- | @- removeCursorRect:cursor:@
removeCursorRect_cursor :: (IsNSView nsView, IsNSCursor object) => nsView -> NSRect -> object -> IO ()
removeCursorRect_cursor nsView  rect object =
  withObjCPtr object $ \raw_object ->
      sendMsg nsView (mkSelector "removeCursorRect:cursor:") retVoid [argNSRect rect, argPtr (castPtr raw_object :: Ptr ())]

-- | @- discardCursorRects@
discardCursorRects :: IsNSView nsView => nsView -> IO ()
discardCursorRects nsView  =
    sendMsg nsView (mkSelector "discardCursorRects") retVoid []

-- | @- resetCursorRects@
resetCursorRects :: IsNSView nsView => nsView -> IO ()
resetCursorRects nsView  =
    sendMsg nsView (mkSelector "resetCursorRects") retVoid []

-- | @- addTrackingRect:owner:userData:assumeInside:@
addTrackingRect_owner_userData_assumeInside :: IsNSView nsView => nsView -> NSRect -> RawId -> Ptr () -> Bool -> IO CLong
addTrackingRect_owner_userData_assumeInside nsView  rect owner data_ flag =
    sendMsg nsView (mkSelector "addTrackingRect:owner:userData:assumeInside:") retCLong [argNSRect rect, argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr data_, argCULong (if flag then 1 else 0)]

-- | @- removeTrackingRect:@
removeTrackingRect :: IsNSView nsView => nsView -> CLong -> IO ()
removeTrackingRect nsView  tag =
    sendMsg nsView (mkSelector "removeTrackingRect:") retVoid [argCLong tag]

-- | @- addGestureRecognizer:@
addGestureRecognizer :: (IsNSView nsView, IsNSGestureRecognizer gestureRecognizer) => nsView -> gestureRecognizer -> IO ()
addGestureRecognizer nsView  gestureRecognizer =
  withObjCPtr gestureRecognizer $ \raw_gestureRecognizer ->
      sendMsg nsView (mkSelector "addGestureRecognizer:") retVoid [argPtr (castPtr raw_gestureRecognizer :: Ptr ())]

-- | @- removeGestureRecognizer:@
removeGestureRecognizer :: (IsNSView nsView, IsNSGestureRecognizer gestureRecognizer) => nsView -> gestureRecognizer -> IO ()
removeGestureRecognizer nsView  gestureRecognizer =
  withObjCPtr gestureRecognizer $ \raw_gestureRecognizer ->
      sendMsg nsView (mkSelector "removeGestureRecognizer:") retVoid [argPtr (castPtr raw_gestureRecognizer :: Ptr ())]

-- | @- showDefinitionForAttributedString:atPoint:@
showDefinitionForAttributedString_atPoint :: (IsNSView nsView, IsNSAttributedString attrString) => nsView -> attrString -> NSPoint -> IO ()
showDefinitionForAttributedString_atPoint nsView  attrString textBaselineOrigin =
  withObjCPtr attrString $ \raw_attrString ->
      sendMsg nsView (mkSelector "showDefinitionForAttributedString:atPoint:") retVoid [argPtr (castPtr raw_attrString :: Ptr ()), argNSPoint textBaselineOrigin]

-- | @- showDefinitionForAttributedString:range:options:baselineOriginProvider:@
showDefinitionForAttributedString_range_options_baselineOriginProvider :: (IsNSView nsView, IsNSAttributedString attrString, IsNSDictionary options) => nsView -> attrString -> NSRange -> options -> Ptr () -> IO ()
showDefinitionForAttributedString_range_options_baselineOriginProvider nsView  attrString targetRange options originProvider =
  withObjCPtr attrString $ \raw_attrString ->
    withObjCPtr options $ \raw_options ->
        sendMsg nsView (mkSelector "showDefinitionForAttributedString:range:options:baselineOriginProvider:") retVoid [argPtr (castPtr raw_attrString :: Ptr ()), argNSRange targetRange, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr originProvider :: Ptr ())]

-- | @- enterFullScreenMode:withOptions:@
enterFullScreenMode_withOptions :: (IsNSView nsView, IsNSScreen screen, IsNSDictionary options) => nsView -> screen -> options -> IO Bool
enterFullScreenMode_withOptions nsView  screen options =
  withObjCPtr screen $ \raw_screen ->
    withObjCPtr options $ \raw_options ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "enterFullScreenMode:withOptions:") retCULong [argPtr (castPtr raw_screen :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- exitFullScreenModeWithOptions:@
exitFullScreenModeWithOptions :: (IsNSView nsView, IsNSDictionary options) => nsView -> options -> IO ()
exitFullScreenModeWithOptions nsView  options =
  withObjCPtr options $ \raw_options ->
      sendMsg nsView (mkSelector "exitFullScreenModeWithOptions:") retVoid [argPtr (castPtr raw_options :: Ptr ())]

-- | @- beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_source :: (IsNSView nsView, IsNSArray items, IsNSEvent event) => nsView -> items -> event -> RawId -> IO (Id NSDraggingSession)
beginDraggingSessionWithItems_event_source nsView  items event source =
  withObjCPtr items $ \raw_items ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsView (mkSelector "beginDraggingSessionWithItems:event:source:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= retainedObject . castPtr

-- | @- registerForDraggedTypes:@
registerForDraggedTypes :: (IsNSView nsView, IsNSArray newTypes) => nsView -> newTypes -> IO ()
registerForDraggedTypes nsView  newTypes =
  withObjCPtr newTypes $ \raw_newTypes ->
      sendMsg nsView (mkSelector "registerForDraggedTypes:") retVoid [argPtr (castPtr raw_newTypes :: Ptr ())]

-- | @- unregisterDraggedTypes@
unregisterDraggedTypes :: IsNSView nsView => nsView -> IO ()
unregisterDraggedTypes nsView  =
    sendMsg nsView (mkSelector "unregisterDraggedTypes") retVoid []

-- | @- writeEPSInsideRect:toPasteboard:@
writeEPSInsideRect_toPasteboard :: (IsNSView nsView, IsNSPasteboard pasteboard) => nsView -> NSRect -> pasteboard -> IO ()
writeEPSInsideRect_toPasteboard nsView  rect pasteboard =
  withObjCPtr pasteboard $ \raw_pasteboard ->
      sendMsg nsView (mkSelector "writeEPSInsideRect:toPasteboard:") retVoid [argNSRect rect, argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | @- dataWithEPSInsideRect:@
dataWithEPSInsideRect :: IsNSView nsView => nsView -> NSRect -> IO (Id NSData)
dataWithEPSInsideRect nsView  rect =
    sendMsg nsView (mkSelector "dataWithEPSInsideRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- writePDFInsideRect:toPasteboard:@
writePDFInsideRect_toPasteboard :: (IsNSView nsView, IsNSPasteboard pasteboard) => nsView -> NSRect -> pasteboard -> IO ()
writePDFInsideRect_toPasteboard nsView  rect pasteboard =
  withObjCPtr pasteboard $ \raw_pasteboard ->
      sendMsg nsView (mkSelector "writePDFInsideRect:toPasteboard:") retVoid [argNSRect rect, argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | @- dataWithPDFInsideRect:@
dataWithPDFInsideRect :: IsNSView nsView => nsView -> NSRect -> IO (Id NSData)
dataWithPDFInsideRect nsView  rect =
    sendMsg nsView (mkSelector "dataWithPDFInsideRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- print:@
print_ :: IsNSView nsView => nsView -> RawId -> IO ()
print_ nsView  sender =
    sendMsg nsView (mkSelector "print:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- knowsPageRange:@
knowsPageRange :: IsNSView nsView => nsView -> Ptr NSRange -> IO Bool
knowsPageRange nsView  range =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "knowsPageRange:") retCULong [argPtr range]

-- | @- adjustPageWidthNew:left:right:limit:@
adjustPageWidthNew_left_right_limit :: IsNSView nsView => nsView -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> IO ()
adjustPageWidthNew_left_right_limit nsView  newRight oldLeft oldRight rightLimit =
    sendMsg nsView (mkSelector "adjustPageWidthNew:left:right:limit:") retVoid [argPtr newRight, argCDouble oldLeft, argCDouble oldRight, argCDouble rightLimit]

-- | @- adjustPageHeightNew:top:bottom:limit:@
adjustPageHeightNew_top_bottom_limit :: IsNSView nsView => nsView -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> IO ()
adjustPageHeightNew_top_bottom_limit nsView  newBottom oldTop oldBottom bottomLimit =
    sendMsg nsView (mkSelector "adjustPageHeightNew:top:bottom:limit:") retVoid [argPtr newBottom, argCDouble oldTop, argCDouble oldBottom, argCDouble bottomLimit]

-- | @- rectForPage:@
rectForPage :: IsNSView nsView => nsView -> CLong -> IO NSRect
rectForPage nsView  page =
    sendMsgStret nsView (mkSelector "rectForPage:") retNSRect [argCLong page]

-- | @- locationOfPrintRect:@
locationOfPrintRect :: IsNSView nsView => nsView -> NSRect -> IO NSPoint
locationOfPrintRect nsView  rect =
    sendMsgStret nsView (mkSelector "locationOfPrintRect:") retNSPoint [argNSRect rect]

-- | @- drawPageBorderWithSize:@
drawPageBorderWithSize :: IsNSView nsView => nsView -> NSSize -> IO ()
drawPageBorderWithSize nsView  borderSize =
    sendMsg nsView (mkSelector "drawPageBorderWithSize:") retVoid [argNSSize borderSize]

-- | * This method is obsolete.  It will never be invoked from within AppKit, and NSView's implementation of it does nothing. **
--
-- ObjC selector: @- drawSheetBorderWithSize:@
drawSheetBorderWithSize :: IsNSView nsView => nsView -> NSSize -> IO ()
drawSheetBorderWithSize nsView  borderSize =
    sendMsg nsView (mkSelector "drawSheetBorderWithSize:") retVoid [argNSSize borderSize]

-- | @- beginDocument@
beginDocument :: IsNSView nsView => nsView -> IO ()
beginDocument nsView  =
    sendMsg nsView (mkSelector "beginDocument") retVoid []

-- | @- endDocument@
endDocument :: IsNSView nsView => nsView -> IO ()
endDocument nsView  =
    sendMsg nsView (mkSelector "endDocument") retVoid []

-- | @- beginPageInRect:atPlacement:@
beginPageInRect_atPlacement :: IsNSView nsView => nsView -> NSRect -> NSPoint -> IO ()
beginPageInRect_atPlacement nsView  rect location =
    sendMsg nsView (mkSelector "beginPageInRect:atPlacement:") retVoid [argNSRect rect, argNSPoint location]

-- | @- endPage@
endPage :: IsNSView nsView => nsView -> IO ()
endPage nsView  =
    sendMsg nsView (mkSelector "endPage") retVoid []

-- | @- setKeyboardFocusRingNeedsDisplayInRect:@
setKeyboardFocusRingNeedsDisplayInRect :: IsNSView nsView => nsView -> NSRect -> IO ()
setKeyboardFocusRingNeedsDisplayInRect nsView  rect =
    sendMsg nsView (mkSelector "setKeyboardFocusRingNeedsDisplayInRect:") retVoid [argNSRect rect]

-- | @- drawFocusRingMask@
drawFocusRingMask :: IsNSView nsView => nsView -> IO ()
drawFocusRingMask nsView  =
    sendMsg nsView (mkSelector "drawFocusRingMask") retVoid []

-- | @- noteFocusRingMaskChanged@
noteFocusRingMaskChanged :: IsNSView nsView => nsView -> IO ()
noteFocusRingMaskChanged nsView  =
    sendMsg nsView (mkSelector "noteFocusRingMaskChanged") retVoid []

-- | @- window@
window :: IsNSView nsView => nsView -> IO (Id NSWindow)
window nsView  =
    sendMsg nsView (mkSelector "window") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- superview@
superview :: IsNSView nsView => nsView -> IO (Id NSView)
superview nsView  =
    sendMsg nsView (mkSelector "superview") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subviews@
subviews :: IsNSView nsView => nsView -> IO (Id NSArray)
subviews nsView  =
    sendMsg nsView (mkSelector "subviews") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubviews:@
setSubviews :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setSubviews nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setSubviews:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- opaqueAncestor@
opaqueAncestor :: IsNSView nsView => nsView -> IO (Id NSView)
opaqueAncestor nsView  =
    sendMsg nsView (mkSelector "opaqueAncestor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hidden@
hidden :: IsNSView nsView => nsView -> IO Bool
hidden nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSView nsView => nsView -> Bool -> IO ()
setHidden nsView  value =
    sendMsg nsView (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestor :: IsNSView nsView => nsView -> IO Bool
hiddenOrHasHiddenAncestor nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "hiddenOrHasHiddenAncestor") retCULong []

-- | @- wantsDefaultClipping@
wantsDefaultClipping :: IsNSView nsView => nsView -> IO Bool
wantsDefaultClipping nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "wantsDefaultClipping") retCULong []

-- | @- postsFrameChangedNotifications@
postsFrameChangedNotifications :: IsNSView nsView => nsView -> IO Bool
postsFrameChangedNotifications nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "postsFrameChangedNotifications") retCULong []

-- | @- setPostsFrameChangedNotifications:@
setPostsFrameChangedNotifications :: IsNSView nsView => nsView -> Bool -> IO ()
setPostsFrameChangedNotifications nsView  value =
    sendMsg nsView (mkSelector "setPostsFrameChangedNotifications:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autoresizesSubviews@
autoresizesSubviews :: IsNSView nsView => nsView -> IO Bool
autoresizesSubviews nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "autoresizesSubviews") retCULong []

-- | @- setAutoresizesSubviews:@
setAutoresizesSubviews :: IsNSView nsView => nsView -> Bool -> IO ()
setAutoresizesSubviews nsView  value =
    sendMsg nsView (mkSelector "setAutoresizesSubviews:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autoresizingMask@
autoresizingMask :: IsNSView nsView => nsView -> IO NSAutoresizingMaskOptions
autoresizingMask nsView  =
    fmap (coerce :: CULong -> NSAutoresizingMaskOptions) $ sendMsg nsView (mkSelector "autoresizingMask") retCULong []

-- | @- setAutoresizingMask:@
setAutoresizingMask :: IsNSView nsView => nsView -> NSAutoresizingMaskOptions -> IO ()
setAutoresizingMask nsView  value =
    sendMsg nsView (mkSelector "setAutoresizingMask:") retVoid [argCULong (coerce value)]

-- | @- frame@
frame :: IsNSView nsView => nsView -> IO NSRect
frame nsView  =
    sendMsgStret nsView (mkSelector "frame") retNSRect []

-- | @- setFrame:@
setFrame :: IsNSView nsView => nsView -> NSRect -> IO ()
setFrame nsView  value =
    sendMsg nsView (mkSelector "setFrame:") retVoid [argNSRect value]

-- | @- frameRotation@
frameRotation :: IsNSView nsView => nsView -> IO CDouble
frameRotation nsView  =
    sendMsg nsView (mkSelector "frameRotation") retCDouble []

-- | @- setFrameRotation:@
setFrameRotation :: IsNSView nsView => nsView -> CDouble -> IO ()
setFrameRotation nsView  value =
    sendMsg nsView (mkSelector "setFrameRotation:") retVoid [argCDouble value]

-- | @- frameCenterRotation@
frameCenterRotation :: IsNSView nsView => nsView -> IO CDouble
frameCenterRotation nsView  =
    sendMsg nsView (mkSelector "frameCenterRotation") retCDouble []

-- | @- setFrameCenterRotation:@
setFrameCenterRotation :: IsNSView nsView => nsView -> CDouble -> IO ()
setFrameCenterRotation nsView  value =
    sendMsg nsView (mkSelector "setFrameCenterRotation:") retVoid [argCDouble value]

-- | @- boundsRotation@
boundsRotation :: IsNSView nsView => nsView -> IO CDouble
boundsRotation nsView  =
    sendMsg nsView (mkSelector "boundsRotation") retCDouble []

-- | @- setBoundsRotation:@
setBoundsRotation :: IsNSView nsView => nsView -> CDouble -> IO ()
setBoundsRotation nsView  value =
    sendMsg nsView (mkSelector "setBoundsRotation:") retVoid [argCDouble value]

-- | @- bounds@
bounds :: IsNSView nsView => nsView -> IO NSRect
bounds nsView  =
    sendMsgStret nsView (mkSelector "bounds") retNSRect []

-- | @- setBounds:@
setBounds :: IsNSView nsView => nsView -> NSRect -> IO ()
setBounds nsView  value =
    sendMsg nsView (mkSelector "setBounds:") retVoid [argNSRect value]

-- | @- flipped@
flipped :: IsNSView nsView => nsView -> IO Bool
flipped nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "flipped") retCULong []

-- | @- rotatedFromBase@
rotatedFromBase :: IsNSView nsView => nsView -> IO Bool
rotatedFromBase nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "rotatedFromBase") retCULong []

-- | @- rotatedOrScaledFromBase@
rotatedOrScaledFromBase :: IsNSView nsView => nsView -> IO Bool
rotatedOrScaledFromBase nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "rotatedOrScaledFromBase") retCULong []

-- | @- opaque@
opaque :: IsNSView nsView => nsView -> IO Bool
opaque nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "opaque") retCULong []

-- | @- canDrawConcurrently@
canDrawConcurrently :: IsNSView nsView => nsView -> IO Bool
canDrawConcurrently nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "canDrawConcurrently") retCULong []

-- | @- setCanDrawConcurrently:@
setCanDrawConcurrently :: IsNSView nsView => nsView -> Bool -> IO ()
setCanDrawConcurrently nsView  value =
    sendMsg nsView (mkSelector "setCanDrawConcurrently:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canDraw@
canDraw :: IsNSView nsView => nsView -> IO Bool
canDraw nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "canDraw") retCULong []

-- | @- needsDisplay@
needsDisplay :: IsNSView nsView => nsView -> IO Bool
needsDisplay nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "needsDisplay") retCULong []

-- | @- setNeedsDisplay:@
setNeedsDisplay :: IsNSView nsView => nsView -> Bool -> IO ()
setNeedsDisplay nsView  value =
    sendMsg nsView (mkSelector "setNeedsDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | @+ focusView@
focusView :: IO (Id NSView)
focusView  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMsg cls' (mkSelector "focusView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The portion of the view that isn’t clipped by its superviews.
--
-- Visibility, as reflected by this property, doesn’t account for whether other view or window objects overlap the current view or whether the current view is installed in a window at all. This value of this property is @NSZeroRect@ if the current view is effectively hidden.
--
-- During a printing operation, the visible rectangle is further clipped to the page being imaged.
--
-- ObjC selector: @- visibleRect@
visibleRect :: IsNSView nsView => nsView -> IO NSRect
visibleRect nsView  =
    sendMsgStret nsView (mkSelector "visibleRect") retNSRect []

-- | @- tag@
tag :: IsNSView nsView => nsView -> IO CLong
tag nsView  =
    sendMsg nsView (mkSelector "tag") retCLong []

-- | @- needsPanelToBecomeKey@
needsPanelToBecomeKey :: IsNSView nsView => nsView -> IO Bool
needsPanelToBecomeKey nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "needsPanelToBecomeKey") retCULong []

-- | @- mouseDownCanMoveWindow@
mouseDownCanMoveWindow :: IsNSView nsView => nsView -> IO Bool
mouseDownCanMoveWindow nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "mouseDownCanMoveWindow") retCULong []

-- | @- acceptsTouchEvents@
acceptsTouchEvents :: IsNSView nsView => nsView -> IO Bool
acceptsTouchEvents nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "acceptsTouchEvents") retCULong []

-- | @- setAcceptsTouchEvents:@
setAcceptsTouchEvents :: IsNSView nsView => nsView -> Bool -> IO ()
setAcceptsTouchEvents nsView  value =
    sendMsg nsView (mkSelector "setAcceptsTouchEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- wantsRestingTouches@
wantsRestingTouches :: IsNSView nsView => nsView -> IO Bool
wantsRestingTouches nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "wantsRestingTouches") retCULong []

-- | @- setWantsRestingTouches:@
setWantsRestingTouches :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsRestingTouches nsView  value =
    sendMsg nsView (mkSelector "setWantsRestingTouches:") retVoid [argCULong (if value then 1 else 0)]

-- | @- layerContentsRedrawPolicy@
layerContentsRedrawPolicy :: IsNSView nsView => nsView -> IO NSViewLayerContentsRedrawPolicy
layerContentsRedrawPolicy nsView  =
    fmap (coerce :: CLong -> NSViewLayerContentsRedrawPolicy) $ sendMsg nsView (mkSelector "layerContentsRedrawPolicy") retCLong []

-- | @- setLayerContentsRedrawPolicy:@
setLayerContentsRedrawPolicy :: IsNSView nsView => nsView -> NSViewLayerContentsRedrawPolicy -> IO ()
setLayerContentsRedrawPolicy nsView  value =
    sendMsg nsView (mkSelector "setLayerContentsRedrawPolicy:") retVoid [argCLong (coerce value)]

-- | @- layerContentsPlacement@
layerContentsPlacement :: IsNSView nsView => nsView -> IO NSViewLayerContentsPlacement
layerContentsPlacement nsView  =
    fmap (coerce :: CLong -> NSViewLayerContentsPlacement) $ sendMsg nsView (mkSelector "layerContentsPlacement") retCLong []

-- | @- setLayerContentsPlacement:@
setLayerContentsPlacement :: IsNSView nsView => nsView -> NSViewLayerContentsPlacement -> IO ()
setLayerContentsPlacement nsView  value =
    sendMsg nsView (mkSelector "setLayerContentsPlacement:") retVoid [argCLong (coerce value)]

-- | @- wantsLayer@
wantsLayer :: IsNSView nsView => nsView -> IO Bool
wantsLayer nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "wantsLayer") retCULong []

-- | @- setWantsLayer:@
setWantsLayer :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsLayer nsView  value =
    sendMsg nsView (mkSelector "setWantsLayer:") retVoid [argCULong (if value then 1 else 0)]

-- | @- layer@
layer :: IsNSView nsView => nsView -> IO (Id CALayer)
layer nsView  =
    sendMsg nsView (mkSelector "layer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayer:@
setLayer :: (IsNSView nsView, IsCALayer value) => nsView -> value -> IO ()
setLayer nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setLayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wantsUpdateLayer@
wantsUpdateLayer :: IsNSView nsView => nsView -> IO Bool
wantsUpdateLayer nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "wantsUpdateLayer") retCULong []

-- | @- canDrawSubviewsIntoLayer@
canDrawSubviewsIntoLayer :: IsNSView nsView => nsView -> IO Bool
canDrawSubviewsIntoLayer nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "canDrawSubviewsIntoLayer") retCULong []

-- | @- setCanDrawSubviewsIntoLayer:@
setCanDrawSubviewsIntoLayer :: IsNSView nsView => nsView -> Bool -> IO ()
setCanDrawSubviewsIntoLayer nsView  value =
    sendMsg nsView (mkSelector "setCanDrawSubviewsIntoLayer:") retVoid [argCULong (if value then 1 else 0)]

-- | @- needsLayout@
needsLayout :: IsNSView nsView => nsView -> IO Bool
needsLayout nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "needsLayout") retCULong []

-- | @- setNeedsLayout:@
setNeedsLayout :: IsNSView nsView => nsView -> Bool -> IO ()
setNeedsLayout nsView  value =
    sendMsg nsView (mkSelector "setNeedsLayout:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alphaValue@
alphaValue :: IsNSView nsView => nsView -> IO CDouble
alphaValue nsView  =
    sendMsg nsView (mkSelector "alphaValue") retCDouble []

-- | @- setAlphaValue:@
setAlphaValue :: IsNSView nsView => nsView -> CDouble -> IO ()
setAlphaValue nsView  value =
    sendMsg nsView (mkSelector "setAlphaValue:") retVoid [argCDouble value]

-- | @- layerUsesCoreImageFilters@
layerUsesCoreImageFilters :: IsNSView nsView => nsView -> IO Bool
layerUsesCoreImageFilters nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "layerUsesCoreImageFilters") retCULong []

-- | @- setLayerUsesCoreImageFilters:@
setLayerUsesCoreImageFilters :: IsNSView nsView => nsView -> Bool -> IO ()
setLayerUsesCoreImageFilters nsView  value =
    sendMsg nsView (mkSelector "setLayerUsesCoreImageFilters:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundFilters@
backgroundFilters :: IsNSView nsView => nsView -> IO (Id NSArray)
backgroundFilters nsView  =
    sendMsg nsView (mkSelector "backgroundFilters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundFilters:@
setBackgroundFilters :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setBackgroundFilters nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setBackgroundFilters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- compositingFilter@
compositingFilter :: IsNSView nsView => nsView -> IO (Id CIFilter)
compositingFilter nsView  =
    sendMsg nsView (mkSelector "compositingFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompositingFilter:@
setCompositingFilter :: (IsNSView nsView, IsCIFilter value) => nsView -> value -> IO ()
setCompositingFilter nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setCompositingFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentFilters@
contentFilters :: IsNSView nsView => nsView -> IO (Id NSArray)
contentFilters nsView  =
    sendMsg nsView (mkSelector "contentFilters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentFilters:@
setContentFilters :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setContentFilters nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setContentFilters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shadow@
shadow :: IsNSView nsView => nsView -> IO (Id NSShadow)
shadow nsView  =
    sendMsg nsView (mkSelector "shadow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShadow:@
setShadow :: (IsNSView nsView, IsNSShadow value) => nsView -> value -> IO ()
setShadow nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setShadow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clipsToBounds@
clipsToBounds :: IsNSView nsView => nsView -> IO Bool
clipsToBounds nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "clipsToBounds") retCULong []

-- | @- setClipsToBounds:@
setClipsToBounds :: IsNSView nsView => nsView -> Bool -> IO ()
setClipsToBounds nsView  value =
    sendMsg nsView (mkSelector "setClipsToBounds:") retVoid [argCULong (if value then 1 else 0)]

-- | @- postsBoundsChangedNotifications@
postsBoundsChangedNotifications :: IsNSView nsView => nsView -> IO Bool
postsBoundsChangedNotifications nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "postsBoundsChangedNotifications") retCULong []

-- | @- setPostsBoundsChangedNotifications:@
setPostsBoundsChangedNotifications :: IsNSView nsView => nsView -> Bool -> IO ()
setPostsBoundsChangedNotifications nsView  value =
    sendMsg nsView (mkSelector "setPostsBoundsChangedNotifications:") retVoid [argCULong (if value then 1 else 0)]

-- | @- enclosingScrollView@
enclosingScrollView :: IsNSView nsView => nsView -> IO (Id NSScrollView)
enclosingScrollView nsView  =
    sendMsg nsView (mkSelector "enclosingScrollView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultMenu@
defaultMenu :: IO (Id NSMenu)
defaultMenu  =
  do
    cls' <- getRequiredClass "NSView"
    sendClassMsg cls' (mkSelector "defaultMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toolTip@
toolTip :: IsNSView nsView => nsView -> IO (Id NSString)
toolTip nsView  =
    sendMsg nsView (mkSelector "toolTip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToolTip:@
setToolTip :: (IsNSView nsView, IsNSString value) => nsView -> value -> IO ()
setToolTip nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setToolTip:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inLiveResize@
inLiveResize :: IsNSView nsView => nsView -> IO Bool
inLiveResize nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "inLiveResize") retCULong []

-- | @- preservesContentDuringLiveResize@
preservesContentDuringLiveResize :: IsNSView nsView => nsView -> IO Bool
preservesContentDuringLiveResize nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "preservesContentDuringLiveResize") retCULong []

-- | @- rectPreservedDuringLiveResize@
rectPreservedDuringLiveResize :: IsNSView nsView => nsView -> IO NSRect
rectPreservedDuringLiveResize nsView  =
    sendMsgStret nsView (mkSelector "rectPreservedDuringLiveResize") retNSRect []

-- | @- inputContext@
inputContext :: IsNSView nsView => nsView -> IO (Id NSTextInputContext)
inputContext nsView  =
    sendMsg nsView (mkSelector "inputContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSView nsView => nsView -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsView  =
    fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsView (mkSelector "userInterfaceLayoutDirection") retCLong []

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSView nsView => nsView -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsView  value =
    sendMsg nsView (mkSelector "setUserInterfaceLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @+ compatibleWithResponsiveScrolling@
compatibleWithResponsiveScrolling :: IO Bool
compatibleWithResponsiveScrolling  =
  do
    cls' <- getRequiredClass "NSView"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "compatibleWithResponsiveScrolling") retCULong []

-- | @- preparedContentRect@
preparedContentRect :: IsNSView nsView => nsView -> IO NSRect
preparedContentRect nsView  =
    sendMsgStret nsView (mkSelector "preparedContentRect") retNSRect []

-- | @- setPreparedContentRect:@
setPreparedContentRect :: IsNSView nsView => nsView -> NSRect -> IO ()
setPreparedContentRect nsView  value =
    sendMsg nsView (mkSelector "setPreparedContentRect:") retVoid [argNSRect value]

-- | @- allowsVibrancy@
allowsVibrancy :: IsNSView nsView => nsView -> IO Bool
allowsVibrancy nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "allowsVibrancy") retCULong []

-- | @- pressureConfiguration@
pressureConfiguration :: IsNSView nsView => nsView -> IO (Id NSPressureConfiguration)
pressureConfiguration nsView  =
    sendMsg nsView (mkSelector "pressureConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPressureConfiguration:@
setPressureConfiguration :: (IsNSView nsView, IsNSPressureConfiguration value) => nsView -> value -> IO ()
setPressureConfiguration nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setPressureConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurface :: IsNSView nsView => nsView -> IO Bool
wantsExtendedDynamicRangeOpenGLSurface nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "wantsExtendedDynamicRangeOpenGLSurface") retCULong []

-- | @- setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurface :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsExtendedDynamicRangeOpenGLSurface nsView  value =
    sendMsg nsView (mkSelector "setWantsExtendedDynamicRangeOpenGLSurface:") retVoid [argCULong (if value then 1 else 0)]

-- | @- wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurface :: IsNSView nsView => nsView -> IO Bool
wantsBestResolutionOpenGLSurface nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "wantsBestResolutionOpenGLSurface") retCULong []

-- | @- setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurface :: IsNSView nsView => nsView -> Bool -> IO ()
setWantsBestResolutionOpenGLSurface nsView  value =
    sendMsg nsView (mkSelector "setWantsBestResolutionOpenGLSurface:") retVoid [argCULong (if value then 1 else 0)]

-- | @- layoutGuides@
layoutGuides :: IsNSView nsView => nsView -> IO (Id NSArray)
layoutGuides nsView  =
    sendMsg nsView (mkSelector "layoutGuides") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasAmbiguousLayout@
hasAmbiguousLayout :: IsNSView nsView => nsView -> IO Bool
hasAmbiguousLayout nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "hasAmbiguousLayout") retCULong []

-- | @- fittingSize@
fittingSize :: IsNSView nsView => nsView -> IO NSSize
fittingSize nsView  =
    sendMsgStret nsView (mkSelector "fittingSize") retNSSize []

-- | @- alignmentRectInsets@
alignmentRectInsets :: IsNSView nsView => nsView -> IO NSEdgeInsets
alignmentRectInsets nsView  =
    sendMsgStret nsView (mkSelector "alignmentRectInsets") retNSEdgeInsets []

-- | @- firstBaselineOffsetFromTop@
firstBaselineOffsetFromTop :: IsNSView nsView => nsView -> IO CDouble
firstBaselineOffsetFromTop nsView  =
    sendMsg nsView (mkSelector "firstBaselineOffsetFromTop") retCDouble []

-- | @- lastBaselineOffsetFromBottom@
lastBaselineOffsetFromBottom :: IsNSView nsView => nsView -> IO CDouble
lastBaselineOffsetFromBottom nsView  =
    sendMsg nsView (mkSelector "lastBaselineOffsetFromBottom") retCDouble []

-- | @- baselineOffsetFromBottom@
baselineOffsetFromBottom :: IsNSView nsView => nsView -> IO CDouble
baselineOffsetFromBottom nsView  =
    sendMsg nsView (mkSelector "baselineOffsetFromBottom") retCDouble []

-- | @- intrinsicContentSize@
intrinsicContentSize :: IsNSView nsView => nsView -> IO NSSize
intrinsicContentSize nsView  =
    sendMsgStret nsView (mkSelector "intrinsicContentSize") retNSSize []

-- | @- horizontalContentSizeConstraintActive@
horizontalContentSizeConstraintActive :: IsNSView nsView => nsView -> IO Bool
horizontalContentSizeConstraintActive nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "horizontalContentSizeConstraintActive") retCULong []

-- | @- setHorizontalContentSizeConstraintActive:@
setHorizontalContentSizeConstraintActive :: IsNSView nsView => nsView -> Bool -> IO ()
setHorizontalContentSizeConstraintActive nsView  value =
    sendMsg nsView (mkSelector "setHorizontalContentSizeConstraintActive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- verticalContentSizeConstraintActive@
verticalContentSizeConstraintActive :: IsNSView nsView => nsView -> IO Bool
verticalContentSizeConstraintActive nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "verticalContentSizeConstraintActive") retCULong []

-- | @- setVerticalContentSizeConstraintActive:@
setVerticalContentSizeConstraintActive :: IsNSView nsView => nsView -> Bool -> IO ()
setVerticalContentSizeConstraintActive nsView  value =
    sendMsg nsView (mkSelector "setVerticalContentSizeConstraintActive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- translatesAutoresizingMaskIntoConstraints@
translatesAutoresizingMaskIntoConstraints :: IsNSView nsView => nsView -> IO Bool
translatesAutoresizingMaskIntoConstraints nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "translatesAutoresizingMaskIntoConstraints") retCULong []

-- | @- setTranslatesAutoresizingMaskIntoConstraints:@
setTranslatesAutoresizingMaskIntoConstraints :: IsNSView nsView => nsView -> Bool -> IO ()
setTranslatesAutoresizingMaskIntoConstraints nsView  value =
    sendMsg nsView (mkSelector "setTranslatesAutoresizingMaskIntoConstraints:") retVoid [argCULong (if value then 1 else 0)]

-- | @+ requiresConstraintBasedLayout@
requiresConstraintBasedLayout :: IO Bool
requiresConstraintBasedLayout  =
  do
    cls' <- getRequiredClass "NSView"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "requiresConstraintBasedLayout") retCULong []

-- | @- needsUpdateConstraints@
needsUpdateConstraints :: IsNSView nsView => nsView -> IO Bool
needsUpdateConstraints nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "needsUpdateConstraints") retCULong []

-- | @- setNeedsUpdateConstraints:@
setNeedsUpdateConstraints :: IsNSView nsView => nsView -> Bool -> IO ()
setNeedsUpdateConstraints nsView  value =
    sendMsg nsView (mkSelector "setNeedsUpdateConstraints:") retVoid [argCULong (if value then 1 else 0)]

-- | @- leadingAnchor@
leadingAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
leadingAnchor nsView  =
    sendMsg nsView (mkSelector "leadingAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trailingAnchor@
trailingAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
trailingAnchor nsView  =
    sendMsg nsView (mkSelector "trailingAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- leftAnchor@
leftAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
leftAnchor nsView  =
    sendMsg nsView (mkSelector "leftAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightAnchor@
rightAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
rightAnchor nsView  =
    sendMsg nsView (mkSelector "rightAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- topAnchor@
topAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
topAnchor nsView  =
    sendMsg nsView (mkSelector "topAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bottomAnchor@
bottomAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
bottomAnchor nsView  =
    sendMsg nsView (mkSelector "bottomAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- widthAnchor@
widthAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutDimension)
widthAnchor nsView  =
    sendMsg nsView (mkSelector "widthAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- heightAnchor@
heightAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutDimension)
heightAnchor nsView  =
    sendMsg nsView (mkSelector "heightAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- centerXAnchor@
centerXAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutXAxisAnchor)
centerXAnchor nsView  =
    sendMsg nsView (mkSelector "centerXAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- centerYAnchor@
centerYAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
centerYAnchor nsView  =
    sendMsg nsView (mkSelector "centerYAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- firstBaselineAnchor@
firstBaselineAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
firstBaselineAnchor nsView  =
    sendMsg nsView (mkSelector "firstBaselineAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastBaselineAnchor@
lastBaselineAnchor :: IsNSView nsView => nsView -> IO (Id NSLayoutYAxisAnchor)
lastBaselineAnchor nsView  =
    sendMsg nsView (mkSelector "lastBaselineAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- constraints@
constraints :: IsNSView nsView => nsView -> IO (Id NSArray)
constraints nsView  =
    sendMsg nsView (mkSelector "constraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- candidateListTouchBarItem@
candidateListTouchBarItem :: IsNSView nsView => nsView -> IO (Id NSCandidateListTouchBarItem)
candidateListTouchBarItem nsView  =
    sendMsg nsView (mkSelector "candidateListTouchBarItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enclosingMenuItem@
enclosingMenuItem :: IsNSView nsView => nsView -> IO (Id NSMenuItem)
enclosingMenuItem nsView  =
    sendMsg nsView (mkSelector "enclosingMenuItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- writingToolsCoordinator@
writingToolsCoordinator :: IsNSView nsView => nsView -> IO (Id NSWritingToolsCoordinator)
writingToolsCoordinator nsView  =
    sendMsg nsView (mkSelector "writingToolsCoordinator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWritingToolsCoordinator:@
setWritingToolsCoordinator :: (IsNSView nsView, IsNSWritingToolsCoordinator value) => nsView -> value -> IO ()
setWritingToolsCoordinator nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setWritingToolsCoordinator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- trackingAreas@
trackingAreas :: IsNSView nsView => nsView -> IO (Id NSArray)
trackingAreas nsView  =
    sendMsg nsView (mkSelector "trackingAreas") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | When this property is true, any NSControls in the view or its descendants will be sized with compact metrics compatible with macOS 15 and earlier. Defaults to false
--
-- ObjC selector: @- prefersCompactControlSizeMetrics@
prefersCompactControlSizeMetrics :: IsNSView nsView => nsView -> IO Bool
prefersCompactControlSizeMetrics nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "prefersCompactControlSizeMetrics") retCULong []

-- | When this property is true, any NSControls in the view or its descendants will be sized with compact metrics compatible with macOS 15 and earlier. Defaults to false
--
-- ObjC selector: @- setPrefersCompactControlSizeMetrics:@
setPrefersCompactControlSizeMetrics :: IsNSView nsView => nsView -> Bool -> IO ()
setPrefersCompactControlSizeMetrics nsView  value =
    sendMsg nsView (mkSelector "setPrefersCompactControlSizeMetrics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- safeAreaInsets@
safeAreaInsets :: IsNSView nsView => nsView -> IO NSEdgeInsets
safeAreaInsets nsView  =
    sendMsgStret nsView (mkSelector "safeAreaInsets") retNSEdgeInsets []

-- | @- additionalSafeAreaInsets@
additionalSafeAreaInsets :: IsNSView nsView => nsView -> IO NSEdgeInsets
additionalSafeAreaInsets nsView  =
    sendMsgStret nsView (mkSelector "additionalSafeAreaInsets") retNSEdgeInsets []

-- | @- setAdditionalSafeAreaInsets:@
setAdditionalSafeAreaInsets :: IsNSView nsView => nsView -> NSEdgeInsets -> IO ()
setAdditionalSafeAreaInsets nsView  value =
    sendMsg nsView (mkSelector "setAdditionalSafeAreaInsets:") retVoid [argNSEdgeInsets value]

-- | @- safeAreaLayoutGuide@
safeAreaLayoutGuide :: IsNSView nsView => nsView -> IO (Id NSLayoutGuide)
safeAreaLayoutGuide nsView  =
    sendMsg nsView (mkSelector "safeAreaLayoutGuide") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- safeAreaRect@
safeAreaRect :: IsNSView nsView => nsView -> IO NSRect
safeAreaRect nsView  =
    sendMsgStret nsView (mkSelector "safeAreaRect") retNSRect []

-- | @- layoutMarginsGuide@
layoutMarginsGuide :: IsNSView nsView => nsView -> IO (Id NSLayoutGuide)
layoutMarginsGuide nsView  =
    sendMsg nsView (mkSelector "layoutMarginsGuide") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allowedTouchTypes@
allowedTouchTypes :: IsNSView nsView => nsView -> IO NSTouchTypeMask
allowedTouchTypes nsView  =
    fmap (coerce :: CULong -> NSTouchTypeMask) $ sendMsg nsView (mkSelector "allowedTouchTypes") retCULong []

-- | @- setAllowedTouchTypes:@
setAllowedTouchTypes :: IsNSView nsView => nsView -> NSTouchTypeMask -> IO ()
setAllowedTouchTypes nsView  value =
    sendMsg nsView (mkSelector "setAllowedTouchTypes:") retVoid [argCULong (coerce value)]

-- | @- gestureRecognizers@
gestureRecognizers :: IsNSView nsView => nsView -> IO (Id NSArray)
gestureRecognizers nsView  =
    sendMsg nsView (mkSelector "gestureRecognizers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGestureRecognizers:@
setGestureRecognizers :: (IsNSView nsView, IsNSArray value) => nsView -> value -> IO ()
setGestureRecognizers nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setGestureRecognizers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawingFindIndicator@
drawingFindIndicator :: IsNSView nsView => nsView -> IO Bool
drawingFindIndicator nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "drawingFindIndicator") retCULong []

-- | @- inFullScreenMode@
inFullScreenMode :: IsNSView nsView => nsView -> IO Bool
inFullScreenMode nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "inFullScreenMode") retCULong []

-- | @- registeredDraggedTypes@
registeredDraggedTypes :: IsNSView nsView => nsView -> IO (Id NSArray)
registeredDraggedTypes nsView  =
    sendMsg nsView (mkSelector "registeredDraggedTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- heightAdjustLimit@
heightAdjustLimit :: IsNSView nsView => nsView -> IO CDouble
heightAdjustLimit nsView  =
    sendMsg nsView (mkSelector "heightAdjustLimit") retCDouble []

-- | @- widthAdjustLimit@
widthAdjustLimit :: IsNSView nsView => nsView -> IO CDouble
widthAdjustLimit nsView  =
    sendMsg nsView (mkSelector "widthAdjustLimit") retCDouble []

-- | @- pageHeader@
pageHeader :: IsNSView nsView => nsView -> IO (Id NSAttributedString)
pageHeader nsView  =
    sendMsg nsView (mkSelector "pageHeader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pageFooter@
pageFooter :: IsNSView nsView => nsView -> IO (Id NSAttributedString)
pageFooter nsView  =
    sendMsg nsView (mkSelector "pageFooter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- printJobTitle@
printJobTitle :: IsNSView nsView => nsView -> IO (Id NSString)
printJobTitle nsView  =
    sendMsg nsView (mkSelector "printJobTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextKeyView@
nextKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
nextKeyView nsView  =
    sendMsg nsView (mkSelector "nextKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextKeyView:@
setNextKeyView :: (IsNSView nsView, IsNSView value) => nsView -> value -> IO ()
setNextKeyView nsView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsView (mkSelector "setNextKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previousKeyView@
previousKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
previousKeyView nsView  =
    sendMsg nsView (mkSelector "previousKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextValidKeyView@
nextValidKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
nextValidKeyView nsView  =
    sendMsg nsView (mkSelector "nextValidKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousValidKeyView@
previousValidKeyView :: IsNSView nsView => nsView -> IO (Id NSView)
previousValidKeyView nsView  =
    sendMsg nsView (mkSelector "previousValidKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- canBecomeKeyView@
canBecomeKeyView :: IsNSView nsView => nsView -> IO Bool
canBecomeKeyView nsView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsView (mkSelector "canBecomeKeyView") retCULong []

-- | @- focusRingType@
focusRingType :: IsNSView nsView => nsView -> IO NSFocusRingType
focusRingType nsView  =
    fmap (coerce :: CULong -> NSFocusRingType) $ sendMsg nsView (mkSelector "focusRingType") retCULong []

-- | @- setFocusRingType:@
setFocusRingType :: IsNSView nsView => nsView -> NSFocusRingType -> IO ()
setFocusRingType nsView  value =
    sendMsg nsView (mkSelector "setFocusRingType:") retVoid [argCULong (coerce value)]

-- | @+ defaultFocusRingType@
defaultFocusRingType :: IO NSFocusRingType
defaultFocusRingType  =
  do
    cls' <- getRequiredClass "NSView"
    fmap (coerce :: CULong -> NSFocusRingType) $ sendClassMsg cls' (mkSelector "defaultFocusRingType") retCULong []

-- | @- focusRingMaskBounds@
focusRingMaskBounds :: IsNSView nsView => nsView -> IO NSRect
focusRingMaskBounds nsView  =
    sendMsgStret nsView (mkSelector "focusRingMaskBounds") retNSRect []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @isDescendantOf:@
isDescendantOfSelector :: Selector
isDescendantOfSelector = mkSelector "isDescendantOf:"

-- | @Selector@ for @ancestorSharedWithView:@
ancestorSharedWithViewSelector :: Selector
ancestorSharedWithViewSelector = mkSelector "ancestorSharedWithView:"

-- | @Selector@ for @getRectsBeingDrawn:count:@
getRectsBeingDrawn_countSelector :: Selector
getRectsBeingDrawn_countSelector = mkSelector "getRectsBeingDrawn:count:"

-- | @Selector@ for @needsToDrawRect:@
needsToDrawRectSelector :: Selector
needsToDrawRectSelector = mkSelector "needsToDrawRect:"

-- | @Selector@ for @viewDidHide@
viewDidHideSelector :: Selector
viewDidHideSelector = mkSelector "viewDidHide"

-- | @Selector@ for @viewDidUnhide@
viewDidUnhideSelector :: Selector
viewDidUnhideSelector = mkSelector "viewDidUnhide"

-- | @Selector@ for @addSubview:@
addSubviewSelector :: Selector
addSubviewSelector = mkSelector "addSubview:"

-- | @Selector@ for @addSubview:positioned:relativeTo:@
addSubview_positioned_relativeToSelector :: Selector
addSubview_positioned_relativeToSelector = mkSelector "addSubview:positioned:relativeTo:"

-- | @Selector@ for @sortSubviewsUsingFunction:context:@
sortSubviewsUsingFunction_contextSelector :: Selector
sortSubviewsUsingFunction_contextSelector = mkSelector "sortSubviewsUsingFunction:context:"

-- | @Selector@ for @viewWillMoveToWindow:@
viewWillMoveToWindowSelector :: Selector
viewWillMoveToWindowSelector = mkSelector "viewWillMoveToWindow:"

-- | @Selector@ for @viewDidMoveToWindow@
viewDidMoveToWindowSelector :: Selector
viewDidMoveToWindowSelector = mkSelector "viewDidMoveToWindow"

-- | @Selector@ for @viewWillMoveToSuperview:@
viewWillMoveToSuperviewSelector :: Selector
viewWillMoveToSuperviewSelector = mkSelector "viewWillMoveToSuperview:"

-- | @Selector@ for @viewDidMoveToSuperview@
viewDidMoveToSuperviewSelector :: Selector
viewDidMoveToSuperviewSelector = mkSelector "viewDidMoveToSuperview"

-- | @Selector@ for @didAddSubview:@
didAddSubviewSelector :: Selector
didAddSubviewSelector = mkSelector "didAddSubview:"

-- | @Selector@ for @willRemoveSubview:@
willRemoveSubviewSelector :: Selector
willRemoveSubviewSelector = mkSelector "willRemoveSubview:"

-- | @Selector@ for @removeFromSuperview@
removeFromSuperviewSelector :: Selector
removeFromSuperviewSelector = mkSelector "removeFromSuperview"

-- | @Selector@ for @replaceSubview:with:@
replaceSubview_withSelector :: Selector
replaceSubview_withSelector = mkSelector "replaceSubview:with:"

-- | @Selector@ for @removeFromSuperviewWithoutNeedingDisplay@
removeFromSuperviewWithoutNeedingDisplaySelector :: Selector
removeFromSuperviewWithoutNeedingDisplaySelector = mkSelector "removeFromSuperviewWithoutNeedingDisplay"

-- | @Selector@ for @viewDidChangeBackingProperties@
viewDidChangeBackingPropertiesSelector :: Selector
viewDidChangeBackingPropertiesSelector = mkSelector "viewDidChangeBackingProperties"

-- | @Selector@ for @resizeSubviewsWithOldSize:@
resizeSubviewsWithOldSizeSelector :: Selector
resizeSubviewsWithOldSizeSelector = mkSelector "resizeSubviewsWithOldSize:"

-- | @Selector@ for @resizeWithOldSuperviewSize:@
resizeWithOldSuperviewSizeSelector :: Selector
resizeWithOldSuperviewSizeSelector = mkSelector "resizeWithOldSuperviewSize:"

-- | @Selector@ for @setFrameOrigin:@
setFrameOriginSelector :: Selector
setFrameOriginSelector = mkSelector "setFrameOrigin:"

-- | @Selector@ for @setFrameSize:@
setFrameSizeSelector :: Selector
setFrameSizeSelector = mkSelector "setFrameSize:"

-- | @Selector@ for @setBoundsOrigin:@
setBoundsOriginSelector :: Selector
setBoundsOriginSelector = mkSelector "setBoundsOrigin:"

-- | @Selector@ for @setBoundsSize:@
setBoundsSizeSelector :: Selector
setBoundsSizeSelector = mkSelector "setBoundsSize:"

-- | @Selector@ for @translateOriginToPoint:@
translateOriginToPointSelector :: Selector
translateOriginToPointSelector = mkSelector "translateOriginToPoint:"

-- | @Selector@ for @scaleUnitSquareToSize:@
scaleUnitSquareToSizeSelector :: Selector
scaleUnitSquareToSizeSelector = mkSelector "scaleUnitSquareToSize:"

-- | @Selector@ for @rotateByAngle:@
rotateByAngleSelector :: Selector
rotateByAngleSelector = mkSelector "rotateByAngle:"

-- | @Selector@ for @convertPoint:fromView:@
convertPoint_fromViewSelector :: Selector
convertPoint_fromViewSelector = mkSelector "convertPoint:fromView:"

-- | @Selector@ for @convertPoint:toView:@
convertPoint_toViewSelector :: Selector
convertPoint_toViewSelector = mkSelector "convertPoint:toView:"

-- | @Selector@ for @convertSize:fromView:@
convertSize_fromViewSelector :: Selector
convertSize_fromViewSelector = mkSelector "convertSize:fromView:"

-- | @Selector@ for @convertSize:toView:@
convertSize_toViewSelector :: Selector
convertSize_toViewSelector = mkSelector "convertSize:toView:"

-- | @Selector@ for @convertRect:fromView:@
convertRect_fromViewSelector :: Selector
convertRect_fromViewSelector = mkSelector "convertRect:fromView:"

-- | @Selector@ for @convertRect:toView:@
convertRect_toViewSelector :: Selector
convertRect_toViewSelector = mkSelector "convertRect:toView:"

-- | @Selector@ for @backingAlignedRect:options:@
backingAlignedRect_optionsSelector :: Selector
backingAlignedRect_optionsSelector = mkSelector "backingAlignedRect:options:"

-- | @Selector@ for @centerScanRect:@
centerScanRectSelector :: Selector
centerScanRectSelector = mkSelector "centerScanRect:"

-- | @Selector@ for @convertPointToBacking:@
convertPointToBackingSelector :: Selector
convertPointToBackingSelector = mkSelector "convertPointToBacking:"

-- | @Selector@ for @convertPointFromBacking:@
convertPointFromBackingSelector :: Selector
convertPointFromBackingSelector = mkSelector "convertPointFromBacking:"

-- | @Selector@ for @convertSizeToBacking:@
convertSizeToBackingSelector :: Selector
convertSizeToBackingSelector = mkSelector "convertSizeToBacking:"

-- | @Selector@ for @convertSizeFromBacking:@
convertSizeFromBackingSelector :: Selector
convertSizeFromBackingSelector = mkSelector "convertSizeFromBacking:"

-- | @Selector@ for @convertRectToBacking:@
convertRectToBackingSelector :: Selector
convertRectToBackingSelector = mkSelector "convertRectToBacking:"

-- | @Selector@ for @convertRectFromBacking:@
convertRectFromBackingSelector :: Selector
convertRectFromBackingSelector = mkSelector "convertRectFromBacking:"

-- | @Selector@ for @convertPointToLayer:@
convertPointToLayerSelector :: Selector
convertPointToLayerSelector = mkSelector "convertPointToLayer:"

-- | @Selector@ for @convertPointFromLayer:@
convertPointFromLayerSelector :: Selector
convertPointFromLayerSelector = mkSelector "convertPointFromLayer:"

-- | @Selector@ for @convertSizeToLayer:@
convertSizeToLayerSelector :: Selector
convertSizeToLayerSelector = mkSelector "convertSizeToLayer:"

-- | @Selector@ for @convertSizeFromLayer:@
convertSizeFromLayerSelector :: Selector
convertSizeFromLayerSelector = mkSelector "convertSizeFromLayer:"

-- | @Selector@ for @convertRectToLayer:@
convertRectToLayerSelector :: Selector
convertRectToLayerSelector = mkSelector "convertRectToLayer:"

-- | @Selector@ for @convertRectFromLayer:@
convertRectFromLayerSelector :: Selector
convertRectFromLayerSelector = mkSelector "convertRectFromLayer:"

-- | @Selector@ for @setNeedsDisplayInRect:@
setNeedsDisplayInRectSelector :: Selector
setNeedsDisplayInRectSelector = mkSelector "setNeedsDisplayInRect:"

-- | @Selector@ for @lockFocus@
lockFocusSelector :: Selector
lockFocusSelector = mkSelector "lockFocus"

-- | @Selector@ for @unlockFocus@
unlockFocusSelector :: Selector
unlockFocusSelector = mkSelector "unlockFocus"

-- | @Selector@ for @lockFocusIfCanDraw@
lockFocusIfCanDrawSelector :: Selector
lockFocusIfCanDrawSelector = mkSelector "lockFocusIfCanDraw"

-- | @Selector@ for @lockFocusIfCanDrawInContext:@
lockFocusIfCanDrawInContextSelector :: Selector
lockFocusIfCanDrawInContextSelector = mkSelector "lockFocusIfCanDrawInContext:"

-- | @Selector@ for @display@
displaySelector :: Selector
displaySelector = mkSelector "display"

-- | @Selector@ for @displayIfNeeded@
displayIfNeededSelector :: Selector
displayIfNeededSelector = mkSelector "displayIfNeeded"

-- | @Selector@ for @displayIfNeededIgnoringOpacity@
displayIfNeededIgnoringOpacitySelector :: Selector
displayIfNeededIgnoringOpacitySelector = mkSelector "displayIfNeededIgnoringOpacity"

-- | @Selector@ for @displayRect:@
displayRectSelector :: Selector
displayRectSelector = mkSelector "displayRect:"

-- | @Selector@ for @displayIfNeededInRect:@
displayIfNeededInRectSelector :: Selector
displayIfNeededInRectSelector = mkSelector "displayIfNeededInRect:"

-- | @Selector@ for @displayRectIgnoringOpacity:@
displayRectIgnoringOpacitySelector :: Selector
displayRectIgnoringOpacitySelector = mkSelector "displayRectIgnoringOpacity:"

-- | @Selector@ for @displayIfNeededInRectIgnoringOpacity:@
displayIfNeededInRectIgnoringOpacitySelector :: Selector
displayIfNeededInRectIgnoringOpacitySelector = mkSelector "displayIfNeededInRectIgnoringOpacity:"

-- | @Selector@ for @drawRect:@
drawRectSelector :: Selector
drawRectSelector = mkSelector "drawRect:"

-- | @Selector@ for @displayRectIgnoringOpacity:inContext:@
displayRectIgnoringOpacity_inContextSelector :: Selector
displayRectIgnoringOpacity_inContextSelector = mkSelector "displayRectIgnoringOpacity:inContext:"

-- | @Selector@ for @bitmapImageRepForCachingDisplayInRect:@
bitmapImageRepForCachingDisplayInRectSelector :: Selector
bitmapImageRepForCachingDisplayInRectSelector = mkSelector "bitmapImageRepForCachingDisplayInRect:"

-- | @Selector@ for @cacheDisplayInRect:toBitmapImageRep:@
cacheDisplayInRect_toBitmapImageRepSelector :: Selector
cacheDisplayInRect_toBitmapImageRepSelector = mkSelector "cacheDisplayInRect:toBitmapImageRep:"

-- | @Selector@ for @viewWillDraw@
viewWillDrawSelector :: Selector
viewWillDrawSelector = mkSelector "viewWillDraw"

-- | @Selector@ for @scrollPoint:@
scrollPointSelector :: Selector
scrollPointSelector = mkSelector "scrollPoint:"

-- | @Selector@ for @scrollRectToVisible:@
scrollRectToVisibleSelector :: Selector
scrollRectToVisibleSelector = mkSelector "scrollRectToVisible:"

-- | @Selector@ for @autoscroll:@
autoscrollSelector :: Selector
autoscrollSelector = mkSelector "autoscroll:"

-- | @Selector@ for @adjustScroll:@
adjustScrollSelector :: Selector
adjustScrollSelector = mkSelector "adjustScroll:"

-- | @Selector@ for @scrollRect:by:@
scrollRect_bySelector :: Selector
scrollRect_bySelector = mkSelector "scrollRect:by:"

-- | @Selector@ for @translateRectsNeedingDisplayInRect:by:@
translateRectsNeedingDisplayInRect_bySelector :: Selector
translateRectsNeedingDisplayInRect_bySelector = mkSelector "translateRectsNeedingDisplayInRect:by:"

-- | @Selector@ for @hitTest:@
hitTestSelector :: Selector
hitTestSelector = mkSelector "hitTest:"

-- | @Selector@ for @mouse:inRect:@
mouse_inRectSelector :: Selector
mouse_inRectSelector = mkSelector "mouse:inRect:"

-- | @Selector@ for @viewWithTag:@
viewWithTagSelector :: Selector
viewWithTagSelector = mkSelector "viewWithTag:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @acceptsFirstMouse:@
acceptsFirstMouseSelector :: Selector
acceptsFirstMouseSelector = mkSelector "acceptsFirstMouse:"

-- | @Selector@ for @shouldDelayWindowOrderingForEvent:@
shouldDelayWindowOrderingForEventSelector :: Selector
shouldDelayWindowOrderingForEventSelector = mkSelector "shouldDelayWindowOrderingForEvent:"

-- | @Selector@ for @makeBackingLayer@
makeBackingLayerSelector :: Selector
makeBackingLayerSelector = mkSelector "makeBackingLayer"

-- | @Selector@ for @updateLayer@
updateLayerSelector :: Selector
updateLayerSelector = mkSelector "updateLayer"

-- | @Selector@ for @layoutSubtreeIfNeeded@
layoutSubtreeIfNeededSelector :: Selector
layoutSubtreeIfNeededSelector = mkSelector "layoutSubtreeIfNeeded"

-- | @Selector@ for @layout@
layoutSelector :: Selector
layoutSelector = mkSelector "layout"

-- | @Selector@ for @menuForEvent:@
menuForEventSelector :: Selector
menuForEventSelector = mkSelector "menuForEvent:"

-- | @Selector@ for @willOpenMenu:withEvent:@
willOpenMenu_withEventSelector :: Selector
willOpenMenu_withEventSelector = mkSelector "willOpenMenu:withEvent:"

-- | @Selector@ for @didCloseMenu:withEvent:@
didCloseMenu_withEventSelector :: Selector
didCloseMenu_withEventSelector = mkSelector "didCloseMenu:withEvent:"

-- | @Selector@ for @addToolTipRect:owner:userData:@
addToolTipRect_owner_userDataSelector :: Selector
addToolTipRect_owner_userDataSelector = mkSelector "addToolTipRect:owner:userData:"

-- | @Selector@ for @removeToolTip:@
removeToolTipSelector :: Selector
removeToolTipSelector = mkSelector "removeToolTip:"

-- | @Selector@ for @removeAllToolTips@
removeAllToolTipsSelector :: Selector
removeAllToolTipsSelector = mkSelector "removeAllToolTips"

-- | @Selector@ for @viewWillStartLiveResize@
viewWillStartLiveResizeSelector :: Selector
viewWillStartLiveResizeSelector = mkSelector "viewWillStartLiveResize"

-- | @Selector@ for @viewDidEndLiveResize@
viewDidEndLiveResizeSelector :: Selector
viewDidEndLiveResizeSelector = mkSelector "viewDidEndLiveResize"

-- | @Selector@ for @getRectsExposedDuringLiveResize:count:@
getRectsExposedDuringLiveResize_countSelector :: Selector
getRectsExposedDuringLiveResize_countSelector = mkSelector "getRectsExposedDuringLiveResize:count:"

-- | @Selector@ for @rectForSmartMagnificationAtPoint:inRect:@
rectForSmartMagnificationAtPoint_inRectSelector :: Selector
rectForSmartMagnificationAtPoint_inRectSelector = mkSelector "rectForSmartMagnificationAtPoint:inRect:"

-- | @Selector@ for @prepareForReuse@
prepareForReuseSelector :: Selector
prepareForReuseSelector = mkSelector "prepareForReuse"

-- | @Selector@ for @prepareContentInRect:@
prepareContentInRectSelector :: Selector
prepareContentInRectSelector = mkSelector "prepareContentInRect:"

-- | @Selector@ for @viewDidChangeEffectiveAppearance@
viewDidChangeEffectiveAppearanceSelector :: Selector
viewDidChangeEffectiveAppearanceSelector = mkSelector "viewDidChangeEffectiveAppearance"

-- | @Selector@ for @rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarkerSelector :: Selector
rulerView_shouldMoveMarkerSelector = mkSelector "rulerView:shouldMoveMarker:"

-- | @Selector@ for @rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocationSelector :: Selector
rulerView_willMoveMarker_toLocationSelector = mkSelector "rulerView:willMoveMarker:toLocation:"

-- | @Selector@ for @rulerView:didMoveMarker:@
rulerView_didMoveMarkerSelector :: Selector
rulerView_didMoveMarkerSelector = mkSelector "rulerView:didMoveMarker:"

-- | @Selector@ for @rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarkerSelector :: Selector
rulerView_shouldRemoveMarkerSelector = mkSelector "rulerView:shouldRemoveMarker:"

-- | @Selector@ for @rulerView:didRemoveMarker:@
rulerView_didRemoveMarkerSelector :: Selector
rulerView_didRemoveMarkerSelector = mkSelector "rulerView:didRemoveMarker:"

-- | @Selector@ for @rulerView:shouldAddMarker:@
rulerView_shouldAddMarkerSelector :: Selector
rulerView_shouldAddMarkerSelector = mkSelector "rulerView:shouldAddMarker:"

-- | @Selector@ for @rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocationSelector :: Selector
rulerView_willAddMarker_atLocationSelector = mkSelector "rulerView:willAddMarker:atLocation:"

-- | @Selector@ for @rulerView:didAddMarker:@
rulerView_didAddMarkerSelector :: Selector
rulerView_didAddMarkerSelector = mkSelector "rulerView:didAddMarker:"

-- | @Selector@ for @rulerView:handleMouseDown:@
rulerView_handleMouseDownSelector :: Selector
rulerView_handleMouseDownSelector = mkSelector "rulerView:handleMouseDown:"

-- | @Selector@ for @rulerView:willSetClientView:@
rulerView_willSetClientViewSelector :: Selector
rulerView_willSetClientViewSelector = mkSelector "rulerView:willSetClientView:"

-- | @Selector@ for @rulerView:locationForPoint:@
rulerView_locationForPointSelector :: Selector
rulerView_locationForPointSelector = mkSelector "rulerView:locationForPoint:"

-- | @Selector@ for @rulerView:pointForLocation:@
rulerView_pointForLocationSelector :: Selector
rulerView_pointForLocationSelector = mkSelector "rulerView:pointForLocation:"

-- | @Selector@ for @layoutGuideForLayoutRegion:@
layoutGuideForLayoutRegionSelector :: Selector
layoutGuideForLayoutRegionSelector = mkSelector "layoutGuideForLayoutRegion:"

-- | @Selector@ for @edgeInsetsForLayoutRegion:@
edgeInsetsForLayoutRegionSelector :: Selector
edgeInsetsForLayoutRegionSelector = mkSelector "edgeInsetsForLayoutRegion:"

-- | @Selector@ for @rectForLayoutRegion:@
rectForLayoutRegionSelector :: Selector
rectForLayoutRegionSelector = mkSelector "rectForLayoutRegion:"

-- | @Selector@ for @addLayoutGuide:@
addLayoutGuideSelector :: Selector
addLayoutGuideSelector = mkSelector "addLayoutGuide:"

-- | @Selector@ for @removeLayoutGuide:@
removeLayoutGuideSelector :: Selector
removeLayoutGuideSelector = mkSelector "removeLayoutGuide:"

-- | @Selector@ for @constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientationSelector :: Selector
constraintsAffectingLayoutForOrientationSelector = mkSelector "constraintsAffectingLayoutForOrientation:"

-- | @Selector@ for @exerciseAmbiguityInLayout@
exerciseAmbiguityInLayoutSelector :: Selector
exerciseAmbiguityInLayoutSelector = mkSelector "exerciseAmbiguityInLayout"

-- | @Selector@ for @alignmentRectForFrame:@
alignmentRectForFrameSelector :: Selector
alignmentRectForFrameSelector = mkSelector "alignmentRectForFrame:"

-- | @Selector@ for @frameForAlignmentRect:@
frameForAlignmentRectSelector :: Selector
frameForAlignmentRectSelector = mkSelector "frameForAlignmentRect:"

-- | @Selector@ for @invalidateIntrinsicContentSize@
invalidateIntrinsicContentSizeSelector :: Selector
invalidateIntrinsicContentSizeSelector = mkSelector "invalidateIntrinsicContentSize"

-- | @Selector@ for @contentHuggingPriorityForOrientation:@
contentHuggingPriorityForOrientationSelector :: Selector
contentHuggingPriorityForOrientationSelector = mkSelector "contentHuggingPriorityForOrientation:"

-- | @Selector@ for @setContentHuggingPriority:forOrientation:@
setContentHuggingPriority_forOrientationSelector :: Selector
setContentHuggingPriority_forOrientationSelector = mkSelector "setContentHuggingPriority:forOrientation:"

-- | @Selector@ for @contentCompressionResistancePriorityForOrientation:@
contentCompressionResistancePriorityForOrientationSelector :: Selector
contentCompressionResistancePriorityForOrientationSelector = mkSelector "contentCompressionResistancePriorityForOrientation:"

-- | @Selector@ for @setContentCompressionResistancePriority:forOrientation:@
setContentCompressionResistancePriority_forOrientationSelector :: Selector
setContentCompressionResistancePriority_forOrientationSelector = mkSelector "setContentCompressionResistancePriority:forOrientation:"

-- | @Selector@ for @updateConstraintsForSubtreeIfNeeded@
updateConstraintsForSubtreeIfNeededSelector :: Selector
updateConstraintsForSubtreeIfNeededSelector = mkSelector "updateConstraintsForSubtreeIfNeeded"

-- | @Selector@ for @updateConstraints@
updateConstraintsSelector :: Selector
updateConstraintsSelector = mkSelector "updateConstraints"

-- | @Selector@ for @addConstraint:@
addConstraintSelector :: Selector
addConstraintSelector = mkSelector "addConstraint:"

-- | @Selector@ for @addConstraints:@
addConstraintsSelector :: Selector
addConstraintsSelector = mkSelector "addConstraints:"

-- | @Selector@ for @removeConstraint:@
removeConstraintSelector :: Selector
removeConstraintSelector = mkSelector "removeConstraint:"

-- | @Selector@ for @removeConstraints:@
removeConstraintsSelector :: Selector
removeConstraintsSelector = mkSelector "removeConstraints:"

-- | @Selector@ for @reflectScrolledClipView:@
reflectScrolledClipViewSelector :: Selector
reflectScrolledClipViewSelector = mkSelector "reflectScrolledClipView:"

-- | @Selector@ for @scrollClipView:toPoint:@
scrollClipView_toPointSelector :: Selector
scrollClipView_toPointSelector = mkSelector "scrollClipView:toPoint:"

-- | @Selector@ for @dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBackSelector :: Selector
dragImage_at_offset_event_pasteboard_source_slideBackSelector = mkSelector "dragImage:at:offset:event:pasteboard:source:slideBack:"

-- | @Selector@ for @dragFile:fromRect:slideBack:event:@
dragFile_fromRect_slideBack_eventSelector :: Selector
dragFile_fromRect_slideBack_eventSelector = mkSelector "dragFile:fromRect:slideBack:event:"

-- | @Selector@ for @dragPromisedFilesOfTypes:fromRect:source:slideBack:event:@
dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector :: Selector
dragPromisedFilesOfTypes_fromRect_source_slideBack_eventSelector = mkSelector "dragPromisedFilesOfTypes:fromRect:source:slideBack:event:"

-- | @Selector@ for @convertPointToBase:@
convertPointToBaseSelector :: Selector
convertPointToBaseSelector = mkSelector "convertPointToBase:"

-- | @Selector@ for @convertPointFromBase:@
convertPointFromBaseSelector :: Selector
convertPointFromBaseSelector = mkSelector "convertPointFromBase:"

-- | @Selector@ for @convertSizeToBase:@
convertSizeToBaseSelector :: Selector
convertSizeToBaseSelector = mkSelector "convertSizeToBase:"

-- | @Selector@ for @convertSizeFromBase:@
convertSizeFromBaseSelector :: Selector
convertSizeFromBaseSelector = mkSelector "convertSizeFromBase:"

-- | @Selector@ for @convertRectToBase:@
convertRectToBaseSelector :: Selector
convertRectToBaseSelector = mkSelector "convertRectToBase:"

-- | @Selector@ for @convertRectFromBase:@
convertRectFromBaseSelector :: Selector
convertRectFromBaseSelector = mkSelector "convertRectFromBase:"

-- | @Selector@ for @performMnemonic:@
performMnemonicSelector :: Selector
performMnemonicSelector = mkSelector "performMnemonic:"

-- | @Selector@ for @shouldDrawColor@
shouldDrawColorSelector :: Selector
shouldDrawColorSelector = mkSelector "shouldDrawColor"

-- | @Selector@ for @gState@
gStateSelector :: Selector
gStateSelector = mkSelector "gState"

-- | @Selector@ for @allocateGState@
allocateGStateSelector :: Selector
allocateGStateSelector = mkSelector "allocateGState"

-- | @Selector@ for @releaseGState@
releaseGStateSelector :: Selector
releaseGStateSelector = mkSelector "releaseGState"

-- | @Selector@ for @setUpGState@
setUpGStateSelector :: Selector
setUpGStateSelector = mkSelector "setUpGState"

-- | @Selector@ for @renewGState@
renewGStateSelector :: Selector
renewGStateSelector = mkSelector "renewGState"

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @addTrackingArea:@
addTrackingAreaSelector :: Selector
addTrackingAreaSelector = mkSelector "addTrackingArea:"

-- | @Selector@ for @removeTrackingArea:@
removeTrackingAreaSelector :: Selector
removeTrackingAreaSelector = mkSelector "removeTrackingArea:"

-- | @Selector@ for @updateTrackingAreas@
updateTrackingAreasSelector :: Selector
updateTrackingAreasSelector = mkSelector "updateTrackingAreas"

-- | @Selector@ for @addCursorRect:cursor:@
addCursorRect_cursorSelector :: Selector
addCursorRect_cursorSelector = mkSelector "addCursorRect:cursor:"

-- | @Selector@ for @removeCursorRect:cursor:@
removeCursorRect_cursorSelector :: Selector
removeCursorRect_cursorSelector = mkSelector "removeCursorRect:cursor:"

-- | @Selector@ for @discardCursorRects@
discardCursorRectsSelector :: Selector
discardCursorRectsSelector = mkSelector "discardCursorRects"

-- | @Selector@ for @resetCursorRects@
resetCursorRectsSelector :: Selector
resetCursorRectsSelector = mkSelector "resetCursorRects"

-- | @Selector@ for @addTrackingRect:owner:userData:assumeInside:@
addTrackingRect_owner_userData_assumeInsideSelector :: Selector
addTrackingRect_owner_userData_assumeInsideSelector = mkSelector "addTrackingRect:owner:userData:assumeInside:"

-- | @Selector@ for @removeTrackingRect:@
removeTrackingRectSelector :: Selector
removeTrackingRectSelector = mkSelector "removeTrackingRect:"

-- | @Selector@ for @addGestureRecognizer:@
addGestureRecognizerSelector :: Selector
addGestureRecognizerSelector = mkSelector "addGestureRecognizer:"

-- | @Selector@ for @removeGestureRecognizer:@
removeGestureRecognizerSelector :: Selector
removeGestureRecognizerSelector = mkSelector "removeGestureRecognizer:"

-- | @Selector@ for @showDefinitionForAttributedString:atPoint:@
showDefinitionForAttributedString_atPointSelector :: Selector
showDefinitionForAttributedString_atPointSelector = mkSelector "showDefinitionForAttributedString:atPoint:"

-- | @Selector@ for @showDefinitionForAttributedString:range:options:baselineOriginProvider:@
showDefinitionForAttributedString_range_options_baselineOriginProviderSelector :: Selector
showDefinitionForAttributedString_range_options_baselineOriginProviderSelector = mkSelector "showDefinitionForAttributedString:range:options:baselineOriginProvider:"

-- | @Selector@ for @enterFullScreenMode:withOptions:@
enterFullScreenMode_withOptionsSelector :: Selector
enterFullScreenMode_withOptionsSelector = mkSelector "enterFullScreenMode:withOptions:"

-- | @Selector@ for @exitFullScreenModeWithOptions:@
exitFullScreenModeWithOptionsSelector :: Selector
exitFullScreenModeWithOptionsSelector = mkSelector "exitFullScreenModeWithOptions:"

-- | @Selector@ for @beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_sourceSelector :: Selector
beginDraggingSessionWithItems_event_sourceSelector = mkSelector "beginDraggingSessionWithItems:event:source:"

-- | @Selector@ for @registerForDraggedTypes:@
registerForDraggedTypesSelector :: Selector
registerForDraggedTypesSelector = mkSelector "registerForDraggedTypes:"

-- | @Selector@ for @unregisterDraggedTypes@
unregisterDraggedTypesSelector :: Selector
unregisterDraggedTypesSelector = mkSelector "unregisterDraggedTypes"

-- | @Selector@ for @writeEPSInsideRect:toPasteboard:@
writeEPSInsideRect_toPasteboardSelector :: Selector
writeEPSInsideRect_toPasteboardSelector = mkSelector "writeEPSInsideRect:toPasteboard:"

-- | @Selector@ for @dataWithEPSInsideRect:@
dataWithEPSInsideRectSelector :: Selector
dataWithEPSInsideRectSelector = mkSelector "dataWithEPSInsideRect:"

-- | @Selector@ for @writePDFInsideRect:toPasteboard:@
writePDFInsideRect_toPasteboardSelector :: Selector
writePDFInsideRect_toPasteboardSelector = mkSelector "writePDFInsideRect:toPasteboard:"

-- | @Selector@ for @dataWithPDFInsideRect:@
dataWithPDFInsideRectSelector :: Selector
dataWithPDFInsideRectSelector = mkSelector "dataWithPDFInsideRect:"

-- | @Selector@ for @print:@
printSelector :: Selector
printSelector = mkSelector "print:"

-- | @Selector@ for @knowsPageRange:@
knowsPageRangeSelector :: Selector
knowsPageRangeSelector = mkSelector "knowsPageRange:"

-- | @Selector@ for @adjustPageWidthNew:left:right:limit:@
adjustPageWidthNew_left_right_limitSelector :: Selector
adjustPageWidthNew_left_right_limitSelector = mkSelector "adjustPageWidthNew:left:right:limit:"

-- | @Selector@ for @adjustPageHeightNew:top:bottom:limit:@
adjustPageHeightNew_top_bottom_limitSelector :: Selector
adjustPageHeightNew_top_bottom_limitSelector = mkSelector "adjustPageHeightNew:top:bottom:limit:"

-- | @Selector@ for @rectForPage:@
rectForPageSelector :: Selector
rectForPageSelector = mkSelector "rectForPage:"

-- | @Selector@ for @locationOfPrintRect:@
locationOfPrintRectSelector :: Selector
locationOfPrintRectSelector = mkSelector "locationOfPrintRect:"

-- | @Selector@ for @drawPageBorderWithSize:@
drawPageBorderWithSizeSelector :: Selector
drawPageBorderWithSizeSelector = mkSelector "drawPageBorderWithSize:"

-- | @Selector@ for @drawSheetBorderWithSize:@
drawSheetBorderWithSizeSelector :: Selector
drawSheetBorderWithSizeSelector = mkSelector "drawSheetBorderWithSize:"

-- | @Selector@ for @beginDocument@
beginDocumentSelector :: Selector
beginDocumentSelector = mkSelector "beginDocument"

-- | @Selector@ for @endDocument@
endDocumentSelector :: Selector
endDocumentSelector = mkSelector "endDocument"

-- | @Selector@ for @beginPageInRect:atPlacement:@
beginPageInRect_atPlacementSelector :: Selector
beginPageInRect_atPlacementSelector = mkSelector "beginPageInRect:atPlacement:"

-- | @Selector@ for @endPage@
endPageSelector :: Selector
endPageSelector = mkSelector "endPage"

-- | @Selector@ for @setKeyboardFocusRingNeedsDisplayInRect:@
setKeyboardFocusRingNeedsDisplayInRectSelector :: Selector
setKeyboardFocusRingNeedsDisplayInRectSelector = mkSelector "setKeyboardFocusRingNeedsDisplayInRect:"

-- | @Selector@ for @drawFocusRingMask@
drawFocusRingMaskSelector :: Selector
drawFocusRingMaskSelector = mkSelector "drawFocusRingMask"

-- | @Selector@ for @noteFocusRingMaskChanged@
noteFocusRingMaskChangedSelector :: Selector
noteFocusRingMaskChangedSelector = mkSelector "noteFocusRingMaskChanged"

-- | @Selector@ for @window@
windowSelector :: Selector
windowSelector = mkSelector "window"

-- | @Selector@ for @superview@
superviewSelector :: Selector
superviewSelector = mkSelector "superview"

-- | @Selector@ for @subviews@
subviewsSelector :: Selector
subviewsSelector = mkSelector "subviews"

-- | @Selector@ for @setSubviews:@
setSubviewsSelector :: Selector
setSubviewsSelector = mkSelector "setSubviews:"

-- | @Selector@ for @opaqueAncestor@
opaqueAncestorSelector :: Selector
opaqueAncestorSelector = mkSelector "opaqueAncestor"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestorSelector :: Selector
hiddenOrHasHiddenAncestorSelector = mkSelector "hiddenOrHasHiddenAncestor"

-- | @Selector@ for @wantsDefaultClipping@
wantsDefaultClippingSelector :: Selector
wantsDefaultClippingSelector = mkSelector "wantsDefaultClipping"

-- | @Selector@ for @postsFrameChangedNotifications@
postsFrameChangedNotificationsSelector :: Selector
postsFrameChangedNotificationsSelector = mkSelector "postsFrameChangedNotifications"

-- | @Selector@ for @setPostsFrameChangedNotifications:@
setPostsFrameChangedNotificationsSelector :: Selector
setPostsFrameChangedNotificationsSelector = mkSelector "setPostsFrameChangedNotifications:"

-- | @Selector@ for @autoresizesSubviews@
autoresizesSubviewsSelector :: Selector
autoresizesSubviewsSelector = mkSelector "autoresizesSubviews"

-- | @Selector@ for @setAutoresizesSubviews:@
setAutoresizesSubviewsSelector :: Selector
setAutoresizesSubviewsSelector = mkSelector "setAutoresizesSubviews:"

-- | @Selector@ for @autoresizingMask@
autoresizingMaskSelector :: Selector
autoresizingMaskSelector = mkSelector "autoresizingMask"

-- | @Selector@ for @setAutoresizingMask:@
setAutoresizingMaskSelector :: Selector
setAutoresizingMaskSelector = mkSelector "setAutoresizingMask:"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @frameRotation@
frameRotationSelector :: Selector
frameRotationSelector = mkSelector "frameRotation"

-- | @Selector@ for @setFrameRotation:@
setFrameRotationSelector :: Selector
setFrameRotationSelector = mkSelector "setFrameRotation:"

-- | @Selector@ for @frameCenterRotation@
frameCenterRotationSelector :: Selector
frameCenterRotationSelector = mkSelector "frameCenterRotation"

-- | @Selector@ for @setFrameCenterRotation:@
setFrameCenterRotationSelector :: Selector
setFrameCenterRotationSelector = mkSelector "setFrameCenterRotation:"

-- | @Selector@ for @boundsRotation@
boundsRotationSelector :: Selector
boundsRotationSelector = mkSelector "boundsRotation"

-- | @Selector@ for @setBoundsRotation:@
setBoundsRotationSelector :: Selector
setBoundsRotationSelector = mkSelector "setBoundsRotation:"

-- | @Selector@ for @bounds@
boundsSelector :: Selector
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @setBounds:@
setBoundsSelector :: Selector
setBoundsSelector = mkSelector "setBounds:"

-- | @Selector@ for @flipped@
flippedSelector :: Selector
flippedSelector = mkSelector "flipped"

-- | @Selector@ for @rotatedFromBase@
rotatedFromBaseSelector :: Selector
rotatedFromBaseSelector = mkSelector "rotatedFromBase"

-- | @Selector@ for @rotatedOrScaledFromBase@
rotatedOrScaledFromBaseSelector :: Selector
rotatedOrScaledFromBaseSelector = mkSelector "rotatedOrScaledFromBase"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @canDrawConcurrently@
canDrawConcurrentlySelector :: Selector
canDrawConcurrentlySelector = mkSelector "canDrawConcurrently"

-- | @Selector@ for @setCanDrawConcurrently:@
setCanDrawConcurrentlySelector :: Selector
setCanDrawConcurrentlySelector = mkSelector "setCanDrawConcurrently:"

-- | @Selector@ for @canDraw@
canDrawSelector :: Selector
canDrawSelector = mkSelector "canDraw"

-- | @Selector@ for @needsDisplay@
needsDisplaySelector :: Selector
needsDisplaySelector = mkSelector "needsDisplay"

-- | @Selector@ for @setNeedsDisplay:@
setNeedsDisplaySelector :: Selector
setNeedsDisplaySelector = mkSelector "setNeedsDisplay:"

-- | @Selector@ for @focusView@
focusViewSelector :: Selector
focusViewSelector = mkSelector "focusView"

-- | @Selector@ for @visibleRect@
visibleRectSelector :: Selector
visibleRectSelector = mkSelector "visibleRect"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @needsPanelToBecomeKey@
needsPanelToBecomeKeySelector :: Selector
needsPanelToBecomeKeySelector = mkSelector "needsPanelToBecomeKey"

-- | @Selector@ for @mouseDownCanMoveWindow@
mouseDownCanMoveWindowSelector :: Selector
mouseDownCanMoveWindowSelector = mkSelector "mouseDownCanMoveWindow"

-- | @Selector@ for @acceptsTouchEvents@
acceptsTouchEventsSelector :: Selector
acceptsTouchEventsSelector = mkSelector "acceptsTouchEvents"

-- | @Selector@ for @setAcceptsTouchEvents:@
setAcceptsTouchEventsSelector :: Selector
setAcceptsTouchEventsSelector = mkSelector "setAcceptsTouchEvents:"

-- | @Selector@ for @wantsRestingTouches@
wantsRestingTouchesSelector :: Selector
wantsRestingTouchesSelector = mkSelector "wantsRestingTouches"

-- | @Selector@ for @setWantsRestingTouches:@
setWantsRestingTouchesSelector :: Selector
setWantsRestingTouchesSelector = mkSelector "setWantsRestingTouches:"

-- | @Selector@ for @layerContentsRedrawPolicy@
layerContentsRedrawPolicySelector :: Selector
layerContentsRedrawPolicySelector = mkSelector "layerContentsRedrawPolicy"

-- | @Selector@ for @setLayerContentsRedrawPolicy:@
setLayerContentsRedrawPolicySelector :: Selector
setLayerContentsRedrawPolicySelector = mkSelector "setLayerContentsRedrawPolicy:"

-- | @Selector@ for @layerContentsPlacement@
layerContentsPlacementSelector :: Selector
layerContentsPlacementSelector = mkSelector "layerContentsPlacement"

-- | @Selector@ for @setLayerContentsPlacement:@
setLayerContentsPlacementSelector :: Selector
setLayerContentsPlacementSelector = mkSelector "setLayerContentsPlacement:"

-- | @Selector@ for @wantsLayer@
wantsLayerSelector :: Selector
wantsLayerSelector = mkSelector "wantsLayer"

-- | @Selector@ for @setWantsLayer:@
setWantsLayerSelector :: Selector
setWantsLayerSelector = mkSelector "setWantsLayer:"

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector
setLayerSelector = mkSelector "setLayer:"

-- | @Selector@ for @wantsUpdateLayer@
wantsUpdateLayerSelector :: Selector
wantsUpdateLayerSelector = mkSelector "wantsUpdateLayer"

-- | @Selector@ for @canDrawSubviewsIntoLayer@
canDrawSubviewsIntoLayerSelector :: Selector
canDrawSubviewsIntoLayerSelector = mkSelector "canDrawSubviewsIntoLayer"

-- | @Selector@ for @setCanDrawSubviewsIntoLayer:@
setCanDrawSubviewsIntoLayerSelector :: Selector
setCanDrawSubviewsIntoLayerSelector = mkSelector "setCanDrawSubviewsIntoLayer:"

-- | @Selector@ for @needsLayout@
needsLayoutSelector :: Selector
needsLayoutSelector = mkSelector "needsLayout"

-- | @Selector@ for @setNeedsLayout:@
setNeedsLayoutSelector :: Selector
setNeedsLayoutSelector = mkSelector "setNeedsLayout:"

-- | @Selector@ for @alphaValue@
alphaValueSelector :: Selector
alphaValueSelector = mkSelector "alphaValue"

-- | @Selector@ for @setAlphaValue:@
setAlphaValueSelector :: Selector
setAlphaValueSelector = mkSelector "setAlphaValue:"

-- | @Selector@ for @layerUsesCoreImageFilters@
layerUsesCoreImageFiltersSelector :: Selector
layerUsesCoreImageFiltersSelector = mkSelector "layerUsesCoreImageFilters"

-- | @Selector@ for @setLayerUsesCoreImageFilters:@
setLayerUsesCoreImageFiltersSelector :: Selector
setLayerUsesCoreImageFiltersSelector = mkSelector "setLayerUsesCoreImageFilters:"

-- | @Selector@ for @backgroundFilters@
backgroundFiltersSelector :: Selector
backgroundFiltersSelector = mkSelector "backgroundFilters"

-- | @Selector@ for @setBackgroundFilters:@
setBackgroundFiltersSelector :: Selector
setBackgroundFiltersSelector = mkSelector "setBackgroundFilters:"

-- | @Selector@ for @compositingFilter@
compositingFilterSelector :: Selector
compositingFilterSelector = mkSelector "compositingFilter"

-- | @Selector@ for @setCompositingFilter:@
setCompositingFilterSelector :: Selector
setCompositingFilterSelector = mkSelector "setCompositingFilter:"

-- | @Selector@ for @contentFilters@
contentFiltersSelector :: Selector
contentFiltersSelector = mkSelector "contentFilters"

-- | @Selector@ for @setContentFilters:@
setContentFiltersSelector :: Selector
setContentFiltersSelector = mkSelector "setContentFilters:"

-- | @Selector@ for @shadow@
shadowSelector :: Selector
shadowSelector = mkSelector "shadow"

-- | @Selector@ for @setShadow:@
setShadowSelector :: Selector
setShadowSelector = mkSelector "setShadow:"

-- | @Selector@ for @clipsToBounds@
clipsToBoundsSelector :: Selector
clipsToBoundsSelector = mkSelector "clipsToBounds"

-- | @Selector@ for @setClipsToBounds:@
setClipsToBoundsSelector :: Selector
setClipsToBoundsSelector = mkSelector "setClipsToBounds:"

-- | @Selector@ for @postsBoundsChangedNotifications@
postsBoundsChangedNotificationsSelector :: Selector
postsBoundsChangedNotificationsSelector = mkSelector "postsBoundsChangedNotifications"

-- | @Selector@ for @setPostsBoundsChangedNotifications:@
setPostsBoundsChangedNotificationsSelector :: Selector
setPostsBoundsChangedNotificationsSelector = mkSelector "setPostsBoundsChangedNotifications:"

-- | @Selector@ for @enclosingScrollView@
enclosingScrollViewSelector :: Selector
enclosingScrollViewSelector = mkSelector "enclosingScrollView"

-- | @Selector@ for @defaultMenu@
defaultMenuSelector :: Selector
defaultMenuSelector = mkSelector "defaultMenu"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @inLiveResize@
inLiveResizeSelector :: Selector
inLiveResizeSelector = mkSelector "inLiveResize"

-- | @Selector@ for @preservesContentDuringLiveResize@
preservesContentDuringLiveResizeSelector :: Selector
preservesContentDuringLiveResizeSelector = mkSelector "preservesContentDuringLiveResize"

-- | @Selector@ for @rectPreservedDuringLiveResize@
rectPreservedDuringLiveResizeSelector :: Selector
rectPreservedDuringLiveResizeSelector = mkSelector "rectPreservedDuringLiveResize"

-- | @Selector@ for @inputContext@
inputContextSelector :: Selector
inputContextSelector = mkSelector "inputContext"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @compatibleWithResponsiveScrolling@
compatibleWithResponsiveScrollingSelector :: Selector
compatibleWithResponsiveScrollingSelector = mkSelector "compatibleWithResponsiveScrolling"

-- | @Selector@ for @preparedContentRect@
preparedContentRectSelector :: Selector
preparedContentRectSelector = mkSelector "preparedContentRect"

-- | @Selector@ for @setPreparedContentRect:@
setPreparedContentRectSelector :: Selector
setPreparedContentRectSelector = mkSelector "setPreparedContentRect:"

-- | @Selector@ for @allowsVibrancy@
allowsVibrancySelector :: Selector
allowsVibrancySelector = mkSelector "allowsVibrancy"

-- | @Selector@ for @pressureConfiguration@
pressureConfigurationSelector :: Selector
pressureConfigurationSelector = mkSelector "pressureConfiguration"

-- | @Selector@ for @setPressureConfiguration:@
setPressureConfigurationSelector :: Selector
setPressureConfigurationSelector = mkSelector "setPressureConfiguration:"

-- | @Selector@ for @wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector
wantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "wantsExtendedDynamicRangeOpenGLSurface"

-- | @Selector@ for @setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector
setWantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "setWantsExtendedDynamicRangeOpenGLSurface:"

-- | @Selector@ for @wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurfaceSelector :: Selector
wantsBestResolutionOpenGLSurfaceSelector = mkSelector "wantsBestResolutionOpenGLSurface"

-- | @Selector@ for @setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurfaceSelector :: Selector
setWantsBestResolutionOpenGLSurfaceSelector = mkSelector "setWantsBestResolutionOpenGLSurface:"

-- | @Selector@ for @layoutGuides@
layoutGuidesSelector :: Selector
layoutGuidesSelector = mkSelector "layoutGuides"

-- | @Selector@ for @hasAmbiguousLayout@
hasAmbiguousLayoutSelector :: Selector
hasAmbiguousLayoutSelector = mkSelector "hasAmbiguousLayout"

-- | @Selector@ for @fittingSize@
fittingSizeSelector :: Selector
fittingSizeSelector = mkSelector "fittingSize"

-- | @Selector@ for @alignmentRectInsets@
alignmentRectInsetsSelector :: Selector
alignmentRectInsetsSelector = mkSelector "alignmentRectInsets"

-- | @Selector@ for @firstBaselineOffsetFromTop@
firstBaselineOffsetFromTopSelector :: Selector
firstBaselineOffsetFromTopSelector = mkSelector "firstBaselineOffsetFromTop"

-- | @Selector@ for @lastBaselineOffsetFromBottom@
lastBaselineOffsetFromBottomSelector :: Selector
lastBaselineOffsetFromBottomSelector = mkSelector "lastBaselineOffsetFromBottom"

-- | @Selector@ for @baselineOffsetFromBottom@
baselineOffsetFromBottomSelector :: Selector
baselineOffsetFromBottomSelector = mkSelector "baselineOffsetFromBottom"

-- | @Selector@ for @intrinsicContentSize@
intrinsicContentSizeSelector :: Selector
intrinsicContentSizeSelector = mkSelector "intrinsicContentSize"

-- | @Selector@ for @horizontalContentSizeConstraintActive@
horizontalContentSizeConstraintActiveSelector :: Selector
horizontalContentSizeConstraintActiveSelector = mkSelector "horizontalContentSizeConstraintActive"

-- | @Selector@ for @setHorizontalContentSizeConstraintActive:@
setHorizontalContentSizeConstraintActiveSelector :: Selector
setHorizontalContentSizeConstraintActiveSelector = mkSelector "setHorizontalContentSizeConstraintActive:"

-- | @Selector@ for @verticalContentSizeConstraintActive@
verticalContentSizeConstraintActiveSelector :: Selector
verticalContentSizeConstraintActiveSelector = mkSelector "verticalContentSizeConstraintActive"

-- | @Selector@ for @setVerticalContentSizeConstraintActive:@
setVerticalContentSizeConstraintActiveSelector :: Selector
setVerticalContentSizeConstraintActiveSelector = mkSelector "setVerticalContentSizeConstraintActive:"

-- | @Selector@ for @translatesAutoresizingMaskIntoConstraints@
translatesAutoresizingMaskIntoConstraintsSelector :: Selector
translatesAutoresizingMaskIntoConstraintsSelector = mkSelector "translatesAutoresizingMaskIntoConstraints"

-- | @Selector@ for @setTranslatesAutoresizingMaskIntoConstraints:@
setTranslatesAutoresizingMaskIntoConstraintsSelector :: Selector
setTranslatesAutoresizingMaskIntoConstraintsSelector = mkSelector "setTranslatesAutoresizingMaskIntoConstraints:"

-- | @Selector@ for @requiresConstraintBasedLayout@
requiresConstraintBasedLayoutSelector :: Selector
requiresConstraintBasedLayoutSelector = mkSelector "requiresConstraintBasedLayout"

-- | @Selector@ for @needsUpdateConstraints@
needsUpdateConstraintsSelector :: Selector
needsUpdateConstraintsSelector = mkSelector "needsUpdateConstraints"

-- | @Selector@ for @setNeedsUpdateConstraints:@
setNeedsUpdateConstraintsSelector :: Selector
setNeedsUpdateConstraintsSelector = mkSelector "setNeedsUpdateConstraints:"

-- | @Selector@ for @leadingAnchor@
leadingAnchorSelector :: Selector
leadingAnchorSelector = mkSelector "leadingAnchor"

-- | @Selector@ for @trailingAnchor@
trailingAnchorSelector :: Selector
trailingAnchorSelector = mkSelector "trailingAnchor"

-- | @Selector@ for @leftAnchor@
leftAnchorSelector :: Selector
leftAnchorSelector = mkSelector "leftAnchor"

-- | @Selector@ for @rightAnchor@
rightAnchorSelector :: Selector
rightAnchorSelector = mkSelector "rightAnchor"

-- | @Selector@ for @topAnchor@
topAnchorSelector :: Selector
topAnchorSelector = mkSelector "topAnchor"

-- | @Selector@ for @bottomAnchor@
bottomAnchorSelector :: Selector
bottomAnchorSelector = mkSelector "bottomAnchor"

-- | @Selector@ for @widthAnchor@
widthAnchorSelector :: Selector
widthAnchorSelector = mkSelector "widthAnchor"

-- | @Selector@ for @heightAnchor@
heightAnchorSelector :: Selector
heightAnchorSelector = mkSelector "heightAnchor"

-- | @Selector@ for @centerXAnchor@
centerXAnchorSelector :: Selector
centerXAnchorSelector = mkSelector "centerXAnchor"

-- | @Selector@ for @centerYAnchor@
centerYAnchorSelector :: Selector
centerYAnchorSelector = mkSelector "centerYAnchor"

-- | @Selector@ for @firstBaselineAnchor@
firstBaselineAnchorSelector :: Selector
firstBaselineAnchorSelector = mkSelector "firstBaselineAnchor"

-- | @Selector@ for @lastBaselineAnchor@
lastBaselineAnchorSelector :: Selector
lastBaselineAnchorSelector = mkSelector "lastBaselineAnchor"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @candidateListTouchBarItem@
candidateListTouchBarItemSelector :: Selector
candidateListTouchBarItemSelector = mkSelector "candidateListTouchBarItem"

-- | @Selector@ for @enclosingMenuItem@
enclosingMenuItemSelector :: Selector
enclosingMenuItemSelector = mkSelector "enclosingMenuItem"

-- | @Selector@ for @writingToolsCoordinator@
writingToolsCoordinatorSelector :: Selector
writingToolsCoordinatorSelector = mkSelector "writingToolsCoordinator"

-- | @Selector@ for @setWritingToolsCoordinator:@
setWritingToolsCoordinatorSelector :: Selector
setWritingToolsCoordinatorSelector = mkSelector "setWritingToolsCoordinator:"

-- | @Selector@ for @trackingAreas@
trackingAreasSelector :: Selector
trackingAreasSelector = mkSelector "trackingAreas"

-- | @Selector@ for @prefersCompactControlSizeMetrics@
prefersCompactControlSizeMetricsSelector :: Selector
prefersCompactControlSizeMetricsSelector = mkSelector "prefersCompactControlSizeMetrics"

-- | @Selector@ for @setPrefersCompactControlSizeMetrics:@
setPrefersCompactControlSizeMetricsSelector :: Selector
setPrefersCompactControlSizeMetricsSelector = mkSelector "setPrefersCompactControlSizeMetrics:"

-- | @Selector@ for @safeAreaInsets@
safeAreaInsetsSelector :: Selector
safeAreaInsetsSelector = mkSelector "safeAreaInsets"

-- | @Selector@ for @additionalSafeAreaInsets@
additionalSafeAreaInsetsSelector :: Selector
additionalSafeAreaInsetsSelector = mkSelector "additionalSafeAreaInsets"

-- | @Selector@ for @setAdditionalSafeAreaInsets:@
setAdditionalSafeAreaInsetsSelector :: Selector
setAdditionalSafeAreaInsetsSelector = mkSelector "setAdditionalSafeAreaInsets:"

-- | @Selector@ for @safeAreaLayoutGuide@
safeAreaLayoutGuideSelector :: Selector
safeAreaLayoutGuideSelector = mkSelector "safeAreaLayoutGuide"

-- | @Selector@ for @safeAreaRect@
safeAreaRectSelector :: Selector
safeAreaRectSelector = mkSelector "safeAreaRect"

-- | @Selector@ for @layoutMarginsGuide@
layoutMarginsGuideSelector :: Selector
layoutMarginsGuideSelector = mkSelector "layoutMarginsGuide"

-- | @Selector@ for @allowedTouchTypes@
allowedTouchTypesSelector :: Selector
allowedTouchTypesSelector = mkSelector "allowedTouchTypes"

-- | @Selector@ for @setAllowedTouchTypes:@
setAllowedTouchTypesSelector :: Selector
setAllowedTouchTypesSelector = mkSelector "setAllowedTouchTypes:"

-- | @Selector@ for @gestureRecognizers@
gestureRecognizersSelector :: Selector
gestureRecognizersSelector = mkSelector "gestureRecognizers"

-- | @Selector@ for @setGestureRecognizers:@
setGestureRecognizersSelector :: Selector
setGestureRecognizersSelector = mkSelector "setGestureRecognizers:"

-- | @Selector@ for @drawingFindIndicator@
drawingFindIndicatorSelector :: Selector
drawingFindIndicatorSelector = mkSelector "drawingFindIndicator"

-- | @Selector@ for @inFullScreenMode@
inFullScreenModeSelector :: Selector
inFullScreenModeSelector = mkSelector "inFullScreenMode"

-- | @Selector@ for @registeredDraggedTypes@
registeredDraggedTypesSelector :: Selector
registeredDraggedTypesSelector = mkSelector "registeredDraggedTypes"

-- | @Selector@ for @heightAdjustLimit@
heightAdjustLimitSelector :: Selector
heightAdjustLimitSelector = mkSelector "heightAdjustLimit"

-- | @Selector@ for @widthAdjustLimit@
widthAdjustLimitSelector :: Selector
widthAdjustLimitSelector = mkSelector "widthAdjustLimit"

-- | @Selector@ for @pageHeader@
pageHeaderSelector :: Selector
pageHeaderSelector = mkSelector "pageHeader"

-- | @Selector@ for @pageFooter@
pageFooterSelector :: Selector
pageFooterSelector = mkSelector "pageFooter"

-- | @Selector@ for @printJobTitle@
printJobTitleSelector :: Selector
printJobTitleSelector = mkSelector "printJobTitle"

-- | @Selector@ for @nextKeyView@
nextKeyViewSelector :: Selector
nextKeyViewSelector = mkSelector "nextKeyView"

-- | @Selector@ for @setNextKeyView:@
setNextKeyViewSelector :: Selector
setNextKeyViewSelector = mkSelector "setNextKeyView:"

-- | @Selector@ for @previousKeyView@
previousKeyViewSelector :: Selector
previousKeyViewSelector = mkSelector "previousKeyView"

-- | @Selector@ for @nextValidKeyView@
nextValidKeyViewSelector :: Selector
nextValidKeyViewSelector = mkSelector "nextValidKeyView"

-- | @Selector@ for @previousValidKeyView@
previousValidKeyViewSelector :: Selector
previousValidKeyViewSelector = mkSelector "previousValidKeyView"

-- | @Selector@ for @canBecomeKeyView@
canBecomeKeyViewSelector :: Selector
canBecomeKeyViewSelector = mkSelector "canBecomeKeyView"

-- | @Selector@ for @focusRingType@
focusRingTypeSelector :: Selector
focusRingTypeSelector = mkSelector "focusRingType"

-- | @Selector@ for @setFocusRingType:@
setFocusRingTypeSelector :: Selector
setFocusRingTypeSelector = mkSelector "setFocusRingType:"

-- | @Selector@ for @defaultFocusRingType@
defaultFocusRingTypeSelector :: Selector
defaultFocusRingTypeSelector = mkSelector "defaultFocusRingType"

-- | @Selector@ for @focusRingMaskBounds@
focusRingMaskBoundsSelector :: Selector
focusRingMaskBoundsSelector = mkSelector "focusRingMaskBounds"

