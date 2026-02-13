{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWindow@.
module ObjC.AppKit.NSWindow
  ( NSWindow
  , IsNSWindow(..)
  , frameRectForContentRect_styleMask
  , contentRectForFrameRect_styleMask
  , minFrameWidthWithTitle_styleMask
  , frameRectForContentRect
  , contentRectForFrameRect
  , initWithContentRect_styleMask_backing_defer
  , initWithContentRect_styleMask_backing_defer_screen
  , initWithCoder
  , addTitlebarAccessoryViewController
  , insertTitlebarAccessoryViewController_atIndex
  , removeTitlebarAccessoryViewControllerAtIndex
  , setTitleWithRepresentedFilename
  , fieldEditor_forObject
  , endEditingFor
  , constrainFrameRect_toScreen
  , setFrame_display
  , setContentSize
  , setFrameOrigin
  , setFrameTopLeftPoint
  , cascadeTopLeftFromPoint
  , animationResizeTime
  , setFrame_display_animate
  , displayIfNeeded
  , display
  , update
  , makeFirstResponder
  , close
  , miniaturize
  , deminiaturize
  , zoom
  , tryToPerform_with
  , validRequestorForSendType_returnType
  , setContentBorderThickness_forEdge
  , contentBorderThicknessForEdge
  , setAutorecalculatesContentBorderThickness_forEdge
  , autorecalculatesContentBorderThicknessForEdge
  , center
  , makeKeyAndOrderFront
  , orderFront
  , orderBack
  , orderOut
  , orderWindow_relativeTo
  , orderFrontRegardless
  , makeKeyWindow
  , makeMainWindow
  , becomeKeyWindow
  , resignKeyWindow
  , becomeMainWindow
  , resignMainWindow
  , convertRectToScreen
  , convertRectFromScreen
  , convertPointToScreen
  , convertPointFromScreen
  , convertRectToBacking
  , convertRectFromBacking
  , convertPointToBacking
  , convertPointFromBacking
  , backingAlignedRect_options
  , performClose
  , performMiniaturize
  , performZoom
  , dataWithEPSInsideRect
  , dataWithPDFInsideRect
  , print_
  , setDynamicDepthLimit
  , invalidateShadow
  , toggleFullScreen
  , setFrameFromString
  , saveFrameUsingName
  , setFrameUsingName_force
  , setFrameUsingName
  , setFrameAutosaveName
  , removeFrameUsingName
  , beginSheet_completionHandler
  , beginCriticalSheet_completionHandler
  , endSheet
  , endSheet_returnCode
  , standardWindowButton_forStyleMask
  , standardWindowButton
  , addChildWindow_ordered
  , removeChildWindow
  , canRepresentDisplayGamut
  , windowNumbersWithOptions
  , windowNumberAtPoint_belowWindowWithWindowNumber
  , windowWithContentViewController
  , performWindowDragWithEvent
  , selectNextKeyView
  , selectPreviousKeyView
  , selectKeyViewFollowingView
  , selectKeyViewPrecedingView
  , disableKeyEquivalentForDefaultButtonCell
  , enableKeyEquivalentForDefaultButtonCell
  , recalculateKeyViewLoop
  , toggleToolbarShown
  , runToolbarCustomizationPalette
  , selectNextTab
  , selectPreviousTab
  , moveTabToNewWindow
  , mergeAllWindows
  , toggleTabBar
  , toggleTabOverview
  , addTabbedWindow_ordered
  , transferWindowSharingToWindow_completionHandler
  , requestSharingOfWindow_completionHandler
  , requestSharingOfWindowUsingPreview_title_completionHandler
  , disableSnapshotRestoration
  , enableSnapshotRestoration
  , setIsMiniaturized
  , setIsVisible
  , setIsZoomed
  , handleCloseScriptCommand
  , handlePrintScriptCommand
  , handleSaveScriptCommand
  , visualizeConstraints
  , anchorAttributeForOrientation
  , setAnchorAttribute_forOrientation
  , updateConstraintsIfNeeded
  , layoutIfNeeded
  , cacheImageInRect
  , restoreCachedImage
  , discardCachedImage
  , menuChanged
  , gState
  , convertBaseToScreen
  , convertScreenToBase
  , userSpaceScaleFactor
  , useOptimizedDrawing
  , canStoreColor
  , disableFlushWindow
  , enableFlushWindow
  , flushWindow
  , flushWindowIfNeeded
  , initWithWindowRef
  , disableScreenUpdatesUntilFlush
  , displayLinkWithTarget_selector
  , beginDraggingSessionWithItems_event_source
  , dragImage_at_offset_event_pasteboard_source_slideBack
  , registerForDraggedTypes
  , unregisterDraggedTypes
  , disableCursorRects
  , enableCursorRects
  , discardCursorRects
  , invalidateCursorRectsForView
  , resetCursorRects
  , trackEventsMatchingMask_timeout_mode_handler
  , nextEventMatchingMask
  , nextEventMatchingMask_untilDate_inMode_dequeue
  , discardEventsMatchingMask_beforeEvent
  , postEvent_atStart
  , sendEvent
  , defaultDepthLimit
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , titleVisibility
  , setTitleVisibility
  , titlebarAppearsTransparent
  , setTitlebarAppearsTransparent
  , toolbarStyle
  , setToolbarStyle
  , contentLayoutRect
  , contentLayoutGuide
  , titlebarAccessoryViewControllers
  , setTitlebarAccessoryViewControllers
  , representedURL
  , setRepresentedURL
  , representedFilename
  , setRepresentedFilename
  , excludedFromWindowsMenu
  , setExcludedFromWindowsMenu
  , contentView
  , setContentView
  , delegate
  , setDelegate
  , windowNumber
  , styleMask
  , setStyleMask
  , cascadingReferenceFrame
  , frame
  , inLiveResize
  , resizeIncrements
  , setResizeIncrements
  , aspectRatio
  , setAspectRatio
  , contentResizeIncrements
  , setContentResizeIncrements
  , contentAspectRatio
  , setContentAspectRatio
  , viewsNeedDisplay
  , setViewsNeedDisplay
  , preservesContentDuringLiveResize
  , setPreservesContentDuringLiveResize
  , firstResponder
  , resizeFlags
  , releasedWhenClosed
  , setReleasedWhenClosed
  , zoomed
  , miniaturized
  , backgroundColor
  , setBackgroundColor
  , movable
  , setMovable
  , movableByWindowBackground
  , setMovableByWindowBackground
  , hidesOnDeactivate
  , setHidesOnDeactivate
  , canHide
  , setCanHide
  , miniwindowImage
  , setMiniwindowImage
  , miniwindowTitle
  , setMiniwindowTitle
  , dockTile
  , documentEdited
  , setDocumentEdited
  , visible
  , keyWindow
  , mainWindow
  , canBecomeKeyWindow
  , canBecomeMainWindow
  , worksWhenModal
  , preventsApplicationTerminationWhenModal
  , setPreventsApplicationTerminationWhenModal
  , backingScaleFactor
  , allowsToolTipsWhenApplicationIsInactive
  , setAllowsToolTipsWhenApplicationIsInactive
  , backingType
  , setBackingType
  , level
  , setLevel
  , depthLimit
  , setDepthLimit
  , hasDynamicDepthLimit
  , screen
  , deepestScreen
  , hasShadow
  , setHasShadow
  , alphaValue
  , setAlphaValue
  , opaque
  , setOpaque
  , sharingType
  , setSharingType
  , allowsConcurrentViewDrawing
  , setAllowsConcurrentViewDrawing
  , displaysWhenScreenProfileChanges
  , setDisplaysWhenScreenProfileChanges
  , canBecomeVisibleWithoutLogin
  , setCanBecomeVisibleWithoutLogin
  , collectionBehavior
  , setCollectionBehavior
  , animationBehavior
  , setAnimationBehavior
  , onActiveSpace
  , stringWithSavedFrame
  , frameAutosaveName
  , minSize
  , setMinSize
  , maxSize
  , setMaxSize
  , contentMinSize
  , setContentMinSize
  , contentMaxSize
  , setContentMaxSize
  , minFullScreenContentSize
  , setMinFullScreenContentSize
  , maxFullScreenContentSize
  , setMaxFullScreenContentSize
  , deviceDescription
  , windowController
  , setWindowController
  , sheets
  , attachedSheet
  , sheet
  , sheetParent
  , childWindows
  , parentWindow
  , setParentWindow
  , appearanceSource
  , setAppearanceSource
  , colorSpace
  , setColorSpace
  , occlusionState
  , titlebarSeparatorStyle
  , setTitlebarSeparatorStyle
  , contentViewController
  , setContentViewController
  , initialFirstResponder
  , setInitialFirstResponder
  , keyViewSelectionDirection
  , defaultButtonCell
  , setDefaultButtonCell
  , autorecalculatesKeyViewLoop
  , setAutorecalculatesKeyViewLoop
  , toolbar
  , setToolbar
  , showsToolbarButton
  , setShowsToolbarButton
  , allowsAutomaticWindowTabbing
  , setAllowsAutomaticWindowTabbing
  , userTabbingPreference
  , tabbingMode
  , setTabbingMode
  , tabbingIdentifier
  , setTabbingIdentifier
  , tabbedWindows
  , tab
  , tabGroup
  , hasActiveWindowSharingSession
  , windowTitlebarLayoutDirection
  , restorable
  , setRestorable
  , restorationClass
  , setRestorationClass
  , hasCloseBox
  , hasTitleBar
  , floatingPanel
  , miniaturizable
  , modalPanel
  , resizable
  , zoomable
  , orderedIndex
  , setOrderedIndex
  , drawers
  , flushWindowDisabled
  , autodisplay
  , setAutodisplay
  , graphicsContext
  , oneShot
  , setOneShot
  , preferredBackingLocation
  , setPreferredBackingLocation
  , backingLocation
  , showsResizeIndicator
  , setShowsResizeIndicator
  , windowRef
  , areCursorRectsEnabled
  , currentEvent
  , acceptsMouseMovedEvents
  , setAcceptsMouseMovedEvents
  , ignoresMouseEvents
  , setIgnoresMouseEvents
  , mouseLocationOutsideOfEventStream
  , acceptsMouseMovedEventsSelector
  , addChildWindow_orderedSelector
  , addTabbedWindow_orderedSelector
  , addTitlebarAccessoryViewControllerSelector
  , allowsAutomaticWindowTabbingSelector
  , allowsConcurrentViewDrawingSelector
  , allowsToolTipsWhenApplicationIsInactiveSelector
  , alphaValueSelector
  , anchorAttributeForOrientationSelector
  , animationBehaviorSelector
  , animationResizeTimeSelector
  , appearanceSourceSelector
  , areCursorRectsEnabledSelector
  , aspectRatioSelector
  , attachedSheetSelector
  , autodisplaySelector
  , autorecalculatesContentBorderThicknessForEdgeSelector
  , autorecalculatesKeyViewLoopSelector
  , backgroundColorSelector
  , backingAlignedRect_optionsSelector
  , backingLocationSelector
  , backingScaleFactorSelector
  , backingTypeSelector
  , becomeKeyWindowSelector
  , becomeMainWindowSelector
  , beginCriticalSheet_completionHandlerSelector
  , beginDraggingSessionWithItems_event_sourceSelector
  , beginSheet_completionHandlerSelector
  , cacheImageInRectSelector
  , canBecomeKeyWindowSelector
  , canBecomeMainWindowSelector
  , canBecomeVisibleWithoutLoginSelector
  , canHideSelector
  , canRepresentDisplayGamutSelector
  , canStoreColorSelector
  , cascadeTopLeftFromPointSelector
  , cascadingReferenceFrameSelector
  , centerSelector
  , childWindowsSelector
  , closeSelector
  , collectionBehaviorSelector
  , colorSpaceSelector
  , constrainFrameRect_toScreenSelector
  , contentAspectRatioSelector
  , contentBorderThicknessForEdgeSelector
  , contentLayoutGuideSelector
  , contentLayoutRectSelector
  , contentMaxSizeSelector
  , contentMinSizeSelector
  , contentRectForFrameRectSelector
  , contentRectForFrameRect_styleMaskSelector
  , contentResizeIncrementsSelector
  , contentViewControllerSelector
  , contentViewSelector
  , convertBaseToScreenSelector
  , convertPointFromBackingSelector
  , convertPointFromScreenSelector
  , convertPointToBackingSelector
  , convertPointToScreenSelector
  , convertRectFromBackingSelector
  , convertRectFromScreenSelector
  , convertRectToBackingSelector
  , convertRectToScreenSelector
  , convertScreenToBaseSelector
  , currentEventSelector
  , dataWithEPSInsideRectSelector
  , dataWithPDFInsideRectSelector
  , deepestScreenSelector
  , defaultButtonCellSelector
  , defaultDepthLimitSelector
  , delegateSelector
  , deminiaturizeSelector
  , depthLimitSelector
  , deviceDescriptionSelector
  , disableCursorRectsSelector
  , disableFlushWindowSelector
  , disableKeyEquivalentForDefaultButtonCellSelector
  , disableScreenUpdatesUntilFlushSelector
  , disableSnapshotRestorationSelector
  , discardCachedImageSelector
  , discardCursorRectsSelector
  , discardEventsMatchingMask_beforeEventSelector
  , displayIfNeededSelector
  , displayLinkWithTarget_selectorSelector
  , displaySelector
  , displaysWhenScreenProfileChangesSelector
  , dockTileSelector
  , documentEditedSelector
  , dragImage_at_offset_event_pasteboard_source_slideBackSelector
  , drawersSelector
  , enableCursorRectsSelector
  , enableFlushWindowSelector
  , enableKeyEquivalentForDefaultButtonCellSelector
  , enableSnapshotRestorationSelector
  , endEditingForSelector
  , endSheetSelector
  , endSheet_returnCodeSelector
  , excludedFromWindowsMenuSelector
  , fieldEditor_forObjectSelector
  , firstResponderSelector
  , floatingPanelSelector
  , flushWindowDisabledSelector
  , flushWindowIfNeededSelector
  , flushWindowSelector
  , frameAutosaveNameSelector
  , frameRectForContentRectSelector
  , frameRectForContentRect_styleMaskSelector
  , frameSelector
  , gStateSelector
  , graphicsContextSelector
  , handleCloseScriptCommandSelector
  , handlePrintScriptCommandSelector
  , handleSaveScriptCommandSelector
  , hasActiveWindowSharingSessionSelector
  , hasCloseBoxSelector
  , hasDynamicDepthLimitSelector
  , hasShadowSelector
  , hasTitleBarSelector
  , hidesOnDeactivateSelector
  , ignoresMouseEventsSelector
  , inLiveResizeSelector
  , initWithCoderSelector
  , initWithContentRect_styleMask_backing_deferSelector
  , initWithContentRect_styleMask_backing_defer_screenSelector
  , initWithWindowRefSelector
  , initialFirstResponderSelector
  , insertTitlebarAccessoryViewController_atIndexSelector
  , invalidateCursorRectsForViewSelector
  , invalidateShadowSelector
  , keyViewSelectionDirectionSelector
  , keyWindowSelector
  , layoutIfNeededSelector
  , levelSelector
  , mainWindowSelector
  , makeFirstResponderSelector
  , makeKeyAndOrderFrontSelector
  , makeKeyWindowSelector
  , makeMainWindowSelector
  , maxFullScreenContentSizeSelector
  , maxSizeSelector
  , menuChangedSelector
  , mergeAllWindowsSelector
  , minFrameWidthWithTitle_styleMaskSelector
  , minFullScreenContentSizeSelector
  , minSizeSelector
  , miniaturizableSelector
  , miniaturizeSelector
  , miniaturizedSelector
  , miniwindowImageSelector
  , miniwindowTitleSelector
  , modalPanelSelector
  , mouseLocationOutsideOfEventStreamSelector
  , movableByWindowBackgroundSelector
  , movableSelector
  , moveTabToNewWindowSelector
  , nextEventMatchingMaskSelector
  , nextEventMatchingMask_untilDate_inMode_dequeueSelector
  , occlusionStateSelector
  , onActiveSpaceSelector
  , oneShotSelector
  , opaqueSelector
  , orderBackSelector
  , orderFrontRegardlessSelector
  , orderFrontSelector
  , orderOutSelector
  , orderWindow_relativeToSelector
  , orderedIndexSelector
  , parentWindowSelector
  , performCloseSelector
  , performMiniaturizeSelector
  , performWindowDragWithEventSelector
  , performZoomSelector
  , postEvent_atStartSelector
  , preferredBackingLocationSelector
  , preservesContentDuringLiveResizeSelector
  , preventsApplicationTerminationWhenModalSelector
  , printSelector
  , recalculateKeyViewLoopSelector
  , registerForDraggedTypesSelector
  , releasedWhenClosedSelector
  , removeChildWindowSelector
  , removeFrameUsingNameSelector
  , removeTitlebarAccessoryViewControllerAtIndexSelector
  , representedFilenameSelector
  , representedURLSelector
  , requestSharingOfWindowUsingPreview_title_completionHandlerSelector
  , requestSharingOfWindow_completionHandlerSelector
  , resetCursorRectsSelector
  , resignKeyWindowSelector
  , resignMainWindowSelector
  , resizableSelector
  , resizeFlagsSelector
  , resizeIncrementsSelector
  , restorableSelector
  , restorationClassSelector
  , restoreCachedImageSelector
  , runToolbarCustomizationPaletteSelector
  , saveFrameUsingNameSelector
  , screenSelector
  , selectKeyViewFollowingViewSelector
  , selectKeyViewPrecedingViewSelector
  , selectNextKeyViewSelector
  , selectNextTabSelector
  , selectPreviousKeyViewSelector
  , selectPreviousTabSelector
  , sendEventSelector
  , setAcceptsMouseMovedEventsSelector
  , setAllowsAutomaticWindowTabbingSelector
  , setAllowsConcurrentViewDrawingSelector
  , setAllowsToolTipsWhenApplicationIsInactiveSelector
  , setAlphaValueSelector
  , setAnchorAttribute_forOrientationSelector
  , setAnimationBehaviorSelector
  , setAppearanceSourceSelector
  , setAspectRatioSelector
  , setAutodisplaySelector
  , setAutorecalculatesContentBorderThickness_forEdgeSelector
  , setAutorecalculatesKeyViewLoopSelector
  , setBackgroundColorSelector
  , setBackingTypeSelector
  , setCanBecomeVisibleWithoutLoginSelector
  , setCanHideSelector
  , setCollectionBehaviorSelector
  , setColorSpaceSelector
  , setContentAspectRatioSelector
  , setContentBorderThickness_forEdgeSelector
  , setContentMaxSizeSelector
  , setContentMinSizeSelector
  , setContentResizeIncrementsSelector
  , setContentSizeSelector
  , setContentViewControllerSelector
  , setContentViewSelector
  , setDefaultButtonCellSelector
  , setDelegateSelector
  , setDepthLimitSelector
  , setDisplaysWhenScreenProfileChangesSelector
  , setDocumentEditedSelector
  , setDynamicDepthLimitSelector
  , setExcludedFromWindowsMenuSelector
  , setFrameAutosaveNameSelector
  , setFrameFromStringSelector
  , setFrameOriginSelector
  , setFrameTopLeftPointSelector
  , setFrameUsingNameSelector
  , setFrameUsingName_forceSelector
  , setFrame_displaySelector
  , setFrame_display_animateSelector
  , setHasShadowSelector
  , setHidesOnDeactivateSelector
  , setIgnoresMouseEventsSelector
  , setInitialFirstResponderSelector
  , setIsMiniaturizedSelector
  , setIsVisibleSelector
  , setIsZoomedSelector
  , setLevelSelector
  , setMaxFullScreenContentSizeSelector
  , setMaxSizeSelector
  , setMinFullScreenContentSizeSelector
  , setMinSizeSelector
  , setMiniwindowImageSelector
  , setMiniwindowTitleSelector
  , setMovableByWindowBackgroundSelector
  , setMovableSelector
  , setOneShotSelector
  , setOpaqueSelector
  , setOrderedIndexSelector
  , setParentWindowSelector
  , setPreferredBackingLocationSelector
  , setPreservesContentDuringLiveResizeSelector
  , setPreventsApplicationTerminationWhenModalSelector
  , setReleasedWhenClosedSelector
  , setRepresentedFilenameSelector
  , setRepresentedURLSelector
  , setResizeIncrementsSelector
  , setRestorableSelector
  , setRestorationClassSelector
  , setSharingTypeSelector
  , setShowsResizeIndicatorSelector
  , setShowsToolbarButtonSelector
  , setStyleMaskSelector
  , setSubtitleSelector
  , setTabbingIdentifierSelector
  , setTabbingModeSelector
  , setTitleSelector
  , setTitleVisibilitySelector
  , setTitleWithRepresentedFilenameSelector
  , setTitlebarAccessoryViewControllersSelector
  , setTitlebarAppearsTransparentSelector
  , setTitlebarSeparatorStyleSelector
  , setToolbarSelector
  , setToolbarStyleSelector
  , setViewsNeedDisplaySelector
  , setWindowControllerSelector
  , sharingTypeSelector
  , sheetParentSelector
  , sheetSelector
  , sheetsSelector
  , showsResizeIndicatorSelector
  , showsToolbarButtonSelector
  , standardWindowButtonSelector
  , standardWindowButton_forStyleMaskSelector
  , stringWithSavedFrameSelector
  , styleMaskSelector
  , subtitleSelector
  , tabGroupSelector
  , tabSelector
  , tabbedWindowsSelector
  , tabbingIdentifierSelector
  , tabbingModeSelector
  , titleSelector
  , titleVisibilitySelector
  , titlebarAccessoryViewControllersSelector
  , titlebarAppearsTransparentSelector
  , titlebarSeparatorStyleSelector
  , toggleFullScreenSelector
  , toggleTabBarSelector
  , toggleTabOverviewSelector
  , toggleToolbarShownSelector
  , toolbarSelector
  , toolbarStyleSelector
  , trackEventsMatchingMask_timeout_mode_handlerSelector
  , transferWindowSharingToWindow_completionHandlerSelector
  , tryToPerform_withSelector
  , unregisterDraggedTypesSelector
  , updateConstraintsIfNeededSelector
  , updateSelector
  , useOptimizedDrawingSelector
  , userSpaceScaleFactorSelector
  , userTabbingPreferenceSelector
  , validRequestorForSendType_returnTypeSelector
  , viewsNeedDisplaySelector
  , visibleSelector
  , visualizeConstraintsSelector
  , windowControllerSelector
  , windowNumberAtPoint_belowWindowWithWindowNumberSelector
  , windowNumberSelector
  , windowNumbersWithOptionsSelector
  , windowRefSelector
  , windowTitlebarLayoutDirectionSelector
  , windowWithContentViewControllerSelector
  , worksWhenModalSelector
  , zoomSelector
  , zoomableSelector
  , zoomedSelector

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
  , NSBackingStoreType(NSBackingStoreType)
  , pattern NSBackingStoreRetained
  , pattern NSBackingStoreNonretained
  , pattern NSBackingStoreBuffered
  , NSDisplayGamut(NSDisplayGamut)
  , pattern NSDisplayGamutSRGB
  , pattern NSDisplayGamutP3
  , NSEventMask(NSEventMask)
  , pattern NSEventMaskLeftMouseDown
  , pattern NSEventMaskLeftMouseUp
  , pattern NSEventMaskRightMouseDown
  , pattern NSEventMaskRightMouseUp
  , pattern NSEventMaskMouseMoved
  , pattern NSEventMaskLeftMouseDragged
  , pattern NSEventMaskRightMouseDragged
  , pattern NSEventMaskMouseEntered
  , pattern NSEventMaskMouseExited
  , pattern NSEventMaskKeyDown
  , pattern NSEventMaskKeyUp
  , pattern NSEventMaskFlagsChanged
  , pattern NSEventMaskAppKitDefined
  , pattern NSEventMaskSystemDefined
  , pattern NSEventMaskApplicationDefined
  , pattern NSEventMaskPeriodic
  , pattern NSEventMaskCursorUpdate
  , pattern NSEventMaskScrollWheel
  , pattern NSEventMaskTabletPoint
  , pattern NSEventMaskTabletProximity
  , pattern NSEventMaskOtherMouseDown
  , pattern NSEventMaskOtherMouseUp
  , pattern NSEventMaskOtherMouseDragged
  , pattern NSEventMaskGesture
  , pattern NSEventMaskMagnify
  , pattern NSEventMaskSwipe
  , pattern NSEventMaskRotate
  , pattern NSEventMaskBeginGesture
  , pattern NSEventMaskEndGesture
  , pattern NSEventMaskSmartMagnify
  , pattern NSEventMaskPressure
  , pattern NSEventMaskDirectTouch
  , pattern NSEventMaskChangeMode
  , pattern NSEventMaskMouseCancelled
  , pattern NSEventMaskAny
  , NSEventModifierFlags(NSEventModifierFlags)
  , pattern NSEventModifierFlagCapsLock
  , pattern NSEventModifierFlagShift
  , pattern NSEventModifierFlagControl
  , pattern NSEventModifierFlagOption
  , pattern NSEventModifierFlagCommand
  , pattern NSEventModifierFlagNumericPad
  , pattern NSEventModifierFlagHelp
  , pattern NSEventModifierFlagFunction
  , pattern NSEventModifierFlagDeviceIndependentFlagsMask
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
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge
  , NSSelectionDirection(NSSelectionDirection)
  , pattern NSDirectSelection
  , pattern NSSelectingNext
  , pattern NSSelectingPrevious
  , NSTitlebarSeparatorStyle(NSTitlebarSeparatorStyle)
  , pattern NSTitlebarSeparatorStyleAutomatic
  , pattern NSTitlebarSeparatorStyleNone
  , pattern NSTitlebarSeparatorStyleLine
  , pattern NSTitlebarSeparatorStyleShadow
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft
  , NSWindowAnimationBehavior(NSWindowAnimationBehavior)
  , pattern NSWindowAnimationBehaviorDefault
  , pattern NSWindowAnimationBehaviorNone
  , pattern NSWindowAnimationBehaviorDocumentWindow
  , pattern NSWindowAnimationBehaviorUtilityWindow
  , pattern NSWindowAnimationBehaviorAlertPanel
  , NSWindowBackingLocation(NSWindowBackingLocation)
  , pattern NSWindowBackingLocationDefault
  , pattern NSWindowBackingLocationVideoMemory
  , pattern NSWindowBackingLocationMainMemory
  , NSWindowButton(NSWindowButton)
  , pattern NSWindowCloseButton
  , pattern NSWindowMiniaturizeButton
  , pattern NSWindowZoomButton
  , pattern NSWindowToolbarButton
  , pattern NSWindowDocumentIconButton
  , pattern NSWindowDocumentVersionsButton
  , NSWindowCollectionBehavior(NSWindowCollectionBehavior)
  , pattern NSWindowCollectionBehaviorDefault
  , pattern NSWindowCollectionBehaviorCanJoinAllSpaces
  , pattern NSWindowCollectionBehaviorMoveToActiveSpace
  , pattern NSWindowCollectionBehaviorManaged
  , pattern NSWindowCollectionBehaviorTransient
  , pattern NSWindowCollectionBehaviorStationary
  , pattern NSWindowCollectionBehaviorParticipatesInCycle
  , pattern NSWindowCollectionBehaviorIgnoresCycle
  , pattern NSWindowCollectionBehaviorFullScreenPrimary
  , pattern NSWindowCollectionBehaviorFullScreenAuxiliary
  , pattern NSWindowCollectionBehaviorFullScreenNone
  , pattern NSWindowCollectionBehaviorFullScreenAllowsTiling
  , pattern NSWindowCollectionBehaviorFullScreenDisallowsTiling
  , pattern NSWindowCollectionBehaviorPrimary
  , pattern NSWindowCollectionBehaviorAuxiliary
  , pattern NSWindowCollectionBehaviorCanJoinAllApplications
  , NSWindowDepth(NSWindowDepth)
  , pattern NSWindowDepthTwentyfourBitRGB
  , pattern NSWindowDepthSixtyfourBitRGB
  , pattern NSWindowDepthOnehundredtwentyeightBitRGB
  , NSWindowNumberListOptions(NSWindowNumberListOptions)
  , pattern NSWindowNumberListAllApplications
  , pattern NSWindowNumberListAllSpaces
  , NSWindowOcclusionState(NSWindowOcclusionState)
  , pattern NSWindowOcclusionStateVisible
  , NSWindowOrderingMode(NSWindowOrderingMode)
  , pattern NSWindowAbove
  , pattern NSWindowBelow
  , pattern NSWindowOut
  , NSWindowSharingType(NSWindowSharingType)
  , pattern NSWindowSharingNone
  , pattern NSWindowSharingReadOnly
  , NSWindowStyleMask(NSWindowStyleMask)
  , pattern NSWindowStyleMaskBorderless
  , pattern NSWindowStyleMaskTitled
  , pattern NSWindowStyleMaskClosable
  , pattern NSWindowStyleMaskMiniaturizable
  , pattern NSWindowStyleMaskResizable
  , pattern NSWindowStyleMaskTexturedBackground
  , pattern NSWindowStyleMaskUnifiedTitleAndToolbar
  , pattern NSWindowStyleMaskFullScreen
  , pattern NSWindowStyleMaskFullSizeContentView
  , pattern NSWindowStyleMaskUtilityWindow
  , pattern NSWindowStyleMaskDocModalWindow
  , pattern NSWindowStyleMaskNonactivatingPanel
  , pattern NSWindowStyleMaskHUDWindow
  , NSWindowTabbingMode(NSWindowTabbingMode)
  , pattern NSWindowTabbingModeAutomatic
  , pattern NSWindowTabbingModePreferred
  , pattern NSWindowTabbingModeDisallowed
  , NSWindowTitleVisibility(NSWindowTitleVisibility)
  , pattern NSWindowTitleVisible
  , pattern NSWindowTitleHidden
  , NSWindowToolbarStyle(NSWindowToolbarStyle)
  , pattern NSWindowToolbarStyleAutomatic
  , pattern NSWindowToolbarStyleExpanded
  , pattern NSWindowToolbarStylePreference
  , pattern NSWindowToolbarStyleUnified
  , pattern NSWindowToolbarStyleUnifiedCompact
  , NSWindowUserTabbingPreference(NSWindowUserTabbingPreference)
  , pattern NSWindowUserTabbingPreferenceManual
  , pattern NSWindowUserTabbingPreferenceAlways
  , pattern NSWindowUserTabbingPreferenceInFullScreen

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

-- | @+ frameRectForContentRect:styleMask:@
frameRectForContentRect_styleMask :: NSRect -> NSWindowStyleMask -> IO NSRect
frameRectForContentRect_styleMask cRect style =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' frameRectForContentRect_styleMaskSelector cRect style

-- | @+ contentRectForFrameRect:styleMask:@
contentRectForFrameRect_styleMask :: NSRect -> NSWindowStyleMask -> IO NSRect
contentRectForFrameRect_styleMask fRect style =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' contentRectForFrameRect_styleMaskSelector fRect style

-- | @+ minFrameWidthWithTitle:styleMask:@
minFrameWidthWithTitle_styleMask :: IsNSString title => title -> NSWindowStyleMask -> IO CDouble
minFrameWidthWithTitle_styleMask title style =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' minFrameWidthWithTitle_styleMaskSelector (toNSString title) style

-- | @- frameRectForContentRect:@
frameRectForContentRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
frameRectForContentRect nsWindow contentRect =
  sendMessage nsWindow frameRectForContentRectSelector contentRect

-- | @- contentRectForFrameRect:@
contentRectForFrameRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
contentRectForFrameRect nsWindow frameRect =
  sendMessage nsWindow contentRectForFrameRectSelector frameRect

-- | @- initWithContentRect:styleMask:backing:defer:@
initWithContentRect_styleMask_backing_defer :: IsNSWindow nsWindow => nsWindow -> NSRect -> NSWindowStyleMask -> NSBackingStoreType -> Bool -> IO (Id NSWindow)
initWithContentRect_styleMask_backing_defer nsWindow contentRect style backingStoreType flag =
  sendOwnedMessage nsWindow initWithContentRect_styleMask_backing_deferSelector contentRect style backingStoreType flag

-- | @- initWithContentRect:styleMask:backing:defer:screen:@
initWithContentRect_styleMask_backing_defer_screen :: (IsNSWindow nsWindow, IsNSScreen screen) => nsWindow -> NSRect -> NSWindowStyleMask -> NSBackingStoreType -> Bool -> screen -> IO (Id NSWindow)
initWithContentRect_styleMask_backing_defer_screen nsWindow contentRect style backingStoreType flag screen =
  sendOwnedMessage nsWindow initWithContentRect_styleMask_backing_defer_screenSelector contentRect style backingStoreType flag (toNSScreen screen)

-- | @- initWithCoder:@
initWithCoder :: (IsNSWindow nsWindow, IsNSCoder coder) => nsWindow -> coder -> IO (Id NSWindow)
initWithCoder nsWindow coder =
  sendOwnedMessage nsWindow initWithCoderSelector (toNSCoder coder)

-- | @- addTitlebarAccessoryViewController:@
addTitlebarAccessoryViewController :: (IsNSWindow nsWindow, IsNSTitlebarAccessoryViewController childViewController) => nsWindow -> childViewController -> IO ()
addTitlebarAccessoryViewController nsWindow childViewController =
  sendMessage nsWindow addTitlebarAccessoryViewControllerSelector (toNSTitlebarAccessoryViewController childViewController)

-- | @- insertTitlebarAccessoryViewController:atIndex:@
insertTitlebarAccessoryViewController_atIndex :: (IsNSWindow nsWindow, IsNSTitlebarAccessoryViewController childViewController) => nsWindow -> childViewController -> CLong -> IO ()
insertTitlebarAccessoryViewController_atIndex nsWindow childViewController index =
  sendMessage nsWindow insertTitlebarAccessoryViewController_atIndexSelector (toNSTitlebarAccessoryViewController childViewController) index

-- | @- removeTitlebarAccessoryViewControllerAtIndex:@
removeTitlebarAccessoryViewControllerAtIndex :: IsNSWindow nsWindow => nsWindow -> CLong -> IO ()
removeTitlebarAccessoryViewControllerAtIndex nsWindow index =
  sendMessage nsWindow removeTitlebarAccessoryViewControllerAtIndexSelector index

-- | @- setTitleWithRepresentedFilename:@
setTitleWithRepresentedFilename :: (IsNSWindow nsWindow, IsNSString filename) => nsWindow -> filename -> IO ()
setTitleWithRepresentedFilename nsWindow filename =
  sendMessage nsWindow setTitleWithRepresentedFilenameSelector (toNSString filename)

-- | @- fieldEditor:forObject:@
fieldEditor_forObject :: IsNSWindow nsWindow => nsWindow -> Bool -> RawId -> IO (Id NSText)
fieldEditor_forObject nsWindow createFlag object =
  sendMessage nsWindow fieldEditor_forObjectSelector createFlag object

-- | @- endEditingFor:@
endEditingFor :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
endEditingFor nsWindow object =
  sendMessage nsWindow endEditingForSelector object

-- | @- constrainFrameRect:toScreen:@
constrainFrameRect_toScreen :: (IsNSWindow nsWindow, IsNSScreen screen) => nsWindow -> NSRect -> screen -> IO NSRect
constrainFrameRect_toScreen nsWindow frameRect screen =
  sendMessage nsWindow constrainFrameRect_toScreenSelector frameRect (toNSScreen screen)

-- | @- setFrame:display:@
setFrame_display :: IsNSWindow nsWindow => nsWindow -> NSRect -> Bool -> IO ()
setFrame_display nsWindow frameRect flag =
  sendMessage nsWindow setFrame_displaySelector frameRect flag

-- | @- setContentSize:@
setContentSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentSize nsWindow size =
  sendMessage nsWindow setContentSizeSelector size

-- | @- setFrameOrigin:@
setFrameOrigin :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO ()
setFrameOrigin nsWindow point =
  sendMessage nsWindow setFrameOriginSelector point

-- | @- setFrameTopLeftPoint:@
setFrameTopLeftPoint :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO ()
setFrameTopLeftPoint nsWindow point =
  sendMessage nsWindow setFrameTopLeftPointSelector point

-- | @- cascadeTopLeftFromPoint:@
cascadeTopLeftFromPoint :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
cascadeTopLeftFromPoint nsWindow topLeftPoint =
  sendMessage nsWindow cascadeTopLeftFromPointSelector topLeftPoint

-- | Subclasses can override @animationResizeTime:@ to control the total time for the frame change. @newFrame@ is the rect passed into @setFrame:display:animate:@
--
-- ObjC selector: @- animationResizeTime:@
animationResizeTime :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO CDouble
animationResizeTime nsWindow newFrame =
  sendMessage nsWindow animationResizeTimeSelector newFrame

-- | @setFrame:display:animate:@ is equivalent to @setFrame:display:@ if the @animateFlag@ is NO.    If the @animationFlag@ is YES, this method will perform a smooth resize of the window, where the total time for the resize is specified by @-animationResizeTime:@
--
-- ObjC selector: @- setFrame:display:animate:@
setFrame_display_animate :: IsNSWindow nsWindow => nsWindow -> NSRect -> Bool -> Bool -> IO ()
setFrame_display_animate nsWindow frameRect displayFlag animateFlag =
  sendMessage nsWindow setFrame_display_animateSelector frameRect displayFlag animateFlag

-- | @- displayIfNeeded@
displayIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
displayIfNeeded nsWindow =
  sendMessage nsWindow displayIfNeededSelector

-- | @- display@
display :: IsNSWindow nsWindow => nsWindow -> IO ()
display nsWindow =
  sendMessage nsWindow displaySelector

-- | @- update@
update :: IsNSWindow nsWindow => nsWindow -> IO ()
update nsWindow =
  sendMessage nsWindow updateSelector

-- | @- makeFirstResponder:@
makeFirstResponder :: (IsNSWindow nsWindow, IsNSResponder responder) => nsWindow -> responder -> IO Bool
makeFirstResponder nsWindow responder =
  sendMessage nsWindow makeFirstResponderSelector (toNSResponder responder)

-- | @- close@
close :: IsNSWindow nsWindow => nsWindow -> IO ()
close nsWindow =
  sendMessage nsWindow closeSelector

-- | @- miniaturize:@
miniaturize :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
miniaturize nsWindow sender =
  sendMessage nsWindow miniaturizeSelector sender

-- | @- deminiaturize:@
deminiaturize :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
deminiaturize nsWindow sender =
  sendMessage nsWindow deminiaturizeSelector sender

-- | @- zoom:@
zoom :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
zoom nsWindow sender =
  sendMessage nsWindow zoomSelector sender

-- | @- tryToPerform:with:@
tryToPerform_with :: IsNSWindow nsWindow => nsWindow -> Sel -> RawId -> IO Bool
tryToPerform_with nsWindow action object =
  sendMessage nsWindow tryToPerform_withSelector action object

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSWindow nsWindow, IsNSString sendType, IsNSString returnType) => nsWindow -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsWindow sendType returnType =
  sendMessage nsWindow validRequestorForSendType_returnTypeSelector (toNSString sendType) (toNSString returnType)

-- | @- setContentBorderThickness:forEdge:@
setContentBorderThickness_forEdge :: IsNSWindow nsWindow => nsWindow -> CDouble -> NSRectEdge -> IO ()
setContentBorderThickness_forEdge nsWindow thickness edge =
  sendMessage nsWindow setContentBorderThickness_forEdgeSelector thickness edge

-- | @- contentBorderThicknessForEdge:@
contentBorderThicknessForEdge :: IsNSWindow nsWindow => nsWindow -> NSRectEdge -> IO CDouble
contentBorderThicknessForEdge nsWindow edge =
  sendMessage nsWindow contentBorderThicknessForEdgeSelector edge

-- | @- setAutorecalculatesContentBorderThickness:forEdge:@
setAutorecalculatesContentBorderThickness_forEdge :: IsNSWindow nsWindow => nsWindow -> Bool -> NSRectEdge -> IO ()
setAutorecalculatesContentBorderThickness_forEdge nsWindow flag edge =
  sendMessage nsWindow setAutorecalculatesContentBorderThickness_forEdgeSelector flag edge

-- | @- autorecalculatesContentBorderThicknessForEdge:@
autorecalculatesContentBorderThicknessForEdge :: IsNSWindow nsWindow => nsWindow -> NSRectEdge -> IO Bool
autorecalculatesContentBorderThicknessForEdge nsWindow edge =
  sendMessage nsWindow autorecalculatesContentBorderThicknessForEdgeSelector edge

-- | @- center@
center :: IsNSWindow nsWindow => nsWindow -> IO ()
center nsWindow =
  sendMessage nsWindow centerSelector

-- | @- makeKeyAndOrderFront:@
makeKeyAndOrderFront :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
makeKeyAndOrderFront nsWindow sender =
  sendMessage nsWindow makeKeyAndOrderFrontSelector sender

-- | @- orderFront:@
orderFront :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
orderFront nsWindow sender =
  sendMessage nsWindow orderFrontSelector sender

-- | @- orderBack:@
orderBack :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
orderBack nsWindow sender =
  sendMessage nsWindow orderBackSelector sender

-- | @- orderOut:@
orderOut :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
orderOut nsWindow sender =
  sendMessage nsWindow orderOutSelector sender

-- | @- orderWindow:relativeTo:@
orderWindow_relativeTo :: IsNSWindow nsWindow => nsWindow -> NSWindowOrderingMode -> CLong -> IO ()
orderWindow_relativeTo nsWindow place otherWin =
  sendMessage nsWindow orderWindow_relativeToSelector place otherWin

-- | @- orderFrontRegardless@
orderFrontRegardless :: IsNSWindow nsWindow => nsWindow -> IO ()
orderFrontRegardless nsWindow =
  sendMessage nsWindow orderFrontRegardlessSelector

-- | Makes the window key and main if eligible, updating NSApplication's @-keyWindow@ and @-mainWindow@ properties.
--
-- ObjC selector: @- makeKeyWindow@
makeKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
makeKeyWindow nsWindow =
  sendMessage nsWindow makeKeyWindowSelector

-- | Makes the window main if eligible. Updates NSApplication's @-mainWindow@ property.
--
-- ObjC selector: @- makeMainWindow@
makeMainWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
makeMainWindow nsWindow =
  sendMessage nsWindow makeMainWindowSelector

-- | Informs the window that it has become the key window. This method exists as an override point. Do not invoke directly. Instead, invoke @-makeKeyWindow@.
--
-- ObjC selector: @- becomeKeyWindow@
becomeKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
becomeKeyWindow nsWindow =
  sendMessage nsWindow becomeKeyWindowSelector

-- | Informs the window that it has stopped being the key window. This method exists as an override point. Do not invoke directly. Windows automatically receive this message when deactivating or when another window has become key.
--
-- ObjC selector: @- resignKeyWindow@
resignKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
resignKeyWindow nsWindow =
  sendMessage nsWindow resignKeyWindowSelector

-- | Informs the window that it has become the main window. This method exists as an override point. Do not invoke directly. Instead, invoke @-makeMainWindow@.
--
-- ObjC selector: @- becomeMainWindow@
becomeMainWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
becomeMainWindow nsWindow =
  sendMessage nsWindow becomeMainWindowSelector

-- | Informs the window that it has stopped being the main window. This method exists as an override point. Do not invoke directly. Windows automatically receive this message when deactivating or when another window has become main.
--
-- ObjC selector: @- resignMainWindow@
resignMainWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
resignMainWindow nsWindow =
  sendMessage nsWindow resignMainWindowSelector

-- | @- convertRectToScreen:@
convertRectToScreen :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectToScreen nsWindow rect =
  sendMessage nsWindow convertRectToScreenSelector rect

-- | @- convertRectFromScreen:@
convertRectFromScreen :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectFromScreen nsWindow rect =
  sendMessage nsWindow convertRectFromScreenSelector rect

-- | @- convertPointToScreen:@
convertPointToScreen :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointToScreen nsWindow point =
  sendMessage nsWindow convertPointToScreenSelector point

-- | @- convertPointFromScreen:@
convertPointFromScreen :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointFromScreen nsWindow point =
  sendMessage nsWindow convertPointFromScreenSelector point

-- | @- convertRectToBacking:@
convertRectToBacking :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectToBacking nsWindow rect =
  sendMessage nsWindow convertRectToBackingSelector rect

-- | @- convertRectFromBacking:@
convertRectFromBacking :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectFromBacking nsWindow rect =
  sendMessage nsWindow convertRectFromBackingSelector rect

-- | @- convertPointToBacking:@
convertPointToBacking :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointToBacking nsWindow point =
  sendMessage nsWindow convertPointToBackingSelector point

-- | @- convertPointFromBacking:@
convertPointFromBacking :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointFromBacking nsWindow point =
  sendMessage nsWindow convertPointFromBackingSelector point

-- | Use @NSIntegralRectWithOptions()@ to produce a backing store pixel aligned rectangle from the given input rectangle in window coordinates.
--
-- ObjC selector: @- backingAlignedRect:options:@
backingAlignedRect_options :: IsNSWindow nsWindow => nsWindow -> NSRect -> NSAlignmentOptions -> IO NSRect
backingAlignedRect_options nsWindow rect options =
  sendMessage nsWindow backingAlignedRect_optionsSelector rect options

-- | @- performClose:@
performClose :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
performClose nsWindow sender =
  sendMessage nsWindow performCloseSelector sender

-- | @- performMiniaturize:@
performMiniaturize :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
performMiniaturize nsWindow sender =
  sendMessage nsWindow performMiniaturizeSelector sender

-- | @- performZoom:@
performZoom :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
performZoom nsWindow sender =
  sendMessage nsWindow performZoomSelector sender

-- | @- dataWithEPSInsideRect:@
dataWithEPSInsideRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO (Id NSData)
dataWithEPSInsideRect nsWindow rect =
  sendMessage nsWindow dataWithEPSInsideRectSelector rect

-- | @- dataWithPDFInsideRect:@
dataWithPDFInsideRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO (Id NSData)
dataWithPDFInsideRect nsWindow rect =
  sendMessage nsWindow dataWithPDFInsideRectSelector rect

-- | @- print:@
print_ :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
print_ nsWindow sender =
  sendMessage nsWindow printSelector sender

-- | @- setDynamicDepthLimit:@
setDynamicDepthLimit :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setDynamicDepthLimit nsWindow flag =
  sendMessage nsWindow setDynamicDepthLimitSelector flag

-- | @- invalidateShadow@
invalidateShadow :: IsNSWindow nsWindow => nsWindow -> IO ()
invalidateShadow nsWindow =
  sendMessage nsWindow invalidateShadowSelector

-- | @-toggleFullScreen:@ enters or exits for full screen. A window must have @NSWindowCollectionBehaviorFullScreenAuxiliary@ or @NSWindowCollectionBehaviorFullScreenPrimary@ included in the @collectionBehavior@ property; if it does not, this method may simply do nothing.
--
-- ObjC selector: @- toggleFullScreen:@
toggleFullScreen :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleFullScreen nsWindow sender =
  sendMessage nsWindow toggleFullScreenSelector sender

-- | @- setFrameFromString:@
setFrameFromString :: (IsNSWindow nsWindow, IsNSString string) => nsWindow -> string -> IO ()
setFrameFromString nsWindow string =
  sendMessage nsWindow setFrameFromStringSelector (toNSString string)

-- | @- saveFrameUsingName:@
saveFrameUsingName :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> IO ()
saveFrameUsingName nsWindow name =
  sendMessage nsWindow saveFrameUsingNameSelector (toNSString name)

-- | @- setFrameUsingName:force:@
setFrameUsingName_force :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> Bool -> IO Bool
setFrameUsingName_force nsWindow name force =
  sendMessage nsWindow setFrameUsingName_forceSelector (toNSString name) force

-- | @- setFrameUsingName:@
setFrameUsingName :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> IO Bool
setFrameUsingName nsWindow name =
  sendMessage nsWindow setFrameUsingNameSelector (toNSString name)

-- | @- setFrameAutosaveName:@
setFrameAutosaveName :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> IO Bool
setFrameAutosaveName nsWindow name =
  sendMessage nsWindow setFrameAutosaveNameSelector (toNSString name)

-- | @+ removeFrameUsingName:@
removeFrameUsingName :: IsNSString name => name -> IO ()
removeFrameUsingName name =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' removeFrameUsingNameSelector (toNSString name)

-- | @- beginSheet:completionHandler:@
beginSheet_completionHandler :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> Ptr () -> IO ()
beginSheet_completionHandler nsWindow sheetWindow handler =
  sendMessage nsWindow beginSheet_completionHandlerSelector (toNSWindow sheetWindow) handler

-- | @- beginCriticalSheet:completionHandler:@
beginCriticalSheet_completionHandler :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> Ptr () -> IO ()
beginCriticalSheet_completionHandler nsWindow sheetWindow handler =
  sendMessage nsWindow beginCriticalSheet_completionHandlerSelector (toNSWindow sheetWindow) handler

-- | @- endSheet:@
endSheet :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> IO ()
endSheet nsWindow sheetWindow =
  sendMessage nsWindow endSheetSelector (toNSWindow sheetWindow)

-- | @- endSheet:returnCode:@
endSheet_returnCode :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> CLong -> IO ()
endSheet_returnCode nsWindow sheetWindow returnCode =
  sendMessage nsWindow endSheet_returnCodeSelector (toNSWindow sheetWindow) returnCode

-- | @+ standardWindowButton:forStyleMask:@
standardWindowButton_forStyleMask :: NSWindowButton -> NSWindowStyleMask -> IO (Id NSButton)
standardWindowButton_forStyleMask b styleMask =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' standardWindowButton_forStyleMaskSelector b styleMask

-- | @- standardWindowButton:@
standardWindowButton :: IsNSWindow nsWindow => nsWindow -> NSWindowButton -> IO (Id NSButton)
standardWindowButton nsWindow b =
  sendMessage nsWindow standardWindowButtonSelector b

-- | @- addChildWindow:ordered:@
addChildWindow_ordered :: (IsNSWindow nsWindow, IsNSWindow childWin) => nsWindow -> childWin -> NSWindowOrderingMode -> IO ()
addChildWindow_ordered nsWindow childWin place =
  sendMessage nsWindow addChildWindow_orderedSelector (toNSWindow childWin) place

-- | @- removeChildWindow:@
removeChildWindow :: (IsNSWindow nsWindow, IsNSWindow childWin) => nsWindow -> childWin -> IO ()
removeChildWindow nsWindow childWin =
  sendMessage nsWindow removeChildWindowSelector (toNSWindow childWin)

-- | @-canRepresentDisplayGamut:@ returns @YES@ if the colorSpace of the receiving window, and the @colorSpace@ of the screen containing that window, are capable of representing the given display gamut
--
-- ObjC selector: @- canRepresentDisplayGamut:@
canRepresentDisplayGamut :: IsNSWindow nsWindow => nsWindow -> NSDisplayGamut -> IO Bool
canRepresentDisplayGamut nsWindow displayGamut =
  sendMessage nsWindow canRepresentDisplayGamutSelector displayGamut

-- | @+windowNumbersWithOptions:@ returns an autoreleased array of @NSNumbers@ containing windowNumbers for all visible windows satisfying options.  If no options are specified, only visible windows belonging to the calling application and on the active space are included.  If options include @NSWindowNumberListAllApplications,@ visible windows belonging to all applications are included.  If options include @NSWindowNumberListAllSpaces,@ visible windows on all spaces are included.  Windows on the active space are returned in z-order.   Examples:       To get an array of windowNumbers visible on the current space and belonging to the calling application:  	@windowNumbers = [NSWindow windowNumbersWithOptions:0];@      To get an array of windowNumbers visible on any space and belonging to any application:	@windowNumbers = [NSWindow windowNumbersWithOptions:NSWindowNumberListAllApplications|NSWindowNumberListAllSpaces];@      To get an array of windowNumbers visible on any space and belonging to the calling application:	@windowNumbers = [NSWindow windowNumbersWithOptions:NSWindowNumberListAllSpaces];@
--
-- ObjC selector: @+ windowNumbersWithOptions:@
windowNumbersWithOptions :: NSWindowNumberListOptions -> IO (Id NSArray)
windowNumbersWithOptions options =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' windowNumbersWithOptionsSelector options

-- | @+windowNumberAtPoint:belowWindowWithWindowNumber:@ returns the number of the frontmost window that would be hit by a mouseDown at the screen location "point".  "windowNum" can be specified to exclude a given window along with all windows above it, and may belong to any application.  If no windows are to be excluded, specify 0 for "windowNum".  The windowNumber returned may correspond to a window in another application.
--
-- ObjC selector: @+ windowNumberAtPoint:belowWindowWithWindowNumber:@
windowNumberAtPoint_belowWindowWithWindowNumber :: NSPoint -> CLong -> IO CLong
windowNumberAtPoint_belowWindowWithWindowNumber point windowNumber =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' windowNumberAtPoint_belowWindowWithWindowNumberSelector point windowNumber

-- | Convenience method for creating an autoreleased titled window with the given contentViewController. A basic NSWindow with the following attributes is made: titled, closable, resizable, miniaturizable. The window's title is automatically bound to the contentViewController's title. The size of the window can easily be controlled by utilizing autolayout and applying size constraints to the view (or its subviews). The window has isReleasedWhenClosed set to NO, and it must be explicitly retained to keep the window instance alive. To have it automatically be freed when it is closed, do the following: [window retain] and [window setReleasedWhenClosed:YES].
--
-- ObjC selector: @+ windowWithContentViewController:@
windowWithContentViewController :: IsNSViewController contentViewController => contentViewController -> IO (Id NSWindow)
windowWithContentViewController contentViewController =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' windowWithContentViewControllerSelector (toNSViewController contentViewController)

-- | Call to start a drag (moving the window) in the Window Server process. In general, this can be done after a mouseDown event has come in and been examined by an application or view. The view may determine it wants to allow that portion of the window to start a window drag, and can hand off the work to the Window Server process by calling this method. This allows the window to participate in space switching, and other system features. Pass the original mouseDown event to the method. The method will return right away, and a mouseUp may not get sent.
--
-- ObjC selector: @- performWindowDragWithEvent:@
performWindowDragWithEvent :: (IsNSWindow nsWindow, IsNSEvent event) => nsWindow -> event -> IO ()
performWindowDragWithEvent nsWindow event =
  sendMessage nsWindow performWindowDragWithEventSelector (toNSEvent event)

-- | @- selectNextKeyView:@
selectNextKeyView :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectNextKeyView nsWindow sender =
  sendMessage nsWindow selectNextKeyViewSelector sender

-- | @- selectPreviousKeyView:@
selectPreviousKeyView :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectPreviousKeyView nsWindow sender =
  sendMessage nsWindow selectPreviousKeyViewSelector sender

-- | @- selectKeyViewFollowingView:@
selectKeyViewFollowingView :: (IsNSWindow nsWindow, IsNSView view) => nsWindow -> view -> IO ()
selectKeyViewFollowingView nsWindow view =
  sendMessage nsWindow selectKeyViewFollowingViewSelector (toNSView view)

-- | @- selectKeyViewPrecedingView:@
selectKeyViewPrecedingView :: (IsNSWindow nsWindow, IsNSView view) => nsWindow -> view -> IO ()
selectKeyViewPrecedingView nsWindow view =
  sendMessage nsWindow selectKeyViewPrecedingViewSelector (toNSView view)

-- | @- disableKeyEquivalentForDefaultButtonCell@
disableKeyEquivalentForDefaultButtonCell :: IsNSWindow nsWindow => nsWindow -> IO ()
disableKeyEquivalentForDefaultButtonCell nsWindow =
  sendMessage nsWindow disableKeyEquivalentForDefaultButtonCellSelector

-- | @- enableKeyEquivalentForDefaultButtonCell@
enableKeyEquivalentForDefaultButtonCell :: IsNSWindow nsWindow => nsWindow -> IO ()
enableKeyEquivalentForDefaultButtonCell nsWindow =
  sendMessage nsWindow enableKeyEquivalentForDefaultButtonCellSelector

-- | @- recalculateKeyViewLoop@
recalculateKeyViewLoop :: IsNSWindow nsWindow => nsWindow -> IO ()
recalculateKeyViewLoop nsWindow =
  sendMessage nsWindow recalculateKeyViewLoopSelector

-- | @- toggleToolbarShown:@
toggleToolbarShown :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleToolbarShown nsWindow sender =
  sendMessage nsWindow toggleToolbarShownSelector sender

-- | @- runToolbarCustomizationPalette:@
runToolbarCustomizationPalette :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
runToolbarCustomizationPalette nsWindow sender =
  sendMessage nsWindow runToolbarCustomizationPaletteSelector sender

-- | Actions that can be called to perform various tabbed window behaviors. UI that is hooked up to these items can be automatically validated by calling @NSWindow@'s @validateUserInterfaceItem.@
--
-- ObjC selector: @- selectNextTab:@
selectNextTab :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectNextTab nsWindow sender =
  sendMessage nsWindow selectNextTabSelector sender

-- | @- selectPreviousTab:@
selectPreviousTab :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectPreviousTab nsWindow sender =
  sendMessage nsWindow selectPreviousTabSelector sender

-- | @- moveTabToNewWindow:@
moveTabToNewWindow :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
moveTabToNewWindow nsWindow sender =
  sendMessage nsWindow moveTabToNewWindowSelector sender

-- | @- mergeAllWindows:@
mergeAllWindows :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
mergeAllWindows nsWindow sender =
  sendMessage nsWindow mergeAllWindowsSelector sender

-- | @- toggleTabBar:@
toggleTabBar :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleTabBar nsWindow sender =
  sendMessage nsWindow toggleTabBarSelector sender

-- | Toggle the Tab Picker / Tab Overview UI which is invoked via "Show All Tabs". Performs the toggle in an animated fashion. Use @tabGroup.isOverviewVisible@ to find out if it is visible or not at a given time.
--
-- ObjC selector: @- toggleTabOverview:@
toggleTabOverview :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleTabOverview nsWindow sender =
  sendMessage nsWindow toggleTabOverviewSelector sender

-- | This is now a cover for @-[self.tabGroup addWindow:]@, which allows more precise placement.
--
-- ObjC selector: @- addTabbedWindow:ordered:@
addTabbedWindow_ordered :: (IsNSWindow nsWindow, IsNSWindow window) => nsWindow -> window -> NSWindowOrderingMode -> IO ()
addTabbedWindow_ordered nsWindow window ordered =
  sendMessage nsWindow addTabbedWindow_orderedSelector (toNSWindow window) ordered

-- | Attempt to move window sharing (i.e. within a SharePlay session) from the receiver to another window. In response to this request, the user may choose to transfer sharing to the new window, or simply stop sharing the content.
--
-- @window@ — A window that is replacing the reciever in representing the user's current activity.
--
-- @completionHandler@ — A completion block that is called after the request finishes.        @error@            In the event of a failed transfer request, a non-nil error contains details about the failure.
--
-- ObjC selector: @- transferWindowSharingToWindow:completionHandler:@
transferWindowSharingToWindow_completionHandler :: (IsNSWindow nsWindow, IsNSWindow window) => nsWindow -> window -> Ptr () -> IO ()
transferWindowSharingToWindow_completionHandler nsWindow window completionHandler =
  sendMessage nsWindow transferWindowSharingToWindow_completionHandlerSelector (toNSWindow window) completionHandler

-- | Request sharing of window.  If there is an available ScreenCaptureKit sharing session, an alert will be presented asking the user to confirm the share
--
-- @window@ — The window to share
--
-- @completionHandler@ — A completion block that is called after the request finishes. @error@ The error will be non-nil if the request does not result in a window being shared.  The error will be NSUserCancelledError if there is no ScreenCaptureKit session, or if the user rejects the offer to share.  If sharing fails for some other reason, the error will provide the details.
--
-- ObjC selector: @- requestSharingOfWindow:completionHandler:@
requestSharingOfWindow_completionHandler :: (IsNSWindow nsWindow, IsNSWindow window) => nsWindow -> window -> Ptr () -> IO ()
requestSharingOfWindow_completionHandler nsWindow window completionHandler =
  sendMessage nsWindow requestSharingOfWindow_completionHandlerSelector (toNSWindow window) completionHandler

-- | Request sharing of window to be provided later.  If there is an available ScreenCaptureKit sharing session, an alert will be presented asking the user to confirm the share.  The delegate will be asked to provide the window to share via windowForSharingRequestFromWindow:
--
-- @image@ — An image showing a preview of the window to share
--
-- @title@ — The title to show in a confirmation dialog
--
-- @completionHandler@ — A completion block that is called after the request finishes. @error@ The error will be non-nil if the request does not result in a window being shared.  The error will be NSUserCancelledError if there is no ScreenCaptureKit session, or if the user rejects the offer to share.  If sharing fails for some other reason, the error will provide the details.
--
-- ObjC selector: @- requestSharingOfWindowUsingPreview:title:completionHandler:@
requestSharingOfWindowUsingPreview_title_completionHandler :: (IsNSWindow nsWindow, IsNSImage image, IsNSString title) => nsWindow -> image -> title -> Ptr () -> IO ()
requestSharingOfWindowUsingPreview_title_completionHandler nsWindow image title completionHandler =
  sendMessage nsWindow requestSharingOfWindowUsingPreview_title_completionHandlerSelector (toNSImage image) (toNSString title) completionHandler

-- | @- disableSnapshotRestoration@
disableSnapshotRestoration :: IsNSWindow nsWindow => nsWindow -> IO ()
disableSnapshotRestoration nsWindow =
  sendMessage nsWindow disableSnapshotRestorationSelector

-- | @- enableSnapshotRestoration@
enableSnapshotRestoration :: IsNSWindow nsWindow => nsWindow -> IO ()
enableSnapshotRestoration nsWindow =
  sendMessage nsWindow enableSnapshotRestorationSelector

-- | @- setIsMiniaturized:@
setIsMiniaturized :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIsMiniaturized nsWindow flag =
  sendMessage nsWindow setIsMiniaturizedSelector flag

-- | @- setIsVisible:@
setIsVisible :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIsVisible nsWindow flag =
  sendMessage nsWindow setIsVisibleSelector flag

-- | @- setIsZoomed:@
setIsZoomed :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIsZoomed nsWindow flag =
  sendMessage nsWindow setIsZoomedSelector flag

-- | @- handleCloseScriptCommand:@
handleCloseScriptCommand :: (IsNSWindow nsWindow, IsNSCloseCommand command) => nsWindow -> command -> IO RawId
handleCloseScriptCommand nsWindow command =
  sendMessage nsWindow handleCloseScriptCommandSelector (toNSCloseCommand command)

-- | @- handlePrintScriptCommand:@
handlePrintScriptCommand :: (IsNSWindow nsWindow, IsNSScriptCommand command) => nsWindow -> command -> IO RawId
handlePrintScriptCommand nsWindow command =
  sendMessage nsWindow handlePrintScriptCommandSelector (toNSScriptCommand command)

-- | @- handleSaveScriptCommand:@
handleSaveScriptCommand :: (IsNSWindow nsWindow, IsNSScriptCommand command) => nsWindow -> command -> IO RawId
handleSaveScriptCommand nsWindow command =
  sendMessage nsWindow handleSaveScriptCommandSelector (toNSScriptCommand command)

-- | @- visualizeConstraints:@
visualizeConstraints :: (IsNSWindow nsWindow, IsNSArray constraints) => nsWindow -> constraints -> IO ()
visualizeConstraints nsWindow constraints =
  sendMessage nsWindow visualizeConstraintsSelector (toNSArray constraints)

-- | @- anchorAttributeForOrientation:@
anchorAttributeForOrientation :: IsNSWindow nsWindow => nsWindow -> NSLayoutConstraintOrientation -> IO NSLayoutAttribute
anchorAttributeForOrientation nsWindow orientation =
  sendMessage nsWindow anchorAttributeForOrientationSelector orientation

-- | @- setAnchorAttribute:forOrientation:@
setAnchorAttribute_forOrientation :: IsNSWindow nsWindow => nsWindow -> NSLayoutAttribute -> NSLayoutConstraintOrientation -> IO ()
setAnchorAttribute_forOrientation nsWindow attr orientation =
  sendMessage nsWindow setAnchorAttribute_forOrientationSelector attr orientation

-- | @- updateConstraintsIfNeeded@
updateConstraintsIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
updateConstraintsIfNeeded nsWindow =
  sendMessage nsWindow updateConstraintsIfNeededSelector

-- | @- layoutIfNeeded@
layoutIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
layoutIfNeeded nsWindow =
  sendMessage nsWindow layoutIfNeededSelector

-- | @- cacheImageInRect:@
cacheImageInRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO ()
cacheImageInRect nsWindow rect =
  sendMessage nsWindow cacheImageInRectSelector rect

-- | @- restoreCachedImage@
restoreCachedImage :: IsNSWindow nsWindow => nsWindow -> IO ()
restoreCachedImage nsWindow =
  sendMessage nsWindow restoreCachedImageSelector

-- | @- discardCachedImage@
discardCachedImage :: IsNSWindow nsWindow => nsWindow -> IO ()
discardCachedImage nsWindow =
  sendMessage nsWindow discardCachedImageSelector

-- | @+ menuChanged:@
menuChanged :: IsNSMenu menu => menu -> IO ()
menuChanged menu =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' menuChangedSelector (toNSMenu menu)

-- | @- gState@
gState :: IsNSWindow nsWindow => nsWindow -> IO CLong
gState nsWindow =
  sendMessage nsWindow gStateSelector

-- | @- convertBaseToScreen:@
convertBaseToScreen :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertBaseToScreen nsWindow point =
  sendMessage nsWindow convertBaseToScreenSelector point

-- | @- convertScreenToBase:@
convertScreenToBase :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertScreenToBase nsWindow point =
  sendMessage nsWindow convertScreenToBaseSelector point

-- | @- userSpaceScaleFactor@
userSpaceScaleFactor :: IsNSWindow nsWindow => nsWindow -> IO CDouble
userSpaceScaleFactor nsWindow =
  sendMessage nsWindow userSpaceScaleFactorSelector

-- | @- useOptimizedDrawing:@
useOptimizedDrawing :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
useOptimizedDrawing nsWindow flag =
  sendMessage nsWindow useOptimizedDrawingSelector flag

-- | @- canStoreColor@
canStoreColor :: IsNSWindow nsWindow => nsWindow -> IO Bool
canStoreColor nsWindow =
  sendMessage nsWindow canStoreColorSelector

-- | @- disableFlushWindow@
disableFlushWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
disableFlushWindow nsWindow =
  sendMessage nsWindow disableFlushWindowSelector

-- | @- enableFlushWindow@
enableFlushWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
enableFlushWindow nsWindow =
  sendMessage nsWindow enableFlushWindowSelector

-- | @- flushWindow@
flushWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
flushWindow nsWindow =
  sendMessage nsWindow flushWindowSelector

-- | @- flushWindowIfNeeded@
flushWindowIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
flushWindowIfNeeded nsWindow =
  sendMessage nsWindow flushWindowIfNeededSelector

-- | @- initWithWindowRef:@
initWithWindowRef :: IsNSWindow nsWindow => nsWindow -> Ptr () -> IO (Id NSWindow)
initWithWindowRef nsWindow windowRef =
  sendOwnedMessage nsWindow initWithWindowRefSelector windowRef

-- | @- disableScreenUpdatesUntilFlush@
disableScreenUpdatesUntilFlush :: IsNSWindow nsWindow => nsWindow -> IO ()
disableScreenUpdatesUntilFlush nsWindow =
  sendMessage nsWindow disableScreenUpdatesUntilFlushSelector

-- | @- displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: IsNSWindow nsWindow => nsWindow -> RawId -> Sel -> IO (Id CADisplayLink)
displayLinkWithTarget_selector nsWindow target selector =
  sendMessage nsWindow displayLinkWithTarget_selectorSelector target selector

-- | @- beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_source :: (IsNSWindow nsWindow, IsNSArray items, IsNSEvent event) => nsWindow -> items -> event -> RawId -> IO (Id NSDraggingSession)
beginDraggingSessionWithItems_event_source nsWindow items event source =
  sendMessage nsWindow beginDraggingSessionWithItems_event_sourceSelector (toNSArray items) (toNSEvent event) source

-- | @- dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBack :: (IsNSWindow nsWindow, IsNSImage image, IsNSEvent event, IsNSPasteboard pboard) => nsWindow -> image -> NSPoint -> NSSize -> event -> pboard -> RawId -> Bool -> IO ()
dragImage_at_offset_event_pasteboard_source_slideBack nsWindow image baseLocation initialOffset event pboard sourceObj slideFlag =
  sendMessage nsWindow dragImage_at_offset_event_pasteboard_source_slideBackSelector (toNSImage image) baseLocation initialOffset (toNSEvent event) (toNSPasteboard pboard) sourceObj slideFlag

-- | @- registerForDraggedTypes:@
registerForDraggedTypes :: (IsNSWindow nsWindow, IsNSArray newTypes) => nsWindow -> newTypes -> IO ()
registerForDraggedTypes nsWindow newTypes =
  sendMessage nsWindow registerForDraggedTypesSelector (toNSArray newTypes)

-- | @- unregisterDraggedTypes@
unregisterDraggedTypes :: IsNSWindow nsWindow => nsWindow -> IO ()
unregisterDraggedTypes nsWindow =
  sendMessage nsWindow unregisterDraggedTypesSelector

-- | @- disableCursorRects@
disableCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
disableCursorRects nsWindow =
  sendMessage nsWindow disableCursorRectsSelector

-- | @- enableCursorRects@
enableCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
enableCursorRects nsWindow =
  sendMessage nsWindow enableCursorRectsSelector

-- | @- discardCursorRects@
discardCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
discardCursorRects nsWindow =
  sendMessage nsWindow discardCursorRectsSelector

-- | @- invalidateCursorRectsForView:@
invalidateCursorRectsForView :: (IsNSWindow nsWindow, IsNSView view) => nsWindow -> view -> IO ()
invalidateCursorRectsForView nsWindow view =
  sendMessage nsWindow invalidateCursorRectsForViewSelector (toNSView view)

-- | @- resetCursorRects@
resetCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
resetCursorRects nsWindow =
  sendMessage nsWindow resetCursorRectsSelector

-- | Tracks events matching the supplied mask with the supplied tracking handler until the tracking handler explicitly terminates tracking. Each event is removed from the event queue then passed to the tracking handler. If a matching event does not exist in the event queue, then the main thread blocks in the specified runloop mode until an event of the requested type is received or the timeout expires. If the timeout expires, the tracking handler is called with a nil event. A negative timeout is interpreted as 0. Use @NSEventDurationForever@ to never timeout. Tracking continues until @*stop@ is set to @YES.@ Calls to @-nextEventMatchingMask:…@ are allowed inside the trackingHandler block. This method returns once tracking is terminated.
--
-- ObjC selector: @- trackEventsMatchingMask:timeout:mode:handler:@
trackEventsMatchingMask_timeout_mode_handler :: (IsNSWindow nsWindow, IsNSString mode) => nsWindow -> NSEventMask -> CDouble -> mode -> Ptr () -> IO ()
trackEventsMatchingMask_timeout_mode_handler nsWindow mask timeout mode trackingHandler =
  sendMessage nsWindow trackEventsMatchingMask_timeout_mode_handlerSelector mask timeout (toNSString mode) trackingHandler

-- | @- nextEventMatchingMask:@
nextEventMatchingMask :: IsNSWindow nsWindow => nsWindow -> NSEventMask -> IO (Id NSEvent)
nextEventMatchingMask nsWindow mask =
  sendMessage nsWindow nextEventMatchingMaskSelector mask

-- | @- nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeue :: (IsNSWindow nsWindow, IsNSDate expiration, IsNSString mode) => nsWindow -> NSEventMask -> expiration -> mode -> Bool -> IO (Id NSEvent)
nextEventMatchingMask_untilDate_inMode_dequeue nsWindow mask expiration mode deqFlag =
  sendMessage nsWindow nextEventMatchingMask_untilDate_inMode_dequeueSelector mask (toNSDate expiration) (toNSString mode) deqFlag

-- | @- discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEvent :: (IsNSWindow nsWindow, IsNSEvent lastEvent) => nsWindow -> NSEventMask -> lastEvent -> IO ()
discardEventsMatchingMask_beforeEvent nsWindow mask lastEvent =
  sendMessage nsWindow discardEventsMatchingMask_beforeEventSelector mask (toNSEvent lastEvent)

-- | @- postEvent:atStart:@
postEvent_atStart :: (IsNSWindow nsWindow, IsNSEvent event) => nsWindow -> event -> Bool -> IO ()
postEvent_atStart nsWindow event flag =
  sendMessage nsWindow postEvent_atStartSelector (toNSEvent event) flag

-- | @- sendEvent:@
sendEvent :: (IsNSWindow nsWindow, IsNSEvent event) => nsWindow -> event -> IO ()
sendEvent nsWindow event =
  sendMessage nsWindow sendEventSelector (toNSEvent event)

-- | @+ defaultDepthLimit@
defaultDepthLimit :: IO NSWindowDepth
defaultDepthLimit  =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' defaultDepthLimitSelector

-- | @- title@
title :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
title nsWindow =
  sendMessage nsWindow titleSelector

-- | @- setTitle:@
setTitle :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setTitle nsWindow value =
  sendMessage nsWindow setTitleSelector (toNSString value)

-- | Secondary text that may be displayed adjacent to or below the primary title depending on the configuration of the window. A value of empty string will remove the subtitle from the window layout.
--
-- ObjC selector: @- subtitle@
subtitle :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
subtitle nsWindow =
  sendMessage nsWindow subtitleSelector

-- | Secondary text that may be displayed adjacent to or below the primary title depending on the configuration of the window. A value of empty string will remove the subtitle from the window layout.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setSubtitle nsWindow value =
  sendMessage nsWindow setSubtitleSelector (toNSString value)

-- | See the enum values for how this property works.
--
-- ObjC selector: @- titleVisibility@
titleVisibility :: IsNSWindow nsWindow => nsWindow -> IO NSWindowTitleVisibility
titleVisibility nsWindow =
  sendMessage nsWindow titleVisibilitySelector

-- | See the enum values for how this property works.
--
-- ObjC selector: @- setTitleVisibility:@
setTitleVisibility :: IsNSWindow nsWindow => nsWindow -> NSWindowTitleVisibility -> IO ()
setTitleVisibility nsWindow value =
  sendMessage nsWindow setTitleVisibilitySelector value

-- | When @YES,@ the titlebar doesn't draw its background, allowing all buttons to show through, and "click through" to happen. In general, this is only useful when @NSFullSizeContentViewWindowMask@ is set.
--
-- ObjC selector: @- titlebarAppearsTransparent@
titlebarAppearsTransparent :: IsNSWindow nsWindow => nsWindow -> IO Bool
titlebarAppearsTransparent nsWindow =
  sendMessage nsWindow titlebarAppearsTransparentSelector

-- | When @YES,@ the titlebar doesn't draw its background, allowing all buttons to show through, and "click through" to happen. In general, this is only useful when @NSFullSizeContentViewWindowMask@ is set.
--
-- ObjC selector: @- setTitlebarAppearsTransparent:@
setTitlebarAppearsTransparent :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setTitlebarAppearsTransparent nsWindow value =
  sendMessage nsWindow setTitlebarAppearsTransparentSelector value

-- | Specifies how the titlebar area of the window should appear when the window displays an NSToolbar
--
-- ObjC selector: @- toolbarStyle@
toolbarStyle :: IsNSWindow nsWindow => nsWindow -> IO NSWindowToolbarStyle
toolbarStyle nsWindow =
  sendMessage nsWindow toolbarStyleSelector

-- | Specifies how the titlebar area of the window should appear when the window displays an NSToolbar
--
-- ObjC selector: @- setToolbarStyle:@
setToolbarStyle :: IsNSWindow nsWindow => nsWindow -> NSWindowToolbarStyle -> IO ()
setToolbarStyle nsWindow value =
  sendMessage nsWindow setToolbarStyleSelector value

-- | The @contentLayoutRect@ will return the area inside the window that is for non-obscured content. Typically, this is the same thing as the @contentView@'s frame. However, for windows with the @NSFullSizeContentViewWindowMask@ set, there needs to be a way to determine the portion that is not under the toolbar. The @contentLayoutRect@ returns the portion of the layout that is not obscured under the toolbar. @contentLayoutRect@ is in window coordinates. It is KVO compliant. */
--
-- ObjC selector: @- contentLayoutRect@
contentLayoutRect :: IsNSWindow nsWindow => nsWindow -> IO NSRect
contentLayoutRect nsWindow =
  sendMessage nsWindow contentLayoutRectSelector

-- | @contentLayoutGuide@ is a corollary to @contentLayoutRect.@ It can be used by autolayout constraints to automatically bind to the @contentLayoutRect.@
--
-- ObjC selector: @- contentLayoutGuide@
contentLayoutGuide :: IsNSWindow nsWindow => nsWindow -> IO RawId
contentLayoutGuide nsWindow =
  sendMessage nsWindow contentLayoutGuideSelector

-- | @- titlebarAccessoryViewControllers@
titlebarAccessoryViewControllers :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
titlebarAccessoryViewControllers nsWindow =
  sendMessage nsWindow titlebarAccessoryViewControllersSelector

-- | @- setTitlebarAccessoryViewControllers:@
setTitlebarAccessoryViewControllers :: (IsNSWindow nsWindow, IsNSArray value) => nsWindow -> value -> IO ()
setTitlebarAccessoryViewControllers nsWindow value =
  sendMessage nsWindow setTitlebarAccessoryViewControllersSelector (toNSArray value)

-- | If url is not nil and its path is not empty, the window will show a document icon in the titlebar. If the url represents a filename or other resource with a known icon, that icon will be used as the document icon.  Otherwise the default document icon will be used.  The icon can be customized using @-[[NSWindow standardWindowButton:NSWindowDocumentIconButton] setImage:customImage]@.  If url is not nil and its path is not empty, the window will have a pop-up menu which can be shown via command-click on the area containing the document icon and title.  By default, this menu will display the path components of the url.  The presence and contents of this menu can be controlled by the delegate method @-[window:shouldPopUpDocumentPathMenu:]@ If the url is nil or has an empty path, the window will not show a document icon and will not have a pop-up menu available via command-click.
--
-- ObjC selector: @- representedURL@
representedURL :: IsNSWindow nsWindow => nsWindow -> IO (Id NSURL)
representedURL nsWindow =
  sendMessage nsWindow representedURLSelector

-- | If url is not nil and its path is not empty, the window will show a document icon in the titlebar. If the url represents a filename or other resource with a known icon, that icon will be used as the document icon.  Otherwise the default document icon will be used.  The icon can be customized using @-[[NSWindow standardWindowButton:NSWindowDocumentIconButton] setImage:customImage]@.  If url is not nil and its path is not empty, the window will have a pop-up menu which can be shown via command-click on the area containing the document icon and title.  By default, this menu will display the path components of the url.  The presence and contents of this menu can be controlled by the delegate method @-[window:shouldPopUpDocumentPathMenu:]@ If the url is nil or has an empty path, the window will not show a document icon and will not have a pop-up menu available via command-click.
--
-- ObjC selector: @- setRepresentedURL:@
setRepresentedURL :: (IsNSWindow nsWindow, IsNSURL value) => nsWindow -> value -> IO ()
setRepresentedURL nsWindow value =
  sendMessage nsWindow setRepresentedURLSelector (toNSURL value)

-- | @- representedFilename@
representedFilename :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
representedFilename nsWindow =
  sendMessage nsWindow representedFilenameSelector

-- | @- setRepresentedFilename:@
setRepresentedFilename :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setRepresentedFilename nsWindow value =
  sendMessage nsWindow setRepresentedFilenameSelector (toNSString value)

-- | @- excludedFromWindowsMenu@
excludedFromWindowsMenu :: IsNSWindow nsWindow => nsWindow -> IO Bool
excludedFromWindowsMenu nsWindow =
  sendMessage nsWindow excludedFromWindowsMenuSelector

-- | @- setExcludedFromWindowsMenu:@
setExcludedFromWindowsMenu :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setExcludedFromWindowsMenu nsWindow value =
  sendMessage nsWindow setExcludedFromWindowsMenuSelector value

-- | @- contentView@
contentView :: IsNSWindow nsWindow => nsWindow -> IO (Id NSView)
contentView nsWindow =
  sendMessage nsWindow contentViewSelector

-- | @- setContentView:@
setContentView :: (IsNSWindow nsWindow, IsNSView value) => nsWindow -> value -> IO ()
setContentView nsWindow value =
  sendMessage nsWindow setContentViewSelector (toNSView value)

-- | @- delegate@
delegate :: IsNSWindow nsWindow => nsWindow -> IO RawId
delegate nsWindow =
  sendMessage nsWindow delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
setDelegate nsWindow value =
  sendMessage nsWindow setDelegateSelector value

-- | @- windowNumber@
windowNumber :: IsNSWindow nsWindow => nsWindow -> IO CLong
windowNumber nsWindow =
  sendMessage nsWindow windowNumberSelector

-- | Note: The styleMask can only be set on macOS 10.6 and later. Valid @styleMask@ settings have the same restrictions as the @styleMask@ passed to @-initWithContentRect:styleMask:backing:defer:@.  Some @styleMask@ changes will cause the view hierarchy to be rebuilt, since there is a different subclass for the top level view of a borderless window than for the top level view of a titled window.
--
-- ObjC selector: @- styleMask@
styleMask :: IsNSWindow nsWindow => nsWindow -> IO NSWindowStyleMask
styleMask nsWindow =
  sendMessage nsWindow styleMaskSelector

-- | Note: The styleMask can only be set on macOS 10.6 and later. Valid @styleMask@ settings have the same restrictions as the @styleMask@ passed to @-initWithContentRect:styleMask:backing:defer:@.  Some @styleMask@ changes will cause the view hierarchy to be rebuilt, since there is a different subclass for the top level view of a borderless window than for the top level view of a titled window.
--
-- ObjC selector: @- setStyleMask:@
setStyleMask :: IsNSWindow nsWindow => nsWindow -> NSWindowStyleMask -> IO ()
setStyleMask nsWindow value =
  sendMessage nsWindow setStyleMaskSelector value

-- | The frame to use when cascading or sizing a new window based on the receiver's position or size. This may be different from @frame@ when the receiver is positioned by the system.
--
-- ObjC selector: @- cascadingReferenceFrame@
cascadingReferenceFrame :: IsNSWindow nsWindow => nsWindow -> IO NSRect
cascadingReferenceFrame nsWindow =
  sendMessage nsWindow cascadingReferenceFrameSelector

-- | @- frame@
frame :: IsNSWindow nsWindow => nsWindow -> IO NSRect
frame nsWindow =
  sendMessage nsWindow frameSelector

-- | @- inLiveResize@
inLiveResize :: IsNSWindow nsWindow => nsWindow -> IO Bool
inLiveResize nsWindow =
  sendMessage nsWindow inLiveResizeSelector

-- | @- resizeIncrements@
resizeIncrements :: IsNSWindow nsWindow => nsWindow -> IO NSSize
resizeIncrements nsWindow =
  sendMessage nsWindow resizeIncrementsSelector

-- | @- setResizeIncrements:@
setResizeIncrements :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setResizeIncrements nsWindow value =
  sendMessage nsWindow setResizeIncrementsSelector value

-- | @- aspectRatio@
aspectRatio :: IsNSWindow nsWindow => nsWindow -> IO NSSize
aspectRatio nsWindow =
  sendMessage nsWindow aspectRatioSelector

-- | @- setAspectRatio:@
setAspectRatio :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setAspectRatio nsWindow value =
  sendMessage nsWindow setAspectRatioSelector value

-- | @- contentResizeIncrements@
contentResizeIncrements :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentResizeIncrements nsWindow =
  sendMessage nsWindow contentResizeIncrementsSelector

-- | @- setContentResizeIncrements:@
setContentResizeIncrements :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentResizeIncrements nsWindow value =
  sendMessage nsWindow setContentResizeIncrementsSelector value

-- | @- contentAspectRatio@
contentAspectRatio :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentAspectRatio nsWindow =
  sendMessage nsWindow contentAspectRatioSelector

-- | @- setContentAspectRatio:@
setContentAspectRatio :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentAspectRatio nsWindow value =
  sendMessage nsWindow setContentAspectRatioSelector value

-- | @- viewsNeedDisplay@
viewsNeedDisplay :: IsNSWindow nsWindow => nsWindow -> IO Bool
viewsNeedDisplay nsWindow =
  sendMessage nsWindow viewsNeedDisplaySelector

-- | @- setViewsNeedDisplay:@
setViewsNeedDisplay :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setViewsNeedDisplay nsWindow value =
  sendMessage nsWindow setViewsNeedDisplaySelector value

-- | @- preservesContentDuringLiveResize@
preservesContentDuringLiveResize :: IsNSWindow nsWindow => nsWindow -> IO Bool
preservesContentDuringLiveResize nsWindow =
  sendMessage nsWindow preservesContentDuringLiveResizeSelector

-- | @- setPreservesContentDuringLiveResize:@
setPreservesContentDuringLiveResize :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setPreservesContentDuringLiveResize nsWindow value =
  sendMessage nsWindow setPreservesContentDuringLiveResizeSelector value

-- | @- firstResponder@
firstResponder :: IsNSWindow nsWindow => nsWindow -> IO (Id NSResponder)
firstResponder nsWindow =
  sendMessage nsWindow firstResponderSelector

-- | @- resizeFlags@
resizeFlags :: IsNSWindow nsWindow => nsWindow -> IO NSEventModifierFlags
resizeFlags nsWindow =
  sendMessage nsWindow resizeFlagsSelector

-- | @- releasedWhenClosed@
releasedWhenClosed :: IsNSWindow nsWindow => nsWindow -> IO Bool
releasedWhenClosed nsWindow =
  sendMessage nsWindow releasedWhenClosedSelector

-- | @- setReleasedWhenClosed:@
setReleasedWhenClosed :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setReleasedWhenClosed nsWindow value =
  sendMessage nsWindow setReleasedWhenClosedSelector value

-- | @- zoomed@
zoomed :: IsNSWindow nsWindow => nsWindow -> IO Bool
zoomed nsWindow =
  sendMessage nsWindow zoomedSelector

-- | @- miniaturized@
miniaturized :: IsNSWindow nsWindow => nsWindow -> IO Bool
miniaturized nsWindow =
  sendMessage nsWindow miniaturizedSelector

-- | @- backgroundColor@
backgroundColor :: IsNSWindow nsWindow => nsWindow -> IO (Id NSColor)
backgroundColor nsWindow =
  sendMessage nsWindow backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSWindow nsWindow, IsNSColor value) => nsWindow -> value -> IO ()
setBackgroundColor nsWindow value =
  sendMessage nsWindow setBackgroundColorSelector (toNSColor value)

-- | @- movable@
movable :: IsNSWindow nsWindow => nsWindow -> IO Bool
movable nsWindow =
  sendMessage nsWindow movableSelector

-- | @- setMovable:@
setMovable :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setMovable nsWindow value =
  sendMessage nsWindow setMovableSelector value

-- | @- movableByWindowBackground@
movableByWindowBackground :: IsNSWindow nsWindow => nsWindow -> IO Bool
movableByWindowBackground nsWindow =
  sendMessage nsWindow movableByWindowBackgroundSelector

-- | @- setMovableByWindowBackground:@
setMovableByWindowBackground :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setMovableByWindowBackground nsWindow value =
  sendMessage nsWindow setMovableByWindowBackgroundSelector value

-- | @- hidesOnDeactivate@
hidesOnDeactivate :: IsNSWindow nsWindow => nsWindow -> IO Bool
hidesOnDeactivate nsWindow =
  sendMessage nsWindow hidesOnDeactivateSelector

-- | @- setHidesOnDeactivate:@
setHidesOnDeactivate :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setHidesOnDeactivate nsWindow value =
  sendMessage nsWindow setHidesOnDeactivateSelector value

-- | Indicates whether a window can be hidden during @-[NSApplication hide:]@.  Default is @YES.@
--
-- ObjC selector: @- canHide@
canHide :: IsNSWindow nsWindow => nsWindow -> IO Bool
canHide nsWindow =
  sendMessage nsWindow canHideSelector

-- | Indicates whether a window can be hidden during @-[NSApplication hide:]@.  Default is @YES.@
--
-- ObjC selector: @- setCanHide:@
setCanHide :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setCanHide nsWindow value =
  sendMessage nsWindow setCanHideSelector value

-- | @- miniwindowImage@
miniwindowImage :: IsNSWindow nsWindow => nsWindow -> IO (Id NSImage)
miniwindowImage nsWindow =
  sendMessage nsWindow miniwindowImageSelector

-- | @- setMiniwindowImage:@
setMiniwindowImage :: (IsNSWindow nsWindow, IsNSImage value) => nsWindow -> value -> IO ()
setMiniwindowImage nsWindow value =
  sendMessage nsWindow setMiniwindowImageSelector (toNSImage value)

-- | @- miniwindowTitle@
miniwindowTitle :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
miniwindowTitle nsWindow =
  sendMessage nsWindow miniwindowTitleSelector

-- | @- setMiniwindowTitle:@
setMiniwindowTitle :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setMiniwindowTitle nsWindow value =
  sendMessage nsWindow setMiniwindowTitleSelector (toNSString value)

-- | @- dockTile@
dockTile :: IsNSWindow nsWindow => nsWindow -> IO (Id NSDockTile)
dockTile nsWindow =
  sendMessage nsWindow dockTileSelector

-- | @- documentEdited@
documentEdited :: IsNSWindow nsWindow => nsWindow -> IO Bool
documentEdited nsWindow =
  sendMessage nsWindow documentEditedSelector

-- | @- setDocumentEdited:@
setDocumentEdited :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setDocumentEdited nsWindow value =
  sendMessage nsWindow setDocumentEditedSelector value

-- | @- visible@
visible :: IsNSWindow nsWindow => nsWindow -> IO Bool
visible nsWindow =
  sendMessage nsWindow visibleSelector

-- | @- keyWindow@
keyWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
keyWindow nsWindow =
  sendMessage nsWindow keyWindowSelector

-- | @- mainWindow@
mainWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
mainWindow nsWindow =
  sendMessage nsWindow mainWindowSelector

-- | @- canBecomeKeyWindow@
canBecomeKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
canBecomeKeyWindow nsWindow =
  sendMessage nsWindow canBecomeKeyWindowSelector

-- | @- canBecomeMainWindow@
canBecomeMainWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
canBecomeMainWindow nsWindow =
  sendMessage nsWindow canBecomeMainWindowSelector

-- | @- worksWhenModal@
worksWhenModal :: IsNSWindow nsWindow => nsWindow -> IO Bool
worksWhenModal nsWindow =
  sendMessage nsWindow worksWhenModalSelector

-- | A Boolean value that indicates whether or not to prevent application termination when the receiving window is presented modally. The value of this property is @YES@ if the window should prevent application termination when modal; otherwise, @NO@. The default value is @YES@. However, note that some window subclasses and some windows created indirectly (like those created by UI frameworks like AppKit and SwiftUI), may have different default values. For example, the Open panel and toolbar customization sheets should not prevent application termination, so those windows have @preventsApplicationTerminationWhenModal@ set to @NO@. Some @NSAlert@s, like those that are simply informational, have windows that do not prevent application termination by default. Setting this property overrides the default behavior.
--
-- ObjC selector: @- preventsApplicationTerminationWhenModal@
preventsApplicationTerminationWhenModal :: IsNSWindow nsWindow => nsWindow -> IO Bool
preventsApplicationTerminationWhenModal nsWindow =
  sendMessage nsWindow preventsApplicationTerminationWhenModalSelector

-- | A Boolean value that indicates whether or not to prevent application termination when the receiving window is presented modally. The value of this property is @YES@ if the window should prevent application termination when modal; otherwise, @NO@. The default value is @YES@. However, note that some window subclasses and some windows created indirectly (like those created by UI frameworks like AppKit and SwiftUI), may have different default values. For example, the Open panel and toolbar customization sheets should not prevent application termination, so those windows have @preventsApplicationTerminationWhenModal@ set to @NO@. Some @NSAlert@s, like those that are simply informational, have windows that do not prevent application termination by default. Setting this property overrides the default behavior.
--
-- ObjC selector: @- setPreventsApplicationTerminationWhenModal:@
setPreventsApplicationTerminationWhenModal :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setPreventsApplicationTerminationWhenModal nsWindow value =
  sendMessage nsWindow setPreventsApplicationTerminationWhenModalSelector value

-- | Returns the scale factor representing the number of backing store pixels corresponding to each linear unit in window space on this @NSWindow.@ This method is provided for rare cases when the explicit scale factor is needed. Please use @-convert*ToBacking:@ methods whenever possible.
--
-- ObjC selector: @- backingScaleFactor@
backingScaleFactor :: IsNSWindow nsWindow => nsWindow -> IO CDouble
backingScaleFactor nsWindow =
  sendMessage nsWindow backingScaleFactorSelector

-- | Default is @NO.@ Set to @YES@ to allow a window to display tooltips even when the application is in the background.  Note that, enabling tooltips in an inactive application will cause the app to do work any time the mouse passes over the window.  This can degrade system performance. Returns @YES@ if this window displays tooltips even when the application is in the background.  To configure this setting you should call @-setAllowsToolTipsWhenApplicationIsInactive:@ instead of overriding @-allowsToolTipsWhenApplicationIsInactive@.
--
-- ObjC selector: @- allowsToolTipsWhenApplicationIsInactive@
allowsToolTipsWhenApplicationIsInactive :: IsNSWindow nsWindow => nsWindow -> IO Bool
allowsToolTipsWhenApplicationIsInactive nsWindow =
  sendMessage nsWindow allowsToolTipsWhenApplicationIsInactiveSelector

-- | Default is @NO.@ Set to @YES@ to allow a window to display tooltips even when the application is in the background.  Note that, enabling tooltips in an inactive application will cause the app to do work any time the mouse passes over the window.  This can degrade system performance. Returns @YES@ if this window displays tooltips even when the application is in the background.  To configure this setting you should call @-setAllowsToolTipsWhenApplicationIsInactive:@ instead of overriding @-allowsToolTipsWhenApplicationIsInactive@.
--
-- ObjC selector: @- setAllowsToolTipsWhenApplicationIsInactive:@
setAllowsToolTipsWhenApplicationIsInactive :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAllowsToolTipsWhenApplicationIsInactive nsWindow value =
  sendMessage nsWindow setAllowsToolTipsWhenApplicationIsInactiveSelector value

-- | @- backingType@
backingType :: IsNSWindow nsWindow => nsWindow -> IO NSBackingStoreType
backingType nsWindow =
  sendMessage nsWindow backingTypeSelector

-- | @- setBackingType:@
setBackingType :: IsNSWindow nsWindow => nsWindow -> NSBackingStoreType -> IO ()
setBackingType nsWindow value =
  sendMessage nsWindow setBackingTypeSelector value

-- | @- level@
level :: IsNSWindow nsWindow => nsWindow -> IO CLong
level nsWindow =
  sendMessage nsWindow levelSelector

-- | @- setLevel:@
setLevel :: IsNSWindow nsWindow => nsWindow -> CLong -> IO ()
setLevel nsWindow value =
  sendMessage nsWindow setLevelSelector value

-- | @- depthLimit@
depthLimit :: IsNSWindow nsWindow => nsWindow -> IO NSWindowDepth
depthLimit nsWindow =
  sendMessage nsWindow depthLimitSelector

-- | @- setDepthLimit:@
setDepthLimit :: IsNSWindow nsWindow => nsWindow -> NSWindowDepth -> IO ()
setDepthLimit nsWindow value =
  sendMessage nsWindow setDepthLimitSelector value

-- | @- hasDynamicDepthLimit@
hasDynamicDepthLimit :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasDynamicDepthLimit nsWindow =
  sendMessage nsWindow hasDynamicDepthLimitSelector

-- | The screen property returns the best screen for the window. If the window only intersects one screen, it returns that screen. If it intersects more than one screen, then it resolves the tie through based on what space it is mostly on. It may return nil if there are no available screens, or it is completely off screen.
--
-- ObjC selector: @- screen@
screen :: IsNSWindow nsWindow => nsWindow -> IO (Id NSScreen)
screen nsWindow =
  sendMessage nsWindow screenSelector

-- | @- deepestScreen@
deepestScreen :: IsNSWindow nsWindow => nsWindow -> IO (Id NSScreen)
deepestScreen nsWindow =
  sendMessage nsWindow deepestScreenSelector

-- | @- hasShadow@
hasShadow :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasShadow nsWindow =
  sendMessage nsWindow hasShadowSelector

-- | @- setHasShadow:@
setHasShadow :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setHasShadow nsWindow value =
  sendMessage nsWindow setHasShadowSelector value

-- | @- alphaValue@
alphaValue :: IsNSWindow nsWindow => nsWindow -> IO CDouble
alphaValue nsWindow =
  sendMessage nsWindow alphaValueSelector

-- | @- setAlphaValue:@
setAlphaValue :: IsNSWindow nsWindow => nsWindow -> CDouble -> IO ()
setAlphaValue nsWindow value =
  sendMessage nsWindow setAlphaValueSelector value

-- | @- opaque@
opaque :: IsNSWindow nsWindow => nsWindow -> IO Bool
opaque nsWindow =
  sendMessage nsWindow opaqueSelector

-- | @- setOpaque:@
setOpaque :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setOpaque nsWindow value =
  sendMessage nsWindow setOpaqueSelector value

-- | @-setSharingType:@ specifies whether the window content can be read from another process.  The default sharing type is @NSWindowSharingReadOnly,@ which means other processes can read the window content (eg. for window capture) but cannot modify it.  If you set your window sharing type to @NSWindowSharingNone,@ so that the content cannot be captured, your window will also not be able to participate in a number of system services, so this setting should be used with caution.
--
-- ObjC selector: @- sharingType@
sharingType :: IsNSWindow nsWindow => nsWindow -> IO NSWindowSharingType
sharingType nsWindow =
  sendMessage nsWindow sharingTypeSelector

-- | @-setSharingType:@ specifies whether the window content can be read from another process.  The default sharing type is @NSWindowSharingReadOnly,@ which means other processes can read the window content (eg. for window capture) but cannot modify it.  If you set your window sharing type to @NSWindowSharingNone,@ so that the content cannot be captured, your window will also not be able to participate in a number of system services, so this setting should be used with caution.
--
-- ObjC selector: @- setSharingType:@
setSharingType :: IsNSWindow nsWindow => nsWindow -> NSWindowSharingType -> IO ()
setSharingType nsWindow value =
  sendMessage nsWindow setSharingTypeSelector value

-- | Controls whether threading of view drawing should be enabled for this window.  Defaults to @YES.@  When this is set to @YES,@ AppKit's view system is allowed to perform @-drawRect:@ activity for the window's views on threads other than the main thread, for views that have @canDrawConcurrently == YES@.  When this is set to @NO,@ the window's views will be drawn serially as on 10.5 and earlier, even though some of the views may have @canDrawConcurrently == YES@.
--
-- ObjC selector: @- allowsConcurrentViewDrawing@
allowsConcurrentViewDrawing :: IsNSWindow nsWindow => nsWindow -> IO Bool
allowsConcurrentViewDrawing nsWindow =
  sendMessage nsWindow allowsConcurrentViewDrawingSelector

-- | Controls whether threading of view drawing should be enabled for this window.  Defaults to @YES.@  When this is set to @YES,@ AppKit's view system is allowed to perform @-drawRect:@ activity for the window's views on threads other than the main thread, for views that have @canDrawConcurrently == YES@.  When this is set to @NO,@ the window's views will be drawn serially as on 10.5 and earlier, even though some of the views may have @canDrawConcurrently == YES@.
--
-- ObjC selector: @- setAllowsConcurrentViewDrawing:@
setAllowsConcurrentViewDrawing :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAllowsConcurrentViewDrawing nsWindow value =
  sendMessage nsWindow setAllowsConcurrentViewDrawingSelector value

-- | @- displaysWhenScreenProfileChanges@
displaysWhenScreenProfileChanges :: IsNSWindow nsWindow => nsWindow -> IO Bool
displaysWhenScreenProfileChanges nsWindow =
  sendMessage nsWindow displaysWhenScreenProfileChangesSelector

-- | @- setDisplaysWhenScreenProfileChanges:@
setDisplaysWhenScreenProfileChanges :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setDisplaysWhenScreenProfileChanges nsWindow value =
  sendMessage nsWindow setDisplaysWhenScreenProfileChangesSelector value

-- | This API controls whether the receiver is permitted onscreen before the user has logged in.  This property is off by default.  Alert panels and windows presented by input managers are examples of windows which should have this property set.
--
-- ObjC selector: @- canBecomeVisibleWithoutLogin@
canBecomeVisibleWithoutLogin :: IsNSWindow nsWindow => nsWindow -> IO Bool
canBecomeVisibleWithoutLogin nsWindow =
  sendMessage nsWindow canBecomeVisibleWithoutLoginSelector

-- | This API controls whether the receiver is permitted onscreen before the user has logged in.  This property is off by default.  Alert panels and windows presented by input managers are examples of windows which should have this property set.
--
-- ObjC selector: @- setCanBecomeVisibleWithoutLogin:@
setCanBecomeVisibleWithoutLogin :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setCanBecomeVisibleWithoutLogin nsWindow value =
  sendMessage nsWindow setCanBecomeVisibleWithoutLoginSelector value

-- | @- collectionBehavior@
collectionBehavior :: IsNSWindow nsWindow => nsWindow -> IO NSWindowCollectionBehavior
collectionBehavior nsWindow =
  sendMessage nsWindow collectionBehaviorSelector

-- | @- setCollectionBehavior:@
setCollectionBehavior :: IsNSWindow nsWindow => nsWindow -> NSWindowCollectionBehavior -> IO ()
setCollectionBehavior nsWindow value =
  sendMessage nsWindow setCollectionBehaviorSelector value

-- | Provides for per-window control over automatic orderFront/orderOut animation behaviors added in 10.7.  Can be set to @NSWindowAnimationBehaviorNone@ to disable Appkit's automatic animations for a given window, or to one of the other non-Default @NSWindowAnimationBehavior@ values to override AppKit's automatic inference of appropriate animation behavior based on the window's apparent type.
--
-- ObjC selector: @- animationBehavior@
animationBehavior :: IsNSWindow nsWindow => nsWindow -> IO NSWindowAnimationBehavior
animationBehavior nsWindow =
  sendMessage nsWindow animationBehaviorSelector

-- | Provides for per-window control over automatic orderFront/orderOut animation behaviors added in 10.7.  Can be set to @NSWindowAnimationBehaviorNone@ to disable Appkit's automatic animations for a given window, or to one of the other non-Default @NSWindowAnimationBehavior@ values to override AppKit's automatic inference of appropriate animation behavior based on the window's apparent type.
--
-- ObjC selector: @- setAnimationBehavior:@
setAnimationBehavior :: IsNSWindow nsWindow => nsWindow -> NSWindowAnimationBehavior -> IO ()
setAnimationBehavior nsWindow value =
  sendMessage nsWindow setAnimationBehaviorSelector value

-- | Returns @YES@ if this window is associated with the active space.  For visible windows, this API indicates whether the window is currently visible on the active space.  For offscreen windows, it indicates whether ordering the window onscreen would make it bring it onto the active space
--
-- ObjC selector: @- onActiveSpace@
onActiveSpace :: IsNSWindow nsWindow => nsWindow -> IO Bool
onActiveSpace nsWindow =
  sendMessage nsWindow onActiveSpaceSelector

-- | @- stringWithSavedFrame@
stringWithSavedFrame :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
stringWithSavedFrame nsWindow =
  sendMessage nsWindow stringWithSavedFrameSelector

-- | @- frameAutosaveName@
frameAutosaveName :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
frameAutosaveName nsWindow =
  sendMessage nsWindow frameAutosaveNameSelector

-- | @- minSize@
minSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
minSize nsWindow =
  sendMessage nsWindow minSizeSelector

-- | @- setMinSize:@
setMinSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMinSize nsWindow value =
  sendMessage nsWindow setMinSizeSelector value

-- | @- maxSize@
maxSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
maxSize nsWindow =
  sendMessage nsWindow maxSizeSelector

-- | @- setMaxSize:@
setMaxSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMaxSize nsWindow value =
  sendMessage nsWindow setMaxSizeSelector value

-- | @- contentMinSize@
contentMinSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentMinSize nsWindow =
  sendMessage nsWindow contentMinSizeSelector

-- | @- setContentMinSize:@
setContentMinSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentMinSize nsWindow value =
  sendMessage nsWindow setContentMinSizeSelector value

-- | @- contentMaxSize@
contentMaxSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentMaxSize nsWindow =
  sendMessage nsWindow contentMaxSizeSelector

-- | @- setContentMaxSize:@
setContentMaxSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentMaxSize nsWindow value =
  sendMessage nsWindow setContentMaxSizeSelector value

-- | @- minFullScreenContentSize@
minFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
minFullScreenContentSize nsWindow =
  sendMessage nsWindow minFullScreenContentSizeSelector

-- | @- setMinFullScreenContentSize:@
setMinFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMinFullScreenContentSize nsWindow value =
  sendMessage nsWindow setMinFullScreenContentSizeSelector value

-- | @- maxFullScreenContentSize@
maxFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
maxFullScreenContentSize nsWindow =
  sendMessage nsWindow maxFullScreenContentSizeSelector

-- | @- setMaxFullScreenContentSize:@
setMaxFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMaxFullScreenContentSize nsWindow value =
  sendMessage nsWindow setMaxFullScreenContentSizeSelector value

-- | @- deviceDescription@
deviceDescription :: IsNSWindow nsWindow => nsWindow -> IO (Id NSDictionary)
deviceDescription nsWindow =
  sendMessage nsWindow deviceDescriptionSelector

-- | @- windowController@
windowController :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindowController)
windowController nsWindow =
  sendMessage nsWindow windowControllerSelector

-- | @- setWindowController:@
setWindowController :: (IsNSWindow nsWindow, IsNSWindowController value) => nsWindow -> value -> IO ()
setWindowController nsWindow value =
  sendMessage nsWindow setWindowControllerSelector (toNSWindowController value)

-- | An ordered array of the sheets on the window. This consists of the presented sheets in top-to-bottom order, followed by queued sheets in the order they were queued. This does not include nested/sub-sheets.
--
-- ObjC selector: @- sheets@
sheets :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
sheets nsWindow =
  sendMessage nsWindow sheetsSelector

-- | Returns the top-most sheet if there is one or more sheets, or nil if there is no sheet.
--
-- ObjC selector: @- attachedSheet@
attachedSheet :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindow)
attachedSheet nsWindow =
  sendMessage nsWindow attachedSheetSelector

-- | @- sheet@
sheet :: IsNSWindow nsWindow => nsWindow -> IO Bool
sheet nsWindow =
  sendMessage nsWindow sheetSelector

-- | Returns the window that the sheet is directly attached to. This is based on the logical attachment of the sheet, not visual attachment. This relationship exists starting when the sheet is begun (using @NSApplication's@ @-beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo: or NSWindow's -beginSheet:completionHandler:@), and ending once it is ordered out. Returns nil if the window is not a sheet or has no sheet parent.
--
-- ObjC selector: @- sheetParent@
sheetParent :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindow)
sheetParent nsWindow =
  sendMessage nsWindow sheetParentSelector

-- | @- childWindows@
childWindows :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
childWindows nsWindow =
  sendMessage nsWindow childWindowsSelector

-- | @- parentWindow@
parentWindow :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindow)
parentWindow nsWindow =
  sendMessage nsWindow parentWindowSelector

-- | @- setParentWindow:@
setParentWindow :: (IsNSWindow nsWindow, IsNSWindow value) => nsWindow -> value -> IO ()
setParentWindow nsWindow value =
  sendMessage nsWindow setParentWindowSelector (toNSWindow value)

-- | If set, the receiver will inherit the appearance of that object, as well as use KVO to observe its effectiveAppearance for changes. Typically this is used for child windows that are shown from a parent window or specific view. Defaults to NSApp.
--
-- ObjC selector: @- appearanceSource@
appearanceSource :: IsNSWindow nsWindow => nsWindow -> IO (Id NSObject)
appearanceSource nsWindow =
  sendMessage nsWindow appearanceSourceSelector

-- | If set, the receiver will inherit the appearance of that object, as well as use KVO to observe its effectiveAppearance for changes. Typically this is used for child windows that are shown from a parent window or specific view. Defaults to NSApp.
--
-- ObjC selector: @- setAppearanceSource:@
setAppearanceSource :: (IsNSWindow nsWindow, IsNSObject value) => nsWindow -> value -> IO ()
setAppearanceSource nsWindow value =
  sendMessage nsWindow setAppearanceSourceSelector (toNSObject value)

-- | @- colorSpace@
colorSpace :: IsNSWindow nsWindow => nsWindow -> IO (Id NSColorSpace)
colorSpace nsWindow =
  sendMessage nsWindow colorSpaceSelector

-- | @- setColorSpace:@
setColorSpace :: (IsNSWindow nsWindow, IsNSColorSpace value) => nsWindow -> value -> IO ()
setColorSpace nsWindow value =
  sendMessage nsWindow setColorSpaceSelector (toNSColorSpace value)

-- | @- occlusionState@
occlusionState :: IsNSWindow nsWindow => nsWindow -> IO NSWindowOcclusionState
occlusionState nsWindow =
  sendMessage nsWindow occlusionStateSelector

-- | Specifies the style of separator displayed between the window's titlebar and content.
--
-- The default value is NSTitlebarSeparatorStyleAutomatic. Changing this value will override any preference made by @NSSplitViewItem@.
--
-- ObjC selector: @- titlebarSeparatorStyle@
titlebarSeparatorStyle :: IsNSWindow nsWindow => nsWindow -> IO NSTitlebarSeparatorStyle
titlebarSeparatorStyle nsWindow =
  sendMessage nsWindow titlebarSeparatorStyleSelector

-- | Specifies the style of separator displayed between the window's titlebar and content.
--
-- The default value is NSTitlebarSeparatorStyleAutomatic. Changing this value will override any preference made by @NSSplitViewItem@.
--
-- ObjC selector: @- setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyle :: IsNSWindow nsWindow => nsWindow -> NSTitlebarSeparatorStyle -> IO ()
setTitlebarSeparatorStyle nsWindow value =
  sendMessage nsWindow setTitlebarSeparatorStyleSelector value

-- | The main content view controller for the window. This provides the contentView of the window. Assigning this value will remove the existing contentView and will make the contentViewController.view the main contentView for the window. The default value is nil. The contentViewController only controls the contentView, and not the title of the window. The window title can easily be bound to the contentViewController with the following: [window bind:NSTitleBinding toObject:contentViewController withKeyPath:"title" options:nil]. Setting the contentViewController will cause the window to resize based on the current size of the contentViewController. Autolayout should be used to restrict the size of the window. The value of the contentViewController is encoded in the NIB. Directly assigning a contentView will clear out the contentViewController.
--
-- ObjC selector: @- contentViewController@
contentViewController :: IsNSWindow nsWindow => nsWindow -> IO (Id NSViewController)
contentViewController nsWindow =
  sendMessage nsWindow contentViewControllerSelector

-- | The main content view controller for the window. This provides the contentView of the window. Assigning this value will remove the existing contentView and will make the contentViewController.view the main contentView for the window. The default value is nil. The contentViewController only controls the contentView, and not the title of the window. The window title can easily be bound to the contentViewController with the following: [window bind:NSTitleBinding toObject:contentViewController withKeyPath:"title" options:nil]. Setting the contentViewController will cause the window to resize based on the current size of the contentViewController. Autolayout should be used to restrict the size of the window. The value of the contentViewController is encoded in the NIB. Directly assigning a contentView will clear out the contentViewController.
--
-- ObjC selector: @- setContentViewController:@
setContentViewController :: (IsNSWindow nsWindow, IsNSViewController value) => nsWindow -> value -> IO ()
setContentViewController nsWindow value =
  sendMessage nsWindow setContentViewControllerSelector (toNSViewController value)

-- | @- initialFirstResponder@
initialFirstResponder :: IsNSWindow nsWindow => nsWindow -> IO (Id NSView)
initialFirstResponder nsWindow =
  sendOwnedMessage nsWindow initialFirstResponderSelector

-- | @- setInitialFirstResponder:@
setInitialFirstResponder :: (IsNSWindow nsWindow, IsNSView value) => nsWindow -> value -> IO ()
setInitialFirstResponder nsWindow value =
  sendMessage nsWindow setInitialFirstResponderSelector (toNSView value)

-- | @- keyViewSelectionDirection@
keyViewSelectionDirection :: IsNSWindow nsWindow => nsWindow -> IO NSSelectionDirection
keyViewSelectionDirection nsWindow =
  sendMessage nsWindow keyViewSelectionDirectionSelector

-- | @- defaultButtonCell@
defaultButtonCell :: IsNSWindow nsWindow => nsWindow -> IO (Id NSButtonCell)
defaultButtonCell nsWindow =
  sendMessage nsWindow defaultButtonCellSelector

-- | @- setDefaultButtonCell:@
setDefaultButtonCell :: (IsNSWindow nsWindow, IsNSButtonCell value) => nsWindow -> value -> IO ()
setDefaultButtonCell nsWindow value =
  sendMessage nsWindow setDefaultButtonCellSelector (toNSButtonCell value)

-- | @- autorecalculatesKeyViewLoop@
autorecalculatesKeyViewLoop :: IsNSWindow nsWindow => nsWindow -> IO Bool
autorecalculatesKeyViewLoop nsWindow =
  sendMessage nsWindow autorecalculatesKeyViewLoopSelector

-- | @- setAutorecalculatesKeyViewLoop:@
setAutorecalculatesKeyViewLoop :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAutorecalculatesKeyViewLoop nsWindow value =
  sendMessage nsWindow setAutorecalculatesKeyViewLoopSelector value

-- | @- toolbar@
toolbar :: IsNSWindow nsWindow => nsWindow -> IO (Id NSToolbar)
toolbar nsWindow =
  sendMessage nsWindow toolbarSelector

-- | @- setToolbar:@
setToolbar :: (IsNSWindow nsWindow, IsNSToolbar value) => nsWindow -> value -> IO ()
setToolbar nsWindow value =
  sendMessage nsWindow setToolbarSelector (toNSToolbar value)

-- | @- showsToolbarButton@
showsToolbarButton :: IsNSWindow nsWindow => nsWindow -> IO Bool
showsToolbarButton nsWindow =
  sendMessage nsWindow showsToolbarButtonSelector

-- | @- setShowsToolbarButton:@
setShowsToolbarButton :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setShowsToolbarButton nsWindow value =
  sendMessage nsWindow setShowsToolbarButtonSelector value

-- | Allows automatic window tabbing when the value is @YES.@ By default, this will be set to @YES,@ but applications can explicitly opt out of all automatic tabbing by setting it to NO, and can still adopted explicit window tabbing, if desired.
--
-- ObjC selector: @+ allowsAutomaticWindowTabbing@
allowsAutomaticWindowTabbing :: IO Bool
allowsAutomaticWindowTabbing  =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' allowsAutomaticWindowTabbingSelector

-- | Allows automatic window tabbing when the value is @YES.@ By default, this will be set to @YES,@ but applications can explicitly opt out of all automatic tabbing by setting it to NO, and can still adopted explicit window tabbing, if desired.
--
-- ObjC selector: @+ setAllowsAutomaticWindowTabbing:@
setAllowsAutomaticWindowTabbing :: Bool -> IO ()
setAllowsAutomaticWindowTabbing value =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' setAllowsAutomaticWindowTabbingSelector value

-- | Returns the user's tabbing preference as set in System Preferences. This value should be queried anytime a new window is made to see if the user wants to automatically show it in tabs.
--
-- ObjC selector: @+ userTabbingPreference@
userTabbingPreference :: IO NSWindowUserTabbingPreference
userTabbingPreference  =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMessage cls' userTabbingPreferenceSelector

-- | Get and set the tabbing mode for this window. This should be set before a window is shown. The default value is @NSWindowTabbingModeAutomatic.@ When the value is @NSWindowTabbingModeAutomatic,@ the system will look at the @userTabbingPreference@ and automatically tab windows together based on the tabbingIdentifier, when it is appropriate to do so.
--
-- ObjC selector: @- tabbingMode@
tabbingMode :: IsNSWindow nsWindow => nsWindow -> IO NSWindowTabbingMode
tabbingMode nsWindow =
  sendMessage nsWindow tabbingModeSelector

-- | Get and set the tabbing mode for this window. This should be set before a window is shown. The default value is @NSWindowTabbingModeAutomatic.@ When the value is @NSWindowTabbingModeAutomatic,@ the system will look at the @userTabbingPreference@ and automatically tab windows together based on the tabbingIdentifier, when it is appropriate to do so.
--
-- ObjC selector: @- setTabbingMode:@
setTabbingMode :: IsNSWindow nsWindow => nsWindow -> NSWindowTabbingMode -> IO ()
setTabbingMode nsWindow value =
  sendMessage nsWindow setTabbingModeSelector value

-- | Windows with the same @tabbingIdentifier@ will have the ability to be tabbed together when a window is being shown. This allows aggregation of similar windows. By default, the @tabbingIdentifier@ will be generated based on inherent window properties, such as the window class name, the delegate class name, the window controller class name, and some additional state. Windows can be explicitly made to group together by using the same @tabbingIdentifier.@
--
-- ObjC selector: @- tabbingIdentifier@
tabbingIdentifier :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
tabbingIdentifier nsWindow =
  sendMessage nsWindow tabbingIdentifierSelector

-- | Windows with the same @tabbingIdentifier@ will have the ability to be tabbed together when a window is being shown. This allows aggregation of similar windows. By default, the @tabbingIdentifier@ will be generated based on inherent window properties, such as the window class name, the delegate class name, the window controller class name, and some additional state. Windows can be explicitly made to group together by using the same @tabbingIdentifier.@
--
-- ObjC selector: @- setTabbingIdentifier:@
setTabbingIdentifier :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setTabbingIdentifier nsWindow value =
  sendMessage nsWindow setTabbingIdentifierSelector (toNSString value)

-- | This is now a cover for @self.tabGroup.windows@, but will return nil if the window is not showing a tab bar.
--
-- ObjC selector: @- tabbedWindows@
tabbedWindows :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
tabbedWindows nsWindow =
  sendMessage nsWindow tabbedWindowsSelector

-- | Access the properties for this window when it is a tabbed window environment. See the @NSWindowTab@ header and comments for more information.
--
-- ObjC selector: @- tab@
tab :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindowTab)
tab nsWindow =
  sendMessage nsWindow tabSelector

-- | Represents a tab group of windows. This @tabGroup@ is lazily created on demand.
--
-- ObjC selector: @- tabGroup@
tabGroup :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindowTabGroup)
tabGroup nsWindow =
  sendMessage nsWindow tabGroupSelector

-- | Indicates whether the receiver is the subject of an active SharePlay sharing session.
--
-- ObjC selector: @- hasActiveWindowSharingSession@
hasActiveWindowSharingSession :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasActiveWindowSharingSession nsWindow =
  sendMessage nsWindow hasActiveWindowSharingSessionSelector

-- | Retrieve the layout direction of the window titlebar: this includes the standard window buttons (close/minimize/maximize buttons) and the title for this window. In general, this will return "right to left" (RTL) if the primary system language is RTL. The layout direction may be RTL even in applications that do not have a RTL language localization. This value should be utilized if an application uses titlebarAppearsTransparent and places controls underneath the titlebar.
--
-- ObjC selector: @- windowTitlebarLayoutDirection@
windowTitlebarLayoutDirection :: IsNSWindow nsWindow => nsWindow -> IO NSUserInterfaceLayoutDirection
windowTitlebarLayoutDirection nsWindow =
  sendMessage nsWindow windowTitlebarLayoutDirectionSelector

-- | @- restorable@
restorable :: IsNSWindow nsWindow => nsWindow -> IO Bool
restorable nsWindow =
  sendMessage nsWindow restorableSelector

-- | @- setRestorable:@
setRestorable :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setRestorable nsWindow value =
  sendMessage nsWindow setRestorableSelector value

-- | @- restorationClass@
restorationClass :: IsNSWindow nsWindow => nsWindow -> IO Class
restorationClass nsWindow =
  sendMessage nsWindow restorationClassSelector

-- | @- setRestorationClass:@
setRestorationClass :: IsNSWindow nsWindow => nsWindow -> Class -> IO ()
setRestorationClass nsWindow value =
  sendMessage nsWindow setRestorationClassSelector value

-- | @- hasCloseBox@
hasCloseBox :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasCloseBox nsWindow =
  sendMessage nsWindow hasCloseBoxSelector

-- | @- hasTitleBar@
hasTitleBar :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasTitleBar nsWindow =
  sendMessage nsWindow hasTitleBarSelector

-- | @- floatingPanel@
floatingPanel :: IsNSWindow nsWindow => nsWindow -> IO Bool
floatingPanel nsWindow =
  sendMessage nsWindow floatingPanelSelector

-- | @- miniaturizable@
miniaturizable :: IsNSWindow nsWindow => nsWindow -> IO Bool
miniaturizable nsWindow =
  sendMessage nsWindow miniaturizableSelector

-- | @- modalPanel@
modalPanel :: IsNSWindow nsWindow => nsWindow -> IO Bool
modalPanel nsWindow =
  sendMessage nsWindow modalPanelSelector

-- | @- resizable@
resizable :: IsNSWindow nsWindow => nsWindow -> IO Bool
resizable nsWindow =
  sendMessage nsWindow resizableSelector

-- | @- zoomable@
zoomable :: IsNSWindow nsWindow => nsWindow -> IO Bool
zoomable nsWindow =
  sendMessage nsWindow zoomableSelector

-- | @- orderedIndex@
orderedIndex :: IsNSWindow nsWindow => nsWindow -> IO CLong
orderedIndex nsWindow =
  sendMessage nsWindow orderedIndexSelector

-- | @- setOrderedIndex:@
setOrderedIndex :: IsNSWindow nsWindow => nsWindow -> CLong -> IO ()
setOrderedIndex nsWindow value =
  sendMessage nsWindow setOrderedIndexSelector value

-- | @- drawers@
drawers :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
drawers nsWindow =
  sendMessage nsWindow drawersSelector

-- | @- flushWindowDisabled@
flushWindowDisabled :: IsNSWindow nsWindow => nsWindow -> IO Bool
flushWindowDisabled nsWindow =
  sendMessage nsWindow flushWindowDisabledSelector

-- | @- autodisplay@
autodisplay :: IsNSWindow nsWindow => nsWindow -> IO Bool
autodisplay nsWindow =
  sendMessage nsWindow autodisplaySelector

-- | @- setAutodisplay:@
setAutodisplay :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAutodisplay nsWindow value =
  sendMessage nsWindow setAutodisplaySelector value

-- | @- graphicsContext@
graphicsContext :: IsNSWindow nsWindow => nsWindow -> IO (Id NSGraphicsContext)
graphicsContext nsWindow =
  sendMessage nsWindow graphicsContextSelector

-- | @- oneShot@
oneShot :: IsNSWindow nsWindow => nsWindow -> IO Bool
oneShot nsWindow =
  sendMessage nsWindow oneShotSelector

-- | @- setOneShot:@
setOneShot :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setOneShot nsWindow value =
  sendMessage nsWindow setOneShotSelector value

-- | @- preferredBackingLocation@
preferredBackingLocation :: IsNSWindow nsWindow => nsWindow -> IO NSWindowBackingLocation
preferredBackingLocation nsWindow =
  sendMessage nsWindow preferredBackingLocationSelector

-- | @- setPreferredBackingLocation:@
setPreferredBackingLocation :: IsNSWindow nsWindow => nsWindow -> NSWindowBackingLocation -> IO ()
setPreferredBackingLocation nsWindow value =
  sendMessage nsWindow setPreferredBackingLocationSelector value

-- | @- backingLocation@
backingLocation :: IsNSWindow nsWindow => nsWindow -> IO NSWindowBackingLocation
backingLocation nsWindow =
  sendMessage nsWindow backingLocationSelector

-- | @- showsResizeIndicator@
showsResizeIndicator :: IsNSWindow nsWindow => nsWindow -> IO Bool
showsResizeIndicator nsWindow =
  sendMessage nsWindow showsResizeIndicatorSelector

-- | @- setShowsResizeIndicator:@
setShowsResizeIndicator :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setShowsResizeIndicator nsWindow value =
  sendMessage nsWindow setShowsResizeIndicatorSelector value

-- | @- windowRef@
windowRef :: IsNSWindow nsWindow => nsWindow -> IO (Ptr ())
windowRef nsWindow =
  sendMessage nsWindow windowRefSelector

-- | @- areCursorRectsEnabled@
areCursorRectsEnabled :: IsNSWindow nsWindow => nsWindow -> IO Bool
areCursorRectsEnabled nsWindow =
  sendMessage nsWindow areCursorRectsEnabledSelector

-- | @- currentEvent@
currentEvent :: IsNSWindow nsWindow => nsWindow -> IO (Id NSEvent)
currentEvent nsWindow =
  sendMessage nsWindow currentEventSelector

-- | @- acceptsMouseMovedEvents@
acceptsMouseMovedEvents :: IsNSWindow nsWindow => nsWindow -> IO Bool
acceptsMouseMovedEvents nsWindow =
  sendMessage nsWindow acceptsMouseMovedEventsSelector

-- | @- setAcceptsMouseMovedEvents:@
setAcceptsMouseMovedEvents :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAcceptsMouseMovedEvents nsWindow value =
  sendMessage nsWindow setAcceptsMouseMovedEventsSelector value

-- | @- ignoresMouseEvents@
ignoresMouseEvents :: IsNSWindow nsWindow => nsWindow -> IO Bool
ignoresMouseEvents nsWindow =
  sendMessage nsWindow ignoresMouseEventsSelector

-- | @- setIgnoresMouseEvents:@
setIgnoresMouseEvents :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIgnoresMouseEvents nsWindow value =
  sendMessage nsWindow setIgnoresMouseEventsSelector value

-- | @- mouseLocationOutsideOfEventStream@
mouseLocationOutsideOfEventStream :: IsNSWindow nsWindow => nsWindow -> IO NSPoint
mouseLocationOutsideOfEventStream nsWindow =
  sendMessage nsWindow mouseLocationOutsideOfEventStreamSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frameRectForContentRect:styleMask:@
frameRectForContentRect_styleMaskSelector :: Selector '[NSRect, NSWindowStyleMask] NSRect
frameRectForContentRect_styleMaskSelector = mkSelector "frameRectForContentRect:styleMask:"

-- | @Selector@ for @contentRectForFrameRect:styleMask:@
contentRectForFrameRect_styleMaskSelector :: Selector '[NSRect, NSWindowStyleMask] NSRect
contentRectForFrameRect_styleMaskSelector = mkSelector "contentRectForFrameRect:styleMask:"

-- | @Selector@ for @minFrameWidthWithTitle:styleMask:@
minFrameWidthWithTitle_styleMaskSelector :: Selector '[Id NSString, NSWindowStyleMask] CDouble
minFrameWidthWithTitle_styleMaskSelector = mkSelector "minFrameWidthWithTitle:styleMask:"

-- | @Selector@ for @frameRectForContentRect:@
frameRectForContentRectSelector :: Selector '[NSRect] NSRect
frameRectForContentRectSelector = mkSelector "frameRectForContentRect:"

-- | @Selector@ for @contentRectForFrameRect:@
contentRectForFrameRectSelector :: Selector '[NSRect] NSRect
contentRectForFrameRectSelector = mkSelector "contentRectForFrameRect:"

-- | @Selector@ for @initWithContentRect:styleMask:backing:defer:@
initWithContentRect_styleMask_backing_deferSelector :: Selector '[NSRect, NSWindowStyleMask, NSBackingStoreType, Bool] (Id NSWindow)
initWithContentRect_styleMask_backing_deferSelector = mkSelector "initWithContentRect:styleMask:backing:defer:"

-- | @Selector@ for @initWithContentRect:styleMask:backing:defer:screen:@
initWithContentRect_styleMask_backing_defer_screenSelector :: Selector '[NSRect, NSWindowStyleMask, NSBackingStoreType, Bool, Id NSScreen] (Id NSWindow)
initWithContentRect_styleMask_backing_defer_screenSelector = mkSelector "initWithContentRect:styleMask:backing:defer:screen:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSWindow)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addTitlebarAccessoryViewController:@
addTitlebarAccessoryViewControllerSelector :: Selector '[Id NSTitlebarAccessoryViewController] ()
addTitlebarAccessoryViewControllerSelector = mkSelector "addTitlebarAccessoryViewController:"

-- | @Selector@ for @insertTitlebarAccessoryViewController:atIndex:@
insertTitlebarAccessoryViewController_atIndexSelector :: Selector '[Id NSTitlebarAccessoryViewController, CLong] ()
insertTitlebarAccessoryViewController_atIndexSelector = mkSelector "insertTitlebarAccessoryViewController:atIndex:"

-- | @Selector@ for @removeTitlebarAccessoryViewControllerAtIndex:@
removeTitlebarAccessoryViewControllerAtIndexSelector :: Selector '[CLong] ()
removeTitlebarAccessoryViewControllerAtIndexSelector = mkSelector "removeTitlebarAccessoryViewControllerAtIndex:"

-- | @Selector@ for @setTitleWithRepresentedFilename:@
setTitleWithRepresentedFilenameSelector :: Selector '[Id NSString] ()
setTitleWithRepresentedFilenameSelector = mkSelector "setTitleWithRepresentedFilename:"

-- | @Selector@ for @fieldEditor:forObject:@
fieldEditor_forObjectSelector :: Selector '[Bool, RawId] (Id NSText)
fieldEditor_forObjectSelector = mkSelector "fieldEditor:forObject:"

-- | @Selector@ for @endEditingFor:@
endEditingForSelector :: Selector '[RawId] ()
endEditingForSelector = mkSelector "endEditingFor:"

-- | @Selector@ for @constrainFrameRect:toScreen:@
constrainFrameRect_toScreenSelector :: Selector '[NSRect, Id NSScreen] NSRect
constrainFrameRect_toScreenSelector = mkSelector "constrainFrameRect:toScreen:"

-- | @Selector@ for @setFrame:display:@
setFrame_displaySelector :: Selector '[NSRect, Bool] ()
setFrame_displaySelector = mkSelector "setFrame:display:"

-- | @Selector@ for @setContentSize:@
setContentSizeSelector :: Selector '[NSSize] ()
setContentSizeSelector = mkSelector "setContentSize:"

-- | @Selector@ for @setFrameOrigin:@
setFrameOriginSelector :: Selector '[NSPoint] ()
setFrameOriginSelector = mkSelector "setFrameOrigin:"

-- | @Selector@ for @setFrameTopLeftPoint:@
setFrameTopLeftPointSelector :: Selector '[NSPoint] ()
setFrameTopLeftPointSelector = mkSelector "setFrameTopLeftPoint:"

-- | @Selector@ for @cascadeTopLeftFromPoint:@
cascadeTopLeftFromPointSelector :: Selector '[NSPoint] NSPoint
cascadeTopLeftFromPointSelector = mkSelector "cascadeTopLeftFromPoint:"

-- | @Selector@ for @animationResizeTime:@
animationResizeTimeSelector :: Selector '[NSRect] CDouble
animationResizeTimeSelector = mkSelector "animationResizeTime:"

-- | @Selector@ for @setFrame:display:animate:@
setFrame_display_animateSelector :: Selector '[NSRect, Bool, Bool] ()
setFrame_display_animateSelector = mkSelector "setFrame:display:animate:"

-- | @Selector@ for @displayIfNeeded@
displayIfNeededSelector :: Selector '[] ()
displayIfNeededSelector = mkSelector "displayIfNeeded"

-- | @Selector@ for @display@
displaySelector :: Selector '[] ()
displaySelector = mkSelector "display"

-- | @Selector@ for @update@
updateSelector :: Selector '[] ()
updateSelector = mkSelector "update"

-- | @Selector@ for @makeFirstResponder:@
makeFirstResponderSelector :: Selector '[Id NSResponder] Bool
makeFirstResponderSelector = mkSelector "makeFirstResponder:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @miniaturize:@
miniaturizeSelector :: Selector '[RawId] ()
miniaturizeSelector = mkSelector "miniaturize:"

-- | @Selector@ for @deminiaturize:@
deminiaturizeSelector :: Selector '[RawId] ()
deminiaturizeSelector = mkSelector "deminiaturize:"

-- | @Selector@ for @zoom:@
zoomSelector :: Selector '[RawId] ()
zoomSelector = mkSelector "zoom:"

-- | @Selector@ for @tryToPerform:with:@
tryToPerform_withSelector :: Selector '[Sel, RawId] Bool
tryToPerform_withSelector = mkSelector "tryToPerform:with:"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector '[Id NSString, Id NSString] RawId
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @setContentBorderThickness:forEdge:@
setContentBorderThickness_forEdgeSelector :: Selector '[CDouble, NSRectEdge] ()
setContentBorderThickness_forEdgeSelector = mkSelector "setContentBorderThickness:forEdge:"

-- | @Selector@ for @contentBorderThicknessForEdge:@
contentBorderThicknessForEdgeSelector :: Selector '[NSRectEdge] CDouble
contentBorderThicknessForEdgeSelector = mkSelector "contentBorderThicknessForEdge:"

-- | @Selector@ for @setAutorecalculatesContentBorderThickness:forEdge:@
setAutorecalculatesContentBorderThickness_forEdgeSelector :: Selector '[Bool, NSRectEdge] ()
setAutorecalculatesContentBorderThickness_forEdgeSelector = mkSelector "setAutorecalculatesContentBorderThickness:forEdge:"

-- | @Selector@ for @autorecalculatesContentBorderThicknessForEdge:@
autorecalculatesContentBorderThicknessForEdgeSelector :: Selector '[NSRectEdge] Bool
autorecalculatesContentBorderThicknessForEdgeSelector = mkSelector "autorecalculatesContentBorderThicknessForEdge:"

-- | @Selector@ for @center@
centerSelector :: Selector '[] ()
centerSelector = mkSelector "center"

-- | @Selector@ for @makeKeyAndOrderFront:@
makeKeyAndOrderFrontSelector :: Selector '[RawId] ()
makeKeyAndOrderFrontSelector = mkSelector "makeKeyAndOrderFront:"

-- | @Selector@ for @orderFront:@
orderFrontSelector :: Selector '[RawId] ()
orderFrontSelector = mkSelector "orderFront:"

-- | @Selector@ for @orderBack:@
orderBackSelector :: Selector '[RawId] ()
orderBackSelector = mkSelector "orderBack:"

-- | @Selector@ for @orderOut:@
orderOutSelector :: Selector '[RawId] ()
orderOutSelector = mkSelector "orderOut:"

-- | @Selector@ for @orderWindow:relativeTo:@
orderWindow_relativeToSelector :: Selector '[NSWindowOrderingMode, CLong] ()
orderWindow_relativeToSelector = mkSelector "orderWindow:relativeTo:"

-- | @Selector@ for @orderFrontRegardless@
orderFrontRegardlessSelector :: Selector '[] ()
orderFrontRegardlessSelector = mkSelector "orderFrontRegardless"

-- | @Selector@ for @makeKeyWindow@
makeKeyWindowSelector :: Selector '[] ()
makeKeyWindowSelector = mkSelector "makeKeyWindow"

-- | @Selector@ for @makeMainWindow@
makeMainWindowSelector :: Selector '[] ()
makeMainWindowSelector = mkSelector "makeMainWindow"

-- | @Selector@ for @becomeKeyWindow@
becomeKeyWindowSelector :: Selector '[] ()
becomeKeyWindowSelector = mkSelector "becomeKeyWindow"

-- | @Selector@ for @resignKeyWindow@
resignKeyWindowSelector :: Selector '[] ()
resignKeyWindowSelector = mkSelector "resignKeyWindow"

-- | @Selector@ for @becomeMainWindow@
becomeMainWindowSelector :: Selector '[] ()
becomeMainWindowSelector = mkSelector "becomeMainWindow"

-- | @Selector@ for @resignMainWindow@
resignMainWindowSelector :: Selector '[] ()
resignMainWindowSelector = mkSelector "resignMainWindow"

-- | @Selector@ for @convertRectToScreen:@
convertRectToScreenSelector :: Selector '[NSRect] NSRect
convertRectToScreenSelector = mkSelector "convertRectToScreen:"

-- | @Selector@ for @convertRectFromScreen:@
convertRectFromScreenSelector :: Selector '[NSRect] NSRect
convertRectFromScreenSelector = mkSelector "convertRectFromScreen:"

-- | @Selector@ for @convertPointToScreen:@
convertPointToScreenSelector :: Selector '[NSPoint] NSPoint
convertPointToScreenSelector = mkSelector "convertPointToScreen:"

-- | @Selector@ for @convertPointFromScreen:@
convertPointFromScreenSelector :: Selector '[NSPoint] NSPoint
convertPointFromScreenSelector = mkSelector "convertPointFromScreen:"

-- | @Selector@ for @convertRectToBacking:@
convertRectToBackingSelector :: Selector '[NSRect] NSRect
convertRectToBackingSelector = mkSelector "convertRectToBacking:"

-- | @Selector@ for @convertRectFromBacking:@
convertRectFromBackingSelector :: Selector '[NSRect] NSRect
convertRectFromBackingSelector = mkSelector "convertRectFromBacking:"

-- | @Selector@ for @convertPointToBacking:@
convertPointToBackingSelector :: Selector '[NSPoint] NSPoint
convertPointToBackingSelector = mkSelector "convertPointToBacking:"

-- | @Selector@ for @convertPointFromBacking:@
convertPointFromBackingSelector :: Selector '[NSPoint] NSPoint
convertPointFromBackingSelector = mkSelector "convertPointFromBacking:"

-- | @Selector@ for @backingAlignedRect:options:@
backingAlignedRect_optionsSelector :: Selector '[NSRect, NSAlignmentOptions] NSRect
backingAlignedRect_optionsSelector = mkSelector "backingAlignedRect:options:"

-- | @Selector@ for @performClose:@
performCloseSelector :: Selector '[RawId] ()
performCloseSelector = mkSelector "performClose:"

-- | @Selector@ for @performMiniaturize:@
performMiniaturizeSelector :: Selector '[RawId] ()
performMiniaturizeSelector = mkSelector "performMiniaturize:"

-- | @Selector@ for @performZoom:@
performZoomSelector :: Selector '[RawId] ()
performZoomSelector = mkSelector "performZoom:"

-- | @Selector@ for @dataWithEPSInsideRect:@
dataWithEPSInsideRectSelector :: Selector '[NSRect] (Id NSData)
dataWithEPSInsideRectSelector = mkSelector "dataWithEPSInsideRect:"

-- | @Selector@ for @dataWithPDFInsideRect:@
dataWithPDFInsideRectSelector :: Selector '[NSRect] (Id NSData)
dataWithPDFInsideRectSelector = mkSelector "dataWithPDFInsideRect:"

-- | @Selector@ for @print:@
printSelector :: Selector '[RawId] ()
printSelector = mkSelector "print:"

-- | @Selector@ for @setDynamicDepthLimit:@
setDynamicDepthLimitSelector :: Selector '[Bool] ()
setDynamicDepthLimitSelector = mkSelector "setDynamicDepthLimit:"

-- | @Selector@ for @invalidateShadow@
invalidateShadowSelector :: Selector '[] ()
invalidateShadowSelector = mkSelector "invalidateShadow"

-- | @Selector@ for @toggleFullScreen:@
toggleFullScreenSelector :: Selector '[RawId] ()
toggleFullScreenSelector = mkSelector "toggleFullScreen:"

-- | @Selector@ for @setFrameFromString:@
setFrameFromStringSelector :: Selector '[Id NSString] ()
setFrameFromStringSelector = mkSelector "setFrameFromString:"

-- | @Selector@ for @saveFrameUsingName:@
saveFrameUsingNameSelector :: Selector '[Id NSString] ()
saveFrameUsingNameSelector = mkSelector "saveFrameUsingName:"

-- | @Selector@ for @setFrameUsingName:force:@
setFrameUsingName_forceSelector :: Selector '[Id NSString, Bool] Bool
setFrameUsingName_forceSelector = mkSelector "setFrameUsingName:force:"

-- | @Selector@ for @setFrameUsingName:@
setFrameUsingNameSelector :: Selector '[Id NSString] Bool
setFrameUsingNameSelector = mkSelector "setFrameUsingName:"

-- | @Selector@ for @setFrameAutosaveName:@
setFrameAutosaveNameSelector :: Selector '[Id NSString] Bool
setFrameAutosaveNameSelector = mkSelector "setFrameAutosaveName:"

-- | @Selector@ for @removeFrameUsingName:@
removeFrameUsingNameSelector :: Selector '[Id NSString] ()
removeFrameUsingNameSelector = mkSelector "removeFrameUsingName:"

-- | @Selector@ for @beginSheet:completionHandler:@
beginSheet_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
beginSheet_completionHandlerSelector = mkSelector "beginSheet:completionHandler:"

-- | @Selector@ for @beginCriticalSheet:completionHandler:@
beginCriticalSheet_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
beginCriticalSheet_completionHandlerSelector = mkSelector "beginCriticalSheet:completionHandler:"

-- | @Selector@ for @endSheet:@
endSheetSelector :: Selector '[Id NSWindow] ()
endSheetSelector = mkSelector "endSheet:"

-- | @Selector@ for @endSheet:returnCode:@
endSheet_returnCodeSelector :: Selector '[Id NSWindow, CLong] ()
endSheet_returnCodeSelector = mkSelector "endSheet:returnCode:"

-- | @Selector@ for @standardWindowButton:forStyleMask:@
standardWindowButton_forStyleMaskSelector :: Selector '[NSWindowButton, NSWindowStyleMask] (Id NSButton)
standardWindowButton_forStyleMaskSelector = mkSelector "standardWindowButton:forStyleMask:"

-- | @Selector@ for @standardWindowButton:@
standardWindowButtonSelector :: Selector '[NSWindowButton] (Id NSButton)
standardWindowButtonSelector = mkSelector "standardWindowButton:"

-- | @Selector@ for @addChildWindow:ordered:@
addChildWindow_orderedSelector :: Selector '[Id NSWindow, NSWindowOrderingMode] ()
addChildWindow_orderedSelector = mkSelector "addChildWindow:ordered:"

-- | @Selector@ for @removeChildWindow:@
removeChildWindowSelector :: Selector '[Id NSWindow] ()
removeChildWindowSelector = mkSelector "removeChildWindow:"

-- | @Selector@ for @canRepresentDisplayGamut:@
canRepresentDisplayGamutSelector :: Selector '[NSDisplayGamut] Bool
canRepresentDisplayGamutSelector = mkSelector "canRepresentDisplayGamut:"

-- | @Selector@ for @windowNumbersWithOptions:@
windowNumbersWithOptionsSelector :: Selector '[NSWindowNumberListOptions] (Id NSArray)
windowNumbersWithOptionsSelector = mkSelector "windowNumbersWithOptions:"

-- | @Selector@ for @windowNumberAtPoint:belowWindowWithWindowNumber:@
windowNumberAtPoint_belowWindowWithWindowNumberSelector :: Selector '[NSPoint, CLong] CLong
windowNumberAtPoint_belowWindowWithWindowNumberSelector = mkSelector "windowNumberAtPoint:belowWindowWithWindowNumber:"

-- | @Selector@ for @windowWithContentViewController:@
windowWithContentViewControllerSelector :: Selector '[Id NSViewController] (Id NSWindow)
windowWithContentViewControllerSelector = mkSelector "windowWithContentViewController:"

-- | @Selector@ for @performWindowDragWithEvent:@
performWindowDragWithEventSelector :: Selector '[Id NSEvent] ()
performWindowDragWithEventSelector = mkSelector "performWindowDragWithEvent:"

-- | @Selector@ for @selectNextKeyView:@
selectNextKeyViewSelector :: Selector '[RawId] ()
selectNextKeyViewSelector = mkSelector "selectNextKeyView:"

-- | @Selector@ for @selectPreviousKeyView:@
selectPreviousKeyViewSelector :: Selector '[RawId] ()
selectPreviousKeyViewSelector = mkSelector "selectPreviousKeyView:"

-- | @Selector@ for @selectKeyViewFollowingView:@
selectKeyViewFollowingViewSelector :: Selector '[Id NSView] ()
selectKeyViewFollowingViewSelector = mkSelector "selectKeyViewFollowingView:"

-- | @Selector@ for @selectKeyViewPrecedingView:@
selectKeyViewPrecedingViewSelector :: Selector '[Id NSView] ()
selectKeyViewPrecedingViewSelector = mkSelector "selectKeyViewPrecedingView:"

-- | @Selector@ for @disableKeyEquivalentForDefaultButtonCell@
disableKeyEquivalentForDefaultButtonCellSelector :: Selector '[] ()
disableKeyEquivalentForDefaultButtonCellSelector = mkSelector "disableKeyEquivalentForDefaultButtonCell"

-- | @Selector@ for @enableKeyEquivalentForDefaultButtonCell@
enableKeyEquivalentForDefaultButtonCellSelector :: Selector '[] ()
enableKeyEquivalentForDefaultButtonCellSelector = mkSelector "enableKeyEquivalentForDefaultButtonCell"

-- | @Selector@ for @recalculateKeyViewLoop@
recalculateKeyViewLoopSelector :: Selector '[] ()
recalculateKeyViewLoopSelector = mkSelector "recalculateKeyViewLoop"

-- | @Selector@ for @toggleToolbarShown:@
toggleToolbarShownSelector :: Selector '[RawId] ()
toggleToolbarShownSelector = mkSelector "toggleToolbarShown:"

-- | @Selector@ for @runToolbarCustomizationPalette:@
runToolbarCustomizationPaletteSelector :: Selector '[RawId] ()
runToolbarCustomizationPaletteSelector = mkSelector "runToolbarCustomizationPalette:"

-- | @Selector@ for @selectNextTab:@
selectNextTabSelector :: Selector '[RawId] ()
selectNextTabSelector = mkSelector "selectNextTab:"

-- | @Selector@ for @selectPreviousTab:@
selectPreviousTabSelector :: Selector '[RawId] ()
selectPreviousTabSelector = mkSelector "selectPreviousTab:"

-- | @Selector@ for @moveTabToNewWindow:@
moveTabToNewWindowSelector :: Selector '[RawId] ()
moveTabToNewWindowSelector = mkSelector "moveTabToNewWindow:"

-- | @Selector@ for @mergeAllWindows:@
mergeAllWindowsSelector :: Selector '[RawId] ()
mergeAllWindowsSelector = mkSelector "mergeAllWindows:"

-- | @Selector@ for @toggleTabBar:@
toggleTabBarSelector :: Selector '[RawId] ()
toggleTabBarSelector = mkSelector "toggleTabBar:"

-- | @Selector@ for @toggleTabOverview:@
toggleTabOverviewSelector :: Selector '[RawId] ()
toggleTabOverviewSelector = mkSelector "toggleTabOverview:"

-- | @Selector@ for @addTabbedWindow:ordered:@
addTabbedWindow_orderedSelector :: Selector '[Id NSWindow, NSWindowOrderingMode] ()
addTabbedWindow_orderedSelector = mkSelector "addTabbedWindow:ordered:"

-- | @Selector@ for @transferWindowSharingToWindow:completionHandler:@
transferWindowSharingToWindow_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
transferWindowSharingToWindow_completionHandlerSelector = mkSelector "transferWindowSharingToWindow:completionHandler:"

-- | @Selector@ for @requestSharingOfWindow:completionHandler:@
requestSharingOfWindow_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
requestSharingOfWindow_completionHandlerSelector = mkSelector "requestSharingOfWindow:completionHandler:"

-- | @Selector@ for @requestSharingOfWindowUsingPreview:title:completionHandler:@
requestSharingOfWindowUsingPreview_title_completionHandlerSelector :: Selector '[Id NSImage, Id NSString, Ptr ()] ()
requestSharingOfWindowUsingPreview_title_completionHandlerSelector = mkSelector "requestSharingOfWindowUsingPreview:title:completionHandler:"

-- | @Selector@ for @disableSnapshotRestoration@
disableSnapshotRestorationSelector :: Selector '[] ()
disableSnapshotRestorationSelector = mkSelector "disableSnapshotRestoration"

-- | @Selector@ for @enableSnapshotRestoration@
enableSnapshotRestorationSelector :: Selector '[] ()
enableSnapshotRestorationSelector = mkSelector "enableSnapshotRestoration"

-- | @Selector@ for @setIsMiniaturized:@
setIsMiniaturizedSelector :: Selector '[Bool] ()
setIsMiniaturizedSelector = mkSelector "setIsMiniaturized:"

-- | @Selector@ for @setIsVisible:@
setIsVisibleSelector :: Selector '[Bool] ()
setIsVisibleSelector = mkSelector "setIsVisible:"

-- | @Selector@ for @setIsZoomed:@
setIsZoomedSelector :: Selector '[Bool] ()
setIsZoomedSelector = mkSelector "setIsZoomed:"

-- | @Selector@ for @handleCloseScriptCommand:@
handleCloseScriptCommandSelector :: Selector '[Id NSCloseCommand] RawId
handleCloseScriptCommandSelector = mkSelector "handleCloseScriptCommand:"

-- | @Selector@ for @handlePrintScriptCommand:@
handlePrintScriptCommandSelector :: Selector '[Id NSScriptCommand] RawId
handlePrintScriptCommandSelector = mkSelector "handlePrintScriptCommand:"

-- | @Selector@ for @handleSaveScriptCommand:@
handleSaveScriptCommandSelector :: Selector '[Id NSScriptCommand] RawId
handleSaveScriptCommandSelector = mkSelector "handleSaveScriptCommand:"

-- | @Selector@ for @visualizeConstraints:@
visualizeConstraintsSelector :: Selector '[Id NSArray] ()
visualizeConstraintsSelector = mkSelector "visualizeConstraints:"

-- | @Selector@ for @anchorAttributeForOrientation:@
anchorAttributeForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] NSLayoutAttribute
anchorAttributeForOrientationSelector = mkSelector "anchorAttributeForOrientation:"

-- | @Selector@ for @setAnchorAttribute:forOrientation:@
setAnchorAttribute_forOrientationSelector :: Selector '[NSLayoutAttribute, NSLayoutConstraintOrientation] ()
setAnchorAttribute_forOrientationSelector = mkSelector "setAnchorAttribute:forOrientation:"

-- | @Selector@ for @updateConstraintsIfNeeded@
updateConstraintsIfNeededSelector :: Selector '[] ()
updateConstraintsIfNeededSelector = mkSelector "updateConstraintsIfNeeded"

-- | @Selector@ for @layoutIfNeeded@
layoutIfNeededSelector :: Selector '[] ()
layoutIfNeededSelector = mkSelector "layoutIfNeeded"

-- | @Selector@ for @cacheImageInRect:@
cacheImageInRectSelector :: Selector '[NSRect] ()
cacheImageInRectSelector = mkSelector "cacheImageInRect:"

-- | @Selector@ for @restoreCachedImage@
restoreCachedImageSelector :: Selector '[] ()
restoreCachedImageSelector = mkSelector "restoreCachedImage"

-- | @Selector@ for @discardCachedImage@
discardCachedImageSelector :: Selector '[] ()
discardCachedImageSelector = mkSelector "discardCachedImage"

-- | @Selector@ for @menuChanged:@
menuChangedSelector :: Selector '[Id NSMenu] ()
menuChangedSelector = mkSelector "menuChanged:"

-- | @Selector@ for @gState@
gStateSelector :: Selector '[] CLong
gStateSelector = mkSelector "gState"

-- | @Selector@ for @convertBaseToScreen:@
convertBaseToScreenSelector :: Selector '[NSPoint] NSPoint
convertBaseToScreenSelector = mkSelector "convertBaseToScreen:"

-- | @Selector@ for @convertScreenToBase:@
convertScreenToBaseSelector :: Selector '[NSPoint] NSPoint
convertScreenToBaseSelector = mkSelector "convertScreenToBase:"

-- | @Selector@ for @userSpaceScaleFactor@
userSpaceScaleFactorSelector :: Selector '[] CDouble
userSpaceScaleFactorSelector = mkSelector "userSpaceScaleFactor"

-- | @Selector@ for @useOptimizedDrawing:@
useOptimizedDrawingSelector :: Selector '[Bool] ()
useOptimizedDrawingSelector = mkSelector "useOptimizedDrawing:"

-- | @Selector@ for @canStoreColor@
canStoreColorSelector :: Selector '[] Bool
canStoreColorSelector = mkSelector "canStoreColor"

-- | @Selector@ for @disableFlushWindow@
disableFlushWindowSelector :: Selector '[] ()
disableFlushWindowSelector = mkSelector "disableFlushWindow"

-- | @Selector@ for @enableFlushWindow@
enableFlushWindowSelector :: Selector '[] ()
enableFlushWindowSelector = mkSelector "enableFlushWindow"

-- | @Selector@ for @flushWindow@
flushWindowSelector :: Selector '[] ()
flushWindowSelector = mkSelector "flushWindow"

-- | @Selector@ for @flushWindowIfNeeded@
flushWindowIfNeededSelector :: Selector '[] ()
flushWindowIfNeededSelector = mkSelector "flushWindowIfNeeded"

-- | @Selector@ for @initWithWindowRef:@
initWithWindowRefSelector :: Selector '[Ptr ()] (Id NSWindow)
initWithWindowRefSelector = mkSelector "initWithWindowRef:"

-- | @Selector@ for @disableScreenUpdatesUntilFlush@
disableScreenUpdatesUntilFlushSelector :: Selector '[] ()
disableScreenUpdatesUntilFlushSelector = mkSelector "disableScreenUpdatesUntilFlush"

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector '[RawId, Sel] (Id CADisplayLink)
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_sourceSelector :: Selector '[Id NSArray, Id NSEvent, RawId] (Id NSDraggingSession)
beginDraggingSessionWithItems_event_sourceSelector = mkSelector "beginDraggingSessionWithItems:event:source:"

-- | @Selector@ for @dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBackSelector :: Selector '[Id NSImage, NSPoint, NSSize, Id NSEvent, Id NSPasteboard, RawId, Bool] ()
dragImage_at_offset_event_pasteboard_source_slideBackSelector = mkSelector "dragImage:at:offset:event:pasteboard:source:slideBack:"

-- | @Selector@ for @registerForDraggedTypes:@
registerForDraggedTypesSelector :: Selector '[Id NSArray] ()
registerForDraggedTypesSelector = mkSelector "registerForDraggedTypes:"

-- | @Selector@ for @unregisterDraggedTypes@
unregisterDraggedTypesSelector :: Selector '[] ()
unregisterDraggedTypesSelector = mkSelector "unregisterDraggedTypes"

-- | @Selector@ for @disableCursorRects@
disableCursorRectsSelector :: Selector '[] ()
disableCursorRectsSelector = mkSelector "disableCursorRects"

-- | @Selector@ for @enableCursorRects@
enableCursorRectsSelector :: Selector '[] ()
enableCursorRectsSelector = mkSelector "enableCursorRects"

-- | @Selector@ for @discardCursorRects@
discardCursorRectsSelector :: Selector '[] ()
discardCursorRectsSelector = mkSelector "discardCursorRects"

-- | @Selector@ for @invalidateCursorRectsForView:@
invalidateCursorRectsForViewSelector :: Selector '[Id NSView] ()
invalidateCursorRectsForViewSelector = mkSelector "invalidateCursorRectsForView:"

-- | @Selector@ for @resetCursorRects@
resetCursorRectsSelector :: Selector '[] ()
resetCursorRectsSelector = mkSelector "resetCursorRects"

-- | @Selector@ for @trackEventsMatchingMask:timeout:mode:handler:@
trackEventsMatchingMask_timeout_mode_handlerSelector :: Selector '[NSEventMask, CDouble, Id NSString, Ptr ()] ()
trackEventsMatchingMask_timeout_mode_handlerSelector = mkSelector "trackEventsMatchingMask:timeout:mode:handler:"

-- | @Selector@ for @nextEventMatchingMask:@
nextEventMatchingMaskSelector :: Selector '[NSEventMask] (Id NSEvent)
nextEventMatchingMaskSelector = mkSelector "nextEventMatchingMask:"

-- | @Selector@ for @nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeueSelector :: Selector '[NSEventMask, Id NSDate, Id NSString, Bool] (Id NSEvent)
nextEventMatchingMask_untilDate_inMode_dequeueSelector = mkSelector "nextEventMatchingMask:untilDate:inMode:dequeue:"

-- | @Selector@ for @discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEventSelector :: Selector '[NSEventMask, Id NSEvent] ()
discardEventsMatchingMask_beforeEventSelector = mkSelector "discardEventsMatchingMask:beforeEvent:"

-- | @Selector@ for @postEvent:atStart:@
postEvent_atStartSelector :: Selector '[Id NSEvent, Bool] ()
postEvent_atStartSelector = mkSelector "postEvent:atStart:"

-- | @Selector@ for @sendEvent:@
sendEventSelector :: Selector '[Id NSEvent] ()
sendEventSelector = mkSelector "sendEvent:"

-- | @Selector@ for @defaultDepthLimit@
defaultDepthLimitSelector :: Selector '[] NSWindowDepth
defaultDepthLimitSelector = mkSelector "defaultDepthLimit"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @titleVisibility@
titleVisibilitySelector :: Selector '[] NSWindowTitleVisibility
titleVisibilitySelector = mkSelector "titleVisibility"

-- | @Selector@ for @setTitleVisibility:@
setTitleVisibilitySelector :: Selector '[NSWindowTitleVisibility] ()
setTitleVisibilitySelector = mkSelector "setTitleVisibility:"

-- | @Selector@ for @titlebarAppearsTransparent@
titlebarAppearsTransparentSelector :: Selector '[] Bool
titlebarAppearsTransparentSelector = mkSelector "titlebarAppearsTransparent"

-- | @Selector@ for @setTitlebarAppearsTransparent:@
setTitlebarAppearsTransparentSelector :: Selector '[Bool] ()
setTitlebarAppearsTransparentSelector = mkSelector "setTitlebarAppearsTransparent:"

-- | @Selector@ for @toolbarStyle@
toolbarStyleSelector :: Selector '[] NSWindowToolbarStyle
toolbarStyleSelector = mkSelector "toolbarStyle"

-- | @Selector@ for @setToolbarStyle:@
setToolbarStyleSelector :: Selector '[NSWindowToolbarStyle] ()
setToolbarStyleSelector = mkSelector "setToolbarStyle:"

-- | @Selector@ for @contentLayoutRect@
contentLayoutRectSelector :: Selector '[] NSRect
contentLayoutRectSelector = mkSelector "contentLayoutRect"

-- | @Selector@ for @contentLayoutGuide@
contentLayoutGuideSelector :: Selector '[] RawId
contentLayoutGuideSelector = mkSelector "contentLayoutGuide"

-- | @Selector@ for @titlebarAccessoryViewControllers@
titlebarAccessoryViewControllersSelector :: Selector '[] (Id NSArray)
titlebarAccessoryViewControllersSelector = mkSelector "titlebarAccessoryViewControllers"

-- | @Selector@ for @setTitlebarAccessoryViewControllers:@
setTitlebarAccessoryViewControllersSelector :: Selector '[Id NSArray] ()
setTitlebarAccessoryViewControllersSelector = mkSelector "setTitlebarAccessoryViewControllers:"

-- | @Selector@ for @representedURL@
representedURLSelector :: Selector '[] (Id NSURL)
representedURLSelector = mkSelector "representedURL"

-- | @Selector@ for @setRepresentedURL:@
setRepresentedURLSelector :: Selector '[Id NSURL] ()
setRepresentedURLSelector = mkSelector "setRepresentedURL:"

-- | @Selector@ for @representedFilename@
representedFilenameSelector :: Selector '[] (Id NSString)
representedFilenameSelector = mkSelector "representedFilename"

-- | @Selector@ for @setRepresentedFilename:@
setRepresentedFilenameSelector :: Selector '[Id NSString] ()
setRepresentedFilenameSelector = mkSelector "setRepresentedFilename:"

-- | @Selector@ for @excludedFromWindowsMenu@
excludedFromWindowsMenuSelector :: Selector '[] Bool
excludedFromWindowsMenuSelector = mkSelector "excludedFromWindowsMenu"

-- | @Selector@ for @setExcludedFromWindowsMenu:@
setExcludedFromWindowsMenuSelector :: Selector '[Bool] ()
setExcludedFromWindowsMenuSelector = mkSelector "setExcludedFromWindowsMenu:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @windowNumber@
windowNumberSelector :: Selector '[] CLong
windowNumberSelector = mkSelector "windowNumber"

-- | @Selector@ for @styleMask@
styleMaskSelector :: Selector '[] NSWindowStyleMask
styleMaskSelector = mkSelector "styleMask"

-- | @Selector@ for @setStyleMask:@
setStyleMaskSelector :: Selector '[NSWindowStyleMask] ()
setStyleMaskSelector = mkSelector "setStyleMask:"

-- | @Selector@ for @cascadingReferenceFrame@
cascadingReferenceFrameSelector :: Selector '[] NSRect
cascadingReferenceFrameSelector = mkSelector "cascadingReferenceFrame"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @inLiveResize@
inLiveResizeSelector :: Selector '[] Bool
inLiveResizeSelector = mkSelector "inLiveResize"

-- | @Selector@ for @resizeIncrements@
resizeIncrementsSelector :: Selector '[] NSSize
resizeIncrementsSelector = mkSelector "resizeIncrements"

-- | @Selector@ for @setResizeIncrements:@
setResizeIncrementsSelector :: Selector '[NSSize] ()
setResizeIncrementsSelector = mkSelector "setResizeIncrements:"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector '[] NSSize
aspectRatioSelector = mkSelector "aspectRatio"

-- | @Selector@ for @setAspectRatio:@
setAspectRatioSelector :: Selector '[NSSize] ()
setAspectRatioSelector = mkSelector "setAspectRatio:"

-- | @Selector@ for @contentResizeIncrements@
contentResizeIncrementsSelector :: Selector '[] NSSize
contentResizeIncrementsSelector = mkSelector "contentResizeIncrements"

-- | @Selector@ for @setContentResizeIncrements:@
setContentResizeIncrementsSelector :: Selector '[NSSize] ()
setContentResizeIncrementsSelector = mkSelector "setContentResizeIncrements:"

-- | @Selector@ for @contentAspectRatio@
contentAspectRatioSelector :: Selector '[] NSSize
contentAspectRatioSelector = mkSelector "contentAspectRatio"

-- | @Selector@ for @setContentAspectRatio:@
setContentAspectRatioSelector :: Selector '[NSSize] ()
setContentAspectRatioSelector = mkSelector "setContentAspectRatio:"

-- | @Selector@ for @viewsNeedDisplay@
viewsNeedDisplaySelector :: Selector '[] Bool
viewsNeedDisplaySelector = mkSelector "viewsNeedDisplay"

-- | @Selector@ for @setViewsNeedDisplay:@
setViewsNeedDisplaySelector :: Selector '[Bool] ()
setViewsNeedDisplaySelector = mkSelector "setViewsNeedDisplay:"

-- | @Selector@ for @preservesContentDuringLiveResize@
preservesContentDuringLiveResizeSelector :: Selector '[] Bool
preservesContentDuringLiveResizeSelector = mkSelector "preservesContentDuringLiveResize"

-- | @Selector@ for @setPreservesContentDuringLiveResize:@
setPreservesContentDuringLiveResizeSelector :: Selector '[Bool] ()
setPreservesContentDuringLiveResizeSelector = mkSelector "setPreservesContentDuringLiveResize:"

-- | @Selector@ for @firstResponder@
firstResponderSelector :: Selector '[] (Id NSResponder)
firstResponderSelector = mkSelector "firstResponder"

-- | @Selector@ for @resizeFlags@
resizeFlagsSelector :: Selector '[] NSEventModifierFlags
resizeFlagsSelector = mkSelector "resizeFlags"

-- | @Selector@ for @releasedWhenClosed@
releasedWhenClosedSelector :: Selector '[] Bool
releasedWhenClosedSelector = mkSelector "releasedWhenClosed"

-- | @Selector@ for @setReleasedWhenClosed:@
setReleasedWhenClosedSelector :: Selector '[Bool] ()
setReleasedWhenClosedSelector = mkSelector "setReleasedWhenClosed:"

-- | @Selector@ for @zoomed@
zoomedSelector :: Selector '[] Bool
zoomedSelector = mkSelector "zoomed"

-- | @Selector@ for @miniaturized@
miniaturizedSelector :: Selector '[] Bool
miniaturizedSelector = mkSelector "miniaturized"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @movable@
movableSelector :: Selector '[] Bool
movableSelector = mkSelector "movable"

-- | @Selector@ for @setMovable:@
setMovableSelector :: Selector '[Bool] ()
setMovableSelector = mkSelector "setMovable:"

-- | @Selector@ for @movableByWindowBackground@
movableByWindowBackgroundSelector :: Selector '[] Bool
movableByWindowBackgroundSelector = mkSelector "movableByWindowBackground"

-- | @Selector@ for @setMovableByWindowBackground:@
setMovableByWindowBackgroundSelector :: Selector '[Bool] ()
setMovableByWindowBackgroundSelector = mkSelector "setMovableByWindowBackground:"

-- | @Selector@ for @hidesOnDeactivate@
hidesOnDeactivateSelector :: Selector '[] Bool
hidesOnDeactivateSelector = mkSelector "hidesOnDeactivate"

-- | @Selector@ for @setHidesOnDeactivate:@
setHidesOnDeactivateSelector :: Selector '[Bool] ()
setHidesOnDeactivateSelector = mkSelector "setHidesOnDeactivate:"

-- | @Selector@ for @canHide@
canHideSelector :: Selector '[] Bool
canHideSelector = mkSelector "canHide"

-- | @Selector@ for @setCanHide:@
setCanHideSelector :: Selector '[Bool] ()
setCanHideSelector = mkSelector "setCanHide:"

-- | @Selector@ for @miniwindowImage@
miniwindowImageSelector :: Selector '[] (Id NSImage)
miniwindowImageSelector = mkSelector "miniwindowImage"

-- | @Selector@ for @setMiniwindowImage:@
setMiniwindowImageSelector :: Selector '[Id NSImage] ()
setMiniwindowImageSelector = mkSelector "setMiniwindowImage:"

-- | @Selector@ for @miniwindowTitle@
miniwindowTitleSelector :: Selector '[] (Id NSString)
miniwindowTitleSelector = mkSelector "miniwindowTitle"

-- | @Selector@ for @setMiniwindowTitle:@
setMiniwindowTitleSelector :: Selector '[Id NSString] ()
setMiniwindowTitleSelector = mkSelector "setMiniwindowTitle:"

-- | @Selector@ for @dockTile@
dockTileSelector :: Selector '[] (Id NSDockTile)
dockTileSelector = mkSelector "dockTile"

-- | @Selector@ for @documentEdited@
documentEditedSelector :: Selector '[] Bool
documentEditedSelector = mkSelector "documentEdited"

-- | @Selector@ for @setDocumentEdited:@
setDocumentEditedSelector :: Selector '[Bool] ()
setDocumentEditedSelector = mkSelector "setDocumentEdited:"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @keyWindow@
keyWindowSelector :: Selector '[] Bool
keyWindowSelector = mkSelector "keyWindow"

-- | @Selector@ for @mainWindow@
mainWindowSelector :: Selector '[] Bool
mainWindowSelector = mkSelector "mainWindow"

-- | @Selector@ for @canBecomeKeyWindow@
canBecomeKeyWindowSelector :: Selector '[] Bool
canBecomeKeyWindowSelector = mkSelector "canBecomeKeyWindow"

-- | @Selector@ for @canBecomeMainWindow@
canBecomeMainWindowSelector :: Selector '[] Bool
canBecomeMainWindowSelector = mkSelector "canBecomeMainWindow"

-- | @Selector@ for @worksWhenModal@
worksWhenModalSelector :: Selector '[] Bool
worksWhenModalSelector = mkSelector "worksWhenModal"

-- | @Selector@ for @preventsApplicationTerminationWhenModal@
preventsApplicationTerminationWhenModalSelector :: Selector '[] Bool
preventsApplicationTerminationWhenModalSelector = mkSelector "preventsApplicationTerminationWhenModal"

-- | @Selector@ for @setPreventsApplicationTerminationWhenModal:@
setPreventsApplicationTerminationWhenModalSelector :: Selector '[Bool] ()
setPreventsApplicationTerminationWhenModalSelector = mkSelector "setPreventsApplicationTerminationWhenModal:"

-- | @Selector@ for @backingScaleFactor@
backingScaleFactorSelector :: Selector '[] CDouble
backingScaleFactorSelector = mkSelector "backingScaleFactor"

-- | @Selector@ for @allowsToolTipsWhenApplicationIsInactive@
allowsToolTipsWhenApplicationIsInactiveSelector :: Selector '[] Bool
allowsToolTipsWhenApplicationIsInactiveSelector = mkSelector "allowsToolTipsWhenApplicationIsInactive"

-- | @Selector@ for @setAllowsToolTipsWhenApplicationIsInactive:@
setAllowsToolTipsWhenApplicationIsInactiveSelector :: Selector '[Bool] ()
setAllowsToolTipsWhenApplicationIsInactiveSelector = mkSelector "setAllowsToolTipsWhenApplicationIsInactive:"

-- | @Selector@ for @backingType@
backingTypeSelector :: Selector '[] NSBackingStoreType
backingTypeSelector = mkSelector "backingType"

-- | @Selector@ for @setBackingType:@
setBackingTypeSelector :: Selector '[NSBackingStoreType] ()
setBackingTypeSelector = mkSelector "setBackingType:"

-- | @Selector@ for @level@
levelSelector :: Selector '[] CLong
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector '[CLong] ()
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @depthLimit@
depthLimitSelector :: Selector '[] NSWindowDepth
depthLimitSelector = mkSelector "depthLimit"

-- | @Selector@ for @setDepthLimit:@
setDepthLimitSelector :: Selector '[NSWindowDepth] ()
setDepthLimitSelector = mkSelector "setDepthLimit:"

-- | @Selector@ for @hasDynamicDepthLimit@
hasDynamicDepthLimitSelector :: Selector '[] Bool
hasDynamicDepthLimitSelector = mkSelector "hasDynamicDepthLimit"

-- | @Selector@ for @screen@
screenSelector :: Selector '[] (Id NSScreen)
screenSelector = mkSelector "screen"

-- | @Selector@ for @deepestScreen@
deepestScreenSelector :: Selector '[] (Id NSScreen)
deepestScreenSelector = mkSelector "deepestScreen"

-- | @Selector@ for @hasShadow@
hasShadowSelector :: Selector '[] Bool
hasShadowSelector = mkSelector "hasShadow"

-- | @Selector@ for @setHasShadow:@
setHasShadowSelector :: Selector '[Bool] ()
setHasShadowSelector = mkSelector "setHasShadow:"

-- | @Selector@ for @alphaValue@
alphaValueSelector :: Selector '[] CDouble
alphaValueSelector = mkSelector "alphaValue"

-- | @Selector@ for @setAlphaValue:@
setAlphaValueSelector :: Selector '[CDouble] ()
setAlphaValueSelector = mkSelector "setAlphaValue:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector '[Bool] ()
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @sharingType@
sharingTypeSelector :: Selector '[] NSWindowSharingType
sharingTypeSelector = mkSelector "sharingType"

-- | @Selector@ for @setSharingType:@
setSharingTypeSelector :: Selector '[NSWindowSharingType] ()
setSharingTypeSelector = mkSelector "setSharingType:"

-- | @Selector@ for @allowsConcurrentViewDrawing@
allowsConcurrentViewDrawingSelector :: Selector '[] Bool
allowsConcurrentViewDrawingSelector = mkSelector "allowsConcurrentViewDrawing"

-- | @Selector@ for @setAllowsConcurrentViewDrawing:@
setAllowsConcurrentViewDrawingSelector :: Selector '[Bool] ()
setAllowsConcurrentViewDrawingSelector = mkSelector "setAllowsConcurrentViewDrawing:"

-- | @Selector@ for @displaysWhenScreenProfileChanges@
displaysWhenScreenProfileChangesSelector :: Selector '[] Bool
displaysWhenScreenProfileChangesSelector = mkSelector "displaysWhenScreenProfileChanges"

-- | @Selector@ for @setDisplaysWhenScreenProfileChanges:@
setDisplaysWhenScreenProfileChangesSelector :: Selector '[Bool] ()
setDisplaysWhenScreenProfileChangesSelector = mkSelector "setDisplaysWhenScreenProfileChanges:"

-- | @Selector@ for @canBecomeVisibleWithoutLogin@
canBecomeVisibleWithoutLoginSelector :: Selector '[] Bool
canBecomeVisibleWithoutLoginSelector = mkSelector "canBecomeVisibleWithoutLogin"

-- | @Selector@ for @setCanBecomeVisibleWithoutLogin:@
setCanBecomeVisibleWithoutLoginSelector :: Selector '[Bool] ()
setCanBecomeVisibleWithoutLoginSelector = mkSelector "setCanBecomeVisibleWithoutLogin:"

-- | @Selector@ for @collectionBehavior@
collectionBehaviorSelector :: Selector '[] NSWindowCollectionBehavior
collectionBehaviorSelector = mkSelector "collectionBehavior"

-- | @Selector@ for @setCollectionBehavior:@
setCollectionBehaviorSelector :: Selector '[NSWindowCollectionBehavior] ()
setCollectionBehaviorSelector = mkSelector "setCollectionBehavior:"

-- | @Selector@ for @animationBehavior@
animationBehaviorSelector :: Selector '[] NSWindowAnimationBehavior
animationBehaviorSelector = mkSelector "animationBehavior"

-- | @Selector@ for @setAnimationBehavior:@
setAnimationBehaviorSelector :: Selector '[NSWindowAnimationBehavior] ()
setAnimationBehaviorSelector = mkSelector "setAnimationBehavior:"

-- | @Selector@ for @onActiveSpace@
onActiveSpaceSelector :: Selector '[] Bool
onActiveSpaceSelector = mkSelector "onActiveSpace"

-- | @Selector@ for @stringWithSavedFrame@
stringWithSavedFrameSelector :: Selector '[] (Id NSString)
stringWithSavedFrameSelector = mkSelector "stringWithSavedFrame"

-- | @Selector@ for @frameAutosaveName@
frameAutosaveNameSelector :: Selector '[] (Id NSString)
frameAutosaveNameSelector = mkSelector "frameAutosaveName"

-- | @Selector@ for @minSize@
minSizeSelector :: Selector '[] NSSize
minSizeSelector = mkSelector "minSize"

-- | @Selector@ for @setMinSize:@
setMinSizeSelector :: Selector '[NSSize] ()
setMinSizeSelector = mkSelector "setMinSize:"

-- | @Selector@ for @maxSize@
maxSizeSelector :: Selector '[] NSSize
maxSizeSelector = mkSelector "maxSize"

-- | @Selector@ for @setMaxSize:@
setMaxSizeSelector :: Selector '[NSSize] ()
setMaxSizeSelector = mkSelector "setMaxSize:"

-- | @Selector@ for @contentMinSize@
contentMinSizeSelector :: Selector '[] NSSize
contentMinSizeSelector = mkSelector "contentMinSize"

-- | @Selector@ for @setContentMinSize:@
setContentMinSizeSelector :: Selector '[NSSize] ()
setContentMinSizeSelector = mkSelector "setContentMinSize:"

-- | @Selector@ for @contentMaxSize@
contentMaxSizeSelector :: Selector '[] NSSize
contentMaxSizeSelector = mkSelector "contentMaxSize"

-- | @Selector@ for @setContentMaxSize:@
setContentMaxSizeSelector :: Selector '[NSSize] ()
setContentMaxSizeSelector = mkSelector "setContentMaxSize:"

-- | @Selector@ for @minFullScreenContentSize@
minFullScreenContentSizeSelector :: Selector '[] NSSize
minFullScreenContentSizeSelector = mkSelector "minFullScreenContentSize"

-- | @Selector@ for @setMinFullScreenContentSize:@
setMinFullScreenContentSizeSelector :: Selector '[NSSize] ()
setMinFullScreenContentSizeSelector = mkSelector "setMinFullScreenContentSize:"

-- | @Selector@ for @maxFullScreenContentSize@
maxFullScreenContentSizeSelector :: Selector '[] NSSize
maxFullScreenContentSizeSelector = mkSelector "maxFullScreenContentSize"

-- | @Selector@ for @setMaxFullScreenContentSize:@
setMaxFullScreenContentSizeSelector :: Selector '[NSSize] ()
setMaxFullScreenContentSizeSelector = mkSelector "setMaxFullScreenContentSize:"

-- | @Selector@ for @deviceDescription@
deviceDescriptionSelector :: Selector '[] (Id NSDictionary)
deviceDescriptionSelector = mkSelector "deviceDescription"

-- | @Selector@ for @windowController@
windowControllerSelector :: Selector '[] (Id NSWindowController)
windowControllerSelector = mkSelector "windowController"

-- | @Selector@ for @setWindowController:@
setWindowControllerSelector :: Selector '[Id NSWindowController] ()
setWindowControllerSelector = mkSelector "setWindowController:"

-- | @Selector@ for @sheets@
sheetsSelector :: Selector '[] (Id NSArray)
sheetsSelector = mkSelector "sheets"

-- | @Selector@ for @attachedSheet@
attachedSheetSelector :: Selector '[] (Id NSWindow)
attachedSheetSelector = mkSelector "attachedSheet"

-- | @Selector@ for @sheet@
sheetSelector :: Selector '[] Bool
sheetSelector = mkSelector "sheet"

-- | @Selector@ for @sheetParent@
sheetParentSelector :: Selector '[] (Id NSWindow)
sheetParentSelector = mkSelector "sheetParent"

-- | @Selector@ for @childWindows@
childWindowsSelector :: Selector '[] (Id NSArray)
childWindowsSelector = mkSelector "childWindows"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector '[] (Id NSWindow)
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector '[Id NSWindow] ()
setParentWindowSelector = mkSelector "setParentWindow:"

-- | @Selector@ for @appearanceSource@
appearanceSourceSelector :: Selector '[] (Id NSObject)
appearanceSourceSelector = mkSelector "appearanceSource"

-- | @Selector@ for @setAppearanceSource:@
setAppearanceSourceSelector :: Selector '[Id NSObject] ()
setAppearanceSourceSelector = mkSelector "setAppearanceSource:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSColorSpace)
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector '[Id NSColorSpace] ()
setColorSpaceSelector = mkSelector "setColorSpace:"

-- | @Selector@ for @occlusionState@
occlusionStateSelector :: Selector '[] NSWindowOcclusionState
occlusionStateSelector = mkSelector "occlusionState"

-- | @Selector@ for @titlebarSeparatorStyle@
titlebarSeparatorStyleSelector :: Selector '[] NSTitlebarSeparatorStyle
titlebarSeparatorStyleSelector = mkSelector "titlebarSeparatorStyle"

-- | @Selector@ for @setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyleSelector :: Selector '[NSTitlebarSeparatorStyle] ()
setTitlebarSeparatorStyleSelector = mkSelector "setTitlebarSeparatorStyle:"

-- | @Selector@ for @contentViewController@
contentViewControllerSelector :: Selector '[] (Id NSViewController)
contentViewControllerSelector = mkSelector "contentViewController"

-- | @Selector@ for @setContentViewController:@
setContentViewControllerSelector :: Selector '[Id NSViewController] ()
setContentViewControllerSelector = mkSelector "setContentViewController:"

-- | @Selector@ for @initialFirstResponder@
initialFirstResponderSelector :: Selector '[] (Id NSView)
initialFirstResponderSelector = mkSelector "initialFirstResponder"

-- | @Selector@ for @setInitialFirstResponder:@
setInitialFirstResponderSelector :: Selector '[Id NSView] ()
setInitialFirstResponderSelector = mkSelector "setInitialFirstResponder:"

-- | @Selector@ for @keyViewSelectionDirection@
keyViewSelectionDirectionSelector :: Selector '[] NSSelectionDirection
keyViewSelectionDirectionSelector = mkSelector "keyViewSelectionDirection"

-- | @Selector@ for @defaultButtonCell@
defaultButtonCellSelector :: Selector '[] (Id NSButtonCell)
defaultButtonCellSelector = mkSelector "defaultButtonCell"

-- | @Selector@ for @setDefaultButtonCell:@
setDefaultButtonCellSelector :: Selector '[Id NSButtonCell] ()
setDefaultButtonCellSelector = mkSelector "setDefaultButtonCell:"

-- | @Selector@ for @autorecalculatesKeyViewLoop@
autorecalculatesKeyViewLoopSelector :: Selector '[] Bool
autorecalculatesKeyViewLoopSelector = mkSelector "autorecalculatesKeyViewLoop"

-- | @Selector@ for @setAutorecalculatesKeyViewLoop:@
setAutorecalculatesKeyViewLoopSelector :: Selector '[Bool] ()
setAutorecalculatesKeyViewLoopSelector = mkSelector "setAutorecalculatesKeyViewLoop:"

-- | @Selector@ for @toolbar@
toolbarSelector :: Selector '[] (Id NSToolbar)
toolbarSelector = mkSelector "toolbar"

-- | @Selector@ for @setToolbar:@
setToolbarSelector :: Selector '[Id NSToolbar] ()
setToolbarSelector = mkSelector "setToolbar:"

-- | @Selector@ for @showsToolbarButton@
showsToolbarButtonSelector :: Selector '[] Bool
showsToolbarButtonSelector = mkSelector "showsToolbarButton"

-- | @Selector@ for @setShowsToolbarButton:@
setShowsToolbarButtonSelector :: Selector '[Bool] ()
setShowsToolbarButtonSelector = mkSelector "setShowsToolbarButton:"

-- | @Selector@ for @allowsAutomaticWindowTabbing@
allowsAutomaticWindowTabbingSelector :: Selector '[] Bool
allowsAutomaticWindowTabbingSelector = mkSelector "allowsAutomaticWindowTabbing"

-- | @Selector@ for @setAllowsAutomaticWindowTabbing:@
setAllowsAutomaticWindowTabbingSelector :: Selector '[Bool] ()
setAllowsAutomaticWindowTabbingSelector = mkSelector "setAllowsAutomaticWindowTabbing:"

-- | @Selector@ for @userTabbingPreference@
userTabbingPreferenceSelector :: Selector '[] NSWindowUserTabbingPreference
userTabbingPreferenceSelector = mkSelector "userTabbingPreference"

-- | @Selector@ for @tabbingMode@
tabbingModeSelector :: Selector '[] NSWindowTabbingMode
tabbingModeSelector = mkSelector "tabbingMode"

-- | @Selector@ for @setTabbingMode:@
setTabbingModeSelector :: Selector '[NSWindowTabbingMode] ()
setTabbingModeSelector = mkSelector "setTabbingMode:"

-- | @Selector@ for @tabbingIdentifier@
tabbingIdentifierSelector :: Selector '[] (Id NSString)
tabbingIdentifierSelector = mkSelector "tabbingIdentifier"

-- | @Selector@ for @setTabbingIdentifier:@
setTabbingIdentifierSelector :: Selector '[Id NSString] ()
setTabbingIdentifierSelector = mkSelector "setTabbingIdentifier:"

-- | @Selector@ for @tabbedWindows@
tabbedWindowsSelector :: Selector '[] (Id NSArray)
tabbedWindowsSelector = mkSelector "tabbedWindows"

-- | @Selector@ for @tab@
tabSelector :: Selector '[] (Id NSWindowTab)
tabSelector = mkSelector "tab"

-- | @Selector@ for @tabGroup@
tabGroupSelector :: Selector '[] (Id NSWindowTabGroup)
tabGroupSelector = mkSelector "tabGroup"

-- | @Selector@ for @hasActiveWindowSharingSession@
hasActiveWindowSharingSessionSelector :: Selector '[] Bool
hasActiveWindowSharingSessionSelector = mkSelector "hasActiveWindowSharingSession"

-- | @Selector@ for @windowTitlebarLayoutDirection@
windowTitlebarLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
windowTitlebarLayoutDirectionSelector = mkSelector "windowTitlebarLayoutDirection"

-- | @Selector@ for @restorable@
restorableSelector :: Selector '[] Bool
restorableSelector = mkSelector "restorable"

-- | @Selector@ for @setRestorable:@
setRestorableSelector :: Selector '[Bool] ()
setRestorableSelector = mkSelector "setRestorable:"

-- | @Selector@ for @restorationClass@
restorationClassSelector :: Selector '[] Class
restorationClassSelector = mkSelector "restorationClass"

-- | @Selector@ for @setRestorationClass:@
setRestorationClassSelector :: Selector '[Class] ()
setRestorationClassSelector = mkSelector "setRestorationClass:"

-- | @Selector@ for @hasCloseBox@
hasCloseBoxSelector :: Selector '[] Bool
hasCloseBoxSelector = mkSelector "hasCloseBox"

-- | @Selector@ for @hasTitleBar@
hasTitleBarSelector :: Selector '[] Bool
hasTitleBarSelector = mkSelector "hasTitleBar"

-- | @Selector@ for @floatingPanel@
floatingPanelSelector :: Selector '[] Bool
floatingPanelSelector = mkSelector "floatingPanel"

-- | @Selector@ for @miniaturizable@
miniaturizableSelector :: Selector '[] Bool
miniaturizableSelector = mkSelector "miniaturizable"

-- | @Selector@ for @modalPanel@
modalPanelSelector :: Selector '[] Bool
modalPanelSelector = mkSelector "modalPanel"

-- | @Selector@ for @resizable@
resizableSelector :: Selector '[] Bool
resizableSelector = mkSelector "resizable"

-- | @Selector@ for @zoomable@
zoomableSelector :: Selector '[] Bool
zoomableSelector = mkSelector "zoomable"

-- | @Selector@ for @orderedIndex@
orderedIndexSelector :: Selector '[] CLong
orderedIndexSelector = mkSelector "orderedIndex"

-- | @Selector@ for @setOrderedIndex:@
setOrderedIndexSelector :: Selector '[CLong] ()
setOrderedIndexSelector = mkSelector "setOrderedIndex:"

-- | @Selector@ for @drawers@
drawersSelector :: Selector '[] (Id NSArray)
drawersSelector = mkSelector "drawers"

-- | @Selector@ for @flushWindowDisabled@
flushWindowDisabledSelector :: Selector '[] Bool
flushWindowDisabledSelector = mkSelector "flushWindowDisabled"

-- | @Selector@ for @autodisplay@
autodisplaySelector :: Selector '[] Bool
autodisplaySelector = mkSelector "autodisplay"

-- | @Selector@ for @setAutodisplay:@
setAutodisplaySelector :: Selector '[Bool] ()
setAutodisplaySelector = mkSelector "setAutodisplay:"

-- | @Selector@ for @graphicsContext@
graphicsContextSelector :: Selector '[] (Id NSGraphicsContext)
graphicsContextSelector = mkSelector "graphicsContext"

-- | @Selector@ for @oneShot@
oneShotSelector :: Selector '[] Bool
oneShotSelector = mkSelector "oneShot"

-- | @Selector@ for @setOneShot:@
setOneShotSelector :: Selector '[Bool] ()
setOneShotSelector = mkSelector "setOneShot:"

-- | @Selector@ for @preferredBackingLocation@
preferredBackingLocationSelector :: Selector '[] NSWindowBackingLocation
preferredBackingLocationSelector = mkSelector "preferredBackingLocation"

-- | @Selector@ for @setPreferredBackingLocation:@
setPreferredBackingLocationSelector :: Selector '[NSWindowBackingLocation] ()
setPreferredBackingLocationSelector = mkSelector "setPreferredBackingLocation:"

-- | @Selector@ for @backingLocation@
backingLocationSelector :: Selector '[] NSWindowBackingLocation
backingLocationSelector = mkSelector "backingLocation"

-- | @Selector@ for @showsResizeIndicator@
showsResizeIndicatorSelector :: Selector '[] Bool
showsResizeIndicatorSelector = mkSelector "showsResizeIndicator"

-- | @Selector@ for @setShowsResizeIndicator:@
setShowsResizeIndicatorSelector :: Selector '[Bool] ()
setShowsResizeIndicatorSelector = mkSelector "setShowsResizeIndicator:"

-- | @Selector@ for @windowRef@
windowRefSelector :: Selector '[] (Ptr ())
windowRefSelector = mkSelector "windowRef"

-- | @Selector@ for @areCursorRectsEnabled@
areCursorRectsEnabledSelector :: Selector '[] Bool
areCursorRectsEnabledSelector = mkSelector "areCursorRectsEnabled"

-- | @Selector@ for @currentEvent@
currentEventSelector :: Selector '[] (Id NSEvent)
currentEventSelector = mkSelector "currentEvent"

-- | @Selector@ for @acceptsMouseMovedEvents@
acceptsMouseMovedEventsSelector :: Selector '[] Bool
acceptsMouseMovedEventsSelector = mkSelector "acceptsMouseMovedEvents"

-- | @Selector@ for @setAcceptsMouseMovedEvents:@
setAcceptsMouseMovedEventsSelector :: Selector '[Bool] ()
setAcceptsMouseMovedEventsSelector = mkSelector "setAcceptsMouseMovedEvents:"

-- | @Selector@ for @ignoresMouseEvents@
ignoresMouseEventsSelector :: Selector '[] Bool
ignoresMouseEventsSelector = mkSelector "ignoresMouseEvents"

-- | @Selector@ for @setIgnoresMouseEvents:@
setIgnoresMouseEventsSelector :: Selector '[Bool] ()
setIgnoresMouseEventsSelector = mkSelector "setIgnoresMouseEvents:"

-- | @Selector@ for @mouseLocationOutsideOfEventStream@
mouseLocationOutsideOfEventStreamSelector :: Selector '[] NSPoint
mouseLocationOutsideOfEventStreamSelector = mkSelector "mouseLocationOutsideOfEventStream"

