{-# LANGUAGE PatternSynonyms #-}
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
  , frameRectForContentRect_styleMaskSelector
  , contentRectForFrameRect_styleMaskSelector
  , minFrameWidthWithTitle_styleMaskSelector
  , frameRectForContentRectSelector
  , contentRectForFrameRectSelector
  , initWithContentRect_styleMask_backing_deferSelector
  , initWithContentRect_styleMask_backing_defer_screenSelector
  , initWithCoderSelector
  , addTitlebarAccessoryViewControllerSelector
  , insertTitlebarAccessoryViewController_atIndexSelector
  , removeTitlebarAccessoryViewControllerAtIndexSelector
  , setTitleWithRepresentedFilenameSelector
  , fieldEditor_forObjectSelector
  , endEditingForSelector
  , constrainFrameRect_toScreenSelector
  , setFrame_displaySelector
  , setContentSizeSelector
  , setFrameOriginSelector
  , setFrameTopLeftPointSelector
  , cascadeTopLeftFromPointSelector
  , animationResizeTimeSelector
  , setFrame_display_animateSelector
  , displayIfNeededSelector
  , displaySelector
  , updateSelector
  , makeFirstResponderSelector
  , closeSelector
  , miniaturizeSelector
  , deminiaturizeSelector
  , zoomSelector
  , tryToPerform_withSelector
  , validRequestorForSendType_returnTypeSelector
  , setContentBorderThickness_forEdgeSelector
  , contentBorderThicknessForEdgeSelector
  , setAutorecalculatesContentBorderThickness_forEdgeSelector
  , autorecalculatesContentBorderThicknessForEdgeSelector
  , centerSelector
  , makeKeyAndOrderFrontSelector
  , orderFrontSelector
  , orderBackSelector
  , orderOutSelector
  , orderWindow_relativeToSelector
  , orderFrontRegardlessSelector
  , makeKeyWindowSelector
  , makeMainWindowSelector
  , becomeKeyWindowSelector
  , resignKeyWindowSelector
  , becomeMainWindowSelector
  , resignMainWindowSelector
  , convertRectToScreenSelector
  , convertRectFromScreenSelector
  , convertPointToScreenSelector
  , convertPointFromScreenSelector
  , convertRectToBackingSelector
  , convertRectFromBackingSelector
  , convertPointToBackingSelector
  , convertPointFromBackingSelector
  , backingAlignedRect_optionsSelector
  , performCloseSelector
  , performMiniaturizeSelector
  , performZoomSelector
  , dataWithEPSInsideRectSelector
  , dataWithPDFInsideRectSelector
  , printSelector
  , setDynamicDepthLimitSelector
  , invalidateShadowSelector
  , toggleFullScreenSelector
  , setFrameFromStringSelector
  , saveFrameUsingNameSelector
  , setFrameUsingName_forceSelector
  , setFrameUsingNameSelector
  , setFrameAutosaveNameSelector
  , removeFrameUsingNameSelector
  , beginSheet_completionHandlerSelector
  , beginCriticalSheet_completionHandlerSelector
  , endSheetSelector
  , endSheet_returnCodeSelector
  , standardWindowButton_forStyleMaskSelector
  , standardWindowButtonSelector
  , addChildWindow_orderedSelector
  , removeChildWindowSelector
  , canRepresentDisplayGamutSelector
  , windowNumbersWithOptionsSelector
  , windowNumberAtPoint_belowWindowWithWindowNumberSelector
  , windowWithContentViewControllerSelector
  , performWindowDragWithEventSelector
  , selectNextKeyViewSelector
  , selectPreviousKeyViewSelector
  , selectKeyViewFollowingViewSelector
  , selectKeyViewPrecedingViewSelector
  , disableKeyEquivalentForDefaultButtonCellSelector
  , enableKeyEquivalentForDefaultButtonCellSelector
  , recalculateKeyViewLoopSelector
  , toggleToolbarShownSelector
  , runToolbarCustomizationPaletteSelector
  , selectNextTabSelector
  , selectPreviousTabSelector
  , moveTabToNewWindowSelector
  , mergeAllWindowsSelector
  , toggleTabBarSelector
  , toggleTabOverviewSelector
  , addTabbedWindow_orderedSelector
  , transferWindowSharingToWindow_completionHandlerSelector
  , requestSharingOfWindow_completionHandlerSelector
  , requestSharingOfWindowUsingPreview_title_completionHandlerSelector
  , disableSnapshotRestorationSelector
  , enableSnapshotRestorationSelector
  , setIsMiniaturizedSelector
  , setIsVisibleSelector
  , setIsZoomedSelector
  , handleCloseScriptCommandSelector
  , handlePrintScriptCommandSelector
  , handleSaveScriptCommandSelector
  , visualizeConstraintsSelector
  , anchorAttributeForOrientationSelector
  , setAnchorAttribute_forOrientationSelector
  , updateConstraintsIfNeededSelector
  , layoutIfNeededSelector
  , cacheImageInRectSelector
  , restoreCachedImageSelector
  , discardCachedImageSelector
  , menuChangedSelector
  , gStateSelector
  , convertBaseToScreenSelector
  , convertScreenToBaseSelector
  , userSpaceScaleFactorSelector
  , useOptimizedDrawingSelector
  , canStoreColorSelector
  , disableFlushWindowSelector
  , enableFlushWindowSelector
  , flushWindowSelector
  , flushWindowIfNeededSelector
  , initWithWindowRefSelector
  , disableScreenUpdatesUntilFlushSelector
  , displayLinkWithTarget_selectorSelector
  , beginDraggingSessionWithItems_event_sourceSelector
  , dragImage_at_offset_event_pasteboard_source_slideBackSelector
  , registerForDraggedTypesSelector
  , unregisterDraggedTypesSelector
  , disableCursorRectsSelector
  , enableCursorRectsSelector
  , discardCursorRectsSelector
  , invalidateCursorRectsForViewSelector
  , resetCursorRectsSelector
  , trackEventsMatchingMask_timeout_mode_handlerSelector
  , nextEventMatchingMaskSelector
  , nextEventMatchingMask_untilDate_inMode_dequeueSelector
  , discardEventsMatchingMask_beforeEventSelector
  , postEvent_atStartSelector
  , sendEventSelector
  , defaultDepthLimitSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , titleVisibilitySelector
  , setTitleVisibilitySelector
  , titlebarAppearsTransparentSelector
  , setTitlebarAppearsTransparentSelector
  , toolbarStyleSelector
  , setToolbarStyleSelector
  , contentLayoutRectSelector
  , contentLayoutGuideSelector
  , titlebarAccessoryViewControllersSelector
  , setTitlebarAccessoryViewControllersSelector
  , representedURLSelector
  , setRepresentedURLSelector
  , representedFilenameSelector
  , setRepresentedFilenameSelector
  , excludedFromWindowsMenuSelector
  , setExcludedFromWindowsMenuSelector
  , contentViewSelector
  , setContentViewSelector
  , delegateSelector
  , setDelegateSelector
  , windowNumberSelector
  , styleMaskSelector
  , setStyleMaskSelector
  , cascadingReferenceFrameSelector
  , frameSelector
  , inLiveResizeSelector
  , resizeIncrementsSelector
  , setResizeIncrementsSelector
  , aspectRatioSelector
  , setAspectRatioSelector
  , contentResizeIncrementsSelector
  , setContentResizeIncrementsSelector
  , contentAspectRatioSelector
  , setContentAspectRatioSelector
  , viewsNeedDisplaySelector
  , setViewsNeedDisplaySelector
  , preservesContentDuringLiveResizeSelector
  , setPreservesContentDuringLiveResizeSelector
  , firstResponderSelector
  , resizeFlagsSelector
  , releasedWhenClosedSelector
  , setReleasedWhenClosedSelector
  , zoomedSelector
  , miniaturizedSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , movableSelector
  , setMovableSelector
  , movableByWindowBackgroundSelector
  , setMovableByWindowBackgroundSelector
  , hidesOnDeactivateSelector
  , setHidesOnDeactivateSelector
  , canHideSelector
  , setCanHideSelector
  , miniwindowImageSelector
  , setMiniwindowImageSelector
  , miniwindowTitleSelector
  , setMiniwindowTitleSelector
  , dockTileSelector
  , documentEditedSelector
  , setDocumentEditedSelector
  , visibleSelector
  , keyWindowSelector
  , mainWindowSelector
  , canBecomeKeyWindowSelector
  , canBecomeMainWindowSelector
  , worksWhenModalSelector
  , preventsApplicationTerminationWhenModalSelector
  , setPreventsApplicationTerminationWhenModalSelector
  , backingScaleFactorSelector
  , allowsToolTipsWhenApplicationIsInactiveSelector
  , setAllowsToolTipsWhenApplicationIsInactiveSelector
  , backingTypeSelector
  , setBackingTypeSelector
  , levelSelector
  , setLevelSelector
  , depthLimitSelector
  , setDepthLimitSelector
  , hasDynamicDepthLimitSelector
  , screenSelector
  , deepestScreenSelector
  , hasShadowSelector
  , setHasShadowSelector
  , alphaValueSelector
  , setAlphaValueSelector
  , opaqueSelector
  , setOpaqueSelector
  , sharingTypeSelector
  , setSharingTypeSelector
  , allowsConcurrentViewDrawingSelector
  , setAllowsConcurrentViewDrawingSelector
  , displaysWhenScreenProfileChangesSelector
  , setDisplaysWhenScreenProfileChangesSelector
  , canBecomeVisibleWithoutLoginSelector
  , setCanBecomeVisibleWithoutLoginSelector
  , collectionBehaviorSelector
  , setCollectionBehaviorSelector
  , animationBehaviorSelector
  , setAnimationBehaviorSelector
  , onActiveSpaceSelector
  , stringWithSavedFrameSelector
  , frameAutosaveNameSelector
  , minSizeSelector
  , setMinSizeSelector
  , maxSizeSelector
  , setMaxSizeSelector
  , contentMinSizeSelector
  , setContentMinSizeSelector
  , contentMaxSizeSelector
  , setContentMaxSizeSelector
  , minFullScreenContentSizeSelector
  , setMinFullScreenContentSizeSelector
  , maxFullScreenContentSizeSelector
  , setMaxFullScreenContentSizeSelector
  , deviceDescriptionSelector
  , windowControllerSelector
  , setWindowControllerSelector
  , sheetsSelector
  , attachedSheetSelector
  , sheetSelector
  , sheetParentSelector
  , childWindowsSelector
  , parentWindowSelector
  , setParentWindowSelector
  , appearanceSourceSelector
  , setAppearanceSourceSelector
  , colorSpaceSelector
  , setColorSpaceSelector
  , occlusionStateSelector
  , titlebarSeparatorStyleSelector
  , setTitlebarSeparatorStyleSelector
  , contentViewControllerSelector
  , setContentViewControllerSelector
  , initialFirstResponderSelector
  , setInitialFirstResponderSelector
  , keyViewSelectionDirectionSelector
  , defaultButtonCellSelector
  , setDefaultButtonCellSelector
  , autorecalculatesKeyViewLoopSelector
  , setAutorecalculatesKeyViewLoopSelector
  , toolbarSelector
  , setToolbarSelector
  , showsToolbarButtonSelector
  , setShowsToolbarButtonSelector
  , allowsAutomaticWindowTabbingSelector
  , setAllowsAutomaticWindowTabbingSelector
  , userTabbingPreferenceSelector
  , tabbingModeSelector
  , setTabbingModeSelector
  , tabbingIdentifierSelector
  , setTabbingIdentifierSelector
  , tabbedWindowsSelector
  , tabSelector
  , tabGroupSelector
  , hasActiveWindowSharingSessionSelector
  , windowTitlebarLayoutDirectionSelector
  , restorableSelector
  , setRestorableSelector
  , restorationClassSelector
  , setRestorationClassSelector
  , hasCloseBoxSelector
  , hasTitleBarSelector
  , floatingPanelSelector
  , miniaturizableSelector
  , modalPanelSelector
  , resizableSelector
  , zoomableSelector
  , orderedIndexSelector
  , setOrderedIndexSelector
  , drawersSelector
  , flushWindowDisabledSelector
  , autodisplaySelector
  , setAutodisplaySelector
  , graphicsContextSelector
  , oneShotSelector
  , setOneShotSelector
  , preferredBackingLocationSelector
  , setPreferredBackingLocationSelector
  , backingLocationSelector
  , showsResizeIndicatorSelector
  , setShowsResizeIndicatorSelector
  , windowRefSelector
  , areCursorRectsEnabledSelector
  , currentEventSelector
  , acceptsMouseMovedEventsSelector
  , setAcceptsMouseMovedEventsSelector
  , ignoresMouseEventsSelector
  , setIgnoresMouseEventsSelector
  , mouseLocationOutsideOfEventStreamSelector

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

-- | @+ frameRectForContentRect:styleMask:@
frameRectForContentRect_styleMask :: NSRect -> NSWindowStyleMask -> IO NSRect
frameRectForContentRect_styleMask cRect style =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMsgStret cls' (mkSelector "frameRectForContentRect:styleMask:") retNSRect [argNSRect cRect, argCULong (coerce style)]

-- | @+ contentRectForFrameRect:styleMask:@
contentRectForFrameRect_styleMask :: NSRect -> NSWindowStyleMask -> IO NSRect
contentRectForFrameRect_styleMask fRect style =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMsgStret cls' (mkSelector "contentRectForFrameRect:styleMask:") retNSRect [argNSRect fRect, argCULong (coerce style)]

-- | @+ minFrameWidthWithTitle:styleMask:@
minFrameWidthWithTitle_styleMask :: IsNSString title => title -> NSWindowStyleMask -> IO CDouble
minFrameWidthWithTitle_styleMask title style =
  do
    cls' <- getRequiredClass "NSWindow"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "minFrameWidthWithTitle:styleMask:") retCDouble [argPtr (castPtr raw_title :: Ptr ()), argCULong (coerce style)]

-- | @- frameRectForContentRect:@
frameRectForContentRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
frameRectForContentRect nsWindow  contentRect =
    sendMsgStret nsWindow (mkSelector "frameRectForContentRect:") retNSRect [argNSRect contentRect]

-- | @- contentRectForFrameRect:@
contentRectForFrameRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
contentRectForFrameRect nsWindow  frameRect =
    sendMsgStret nsWindow (mkSelector "contentRectForFrameRect:") retNSRect [argNSRect frameRect]

-- | @- initWithContentRect:styleMask:backing:defer:@
initWithContentRect_styleMask_backing_defer :: IsNSWindow nsWindow => nsWindow -> NSRect -> NSWindowStyleMask -> NSBackingStoreType -> Bool -> IO (Id NSWindow)
initWithContentRect_styleMask_backing_defer nsWindow  contentRect style backingStoreType flag =
    sendMsg nsWindow (mkSelector "initWithContentRect:styleMask:backing:defer:") (retPtr retVoid) [argNSRect contentRect, argCULong (coerce style), argCULong (coerce backingStoreType), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithContentRect:styleMask:backing:defer:screen:@
initWithContentRect_styleMask_backing_defer_screen :: (IsNSWindow nsWindow, IsNSScreen screen) => nsWindow -> NSRect -> NSWindowStyleMask -> NSBackingStoreType -> Bool -> screen -> IO (Id NSWindow)
initWithContentRect_styleMask_backing_defer_screen nsWindow  contentRect style backingStoreType flag screen =
  withObjCPtr screen $ \raw_screen ->
      sendMsg nsWindow (mkSelector "initWithContentRect:styleMask:backing:defer:screen:") (retPtr retVoid) [argNSRect contentRect, argCULong (coerce style), argCULong (coerce backingStoreType), argCULong (if flag then 1 else 0), argPtr (castPtr raw_screen :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSWindow nsWindow, IsNSCoder coder) => nsWindow -> coder -> IO (Id NSWindow)
initWithCoder nsWindow  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsWindow (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- addTitlebarAccessoryViewController:@
addTitlebarAccessoryViewController :: (IsNSWindow nsWindow, IsNSTitlebarAccessoryViewController childViewController) => nsWindow -> childViewController -> IO ()
addTitlebarAccessoryViewController nsWindow  childViewController =
  withObjCPtr childViewController $ \raw_childViewController ->
      sendMsg nsWindow (mkSelector "addTitlebarAccessoryViewController:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ())]

-- | @- insertTitlebarAccessoryViewController:atIndex:@
insertTitlebarAccessoryViewController_atIndex :: (IsNSWindow nsWindow, IsNSTitlebarAccessoryViewController childViewController) => nsWindow -> childViewController -> CLong -> IO ()
insertTitlebarAccessoryViewController_atIndex nsWindow  childViewController index =
  withObjCPtr childViewController $ \raw_childViewController ->
      sendMsg nsWindow (mkSelector "insertTitlebarAccessoryViewController:atIndex:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ()), argCLong index]

-- | @- removeTitlebarAccessoryViewControllerAtIndex:@
removeTitlebarAccessoryViewControllerAtIndex :: IsNSWindow nsWindow => nsWindow -> CLong -> IO ()
removeTitlebarAccessoryViewControllerAtIndex nsWindow  index =
    sendMsg nsWindow (mkSelector "removeTitlebarAccessoryViewControllerAtIndex:") retVoid [argCLong index]

-- | @- setTitleWithRepresentedFilename:@
setTitleWithRepresentedFilename :: (IsNSWindow nsWindow, IsNSString filename) => nsWindow -> filename -> IO ()
setTitleWithRepresentedFilename nsWindow  filename =
  withObjCPtr filename $ \raw_filename ->
      sendMsg nsWindow (mkSelector "setTitleWithRepresentedFilename:") retVoid [argPtr (castPtr raw_filename :: Ptr ())]

-- | @- fieldEditor:forObject:@
fieldEditor_forObject :: IsNSWindow nsWindow => nsWindow -> Bool -> RawId -> IO (Id NSText)
fieldEditor_forObject nsWindow  createFlag object =
    sendMsg nsWindow (mkSelector "fieldEditor:forObject:") (retPtr retVoid) [argCULong (if createFlag then 1 else 0), argPtr (castPtr (unRawId object) :: Ptr ())] >>= retainedObject . castPtr

-- | @- endEditingFor:@
endEditingFor :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
endEditingFor nsWindow  object =
    sendMsg nsWindow (mkSelector "endEditingFor:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- constrainFrameRect:toScreen:@
constrainFrameRect_toScreen :: (IsNSWindow nsWindow, IsNSScreen screen) => nsWindow -> NSRect -> screen -> IO NSRect
constrainFrameRect_toScreen nsWindow  frameRect screen =
  withObjCPtr screen $ \raw_screen ->
      sendMsgStret nsWindow (mkSelector "constrainFrameRect:toScreen:") retNSRect [argNSRect frameRect, argPtr (castPtr raw_screen :: Ptr ())]

-- | @- setFrame:display:@
setFrame_display :: IsNSWindow nsWindow => nsWindow -> NSRect -> Bool -> IO ()
setFrame_display nsWindow  frameRect flag =
    sendMsg nsWindow (mkSelector "setFrame:display:") retVoid [argNSRect frameRect, argCULong (if flag then 1 else 0)]

-- | @- setContentSize:@
setContentSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentSize nsWindow  size =
    sendMsg nsWindow (mkSelector "setContentSize:") retVoid [argNSSize size]

-- | @- setFrameOrigin:@
setFrameOrigin :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO ()
setFrameOrigin nsWindow  point =
    sendMsg nsWindow (mkSelector "setFrameOrigin:") retVoid [argNSPoint point]

-- | @- setFrameTopLeftPoint:@
setFrameTopLeftPoint :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO ()
setFrameTopLeftPoint nsWindow  point =
    sendMsg nsWindow (mkSelector "setFrameTopLeftPoint:") retVoid [argNSPoint point]

-- | @- cascadeTopLeftFromPoint:@
cascadeTopLeftFromPoint :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
cascadeTopLeftFromPoint nsWindow  topLeftPoint =
    sendMsgStret nsWindow (mkSelector "cascadeTopLeftFromPoint:") retNSPoint [argNSPoint topLeftPoint]

-- | Subclasses can override @animationResizeTime:@ to control the total time for the frame change. @newFrame@ is the rect passed into @setFrame:display:animate:@
--
-- ObjC selector: @- animationResizeTime:@
animationResizeTime :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO CDouble
animationResizeTime nsWindow  newFrame =
    sendMsg nsWindow (mkSelector "animationResizeTime:") retCDouble [argNSRect newFrame]

-- | @setFrame:display:animate:@ is equivalent to @setFrame:display:@ if the @animateFlag@ is NO.    If the @animationFlag@ is YES, this method will perform a smooth resize of the window, where the total time for the resize is specified by @-animationResizeTime:@
--
-- ObjC selector: @- setFrame:display:animate:@
setFrame_display_animate :: IsNSWindow nsWindow => nsWindow -> NSRect -> Bool -> Bool -> IO ()
setFrame_display_animate nsWindow  frameRect displayFlag animateFlag =
    sendMsg nsWindow (mkSelector "setFrame:display:animate:") retVoid [argNSRect frameRect, argCULong (if displayFlag then 1 else 0), argCULong (if animateFlag then 1 else 0)]

-- | @- displayIfNeeded@
displayIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
displayIfNeeded nsWindow  =
    sendMsg nsWindow (mkSelector "displayIfNeeded") retVoid []

-- | @- display@
display :: IsNSWindow nsWindow => nsWindow -> IO ()
display nsWindow  =
    sendMsg nsWindow (mkSelector "display") retVoid []

-- | @- update@
update :: IsNSWindow nsWindow => nsWindow -> IO ()
update nsWindow  =
    sendMsg nsWindow (mkSelector "update") retVoid []

-- | @- makeFirstResponder:@
makeFirstResponder :: (IsNSWindow nsWindow, IsNSResponder responder) => nsWindow -> responder -> IO Bool
makeFirstResponder nsWindow  responder =
  withObjCPtr responder $ \raw_responder ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "makeFirstResponder:") retCULong [argPtr (castPtr raw_responder :: Ptr ())]

-- | @- close@
close :: IsNSWindow nsWindow => nsWindow -> IO ()
close nsWindow  =
    sendMsg nsWindow (mkSelector "close") retVoid []

-- | @- miniaturize:@
miniaturize :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
miniaturize nsWindow  sender =
    sendMsg nsWindow (mkSelector "miniaturize:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- deminiaturize:@
deminiaturize :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
deminiaturize nsWindow  sender =
    sendMsg nsWindow (mkSelector "deminiaturize:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- zoom:@
zoom :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
zoom nsWindow  sender =
    sendMsg nsWindow (mkSelector "zoom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- tryToPerform:with:@
tryToPerform_with :: IsNSWindow nsWindow => nsWindow -> Selector -> RawId -> IO Bool
tryToPerform_with nsWindow  action object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "tryToPerform:with:") retCULong [argPtr (unSelector action), argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSWindow nsWindow, IsNSString sendType, IsNSString returnType) => nsWindow -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsWindow  sendType returnType =
  withObjCPtr sendType $ \raw_sendType ->
    withObjCPtr returnType $ \raw_returnType ->
        fmap (RawId . castPtr) $ sendMsg nsWindow (mkSelector "validRequestorForSendType:returnType:") (retPtr retVoid) [argPtr (castPtr raw_sendType :: Ptr ()), argPtr (castPtr raw_returnType :: Ptr ())]

-- | @- setContentBorderThickness:forEdge:@
setContentBorderThickness_forEdge :: IsNSWindow nsWindow => nsWindow -> CDouble -> NSRectEdge -> IO ()
setContentBorderThickness_forEdge nsWindow  thickness edge =
    sendMsg nsWindow (mkSelector "setContentBorderThickness:forEdge:") retVoid [argCDouble thickness, argCULong (coerce edge)]

-- | @- contentBorderThicknessForEdge:@
contentBorderThicknessForEdge :: IsNSWindow nsWindow => nsWindow -> NSRectEdge -> IO CDouble
contentBorderThicknessForEdge nsWindow  edge =
    sendMsg nsWindow (mkSelector "contentBorderThicknessForEdge:") retCDouble [argCULong (coerce edge)]

-- | @- setAutorecalculatesContentBorderThickness:forEdge:@
setAutorecalculatesContentBorderThickness_forEdge :: IsNSWindow nsWindow => nsWindow -> Bool -> NSRectEdge -> IO ()
setAutorecalculatesContentBorderThickness_forEdge nsWindow  flag edge =
    sendMsg nsWindow (mkSelector "setAutorecalculatesContentBorderThickness:forEdge:") retVoid [argCULong (if flag then 1 else 0), argCULong (coerce edge)]

-- | @- autorecalculatesContentBorderThicknessForEdge:@
autorecalculatesContentBorderThicknessForEdge :: IsNSWindow nsWindow => nsWindow -> NSRectEdge -> IO Bool
autorecalculatesContentBorderThicknessForEdge nsWindow  edge =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "autorecalculatesContentBorderThicknessForEdge:") retCULong [argCULong (coerce edge)]

-- | @- center@
center :: IsNSWindow nsWindow => nsWindow -> IO ()
center nsWindow  =
    sendMsg nsWindow (mkSelector "center") retVoid []

-- | @- makeKeyAndOrderFront:@
makeKeyAndOrderFront :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
makeKeyAndOrderFront nsWindow  sender =
    sendMsg nsWindow (mkSelector "makeKeyAndOrderFront:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFront:@
orderFront :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
orderFront nsWindow  sender =
    sendMsg nsWindow (mkSelector "orderFront:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderBack:@
orderBack :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
orderBack nsWindow  sender =
    sendMsg nsWindow (mkSelector "orderBack:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderOut:@
orderOut :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
orderOut nsWindow  sender =
    sendMsg nsWindow (mkSelector "orderOut:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderWindow:relativeTo:@
orderWindow_relativeTo :: IsNSWindow nsWindow => nsWindow -> NSWindowOrderingMode -> CLong -> IO ()
orderWindow_relativeTo nsWindow  place otherWin =
    sendMsg nsWindow (mkSelector "orderWindow:relativeTo:") retVoid [argCLong (coerce place), argCLong otherWin]

-- | @- orderFrontRegardless@
orderFrontRegardless :: IsNSWindow nsWindow => nsWindow -> IO ()
orderFrontRegardless nsWindow  =
    sendMsg nsWindow (mkSelector "orderFrontRegardless") retVoid []

-- | Makes the window key and main if eligible, updating NSApplication's @-keyWindow@ and @-mainWindow@ properties.
--
-- ObjC selector: @- makeKeyWindow@
makeKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
makeKeyWindow nsWindow  =
    sendMsg nsWindow (mkSelector "makeKeyWindow") retVoid []

-- | Makes the window main if eligible. Updates NSApplication's @-mainWindow@ property.
--
-- ObjC selector: @- makeMainWindow@
makeMainWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
makeMainWindow nsWindow  =
    sendMsg nsWindow (mkSelector "makeMainWindow") retVoid []

-- | Informs the window that it has become the key window. This method exists as an override point. Do not invoke directly. Instead, invoke @-makeKeyWindow@.
--
-- ObjC selector: @- becomeKeyWindow@
becomeKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
becomeKeyWindow nsWindow  =
    sendMsg nsWindow (mkSelector "becomeKeyWindow") retVoid []

-- | Informs the window that it has stopped being the key window. This method exists as an override point. Do not invoke directly. Windows automatically receive this message when deactivating or when another window has become key.
--
-- ObjC selector: @- resignKeyWindow@
resignKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
resignKeyWindow nsWindow  =
    sendMsg nsWindow (mkSelector "resignKeyWindow") retVoid []

-- | Informs the window that it has become the main window. This method exists as an override point. Do not invoke directly. Instead, invoke @-makeMainWindow@.
--
-- ObjC selector: @- becomeMainWindow@
becomeMainWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
becomeMainWindow nsWindow  =
    sendMsg nsWindow (mkSelector "becomeMainWindow") retVoid []

-- | Informs the window that it has stopped being the main window. This method exists as an override point. Do not invoke directly. Windows automatically receive this message when deactivating or when another window has become main.
--
-- ObjC selector: @- resignMainWindow@
resignMainWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
resignMainWindow nsWindow  =
    sendMsg nsWindow (mkSelector "resignMainWindow") retVoid []

-- | @- convertRectToScreen:@
convertRectToScreen :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectToScreen nsWindow  rect =
    sendMsgStret nsWindow (mkSelector "convertRectToScreen:") retNSRect [argNSRect rect]

-- | @- convertRectFromScreen:@
convertRectFromScreen :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectFromScreen nsWindow  rect =
    sendMsgStret nsWindow (mkSelector "convertRectFromScreen:") retNSRect [argNSRect rect]

-- | @- convertPointToScreen:@
convertPointToScreen :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointToScreen nsWindow  point =
    sendMsgStret nsWindow (mkSelector "convertPointToScreen:") retNSPoint [argNSPoint point]

-- | @- convertPointFromScreen:@
convertPointFromScreen :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointFromScreen nsWindow  point =
    sendMsgStret nsWindow (mkSelector "convertPointFromScreen:") retNSPoint [argNSPoint point]

-- | @- convertRectToBacking:@
convertRectToBacking :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectToBacking nsWindow  rect =
    sendMsgStret nsWindow (mkSelector "convertRectToBacking:") retNSRect [argNSRect rect]

-- | @- convertRectFromBacking:@
convertRectFromBacking :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO NSRect
convertRectFromBacking nsWindow  rect =
    sendMsgStret nsWindow (mkSelector "convertRectFromBacking:") retNSRect [argNSRect rect]

-- | @- convertPointToBacking:@
convertPointToBacking :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointToBacking nsWindow  point =
    sendMsgStret nsWindow (mkSelector "convertPointToBacking:") retNSPoint [argNSPoint point]

-- | @- convertPointFromBacking:@
convertPointFromBacking :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertPointFromBacking nsWindow  point =
    sendMsgStret nsWindow (mkSelector "convertPointFromBacking:") retNSPoint [argNSPoint point]

-- | Use @NSIntegralRectWithOptions()@ to produce a backing store pixel aligned rectangle from the given input rectangle in window coordinates.
--
-- ObjC selector: @- backingAlignedRect:options:@
backingAlignedRect_options :: IsNSWindow nsWindow => nsWindow -> NSRect -> NSAlignmentOptions -> IO NSRect
backingAlignedRect_options nsWindow  rect options =
    sendMsgStret nsWindow (mkSelector "backingAlignedRect:options:") retNSRect [argNSRect rect, argCULong (coerce options)]

-- | @- performClose:@
performClose :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
performClose nsWindow  sender =
    sendMsg nsWindow (mkSelector "performClose:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- performMiniaturize:@
performMiniaturize :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
performMiniaturize nsWindow  sender =
    sendMsg nsWindow (mkSelector "performMiniaturize:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- performZoom:@
performZoom :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
performZoom nsWindow  sender =
    sendMsg nsWindow (mkSelector "performZoom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- dataWithEPSInsideRect:@
dataWithEPSInsideRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO (Id NSData)
dataWithEPSInsideRect nsWindow  rect =
    sendMsg nsWindow (mkSelector "dataWithEPSInsideRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- dataWithPDFInsideRect:@
dataWithPDFInsideRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO (Id NSData)
dataWithPDFInsideRect nsWindow  rect =
    sendMsg nsWindow (mkSelector "dataWithPDFInsideRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- print:@
print_ :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
print_ nsWindow  sender =
    sendMsg nsWindow (mkSelector "print:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- setDynamicDepthLimit:@
setDynamicDepthLimit :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setDynamicDepthLimit nsWindow  flag =
    sendMsg nsWindow (mkSelector "setDynamicDepthLimit:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- invalidateShadow@
invalidateShadow :: IsNSWindow nsWindow => nsWindow -> IO ()
invalidateShadow nsWindow  =
    sendMsg nsWindow (mkSelector "invalidateShadow") retVoid []

-- | @-toggleFullScreen:@ enters or exits for full screen. A window must have @NSWindowCollectionBehaviorFullScreenAuxiliary@ or @NSWindowCollectionBehaviorFullScreenPrimary@ included in the @collectionBehavior@ property; if it does not, this method may simply do nothing.
--
-- ObjC selector: @- toggleFullScreen:@
toggleFullScreen :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleFullScreen nsWindow  sender =
    sendMsg nsWindow (mkSelector "toggleFullScreen:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- setFrameFromString:@
setFrameFromString :: (IsNSWindow nsWindow, IsNSString string) => nsWindow -> string -> IO ()
setFrameFromString nsWindow  string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsWindow (mkSelector "setFrameFromString:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- saveFrameUsingName:@
saveFrameUsingName :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> IO ()
saveFrameUsingName nsWindow  name =
  withObjCPtr name $ \raw_name ->
      sendMsg nsWindow (mkSelector "saveFrameUsingName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- setFrameUsingName:force:@
setFrameUsingName_force :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> Bool -> IO Bool
setFrameUsingName_force nsWindow  name force =
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "setFrameUsingName:force:") retCULong [argPtr (castPtr raw_name :: Ptr ()), argCULong (if force then 1 else 0)]

-- | @- setFrameUsingName:@
setFrameUsingName :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> IO Bool
setFrameUsingName nsWindow  name =
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "setFrameUsingName:") retCULong [argPtr (castPtr raw_name :: Ptr ())]

-- | @- setFrameAutosaveName:@
setFrameAutosaveName :: (IsNSWindow nsWindow, IsNSString name) => nsWindow -> name -> IO Bool
setFrameAutosaveName nsWindow  name =
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "setFrameAutosaveName:") retCULong [argPtr (castPtr raw_name :: Ptr ())]

-- | @+ removeFrameUsingName:@
removeFrameUsingName :: IsNSString name => name -> IO ()
removeFrameUsingName name =
  do
    cls' <- getRequiredClass "NSWindow"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "removeFrameUsingName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- beginSheet:completionHandler:@
beginSheet_completionHandler :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> Ptr () -> IO ()
beginSheet_completionHandler nsWindow  sheetWindow handler =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
      sendMsg nsWindow (mkSelector "beginSheet:completionHandler:") retVoid [argPtr (castPtr raw_sheetWindow :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- beginCriticalSheet:completionHandler:@
beginCriticalSheet_completionHandler :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> Ptr () -> IO ()
beginCriticalSheet_completionHandler nsWindow  sheetWindow handler =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
      sendMsg nsWindow (mkSelector "beginCriticalSheet:completionHandler:") retVoid [argPtr (castPtr raw_sheetWindow :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- endSheet:@
endSheet :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> IO ()
endSheet nsWindow  sheetWindow =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
      sendMsg nsWindow (mkSelector "endSheet:") retVoid [argPtr (castPtr raw_sheetWindow :: Ptr ())]

-- | @- endSheet:returnCode:@
endSheet_returnCode :: (IsNSWindow nsWindow, IsNSWindow sheetWindow) => nsWindow -> sheetWindow -> CLong -> IO ()
endSheet_returnCode nsWindow  sheetWindow returnCode =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
      sendMsg nsWindow (mkSelector "endSheet:returnCode:") retVoid [argPtr (castPtr raw_sheetWindow :: Ptr ()), argCLong returnCode]

-- | @+ standardWindowButton:forStyleMask:@
standardWindowButton_forStyleMask :: NSWindowButton -> NSWindowStyleMask -> IO (Id NSButton)
standardWindowButton_forStyleMask b styleMask =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMsg cls' (mkSelector "standardWindowButton:forStyleMask:") (retPtr retVoid) [argCULong (coerce b), argCULong (coerce styleMask)] >>= retainedObject . castPtr

-- | @- standardWindowButton:@
standardWindowButton :: IsNSWindow nsWindow => nsWindow -> NSWindowButton -> IO (Id NSButton)
standardWindowButton nsWindow  b =
    sendMsg nsWindow (mkSelector "standardWindowButton:") (retPtr retVoid) [argCULong (coerce b)] >>= retainedObject . castPtr

-- | @- addChildWindow:ordered:@
addChildWindow_ordered :: (IsNSWindow nsWindow, IsNSWindow childWin) => nsWindow -> childWin -> NSWindowOrderingMode -> IO ()
addChildWindow_ordered nsWindow  childWin place =
  withObjCPtr childWin $ \raw_childWin ->
      sendMsg nsWindow (mkSelector "addChildWindow:ordered:") retVoid [argPtr (castPtr raw_childWin :: Ptr ()), argCLong (coerce place)]

-- | @- removeChildWindow:@
removeChildWindow :: (IsNSWindow nsWindow, IsNSWindow childWin) => nsWindow -> childWin -> IO ()
removeChildWindow nsWindow  childWin =
  withObjCPtr childWin $ \raw_childWin ->
      sendMsg nsWindow (mkSelector "removeChildWindow:") retVoid [argPtr (castPtr raw_childWin :: Ptr ())]

-- | @-canRepresentDisplayGamut:@ returns @YES@ if the colorSpace of the receiving window, and the @colorSpace@ of the screen containing that window, are capable of representing the given display gamut
--
-- ObjC selector: @- canRepresentDisplayGamut:@
canRepresentDisplayGamut :: IsNSWindow nsWindow => nsWindow -> NSDisplayGamut -> IO Bool
canRepresentDisplayGamut nsWindow  displayGamut =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "canRepresentDisplayGamut:") retCULong [argCLong (coerce displayGamut)]

-- | @+windowNumbersWithOptions:@ returns an autoreleased array of @NSNumbers@ containing windowNumbers for all visible windows satisfying options.  If no options are specified, only visible windows belonging to the calling application and on the active space are included.  If options include @NSWindowNumberListAllApplications,@ visible windows belonging to all applications are included.  If options include @NSWindowNumberListAllSpaces,@ visible windows on all spaces are included.  Windows on the active space are returned in z-order.   Examples:       To get an array of windowNumbers visible on the current space and belonging to the calling application:  	@windowNumbers = [NSWindow windowNumbersWithOptions:0];@      To get an array of windowNumbers visible on any space and belonging to any application:	@windowNumbers = [NSWindow windowNumbersWithOptions:NSWindowNumberListAllApplications|NSWindowNumberListAllSpaces];@      To get an array of windowNumbers visible on any space and belonging to the calling application:	@windowNumbers = [NSWindow windowNumbersWithOptions:NSWindowNumberListAllSpaces];@
--
-- ObjC selector: @+ windowNumbersWithOptions:@
windowNumbersWithOptions :: NSWindowNumberListOptions -> IO (Id NSArray)
windowNumbersWithOptions options =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMsg cls' (mkSelector "windowNumbersWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+windowNumberAtPoint:belowWindowWithWindowNumber:@ returns the number of the frontmost window that would be hit by a mouseDown at the screen location "point".  "windowNum" can be specified to exclude a given window along with all windows above it, and may belong to any application.  If no windows are to be excluded, specify 0 for "windowNum".  The windowNumber returned may correspond to a window in another application.
--
-- ObjC selector: @+ windowNumberAtPoint:belowWindowWithWindowNumber:@
windowNumberAtPoint_belowWindowWithWindowNumber :: NSPoint -> CLong -> IO CLong
windowNumberAtPoint_belowWindowWithWindowNumber point windowNumber =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMsg cls' (mkSelector "windowNumberAtPoint:belowWindowWithWindowNumber:") retCLong [argNSPoint point, argCLong windowNumber]

-- | Convenience method for creating an autoreleased titled window with the given contentViewController. A basic NSWindow with the following attributes is made: titled, closable, resizable, miniaturizable. The window's title is automatically bound to the contentViewController's title. The size of the window can easily be controlled by utilizing autolayout and applying size constraints to the view (or its subviews). The window has isReleasedWhenClosed set to NO, and it must be explicitly retained to keep the window instance alive. To have it automatically be freed when it is closed, do the following: [window retain] and [window setReleasedWhenClosed:YES].
--
-- ObjC selector: @+ windowWithContentViewController:@
windowWithContentViewController :: IsNSViewController contentViewController => contentViewController -> IO (Id NSWindow)
windowWithContentViewController contentViewController =
  do
    cls' <- getRequiredClass "NSWindow"
    withObjCPtr contentViewController $ \raw_contentViewController ->
      sendClassMsg cls' (mkSelector "windowWithContentViewController:") (retPtr retVoid) [argPtr (castPtr raw_contentViewController :: Ptr ())] >>= retainedObject . castPtr

-- | Call to start a drag (moving the window) in the Window Server process. In general, this can be done after a mouseDown event has come in and been examined by an application or view. The view may determine it wants to allow that portion of the window to start a window drag, and can hand off the work to the Window Server process by calling this method. This allows the window to participate in space switching, and other system features. Pass the original mouseDown event to the method. The method will return right away, and a mouseUp may not get sent.
--
-- ObjC selector: @- performWindowDragWithEvent:@
performWindowDragWithEvent :: (IsNSWindow nsWindow, IsNSEvent event) => nsWindow -> event -> IO ()
performWindowDragWithEvent nsWindow  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsWindow (mkSelector "performWindowDragWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- selectNextKeyView:@
selectNextKeyView :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectNextKeyView nsWindow  sender =
    sendMsg nsWindow (mkSelector "selectNextKeyView:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectPreviousKeyView:@
selectPreviousKeyView :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectPreviousKeyView nsWindow  sender =
    sendMsg nsWindow (mkSelector "selectPreviousKeyView:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectKeyViewFollowingView:@
selectKeyViewFollowingView :: (IsNSWindow nsWindow, IsNSView view) => nsWindow -> view -> IO ()
selectKeyViewFollowingView nsWindow  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsWindow (mkSelector "selectKeyViewFollowingView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- selectKeyViewPrecedingView:@
selectKeyViewPrecedingView :: (IsNSWindow nsWindow, IsNSView view) => nsWindow -> view -> IO ()
selectKeyViewPrecedingView nsWindow  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsWindow (mkSelector "selectKeyViewPrecedingView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- disableKeyEquivalentForDefaultButtonCell@
disableKeyEquivalentForDefaultButtonCell :: IsNSWindow nsWindow => nsWindow -> IO ()
disableKeyEquivalentForDefaultButtonCell nsWindow  =
    sendMsg nsWindow (mkSelector "disableKeyEquivalentForDefaultButtonCell") retVoid []

-- | @- enableKeyEquivalentForDefaultButtonCell@
enableKeyEquivalentForDefaultButtonCell :: IsNSWindow nsWindow => nsWindow -> IO ()
enableKeyEquivalentForDefaultButtonCell nsWindow  =
    sendMsg nsWindow (mkSelector "enableKeyEquivalentForDefaultButtonCell") retVoid []

-- | @- recalculateKeyViewLoop@
recalculateKeyViewLoop :: IsNSWindow nsWindow => nsWindow -> IO ()
recalculateKeyViewLoop nsWindow  =
    sendMsg nsWindow (mkSelector "recalculateKeyViewLoop") retVoid []

-- | @- toggleToolbarShown:@
toggleToolbarShown :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleToolbarShown nsWindow  sender =
    sendMsg nsWindow (mkSelector "toggleToolbarShown:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- runToolbarCustomizationPalette:@
runToolbarCustomizationPalette :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
runToolbarCustomizationPalette nsWindow  sender =
    sendMsg nsWindow (mkSelector "runToolbarCustomizationPalette:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Actions that can be called to perform various tabbed window behaviors. UI that is hooked up to these items can be automatically validated by calling @NSWindow@'s @validateUserInterfaceItem.@
--
-- ObjC selector: @- selectNextTab:@
selectNextTab :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectNextTab nsWindow  sender =
    sendMsg nsWindow (mkSelector "selectNextTab:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectPreviousTab:@
selectPreviousTab :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
selectPreviousTab nsWindow  sender =
    sendMsg nsWindow (mkSelector "selectPreviousTab:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveTabToNewWindow:@
moveTabToNewWindow :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
moveTabToNewWindow nsWindow  sender =
    sendMsg nsWindow (mkSelector "moveTabToNewWindow:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- mergeAllWindows:@
mergeAllWindows :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
mergeAllWindows nsWindow  sender =
    sendMsg nsWindow (mkSelector "mergeAllWindows:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleTabBar:@
toggleTabBar :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleTabBar nsWindow  sender =
    sendMsg nsWindow (mkSelector "toggleTabBar:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Toggle the Tab Picker / Tab Overview UI which is invoked via "Show All Tabs". Performs the toggle in an animated fashion. Use @tabGroup.isOverviewVisible@ to find out if it is visible or not at a given time.
--
-- ObjC selector: @- toggleTabOverview:@
toggleTabOverview :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
toggleTabOverview nsWindow  sender =
    sendMsg nsWindow (mkSelector "toggleTabOverview:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | This is now a cover for @-[self.tabGroup addWindow:]@, which allows more precise placement.
--
-- ObjC selector: @- addTabbedWindow:ordered:@
addTabbedWindow_ordered :: (IsNSWindow nsWindow, IsNSWindow window) => nsWindow -> window -> NSWindowOrderingMode -> IO ()
addTabbedWindow_ordered nsWindow  window ordered =
  withObjCPtr window $ \raw_window ->
      sendMsg nsWindow (mkSelector "addTabbedWindow:ordered:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argCLong (coerce ordered)]

-- | Attempt to move window sharing (i.e. within a SharePlay session) from the receiver to another window. In response to this request, the user may choose to transfer sharing to the new window, or simply stop sharing the content.
--
-- @window@ — A window that is replacing the reciever in representing the user's current activity.
--
-- @completionHandler@ — A completion block that is called after the request finishes.        @error@            In the event of a failed transfer request, a non-nil error contains details about the failure.
--
-- ObjC selector: @- transferWindowSharingToWindow:completionHandler:@
transferWindowSharingToWindow_completionHandler :: (IsNSWindow nsWindow, IsNSWindow window) => nsWindow -> window -> Ptr () -> IO ()
transferWindowSharingToWindow_completionHandler nsWindow  window completionHandler =
  withObjCPtr window $ \raw_window ->
      sendMsg nsWindow (mkSelector "transferWindowSharingToWindow:completionHandler:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Request sharing of window.  If there is an available ScreenCaptureKit sharing session, an alert will be presented asking the user to confirm the share
--
-- @window@ — The window to share
--
-- @completionHandler@ — A completion block that is called after the request finishes. @error@ The error will be non-nil if the request does not result in a window being shared.  The error will be NSUserCancelledError if there is no ScreenCaptureKit session, or if the user rejects the offer to share.  If sharing fails for some other reason, the error will provide the details.
--
-- ObjC selector: @- requestSharingOfWindow:completionHandler:@
requestSharingOfWindow_completionHandler :: (IsNSWindow nsWindow, IsNSWindow window) => nsWindow -> window -> Ptr () -> IO ()
requestSharingOfWindow_completionHandler nsWindow  window completionHandler =
  withObjCPtr window $ \raw_window ->
      sendMsg nsWindow (mkSelector "requestSharingOfWindow:completionHandler:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
requestSharingOfWindowUsingPreview_title_completionHandler nsWindow  image title completionHandler =
  withObjCPtr image $ \raw_image ->
    withObjCPtr title $ \raw_title ->
        sendMsg nsWindow (mkSelector "requestSharingOfWindowUsingPreview:title:completionHandler:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- disableSnapshotRestoration@
disableSnapshotRestoration :: IsNSWindow nsWindow => nsWindow -> IO ()
disableSnapshotRestoration nsWindow  =
    sendMsg nsWindow (mkSelector "disableSnapshotRestoration") retVoid []

-- | @- enableSnapshotRestoration@
enableSnapshotRestoration :: IsNSWindow nsWindow => nsWindow -> IO ()
enableSnapshotRestoration nsWindow  =
    sendMsg nsWindow (mkSelector "enableSnapshotRestoration") retVoid []

-- | @- setIsMiniaturized:@
setIsMiniaturized :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIsMiniaturized nsWindow  flag =
    sendMsg nsWindow (mkSelector "setIsMiniaturized:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- setIsVisible:@
setIsVisible :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIsVisible nsWindow  flag =
    sendMsg nsWindow (mkSelector "setIsVisible:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- setIsZoomed:@
setIsZoomed :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIsZoomed nsWindow  flag =
    sendMsg nsWindow (mkSelector "setIsZoomed:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- handleCloseScriptCommand:@
handleCloseScriptCommand :: (IsNSWindow nsWindow, IsNSCloseCommand command) => nsWindow -> command -> IO RawId
handleCloseScriptCommand nsWindow  command =
  withObjCPtr command $ \raw_command ->
      fmap (RawId . castPtr) $ sendMsg nsWindow (mkSelector "handleCloseScriptCommand:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())]

-- | @- handlePrintScriptCommand:@
handlePrintScriptCommand :: (IsNSWindow nsWindow, IsNSScriptCommand command) => nsWindow -> command -> IO RawId
handlePrintScriptCommand nsWindow  command =
  withObjCPtr command $ \raw_command ->
      fmap (RawId . castPtr) $ sendMsg nsWindow (mkSelector "handlePrintScriptCommand:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())]

-- | @- handleSaveScriptCommand:@
handleSaveScriptCommand :: (IsNSWindow nsWindow, IsNSScriptCommand command) => nsWindow -> command -> IO RawId
handleSaveScriptCommand nsWindow  command =
  withObjCPtr command $ \raw_command ->
      fmap (RawId . castPtr) $ sendMsg nsWindow (mkSelector "handleSaveScriptCommand:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())]

-- | @- visualizeConstraints:@
visualizeConstraints :: (IsNSWindow nsWindow, IsNSArray constraints) => nsWindow -> constraints -> IO ()
visualizeConstraints nsWindow  constraints =
  withObjCPtr constraints $ \raw_constraints ->
      sendMsg nsWindow (mkSelector "visualizeConstraints:") retVoid [argPtr (castPtr raw_constraints :: Ptr ())]

-- | @- anchorAttributeForOrientation:@
anchorAttributeForOrientation :: IsNSWindow nsWindow => nsWindow -> NSLayoutConstraintOrientation -> IO NSLayoutAttribute
anchorAttributeForOrientation nsWindow  orientation =
    fmap (coerce :: CLong -> NSLayoutAttribute) $ sendMsg nsWindow (mkSelector "anchorAttributeForOrientation:") retCLong [argCLong (coerce orientation)]

-- | @- setAnchorAttribute:forOrientation:@
setAnchorAttribute_forOrientation :: IsNSWindow nsWindow => nsWindow -> NSLayoutAttribute -> NSLayoutConstraintOrientation -> IO ()
setAnchorAttribute_forOrientation nsWindow  attr orientation =
    sendMsg nsWindow (mkSelector "setAnchorAttribute:forOrientation:") retVoid [argCLong (coerce attr), argCLong (coerce orientation)]

-- | @- updateConstraintsIfNeeded@
updateConstraintsIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
updateConstraintsIfNeeded nsWindow  =
    sendMsg nsWindow (mkSelector "updateConstraintsIfNeeded") retVoid []

-- | @- layoutIfNeeded@
layoutIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
layoutIfNeeded nsWindow  =
    sendMsg nsWindow (mkSelector "layoutIfNeeded") retVoid []

-- | @- cacheImageInRect:@
cacheImageInRect :: IsNSWindow nsWindow => nsWindow -> NSRect -> IO ()
cacheImageInRect nsWindow  rect =
    sendMsg nsWindow (mkSelector "cacheImageInRect:") retVoid [argNSRect rect]

-- | @- restoreCachedImage@
restoreCachedImage :: IsNSWindow nsWindow => nsWindow -> IO ()
restoreCachedImage nsWindow  =
    sendMsg nsWindow (mkSelector "restoreCachedImage") retVoid []

-- | @- discardCachedImage@
discardCachedImage :: IsNSWindow nsWindow => nsWindow -> IO ()
discardCachedImage nsWindow  =
    sendMsg nsWindow (mkSelector "discardCachedImage") retVoid []

-- | @+ menuChanged:@
menuChanged :: IsNSMenu menu => menu -> IO ()
menuChanged menu =
  do
    cls' <- getRequiredClass "NSWindow"
    withObjCPtr menu $ \raw_menu ->
      sendClassMsg cls' (mkSelector "menuChanged:") retVoid [argPtr (castPtr raw_menu :: Ptr ())]

-- | @- gState@
gState :: IsNSWindow nsWindow => nsWindow -> IO CLong
gState nsWindow  =
    sendMsg nsWindow (mkSelector "gState") retCLong []

-- | @- convertBaseToScreen:@
convertBaseToScreen :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertBaseToScreen nsWindow  point =
    sendMsgStret nsWindow (mkSelector "convertBaseToScreen:") retNSPoint [argNSPoint point]

-- | @- convertScreenToBase:@
convertScreenToBase :: IsNSWindow nsWindow => nsWindow -> NSPoint -> IO NSPoint
convertScreenToBase nsWindow  point =
    sendMsgStret nsWindow (mkSelector "convertScreenToBase:") retNSPoint [argNSPoint point]

-- | @- userSpaceScaleFactor@
userSpaceScaleFactor :: IsNSWindow nsWindow => nsWindow -> IO CDouble
userSpaceScaleFactor nsWindow  =
    sendMsg nsWindow (mkSelector "userSpaceScaleFactor") retCDouble []

-- | @- useOptimizedDrawing:@
useOptimizedDrawing :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
useOptimizedDrawing nsWindow  flag =
    sendMsg nsWindow (mkSelector "useOptimizedDrawing:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- canStoreColor@
canStoreColor :: IsNSWindow nsWindow => nsWindow -> IO Bool
canStoreColor nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "canStoreColor") retCULong []

-- | @- disableFlushWindow@
disableFlushWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
disableFlushWindow nsWindow  =
    sendMsg nsWindow (mkSelector "disableFlushWindow") retVoid []

-- | @- enableFlushWindow@
enableFlushWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
enableFlushWindow nsWindow  =
    sendMsg nsWindow (mkSelector "enableFlushWindow") retVoid []

-- | @- flushWindow@
flushWindow :: IsNSWindow nsWindow => nsWindow -> IO ()
flushWindow nsWindow  =
    sendMsg nsWindow (mkSelector "flushWindow") retVoid []

-- | @- flushWindowIfNeeded@
flushWindowIfNeeded :: IsNSWindow nsWindow => nsWindow -> IO ()
flushWindowIfNeeded nsWindow  =
    sendMsg nsWindow (mkSelector "flushWindowIfNeeded") retVoid []

-- | @- initWithWindowRef:@
initWithWindowRef :: IsNSWindow nsWindow => nsWindow -> Ptr () -> IO (Id NSWindow)
initWithWindowRef nsWindow  windowRef =
    sendMsg nsWindow (mkSelector "initWithWindowRef:") (retPtr retVoid) [argPtr windowRef] >>= ownedObject . castPtr

-- | @- disableScreenUpdatesUntilFlush@
disableScreenUpdatesUntilFlush :: IsNSWindow nsWindow => nsWindow -> IO ()
disableScreenUpdatesUntilFlush nsWindow  =
    sendMsg nsWindow (mkSelector "disableScreenUpdatesUntilFlush") retVoid []

-- | @- displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: IsNSWindow nsWindow => nsWindow -> RawId -> Selector -> IO (Id CADisplayLink)
displayLinkWithTarget_selector nsWindow  target selector =
    sendMsg nsWindow (mkSelector "displayLinkWithTarget:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | @- beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_source :: (IsNSWindow nsWindow, IsNSArray items, IsNSEvent event) => nsWindow -> items -> event -> RawId -> IO (Id NSDraggingSession)
beginDraggingSessionWithItems_event_source nsWindow  items event source =
  withObjCPtr items $ \raw_items ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsWindow (mkSelector "beginDraggingSessionWithItems:event:source:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= retainedObject . castPtr

-- | @- dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBack :: (IsNSWindow nsWindow, IsNSImage image, IsNSEvent event, IsNSPasteboard pboard) => nsWindow -> image -> NSPoint -> NSSize -> event -> pboard -> RawId -> Bool -> IO ()
dragImage_at_offset_event_pasteboard_source_slideBack nsWindow  image baseLocation initialOffset event pboard sourceObj slideFlag =
  withObjCPtr image $ \raw_image ->
    withObjCPtr event $ \raw_event ->
      withObjCPtr pboard $ \raw_pboard ->
          sendMsg nsWindow (mkSelector "dragImage:at:offset:event:pasteboard:source:slideBack:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint baseLocation, argNSSize initialOffset, argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_pboard :: Ptr ()), argPtr (castPtr (unRawId sourceObj) :: Ptr ()), argCULong (if slideFlag then 1 else 0)]

-- | @- registerForDraggedTypes:@
registerForDraggedTypes :: (IsNSWindow nsWindow, IsNSArray newTypes) => nsWindow -> newTypes -> IO ()
registerForDraggedTypes nsWindow  newTypes =
  withObjCPtr newTypes $ \raw_newTypes ->
      sendMsg nsWindow (mkSelector "registerForDraggedTypes:") retVoid [argPtr (castPtr raw_newTypes :: Ptr ())]

-- | @- unregisterDraggedTypes@
unregisterDraggedTypes :: IsNSWindow nsWindow => nsWindow -> IO ()
unregisterDraggedTypes nsWindow  =
    sendMsg nsWindow (mkSelector "unregisterDraggedTypes") retVoid []

-- | @- disableCursorRects@
disableCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
disableCursorRects nsWindow  =
    sendMsg nsWindow (mkSelector "disableCursorRects") retVoid []

-- | @- enableCursorRects@
enableCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
enableCursorRects nsWindow  =
    sendMsg nsWindow (mkSelector "enableCursorRects") retVoid []

-- | @- discardCursorRects@
discardCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
discardCursorRects nsWindow  =
    sendMsg nsWindow (mkSelector "discardCursorRects") retVoid []

-- | @- invalidateCursorRectsForView:@
invalidateCursorRectsForView :: (IsNSWindow nsWindow, IsNSView view) => nsWindow -> view -> IO ()
invalidateCursorRectsForView nsWindow  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsWindow (mkSelector "invalidateCursorRectsForView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- resetCursorRects@
resetCursorRects :: IsNSWindow nsWindow => nsWindow -> IO ()
resetCursorRects nsWindow  =
    sendMsg nsWindow (mkSelector "resetCursorRects") retVoid []

-- | Tracks events matching the supplied mask with the supplied tracking handler until the tracking handler explicitly terminates tracking. Each event is removed from the event queue then passed to the tracking handler. If a matching event does not exist in the event queue, then the main thread blocks in the specified runloop mode until an event of the requested type is received or the timeout expires. If the timeout expires, the tracking handler is called with a nil event. A negative timeout is interpreted as 0. Use @NSEventDurationForever@ to never timeout. Tracking continues until @*stop@ is set to @YES.@ Calls to @-nextEventMatchingMask:…@ are allowed inside the trackingHandler block. This method returns once tracking is terminated.
--
-- ObjC selector: @- trackEventsMatchingMask:timeout:mode:handler:@
trackEventsMatchingMask_timeout_mode_handler :: (IsNSWindow nsWindow, IsNSString mode) => nsWindow -> NSEventMask -> CDouble -> mode -> Ptr () -> IO ()
trackEventsMatchingMask_timeout_mode_handler nsWindow  mask timeout mode trackingHandler =
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsWindow (mkSelector "trackEventsMatchingMask:timeout:mode:handler:") retVoid [argCULong (coerce mask), argCDouble timeout, argPtr (castPtr raw_mode :: Ptr ()), argPtr (castPtr trackingHandler :: Ptr ())]

-- | @- nextEventMatchingMask:@
nextEventMatchingMask :: IsNSWindow nsWindow => nsWindow -> NSEventMask -> IO (Id NSEvent)
nextEventMatchingMask nsWindow  mask =
    sendMsg nsWindow (mkSelector "nextEventMatchingMask:") (retPtr retVoid) [argCULong (coerce mask)] >>= retainedObject . castPtr

-- | @- nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeue :: (IsNSWindow nsWindow, IsNSDate expiration, IsNSString mode) => nsWindow -> NSEventMask -> expiration -> mode -> Bool -> IO (Id NSEvent)
nextEventMatchingMask_untilDate_inMode_dequeue nsWindow  mask expiration mode deqFlag =
  withObjCPtr expiration $ \raw_expiration ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsWindow (mkSelector "nextEventMatchingMask:untilDate:inMode:dequeue:") (retPtr retVoid) [argCULong (coerce mask), argPtr (castPtr raw_expiration :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ()), argCULong (if deqFlag then 1 else 0)] >>= retainedObject . castPtr

-- | @- discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEvent :: (IsNSWindow nsWindow, IsNSEvent lastEvent) => nsWindow -> NSEventMask -> lastEvent -> IO ()
discardEventsMatchingMask_beforeEvent nsWindow  mask lastEvent =
  withObjCPtr lastEvent $ \raw_lastEvent ->
      sendMsg nsWindow (mkSelector "discardEventsMatchingMask:beforeEvent:") retVoid [argCULong (coerce mask), argPtr (castPtr raw_lastEvent :: Ptr ())]

-- | @- postEvent:atStart:@
postEvent_atStart :: (IsNSWindow nsWindow, IsNSEvent event) => nsWindow -> event -> Bool -> IO ()
postEvent_atStart nsWindow  event flag =
  withObjCPtr event $ \raw_event ->
      sendMsg nsWindow (mkSelector "postEvent:atStart:") retVoid [argPtr (castPtr raw_event :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- sendEvent:@
sendEvent :: (IsNSWindow nsWindow, IsNSEvent event) => nsWindow -> event -> IO ()
sendEvent nsWindow  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsWindow (mkSelector "sendEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @+ defaultDepthLimit@
defaultDepthLimit :: IO NSWindowDepth
defaultDepthLimit  =
  do
    cls' <- getRequiredClass "NSWindow"
    fmap (coerce :: CInt -> NSWindowDepth) $ sendClassMsg cls' (mkSelector "defaultDepthLimit") retCInt []

-- | @- title@
title :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
title nsWindow  =
    sendMsg nsWindow (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setTitle nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Secondary text that may be displayed adjacent to or below the primary title depending on the configuration of the window. A value of empty string will remove the subtitle from the window layout.
--
-- ObjC selector: @- subtitle@
subtitle :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
subtitle nsWindow  =
    sendMsg nsWindow (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Secondary text that may be displayed adjacent to or below the primary title depending on the configuration of the window. A value of empty string will remove the subtitle from the window layout.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setSubtitle nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | See the enum values for how this property works.
--
-- ObjC selector: @- titleVisibility@
titleVisibility :: IsNSWindow nsWindow => nsWindow -> IO NSWindowTitleVisibility
titleVisibility nsWindow  =
    fmap (coerce :: CLong -> NSWindowTitleVisibility) $ sendMsg nsWindow (mkSelector "titleVisibility") retCLong []

-- | See the enum values for how this property works.
--
-- ObjC selector: @- setTitleVisibility:@
setTitleVisibility :: IsNSWindow nsWindow => nsWindow -> NSWindowTitleVisibility -> IO ()
setTitleVisibility nsWindow  value =
    sendMsg nsWindow (mkSelector "setTitleVisibility:") retVoid [argCLong (coerce value)]

-- | When @YES,@ the titlebar doesn't draw its background, allowing all buttons to show through, and "click through" to happen. In general, this is only useful when @NSFullSizeContentViewWindowMask@ is set.
--
-- ObjC selector: @- titlebarAppearsTransparent@
titlebarAppearsTransparent :: IsNSWindow nsWindow => nsWindow -> IO Bool
titlebarAppearsTransparent nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "titlebarAppearsTransparent") retCULong []

-- | When @YES,@ the titlebar doesn't draw its background, allowing all buttons to show through, and "click through" to happen. In general, this is only useful when @NSFullSizeContentViewWindowMask@ is set.
--
-- ObjC selector: @- setTitlebarAppearsTransparent:@
setTitlebarAppearsTransparent :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setTitlebarAppearsTransparent nsWindow  value =
    sendMsg nsWindow (mkSelector "setTitlebarAppearsTransparent:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies how the titlebar area of the window should appear when the window displays an NSToolbar
--
-- ObjC selector: @- toolbarStyle@
toolbarStyle :: IsNSWindow nsWindow => nsWindow -> IO NSWindowToolbarStyle
toolbarStyle nsWindow  =
    fmap (coerce :: CLong -> NSWindowToolbarStyle) $ sendMsg nsWindow (mkSelector "toolbarStyle") retCLong []

-- | Specifies how the titlebar area of the window should appear when the window displays an NSToolbar
--
-- ObjC selector: @- setToolbarStyle:@
setToolbarStyle :: IsNSWindow nsWindow => nsWindow -> NSWindowToolbarStyle -> IO ()
setToolbarStyle nsWindow  value =
    sendMsg nsWindow (mkSelector "setToolbarStyle:") retVoid [argCLong (coerce value)]

-- | The @contentLayoutRect@ will return the area inside the window that is for non-obscured content. Typically, this is the same thing as the @contentView@'s frame. However, for windows with the @NSFullSizeContentViewWindowMask@ set, there needs to be a way to determine the portion that is not under the toolbar. The @contentLayoutRect@ returns the portion of the layout that is not obscured under the toolbar. @contentLayoutRect@ is in window coordinates. It is KVO compliant. */
--
-- ObjC selector: @- contentLayoutRect@
contentLayoutRect :: IsNSWindow nsWindow => nsWindow -> IO NSRect
contentLayoutRect nsWindow  =
    sendMsgStret nsWindow (mkSelector "contentLayoutRect") retNSRect []

-- | @contentLayoutGuide@ is a corollary to @contentLayoutRect.@ It can be used by autolayout constraints to automatically bind to the @contentLayoutRect.@
--
-- ObjC selector: @- contentLayoutGuide@
contentLayoutGuide :: IsNSWindow nsWindow => nsWindow -> IO RawId
contentLayoutGuide nsWindow  =
    fmap (RawId . castPtr) $ sendMsg nsWindow (mkSelector "contentLayoutGuide") (retPtr retVoid) []

-- | @- titlebarAccessoryViewControllers@
titlebarAccessoryViewControllers :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
titlebarAccessoryViewControllers nsWindow  =
    sendMsg nsWindow (mkSelector "titlebarAccessoryViewControllers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitlebarAccessoryViewControllers:@
setTitlebarAccessoryViewControllers :: (IsNSWindow nsWindow, IsNSArray value) => nsWindow -> value -> IO ()
setTitlebarAccessoryViewControllers nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setTitlebarAccessoryViewControllers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If url is not nil and its path is not empty, the window will show a document icon in the titlebar. If the url represents a filename or other resource with a known icon, that icon will be used as the document icon.  Otherwise the default document icon will be used.  The icon can be customized using @-[[NSWindow standardWindowButton:NSWindowDocumentIconButton] setImage:customImage]@.  If url is not nil and its path is not empty, the window will have a pop-up menu which can be shown via command-click on the area containing the document icon and title.  By default, this menu will display the path components of the url.  The presence and contents of this menu can be controlled by the delegate method @-[window:shouldPopUpDocumentPathMenu:]@ If the url is nil or has an empty path, the window will not show a document icon and will not have a pop-up menu available via command-click.
--
-- ObjC selector: @- representedURL@
representedURL :: IsNSWindow nsWindow => nsWindow -> IO (Id NSURL)
representedURL nsWindow  =
    sendMsg nsWindow (mkSelector "representedURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If url is not nil and its path is not empty, the window will show a document icon in the titlebar. If the url represents a filename or other resource with a known icon, that icon will be used as the document icon.  Otherwise the default document icon will be used.  The icon can be customized using @-[[NSWindow standardWindowButton:NSWindowDocumentIconButton] setImage:customImage]@.  If url is not nil and its path is not empty, the window will have a pop-up menu which can be shown via command-click on the area containing the document icon and title.  By default, this menu will display the path components of the url.  The presence and contents of this menu can be controlled by the delegate method @-[window:shouldPopUpDocumentPathMenu:]@ If the url is nil or has an empty path, the window will not show a document icon and will not have a pop-up menu available via command-click.
--
-- ObjC selector: @- setRepresentedURL:@
setRepresentedURL :: (IsNSWindow nsWindow, IsNSURL value) => nsWindow -> value -> IO ()
setRepresentedURL nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setRepresentedURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- representedFilename@
representedFilename :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
representedFilename nsWindow  =
    sendMsg nsWindow (mkSelector "representedFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRepresentedFilename:@
setRepresentedFilename :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setRepresentedFilename nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setRepresentedFilename:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- excludedFromWindowsMenu@
excludedFromWindowsMenu :: IsNSWindow nsWindow => nsWindow -> IO Bool
excludedFromWindowsMenu nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "excludedFromWindowsMenu") retCULong []

-- | @- setExcludedFromWindowsMenu:@
setExcludedFromWindowsMenu :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setExcludedFromWindowsMenu nsWindow  value =
    sendMsg nsWindow (mkSelector "setExcludedFromWindowsMenu:") retVoid [argCULong (if value then 1 else 0)]

-- | @- contentView@
contentView :: IsNSWindow nsWindow => nsWindow -> IO (Id NSView)
contentView nsWindow  =
    sendMsg nsWindow (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentView:@
setContentView :: (IsNSWindow nsWindow, IsNSView value) => nsWindow -> value -> IO ()
setContentView nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSWindow nsWindow => nsWindow -> IO RawId
delegate nsWindow  =
    fmap (RawId . castPtr) $ sendMsg nsWindow (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
setDelegate nsWindow  value =
    sendMsg nsWindow (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- windowNumber@
windowNumber :: IsNSWindow nsWindow => nsWindow -> IO CLong
windowNumber nsWindow  =
    sendMsg nsWindow (mkSelector "windowNumber") retCLong []

-- | Note: The styleMask can only be set on macOS 10.6 and later. Valid @styleMask@ settings have the same restrictions as the @styleMask@ passed to @-initWithContentRect:styleMask:backing:defer:@.  Some @styleMask@ changes will cause the view hierarchy to be rebuilt, since there is a different subclass for the top level view of a borderless window than for the top level view of a titled window.
--
-- ObjC selector: @- styleMask@
styleMask :: IsNSWindow nsWindow => nsWindow -> IO NSWindowStyleMask
styleMask nsWindow  =
    fmap (coerce :: CULong -> NSWindowStyleMask) $ sendMsg nsWindow (mkSelector "styleMask") retCULong []

-- | Note: The styleMask can only be set on macOS 10.6 and later. Valid @styleMask@ settings have the same restrictions as the @styleMask@ passed to @-initWithContentRect:styleMask:backing:defer:@.  Some @styleMask@ changes will cause the view hierarchy to be rebuilt, since there is a different subclass for the top level view of a borderless window than for the top level view of a titled window.
--
-- ObjC selector: @- setStyleMask:@
setStyleMask :: IsNSWindow nsWindow => nsWindow -> NSWindowStyleMask -> IO ()
setStyleMask nsWindow  value =
    sendMsg nsWindow (mkSelector "setStyleMask:") retVoid [argCULong (coerce value)]

-- | The frame to use when cascading or sizing a new window based on the receiver's position or size. This may be different from @frame@ when the receiver is positioned by the system.
--
-- ObjC selector: @- cascadingReferenceFrame@
cascadingReferenceFrame :: IsNSWindow nsWindow => nsWindow -> IO NSRect
cascadingReferenceFrame nsWindow  =
    sendMsgStret nsWindow (mkSelector "cascadingReferenceFrame") retNSRect []

-- | @- frame@
frame :: IsNSWindow nsWindow => nsWindow -> IO NSRect
frame nsWindow  =
    sendMsgStret nsWindow (mkSelector "frame") retNSRect []

-- | @- inLiveResize@
inLiveResize :: IsNSWindow nsWindow => nsWindow -> IO Bool
inLiveResize nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "inLiveResize") retCULong []

-- | @- resizeIncrements@
resizeIncrements :: IsNSWindow nsWindow => nsWindow -> IO NSSize
resizeIncrements nsWindow  =
    sendMsgStret nsWindow (mkSelector "resizeIncrements") retNSSize []

-- | @- setResizeIncrements:@
setResizeIncrements :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setResizeIncrements nsWindow  value =
    sendMsg nsWindow (mkSelector "setResizeIncrements:") retVoid [argNSSize value]

-- | @- aspectRatio@
aspectRatio :: IsNSWindow nsWindow => nsWindow -> IO NSSize
aspectRatio nsWindow  =
    sendMsgStret nsWindow (mkSelector "aspectRatio") retNSSize []

-- | @- setAspectRatio:@
setAspectRatio :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setAspectRatio nsWindow  value =
    sendMsg nsWindow (mkSelector "setAspectRatio:") retVoid [argNSSize value]

-- | @- contentResizeIncrements@
contentResizeIncrements :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentResizeIncrements nsWindow  =
    sendMsgStret nsWindow (mkSelector "contentResizeIncrements") retNSSize []

-- | @- setContentResizeIncrements:@
setContentResizeIncrements :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentResizeIncrements nsWindow  value =
    sendMsg nsWindow (mkSelector "setContentResizeIncrements:") retVoid [argNSSize value]

-- | @- contentAspectRatio@
contentAspectRatio :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentAspectRatio nsWindow  =
    sendMsgStret nsWindow (mkSelector "contentAspectRatio") retNSSize []

-- | @- setContentAspectRatio:@
setContentAspectRatio :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentAspectRatio nsWindow  value =
    sendMsg nsWindow (mkSelector "setContentAspectRatio:") retVoid [argNSSize value]

-- | @- viewsNeedDisplay@
viewsNeedDisplay :: IsNSWindow nsWindow => nsWindow -> IO Bool
viewsNeedDisplay nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "viewsNeedDisplay") retCULong []

-- | @- setViewsNeedDisplay:@
setViewsNeedDisplay :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setViewsNeedDisplay nsWindow  value =
    sendMsg nsWindow (mkSelector "setViewsNeedDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preservesContentDuringLiveResize@
preservesContentDuringLiveResize :: IsNSWindow nsWindow => nsWindow -> IO Bool
preservesContentDuringLiveResize nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "preservesContentDuringLiveResize") retCULong []

-- | @- setPreservesContentDuringLiveResize:@
setPreservesContentDuringLiveResize :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setPreservesContentDuringLiveResize nsWindow  value =
    sendMsg nsWindow (mkSelector "setPreservesContentDuringLiveResize:") retVoid [argCULong (if value then 1 else 0)]

-- | @- firstResponder@
firstResponder :: IsNSWindow nsWindow => nsWindow -> IO (Id NSResponder)
firstResponder nsWindow  =
    sendMsg nsWindow (mkSelector "firstResponder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resizeFlags@
resizeFlags :: IsNSWindow nsWindow => nsWindow -> IO NSEventModifierFlags
resizeFlags nsWindow  =
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg nsWindow (mkSelector "resizeFlags") retCULong []

-- | @- releasedWhenClosed@
releasedWhenClosed :: IsNSWindow nsWindow => nsWindow -> IO Bool
releasedWhenClosed nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "releasedWhenClosed") retCULong []

-- | @- setReleasedWhenClosed:@
setReleasedWhenClosed :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setReleasedWhenClosed nsWindow  value =
    sendMsg nsWindow (mkSelector "setReleasedWhenClosed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- zoomed@
zoomed :: IsNSWindow nsWindow => nsWindow -> IO Bool
zoomed nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "zoomed") retCULong []

-- | @- miniaturized@
miniaturized :: IsNSWindow nsWindow => nsWindow -> IO Bool
miniaturized nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "miniaturized") retCULong []

-- | @- backgroundColor@
backgroundColor :: IsNSWindow nsWindow => nsWindow -> IO (Id NSColor)
backgroundColor nsWindow  =
    sendMsg nsWindow (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSWindow nsWindow, IsNSColor value) => nsWindow -> value -> IO ()
setBackgroundColor nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- movable@
movable :: IsNSWindow nsWindow => nsWindow -> IO Bool
movable nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "movable") retCULong []

-- | @- setMovable:@
setMovable :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setMovable nsWindow  value =
    sendMsg nsWindow (mkSelector "setMovable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- movableByWindowBackground@
movableByWindowBackground :: IsNSWindow nsWindow => nsWindow -> IO Bool
movableByWindowBackground nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "movableByWindowBackground") retCULong []

-- | @- setMovableByWindowBackground:@
setMovableByWindowBackground :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setMovableByWindowBackground nsWindow  value =
    sendMsg nsWindow (mkSelector "setMovableByWindowBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hidesOnDeactivate@
hidesOnDeactivate :: IsNSWindow nsWindow => nsWindow -> IO Bool
hidesOnDeactivate nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "hidesOnDeactivate") retCULong []

-- | @- setHidesOnDeactivate:@
setHidesOnDeactivate :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setHidesOnDeactivate nsWindow  value =
    sendMsg nsWindow (mkSelector "setHidesOnDeactivate:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether a window can be hidden during @-[NSApplication hide:]@.  Default is @YES.@
--
-- ObjC selector: @- canHide@
canHide :: IsNSWindow nsWindow => nsWindow -> IO Bool
canHide nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "canHide") retCULong []

-- | Indicates whether a window can be hidden during @-[NSApplication hide:]@.  Default is @YES.@
--
-- ObjC selector: @- setCanHide:@
setCanHide :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setCanHide nsWindow  value =
    sendMsg nsWindow (mkSelector "setCanHide:") retVoid [argCULong (if value then 1 else 0)]

-- | @- miniwindowImage@
miniwindowImage :: IsNSWindow nsWindow => nsWindow -> IO (Id NSImage)
miniwindowImage nsWindow  =
    sendMsg nsWindow (mkSelector "miniwindowImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMiniwindowImage:@
setMiniwindowImage :: (IsNSWindow nsWindow, IsNSImage value) => nsWindow -> value -> IO ()
setMiniwindowImage nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setMiniwindowImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- miniwindowTitle@
miniwindowTitle :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
miniwindowTitle nsWindow  =
    sendMsg nsWindow (mkSelector "miniwindowTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMiniwindowTitle:@
setMiniwindowTitle :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setMiniwindowTitle nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setMiniwindowTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dockTile@
dockTile :: IsNSWindow nsWindow => nsWindow -> IO (Id NSDockTile)
dockTile nsWindow  =
    sendMsg nsWindow (mkSelector "dockTile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- documentEdited@
documentEdited :: IsNSWindow nsWindow => nsWindow -> IO Bool
documentEdited nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "documentEdited") retCULong []

-- | @- setDocumentEdited:@
setDocumentEdited :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setDocumentEdited nsWindow  value =
    sendMsg nsWindow (mkSelector "setDocumentEdited:") retVoid [argCULong (if value then 1 else 0)]

-- | @- visible@
visible :: IsNSWindow nsWindow => nsWindow -> IO Bool
visible nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "visible") retCULong []

-- | @- keyWindow@
keyWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
keyWindow nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "keyWindow") retCULong []

-- | @- mainWindow@
mainWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
mainWindow nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "mainWindow") retCULong []

-- | @- canBecomeKeyWindow@
canBecomeKeyWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
canBecomeKeyWindow nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "canBecomeKeyWindow") retCULong []

-- | @- canBecomeMainWindow@
canBecomeMainWindow :: IsNSWindow nsWindow => nsWindow -> IO Bool
canBecomeMainWindow nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "canBecomeMainWindow") retCULong []

-- | @- worksWhenModal@
worksWhenModal :: IsNSWindow nsWindow => nsWindow -> IO Bool
worksWhenModal nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "worksWhenModal") retCULong []

-- | A Boolean value that indicates whether or not to prevent application termination when the receiving window is presented modally. The value of this property is @YES@ if the window should prevent application termination when modal; otherwise, @NO@. The default value is @YES@. However, note that some window subclasses and some windows created indirectly (like those created by UI frameworks like AppKit and SwiftUI), may have different default values. For example, the Open panel and toolbar customization sheets should not prevent application termination, so those windows have @preventsApplicationTerminationWhenModal@ set to @NO@. Some @NSAlert@s, like those that are simply informational, have windows that do not prevent application termination by default. Setting this property overrides the default behavior.
--
-- ObjC selector: @- preventsApplicationTerminationWhenModal@
preventsApplicationTerminationWhenModal :: IsNSWindow nsWindow => nsWindow -> IO Bool
preventsApplicationTerminationWhenModal nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "preventsApplicationTerminationWhenModal") retCULong []

-- | A Boolean value that indicates whether or not to prevent application termination when the receiving window is presented modally. The value of this property is @YES@ if the window should prevent application termination when modal; otherwise, @NO@. The default value is @YES@. However, note that some window subclasses and some windows created indirectly (like those created by UI frameworks like AppKit and SwiftUI), may have different default values. For example, the Open panel and toolbar customization sheets should not prevent application termination, so those windows have @preventsApplicationTerminationWhenModal@ set to @NO@. Some @NSAlert@s, like those that are simply informational, have windows that do not prevent application termination by default. Setting this property overrides the default behavior.
--
-- ObjC selector: @- setPreventsApplicationTerminationWhenModal:@
setPreventsApplicationTerminationWhenModal :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setPreventsApplicationTerminationWhenModal nsWindow  value =
    sendMsg nsWindow (mkSelector "setPreventsApplicationTerminationWhenModal:") retVoid [argCULong (if value then 1 else 0)]

-- | Returns the scale factor representing the number of backing store pixels corresponding to each linear unit in window space on this @NSWindow.@ This method is provided for rare cases when the explicit scale factor is needed. Please use @-convert*ToBacking:@ methods whenever possible.
--
-- ObjC selector: @- backingScaleFactor@
backingScaleFactor :: IsNSWindow nsWindow => nsWindow -> IO CDouble
backingScaleFactor nsWindow  =
    sendMsg nsWindow (mkSelector "backingScaleFactor") retCDouble []

-- | Default is @NO.@ Set to @YES@ to allow a window to display tooltips even when the application is in the background.  Note that, enabling tooltips in an inactive application will cause the app to do work any time the mouse passes over the window.  This can degrade system performance. Returns @YES@ if this window displays tooltips even when the application is in the background.  To configure this setting you should call @-setAllowsToolTipsWhenApplicationIsInactive:@ instead of overriding @-allowsToolTipsWhenApplicationIsInactive@.
--
-- ObjC selector: @- allowsToolTipsWhenApplicationIsInactive@
allowsToolTipsWhenApplicationIsInactive :: IsNSWindow nsWindow => nsWindow -> IO Bool
allowsToolTipsWhenApplicationIsInactive nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "allowsToolTipsWhenApplicationIsInactive") retCULong []

-- | Default is @NO.@ Set to @YES@ to allow a window to display tooltips even when the application is in the background.  Note that, enabling tooltips in an inactive application will cause the app to do work any time the mouse passes over the window.  This can degrade system performance. Returns @YES@ if this window displays tooltips even when the application is in the background.  To configure this setting you should call @-setAllowsToolTipsWhenApplicationIsInactive:@ instead of overriding @-allowsToolTipsWhenApplicationIsInactive@.
--
-- ObjC selector: @- setAllowsToolTipsWhenApplicationIsInactive:@
setAllowsToolTipsWhenApplicationIsInactive :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAllowsToolTipsWhenApplicationIsInactive nsWindow  value =
    sendMsg nsWindow (mkSelector "setAllowsToolTipsWhenApplicationIsInactive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backingType@
backingType :: IsNSWindow nsWindow => nsWindow -> IO NSBackingStoreType
backingType nsWindow  =
    fmap (coerce :: CULong -> NSBackingStoreType) $ sendMsg nsWindow (mkSelector "backingType") retCULong []

-- | @- setBackingType:@
setBackingType :: IsNSWindow nsWindow => nsWindow -> NSBackingStoreType -> IO ()
setBackingType nsWindow  value =
    sendMsg nsWindow (mkSelector "setBackingType:") retVoid [argCULong (coerce value)]

-- | @- level@
level :: IsNSWindow nsWindow => nsWindow -> IO CLong
level nsWindow  =
    sendMsg nsWindow (mkSelector "level") retCLong []

-- | @- setLevel:@
setLevel :: IsNSWindow nsWindow => nsWindow -> CLong -> IO ()
setLevel nsWindow  value =
    sendMsg nsWindow (mkSelector "setLevel:") retVoid [argCLong value]

-- | @- depthLimit@
depthLimit :: IsNSWindow nsWindow => nsWindow -> IO NSWindowDepth
depthLimit nsWindow  =
    fmap (coerce :: CInt -> NSWindowDepth) $ sendMsg nsWindow (mkSelector "depthLimit") retCInt []

-- | @- setDepthLimit:@
setDepthLimit :: IsNSWindow nsWindow => nsWindow -> NSWindowDepth -> IO ()
setDepthLimit nsWindow  value =
    sendMsg nsWindow (mkSelector "setDepthLimit:") retVoid [argCInt (coerce value)]

-- | @- hasDynamicDepthLimit@
hasDynamicDepthLimit :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasDynamicDepthLimit nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "hasDynamicDepthLimit") retCULong []

-- | The screen property returns the best screen for the window. If the window only intersects one screen, it returns that screen. If it intersects more than one screen, then it resolves the tie through based on what space it is mostly on. It may return nil if there are no available screens, or it is completely off screen.
--
-- ObjC selector: @- screen@
screen :: IsNSWindow nsWindow => nsWindow -> IO (Id NSScreen)
screen nsWindow  =
    sendMsg nsWindow (mkSelector "screen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deepestScreen@
deepestScreen :: IsNSWindow nsWindow => nsWindow -> IO (Id NSScreen)
deepestScreen nsWindow  =
    sendMsg nsWindow (mkSelector "deepestScreen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasShadow@
hasShadow :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasShadow nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "hasShadow") retCULong []

-- | @- setHasShadow:@
setHasShadow :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setHasShadow nsWindow  value =
    sendMsg nsWindow (mkSelector "setHasShadow:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alphaValue@
alphaValue :: IsNSWindow nsWindow => nsWindow -> IO CDouble
alphaValue nsWindow  =
    sendMsg nsWindow (mkSelector "alphaValue") retCDouble []

-- | @- setAlphaValue:@
setAlphaValue :: IsNSWindow nsWindow => nsWindow -> CDouble -> IO ()
setAlphaValue nsWindow  value =
    sendMsg nsWindow (mkSelector "setAlphaValue:") retVoid [argCDouble value]

-- | @- opaque@
opaque :: IsNSWindow nsWindow => nsWindow -> IO Bool
opaque nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "opaque") retCULong []

-- | @- setOpaque:@
setOpaque :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setOpaque nsWindow  value =
    sendMsg nsWindow (mkSelector "setOpaque:") retVoid [argCULong (if value then 1 else 0)]

-- | @-setSharingType:@ specifies whether the window content can be read from another process.  The default sharing type is @NSWindowSharingReadOnly,@ which means other processes can read the window content (eg. for window capture) but cannot modify it.  If you set your window sharing type to @NSWindowSharingNone,@ so that the content cannot be captured, your window will also not be able to participate in a number of system services, so this setting should be used with caution.
--
-- ObjC selector: @- sharingType@
sharingType :: IsNSWindow nsWindow => nsWindow -> IO NSWindowSharingType
sharingType nsWindow  =
    fmap (coerce :: CULong -> NSWindowSharingType) $ sendMsg nsWindow (mkSelector "sharingType") retCULong []

-- | @-setSharingType:@ specifies whether the window content can be read from another process.  The default sharing type is @NSWindowSharingReadOnly,@ which means other processes can read the window content (eg. for window capture) but cannot modify it.  If you set your window sharing type to @NSWindowSharingNone,@ so that the content cannot be captured, your window will also not be able to participate in a number of system services, so this setting should be used with caution.
--
-- ObjC selector: @- setSharingType:@
setSharingType :: IsNSWindow nsWindow => nsWindow -> NSWindowSharingType -> IO ()
setSharingType nsWindow  value =
    sendMsg nsWindow (mkSelector "setSharingType:") retVoid [argCULong (coerce value)]

-- | Controls whether threading of view drawing should be enabled for this window.  Defaults to @YES.@  When this is set to @YES,@ AppKit's view system is allowed to perform @-drawRect:@ activity for the window's views on threads other than the main thread, for views that have @canDrawConcurrently == YES@.  When this is set to @NO,@ the window's views will be drawn serially as on 10.5 and earlier, even though some of the views may have @canDrawConcurrently == YES@.
--
-- ObjC selector: @- allowsConcurrentViewDrawing@
allowsConcurrentViewDrawing :: IsNSWindow nsWindow => nsWindow -> IO Bool
allowsConcurrentViewDrawing nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "allowsConcurrentViewDrawing") retCULong []

-- | Controls whether threading of view drawing should be enabled for this window.  Defaults to @YES.@  When this is set to @YES,@ AppKit's view system is allowed to perform @-drawRect:@ activity for the window's views on threads other than the main thread, for views that have @canDrawConcurrently == YES@.  When this is set to @NO,@ the window's views will be drawn serially as on 10.5 and earlier, even though some of the views may have @canDrawConcurrently == YES@.
--
-- ObjC selector: @- setAllowsConcurrentViewDrawing:@
setAllowsConcurrentViewDrawing :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAllowsConcurrentViewDrawing nsWindow  value =
    sendMsg nsWindow (mkSelector "setAllowsConcurrentViewDrawing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- displaysWhenScreenProfileChanges@
displaysWhenScreenProfileChanges :: IsNSWindow nsWindow => nsWindow -> IO Bool
displaysWhenScreenProfileChanges nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "displaysWhenScreenProfileChanges") retCULong []

-- | @- setDisplaysWhenScreenProfileChanges:@
setDisplaysWhenScreenProfileChanges :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setDisplaysWhenScreenProfileChanges nsWindow  value =
    sendMsg nsWindow (mkSelector "setDisplaysWhenScreenProfileChanges:") retVoid [argCULong (if value then 1 else 0)]

-- | This API controls whether the receiver is permitted onscreen before the user has logged in.  This property is off by default.  Alert panels and windows presented by input managers are examples of windows which should have this property set.
--
-- ObjC selector: @- canBecomeVisibleWithoutLogin@
canBecomeVisibleWithoutLogin :: IsNSWindow nsWindow => nsWindow -> IO Bool
canBecomeVisibleWithoutLogin nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "canBecomeVisibleWithoutLogin") retCULong []

-- | This API controls whether the receiver is permitted onscreen before the user has logged in.  This property is off by default.  Alert panels and windows presented by input managers are examples of windows which should have this property set.
--
-- ObjC selector: @- setCanBecomeVisibleWithoutLogin:@
setCanBecomeVisibleWithoutLogin :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setCanBecomeVisibleWithoutLogin nsWindow  value =
    sendMsg nsWindow (mkSelector "setCanBecomeVisibleWithoutLogin:") retVoid [argCULong (if value then 1 else 0)]

-- | @- collectionBehavior@
collectionBehavior :: IsNSWindow nsWindow => nsWindow -> IO NSWindowCollectionBehavior
collectionBehavior nsWindow  =
    fmap (coerce :: CULong -> NSWindowCollectionBehavior) $ sendMsg nsWindow (mkSelector "collectionBehavior") retCULong []

-- | @- setCollectionBehavior:@
setCollectionBehavior :: IsNSWindow nsWindow => nsWindow -> NSWindowCollectionBehavior -> IO ()
setCollectionBehavior nsWindow  value =
    sendMsg nsWindow (mkSelector "setCollectionBehavior:") retVoid [argCULong (coerce value)]

-- | Provides for per-window control over automatic orderFront/orderOut animation behaviors added in 10.7.  Can be set to @NSWindowAnimationBehaviorNone@ to disable Appkit's automatic animations for a given window, or to one of the other non-Default @NSWindowAnimationBehavior@ values to override AppKit's automatic inference of appropriate animation behavior based on the window's apparent type.
--
-- ObjC selector: @- animationBehavior@
animationBehavior :: IsNSWindow nsWindow => nsWindow -> IO NSWindowAnimationBehavior
animationBehavior nsWindow  =
    fmap (coerce :: CLong -> NSWindowAnimationBehavior) $ sendMsg nsWindow (mkSelector "animationBehavior") retCLong []

-- | Provides for per-window control over automatic orderFront/orderOut animation behaviors added in 10.7.  Can be set to @NSWindowAnimationBehaviorNone@ to disable Appkit's automatic animations for a given window, or to one of the other non-Default @NSWindowAnimationBehavior@ values to override AppKit's automatic inference of appropriate animation behavior based on the window's apparent type.
--
-- ObjC selector: @- setAnimationBehavior:@
setAnimationBehavior :: IsNSWindow nsWindow => nsWindow -> NSWindowAnimationBehavior -> IO ()
setAnimationBehavior nsWindow  value =
    sendMsg nsWindow (mkSelector "setAnimationBehavior:") retVoid [argCLong (coerce value)]

-- | Returns @YES@ if this window is associated with the active space.  For visible windows, this API indicates whether the window is currently visible on the active space.  For offscreen windows, it indicates whether ordering the window onscreen would make it bring it onto the active space
--
-- ObjC selector: @- onActiveSpace@
onActiveSpace :: IsNSWindow nsWindow => nsWindow -> IO Bool
onActiveSpace nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "onActiveSpace") retCULong []

-- | @- stringWithSavedFrame@
stringWithSavedFrame :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
stringWithSavedFrame nsWindow  =
    sendMsg nsWindow (mkSelector "stringWithSavedFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- frameAutosaveName@
frameAutosaveName :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
frameAutosaveName nsWindow  =
    sendMsg nsWindow (mkSelector "frameAutosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minSize@
minSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
minSize nsWindow  =
    sendMsgStret nsWindow (mkSelector "minSize") retNSSize []

-- | @- setMinSize:@
setMinSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMinSize nsWindow  value =
    sendMsg nsWindow (mkSelector "setMinSize:") retVoid [argNSSize value]

-- | @- maxSize@
maxSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
maxSize nsWindow  =
    sendMsgStret nsWindow (mkSelector "maxSize") retNSSize []

-- | @- setMaxSize:@
setMaxSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMaxSize nsWindow  value =
    sendMsg nsWindow (mkSelector "setMaxSize:") retVoid [argNSSize value]

-- | @- contentMinSize@
contentMinSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentMinSize nsWindow  =
    sendMsgStret nsWindow (mkSelector "contentMinSize") retNSSize []

-- | @- setContentMinSize:@
setContentMinSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentMinSize nsWindow  value =
    sendMsg nsWindow (mkSelector "setContentMinSize:") retVoid [argNSSize value]

-- | @- contentMaxSize@
contentMaxSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
contentMaxSize nsWindow  =
    sendMsgStret nsWindow (mkSelector "contentMaxSize") retNSSize []

-- | @- setContentMaxSize:@
setContentMaxSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setContentMaxSize nsWindow  value =
    sendMsg nsWindow (mkSelector "setContentMaxSize:") retVoid [argNSSize value]

-- | @- minFullScreenContentSize@
minFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
minFullScreenContentSize nsWindow  =
    sendMsgStret nsWindow (mkSelector "minFullScreenContentSize") retNSSize []

-- | @- setMinFullScreenContentSize:@
setMinFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMinFullScreenContentSize nsWindow  value =
    sendMsg nsWindow (mkSelector "setMinFullScreenContentSize:") retVoid [argNSSize value]

-- | @- maxFullScreenContentSize@
maxFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> IO NSSize
maxFullScreenContentSize nsWindow  =
    sendMsgStret nsWindow (mkSelector "maxFullScreenContentSize") retNSSize []

-- | @- setMaxFullScreenContentSize:@
setMaxFullScreenContentSize :: IsNSWindow nsWindow => nsWindow -> NSSize -> IO ()
setMaxFullScreenContentSize nsWindow  value =
    sendMsg nsWindow (mkSelector "setMaxFullScreenContentSize:") retVoid [argNSSize value]

-- | @- deviceDescription@
deviceDescription :: IsNSWindow nsWindow => nsWindow -> IO (Id NSDictionary)
deviceDescription nsWindow  =
    sendMsg nsWindow (mkSelector "deviceDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windowController@
windowController :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindowController)
windowController nsWindow  =
    sendMsg nsWindow (mkSelector "windowController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWindowController:@
setWindowController :: (IsNSWindow nsWindow, IsNSWindowController value) => nsWindow -> value -> IO ()
setWindowController nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setWindowController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An ordered array of the sheets on the window. This consists of the presented sheets in top-to-bottom order, followed by queued sheets in the order they were queued. This does not include nested/sub-sheets.
--
-- ObjC selector: @- sheets@
sheets :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
sheets nsWindow  =
    sendMsg nsWindow (mkSelector "sheets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the top-most sheet if there is one or more sheets, or nil if there is no sheet.
--
-- ObjC selector: @- attachedSheet@
attachedSheet :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindow)
attachedSheet nsWindow  =
    sendMsg nsWindow (mkSelector "attachedSheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sheet@
sheet :: IsNSWindow nsWindow => nsWindow -> IO Bool
sheet nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "sheet") retCULong []

-- | Returns the window that the sheet is directly attached to. This is based on the logical attachment of the sheet, not visual attachment. This relationship exists starting when the sheet is begun (using @NSApplication's@ @-beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo: or NSWindow's -beginSheet:completionHandler:@), and ending once it is ordered out. Returns nil if the window is not a sheet or has no sheet parent.
--
-- ObjC selector: @- sheetParent@
sheetParent :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindow)
sheetParent nsWindow  =
    sendMsg nsWindow (mkSelector "sheetParent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- childWindows@
childWindows :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
childWindows nsWindow  =
    sendMsg nsWindow (mkSelector "childWindows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parentWindow@
parentWindow :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindow)
parentWindow nsWindow  =
    sendMsg nsWindow (mkSelector "parentWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParentWindow:@
setParentWindow :: (IsNSWindow nsWindow, IsNSWindow value) => nsWindow -> value -> IO ()
setParentWindow nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setParentWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If set, the receiver will inherit the appearance of that object, as well as use KVO to observe its effectiveAppearance for changes. Typically this is used for child windows that are shown from a parent window or specific view. Defaults to NSApp.
--
-- ObjC selector: @- appearanceSource@
appearanceSource :: IsNSWindow nsWindow => nsWindow -> IO (Id NSObject)
appearanceSource nsWindow  =
    sendMsg nsWindow (mkSelector "appearanceSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If set, the receiver will inherit the appearance of that object, as well as use KVO to observe its effectiveAppearance for changes. Typically this is used for child windows that are shown from a parent window or specific view. Defaults to NSApp.
--
-- ObjC selector: @- setAppearanceSource:@
setAppearanceSource :: (IsNSWindow nsWindow, IsNSObject value) => nsWindow -> value -> IO ()
setAppearanceSource nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setAppearanceSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorSpace@
colorSpace :: IsNSWindow nsWindow => nsWindow -> IO (Id NSColorSpace)
colorSpace nsWindow  =
    sendMsg nsWindow (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorSpace:@
setColorSpace :: (IsNSWindow nsWindow, IsNSColorSpace value) => nsWindow -> value -> IO ()
setColorSpace nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setColorSpace:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- occlusionState@
occlusionState :: IsNSWindow nsWindow => nsWindow -> IO NSWindowOcclusionState
occlusionState nsWindow  =
    fmap (coerce :: CULong -> NSWindowOcclusionState) $ sendMsg nsWindow (mkSelector "occlusionState") retCULong []

-- | Specifies the style of separator displayed between the window's titlebar and content.
--
-- The default value is NSTitlebarSeparatorStyleAutomatic. Changing this value will override any preference made by @NSSplitViewItem@.
--
-- ObjC selector: @- titlebarSeparatorStyle@
titlebarSeparatorStyle :: IsNSWindow nsWindow => nsWindow -> IO NSTitlebarSeparatorStyle
titlebarSeparatorStyle nsWindow  =
    fmap (coerce :: CLong -> NSTitlebarSeparatorStyle) $ sendMsg nsWindow (mkSelector "titlebarSeparatorStyle") retCLong []

-- | Specifies the style of separator displayed between the window's titlebar and content.
--
-- The default value is NSTitlebarSeparatorStyleAutomatic. Changing this value will override any preference made by @NSSplitViewItem@.
--
-- ObjC selector: @- setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyle :: IsNSWindow nsWindow => nsWindow -> NSTitlebarSeparatorStyle -> IO ()
setTitlebarSeparatorStyle nsWindow  value =
    sendMsg nsWindow (mkSelector "setTitlebarSeparatorStyle:") retVoid [argCLong (coerce value)]

-- | The main content view controller for the window. This provides the contentView of the window. Assigning this value will remove the existing contentView and will make the contentViewController.view the main contentView for the window. The default value is nil. The contentViewController only controls the contentView, and not the title of the window. The window title can easily be bound to the contentViewController with the following: [window bind:NSTitleBinding toObject:contentViewController withKeyPath:"title" options:nil]. Setting the contentViewController will cause the window to resize based on the current size of the contentViewController. Autolayout should be used to restrict the size of the window. The value of the contentViewController is encoded in the NIB. Directly assigning a contentView will clear out the contentViewController.
--
-- ObjC selector: @- contentViewController@
contentViewController :: IsNSWindow nsWindow => nsWindow -> IO (Id NSViewController)
contentViewController nsWindow  =
    sendMsg nsWindow (mkSelector "contentViewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The main content view controller for the window. This provides the contentView of the window. Assigning this value will remove the existing contentView and will make the contentViewController.view the main contentView for the window. The default value is nil. The contentViewController only controls the contentView, and not the title of the window. The window title can easily be bound to the contentViewController with the following: [window bind:NSTitleBinding toObject:contentViewController withKeyPath:"title" options:nil]. Setting the contentViewController will cause the window to resize based on the current size of the contentViewController. Autolayout should be used to restrict the size of the window. The value of the contentViewController is encoded in the NIB. Directly assigning a contentView will clear out the contentViewController.
--
-- ObjC selector: @- setContentViewController:@
setContentViewController :: (IsNSWindow nsWindow, IsNSViewController value) => nsWindow -> value -> IO ()
setContentViewController nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setContentViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- initialFirstResponder@
initialFirstResponder :: IsNSWindow nsWindow => nsWindow -> IO (Id NSView)
initialFirstResponder nsWindow  =
    sendMsg nsWindow (mkSelector "initialFirstResponder") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialFirstResponder:@
setInitialFirstResponder :: (IsNSWindow nsWindow, IsNSView value) => nsWindow -> value -> IO ()
setInitialFirstResponder nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setInitialFirstResponder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyViewSelectionDirection@
keyViewSelectionDirection :: IsNSWindow nsWindow => nsWindow -> IO NSSelectionDirection
keyViewSelectionDirection nsWindow  =
    fmap (coerce :: CULong -> NSSelectionDirection) $ sendMsg nsWindow (mkSelector "keyViewSelectionDirection") retCULong []

-- | @- defaultButtonCell@
defaultButtonCell :: IsNSWindow nsWindow => nsWindow -> IO (Id NSButtonCell)
defaultButtonCell nsWindow  =
    sendMsg nsWindow (mkSelector "defaultButtonCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultButtonCell:@
setDefaultButtonCell :: (IsNSWindow nsWindow, IsNSButtonCell value) => nsWindow -> value -> IO ()
setDefaultButtonCell nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setDefaultButtonCell:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autorecalculatesKeyViewLoop@
autorecalculatesKeyViewLoop :: IsNSWindow nsWindow => nsWindow -> IO Bool
autorecalculatesKeyViewLoop nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "autorecalculatesKeyViewLoop") retCULong []

-- | @- setAutorecalculatesKeyViewLoop:@
setAutorecalculatesKeyViewLoop :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAutorecalculatesKeyViewLoop nsWindow  value =
    sendMsg nsWindow (mkSelector "setAutorecalculatesKeyViewLoop:") retVoid [argCULong (if value then 1 else 0)]

-- | @- toolbar@
toolbar :: IsNSWindow nsWindow => nsWindow -> IO (Id NSToolbar)
toolbar nsWindow  =
    sendMsg nsWindow (mkSelector "toolbar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToolbar:@
setToolbar :: (IsNSWindow nsWindow, IsNSToolbar value) => nsWindow -> value -> IO ()
setToolbar nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setToolbar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- showsToolbarButton@
showsToolbarButton :: IsNSWindow nsWindow => nsWindow -> IO Bool
showsToolbarButton nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "showsToolbarButton") retCULong []

-- | @- setShowsToolbarButton:@
setShowsToolbarButton :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setShowsToolbarButton nsWindow  value =
    sendMsg nsWindow (mkSelector "setShowsToolbarButton:") retVoid [argCULong (if value then 1 else 0)]

-- | Allows automatic window tabbing when the value is @YES.@ By default, this will be set to @YES,@ but applications can explicitly opt out of all automatic tabbing by setting it to NO, and can still adopted explicit window tabbing, if desired.
--
-- ObjC selector: @+ allowsAutomaticWindowTabbing@
allowsAutomaticWindowTabbing :: IO Bool
allowsAutomaticWindowTabbing  =
  do
    cls' <- getRequiredClass "NSWindow"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "allowsAutomaticWindowTabbing") retCULong []

-- | Allows automatic window tabbing when the value is @YES.@ By default, this will be set to @YES,@ but applications can explicitly opt out of all automatic tabbing by setting it to NO, and can still adopted explicit window tabbing, if desired.
--
-- ObjC selector: @+ setAllowsAutomaticWindowTabbing:@
setAllowsAutomaticWindowTabbing :: Bool -> IO ()
setAllowsAutomaticWindowTabbing value =
  do
    cls' <- getRequiredClass "NSWindow"
    sendClassMsg cls' (mkSelector "setAllowsAutomaticWindowTabbing:") retVoid [argCULong (if value then 1 else 0)]

-- | Returns the user's tabbing preference as set in System Preferences. This value should be queried anytime a new window is made to see if the user wants to automatically show it in tabs.
--
-- ObjC selector: @+ userTabbingPreference@
userTabbingPreference :: IO NSWindowUserTabbingPreference
userTabbingPreference  =
  do
    cls' <- getRequiredClass "NSWindow"
    fmap (coerce :: CLong -> NSWindowUserTabbingPreference) $ sendClassMsg cls' (mkSelector "userTabbingPreference") retCLong []

-- | Get and set the tabbing mode for this window. This should be set before a window is shown. The default value is @NSWindowTabbingModeAutomatic.@ When the value is @NSWindowTabbingModeAutomatic,@ the system will look at the @userTabbingPreference@ and automatically tab windows together based on the tabbingIdentifier, when it is appropriate to do so.
--
-- ObjC selector: @- tabbingMode@
tabbingMode :: IsNSWindow nsWindow => nsWindow -> IO NSWindowTabbingMode
tabbingMode nsWindow  =
    fmap (coerce :: CLong -> NSWindowTabbingMode) $ sendMsg nsWindow (mkSelector "tabbingMode") retCLong []

-- | Get and set the tabbing mode for this window. This should be set before a window is shown. The default value is @NSWindowTabbingModeAutomatic.@ When the value is @NSWindowTabbingModeAutomatic,@ the system will look at the @userTabbingPreference@ and automatically tab windows together based on the tabbingIdentifier, when it is appropriate to do so.
--
-- ObjC selector: @- setTabbingMode:@
setTabbingMode :: IsNSWindow nsWindow => nsWindow -> NSWindowTabbingMode -> IO ()
setTabbingMode nsWindow  value =
    sendMsg nsWindow (mkSelector "setTabbingMode:") retVoid [argCLong (coerce value)]

-- | Windows with the same @tabbingIdentifier@ will have the ability to be tabbed together when a window is being shown. This allows aggregation of similar windows. By default, the @tabbingIdentifier@ will be generated based on inherent window properties, such as the window class name, the delegate class name, the window controller class name, and some additional state. Windows can be explicitly made to group together by using the same @tabbingIdentifier.@
--
-- ObjC selector: @- tabbingIdentifier@
tabbingIdentifier :: IsNSWindow nsWindow => nsWindow -> IO (Id NSString)
tabbingIdentifier nsWindow  =
    sendMsg nsWindow (mkSelector "tabbingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Windows with the same @tabbingIdentifier@ will have the ability to be tabbed together when a window is being shown. This allows aggregation of similar windows. By default, the @tabbingIdentifier@ will be generated based on inherent window properties, such as the window class name, the delegate class name, the window controller class name, and some additional state. Windows can be explicitly made to group together by using the same @tabbingIdentifier.@
--
-- ObjC selector: @- setTabbingIdentifier:@
setTabbingIdentifier :: (IsNSWindow nsWindow, IsNSString value) => nsWindow -> value -> IO ()
setTabbingIdentifier nsWindow  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindow (mkSelector "setTabbingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This is now a cover for @self.tabGroup.windows@, but will return nil if the window is not showing a tab bar.
--
-- ObjC selector: @- tabbedWindows@
tabbedWindows :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
tabbedWindows nsWindow  =
    sendMsg nsWindow (mkSelector "tabbedWindows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Access the properties for this window when it is a tabbed window environment. See the @NSWindowTab@ header and comments for more information.
--
-- ObjC selector: @- tab@
tab :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindowTab)
tab nsWindow  =
    sendMsg nsWindow (mkSelector "tab") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Represents a tab group of windows. This @tabGroup@ is lazily created on demand.
--
-- ObjC selector: @- tabGroup@
tabGroup :: IsNSWindow nsWindow => nsWindow -> IO (Id NSWindowTabGroup)
tabGroup nsWindow  =
    sendMsg nsWindow (mkSelector "tabGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the receiver is the subject of an active SharePlay sharing session.
--
-- ObjC selector: @- hasActiveWindowSharingSession@
hasActiveWindowSharingSession :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasActiveWindowSharingSession nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "hasActiveWindowSharingSession") retCULong []

-- | Retrieve the layout direction of the window titlebar: this includes the standard window buttons (close/minimize/maximize buttons) and the title for this window. In general, this will return "right to left" (RTL) if the primary system language is RTL. The layout direction may be RTL even in applications that do not have a RTL language localization. This value should be utilized if an application uses titlebarAppearsTransparent and places controls underneath the titlebar.
--
-- ObjC selector: @- windowTitlebarLayoutDirection@
windowTitlebarLayoutDirection :: IsNSWindow nsWindow => nsWindow -> IO NSUserInterfaceLayoutDirection
windowTitlebarLayoutDirection nsWindow  =
    fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsWindow (mkSelector "windowTitlebarLayoutDirection") retCLong []

-- | @- restorable@
restorable :: IsNSWindow nsWindow => nsWindow -> IO Bool
restorable nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "restorable") retCULong []

-- | @- setRestorable:@
setRestorable :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setRestorable nsWindow  value =
    sendMsg nsWindow (mkSelector "setRestorable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- restorationClass@
restorationClass :: IsNSWindow nsWindow => nsWindow -> IO Class
restorationClass nsWindow  =
    fmap (Class . castPtr) $ sendMsg nsWindow (mkSelector "restorationClass") (retPtr retVoid) []

-- | @- setRestorationClass:@
setRestorationClass :: IsNSWindow nsWindow => nsWindow -> Class -> IO ()
setRestorationClass nsWindow  value =
    sendMsg nsWindow (mkSelector "setRestorationClass:") retVoid [argPtr (unClass value)]

-- | @- hasCloseBox@
hasCloseBox :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasCloseBox nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "hasCloseBox") retCULong []

-- | @- hasTitleBar@
hasTitleBar :: IsNSWindow nsWindow => nsWindow -> IO Bool
hasTitleBar nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "hasTitleBar") retCULong []

-- | @- floatingPanel@
floatingPanel :: IsNSWindow nsWindow => nsWindow -> IO Bool
floatingPanel nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "floatingPanel") retCULong []

-- | @- miniaturizable@
miniaturizable :: IsNSWindow nsWindow => nsWindow -> IO Bool
miniaturizable nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "miniaturizable") retCULong []

-- | @- modalPanel@
modalPanel :: IsNSWindow nsWindow => nsWindow -> IO Bool
modalPanel nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "modalPanel") retCULong []

-- | @- resizable@
resizable :: IsNSWindow nsWindow => nsWindow -> IO Bool
resizable nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "resizable") retCULong []

-- | @- zoomable@
zoomable :: IsNSWindow nsWindow => nsWindow -> IO Bool
zoomable nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "zoomable") retCULong []

-- | @- orderedIndex@
orderedIndex :: IsNSWindow nsWindow => nsWindow -> IO CLong
orderedIndex nsWindow  =
    sendMsg nsWindow (mkSelector "orderedIndex") retCLong []

-- | @- setOrderedIndex:@
setOrderedIndex :: IsNSWindow nsWindow => nsWindow -> CLong -> IO ()
setOrderedIndex nsWindow  value =
    sendMsg nsWindow (mkSelector "setOrderedIndex:") retVoid [argCLong value]

-- | @- drawers@
drawers :: IsNSWindow nsWindow => nsWindow -> IO (Id NSArray)
drawers nsWindow  =
    sendMsg nsWindow (mkSelector "drawers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- flushWindowDisabled@
flushWindowDisabled :: IsNSWindow nsWindow => nsWindow -> IO Bool
flushWindowDisabled nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "flushWindowDisabled") retCULong []

-- | @- autodisplay@
autodisplay :: IsNSWindow nsWindow => nsWindow -> IO Bool
autodisplay nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "autodisplay") retCULong []

-- | @- setAutodisplay:@
setAutodisplay :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAutodisplay nsWindow  value =
    sendMsg nsWindow (mkSelector "setAutodisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | @- graphicsContext@
graphicsContext :: IsNSWindow nsWindow => nsWindow -> IO (Id NSGraphicsContext)
graphicsContext nsWindow  =
    sendMsg nsWindow (mkSelector "graphicsContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- oneShot@
oneShot :: IsNSWindow nsWindow => nsWindow -> IO Bool
oneShot nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "oneShot") retCULong []

-- | @- setOneShot:@
setOneShot :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setOneShot nsWindow  value =
    sendMsg nsWindow (mkSelector "setOneShot:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preferredBackingLocation@
preferredBackingLocation :: IsNSWindow nsWindow => nsWindow -> IO NSWindowBackingLocation
preferredBackingLocation nsWindow  =
    fmap (coerce :: CULong -> NSWindowBackingLocation) $ sendMsg nsWindow (mkSelector "preferredBackingLocation") retCULong []

-- | @- setPreferredBackingLocation:@
setPreferredBackingLocation :: IsNSWindow nsWindow => nsWindow -> NSWindowBackingLocation -> IO ()
setPreferredBackingLocation nsWindow  value =
    sendMsg nsWindow (mkSelector "setPreferredBackingLocation:") retVoid [argCULong (coerce value)]

-- | @- backingLocation@
backingLocation :: IsNSWindow nsWindow => nsWindow -> IO NSWindowBackingLocation
backingLocation nsWindow  =
    fmap (coerce :: CULong -> NSWindowBackingLocation) $ sendMsg nsWindow (mkSelector "backingLocation") retCULong []

-- | @- showsResizeIndicator@
showsResizeIndicator :: IsNSWindow nsWindow => nsWindow -> IO Bool
showsResizeIndicator nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "showsResizeIndicator") retCULong []

-- | @- setShowsResizeIndicator:@
setShowsResizeIndicator :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setShowsResizeIndicator nsWindow  value =
    sendMsg nsWindow (mkSelector "setShowsResizeIndicator:") retVoid [argCULong (if value then 1 else 0)]

-- | @- windowRef@
windowRef :: IsNSWindow nsWindow => nsWindow -> IO (Ptr ())
windowRef nsWindow  =
    fmap castPtr $ sendMsg nsWindow (mkSelector "windowRef") (retPtr retVoid) []

-- | @- areCursorRectsEnabled@
areCursorRectsEnabled :: IsNSWindow nsWindow => nsWindow -> IO Bool
areCursorRectsEnabled nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "areCursorRectsEnabled") retCULong []

-- | @- currentEvent@
currentEvent :: IsNSWindow nsWindow => nsWindow -> IO (Id NSEvent)
currentEvent nsWindow  =
    sendMsg nsWindow (mkSelector "currentEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- acceptsMouseMovedEvents@
acceptsMouseMovedEvents :: IsNSWindow nsWindow => nsWindow -> IO Bool
acceptsMouseMovedEvents nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "acceptsMouseMovedEvents") retCULong []

-- | @- setAcceptsMouseMovedEvents:@
setAcceptsMouseMovedEvents :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setAcceptsMouseMovedEvents nsWindow  value =
    sendMsg nsWindow (mkSelector "setAcceptsMouseMovedEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- ignoresMouseEvents@
ignoresMouseEvents :: IsNSWindow nsWindow => nsWindow -> IO Bool
ignoresMouseEvents nsWindow  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindow (mkSelector "ignoresMouseEvents") retCULong []

-- | @- setIgnoresMouseEvents:@
setIgnoresMouseEvents :: IsNSWindow nsWindow => nsWindow -> Bool -> IO ()
setIgnoresMouseEvents nsWindow  value =
    sendMsg nsWindow (mkSelector "setIgnoresMouseEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- mouseLocationOutsideOfEventStream@
mouseLocationOutsideOfEventStream :: IsNSWindow nsWindow => nsWindow -> IO NSPoint
mouseLocationOutsideOfEventStream nsWindow  =
    sendMsgStret nsWindow (mkSelector "mouseLocationOutsideOfEventStream") retNSPoint []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frameRectForContentRect:styleMask:@
frameRectForContentRect_styleMaskSelector :: Selector
frameRectForContentRect_styleMaskSelector = mkSelector "frameRectForContentRect:styleMask:"

-- | @Selector@ for @contentRectForFrameRect:styleMask:@
contentRectForFrameRect_styleMaskSelector :: Selector
contentRectForFrameRect_styleMaskSelector = mkSelector "contentRectForFrameRect:styleMask:"

-- | @Selector@ for @minFrameWidthWithTitle:styleMask:@
minFrameWidthWithTitle_styleMaskSelector :: Selector
minFrameWidthWithTitle_styleMaskSelector = mkSelector "minFrameWidthWithTitle:styleMask:"

-- | @Selector@ for @frameRectForContentRect:@
frameRectForContentRectSelector :: Selector
frameRectForContentRectSelector = mkSelector "frameRectForContentRect:"

-- | @Selector@ for @contentRectForFrameRect:@
contentRectForFrameRectSelector :: Selector
contentRectForFrameRectSelector = mkSelector "contentRectForFrameRect:"

-- | @Selector@ for @initWithContentRect:styleMask:backing:defer:@
initWithContentRect_styleMask_backing_deferSelector :: Selector
initWithContentRect_styleMask_backing_deferSelector = mkSelector "initWithContentRect:styleMask:backing:defer:"

-- | @Selector@ for @initWithContentRect:styleMask:backing:defer:screen:@
initWithContentRect_styleMask_backing_defer_screenSelector :: Selector
initWithContentRect_styleMask_backing_defer_screenSelector = mkSelector "initWithContentRect:styleMask:backing:defer:screen:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addTitlebarAccessoryViewController:@
addTitlebarAccessoryViewControllerSelector :: Selector
addTitlebarAccessoryViewControllerSelector = mkSelector "addTitlebarAccessoryViewController:"

-- | @Selector@ for @insertTitlebarAccessoryViewController:atIndex:@
insertTitlebarAccessoryViewController_atIndexSelector :: Selector
insertTitlebarAccessoryViewController_atIndexSelector = mkSelector "insertTitlebarAccessoryViewController:atIndex:"

-- | @Selector@ for @removeTitlebarAccessoryViewControllerAtIndex:@
removeTitlebarAccessoryViewControllerAtIndexSelector :: Selector
removeTitlebarAccessoryViewControllerAtIndexSelector = mkSelector "removeTitlebarAccessoryViewControllerAtIndex:"

-- | @Selector@ for @setTitleWithRepresentedFilename:@
setTitleWithRepresentedFilenameSelector :: Selector
setTitleWithRepresentedFilenameSelector = mkSelector "setTitleWithRepresentedFilename:"

-- | @Selector@ for @fieldEditor:forObject:@
fieldEditor_forObjectSelector :: Selector
fieldEditor_forObjectSelector = mkSelector "fieldEditor:forObject:"

-- | @Selector@ for @endEditingFor:@
endEditingForSelector :: Selector
endEditingForSelector = mkSelector "endEditingFor:"

-- | @Selector@ for @constrainFrameRect:toScreen:@
constrainFrameRect_toScreenSelector :: Selector
constrainFrameRect_toScreenSelector = mkSelector "constrainFrameRect:toScreen:"

-- | @Selector@ for @setFrame:display:@
setFrame_displaySelector :: Selector
setFrame_displaySelector = mkSelector "setFrame:display:"

-- | @Selector@ for @setContentSize:@
setContentSizeSelector :: Selector
setContentSizeSelector = mkSelector "setContentSize:"

-- | @Selector@ for @setFrameOrigin:@
setFrameOriginSelector :: Selector
setFrameOriginSelector = mkSelector "setFrameOrigin:"

-- | @Selector@ for @setFrameTopLeftPoint:@
setFrameTopLeftPointSelector :: Selector
setFrameTopLeftPointSelector = mkSelector "setFrameTopLeftPoint:"

-- | @Selector@ for @cascadeTopLeftFromPoint:@
cascadeTopLeftFromPointSelector :: Selector
cascadeTopLeftFromPointSelector = mkSelector "cascadeTopLeftFromPoint:"

-- | @Selector@ for @animationResizeTime:@
animationResizeTimeSelector :: Selector
animationResizeTimeSelector = mkSelector "animationResizeTime:"

-- | @Selector@ for @setFrame:display:animate:@
setFrame_display_animateSelector :: Selector
setFrame_display_animateSelector = mkSelector "setFrame:display:animate:"

-- | @Selector@ for @displayIfNeeded@
displayIfNeededSelector :: Selector
displayIfNeededSelector = mkSelector "displayIfNeeded"

-- | @Selector@ for @display@
displaySelector :: Selector
displaySelector = mkSelector "display"

-- | @Selector@ for @update@
updateSelector :: Selector
updateSelector = mkSelector "update"

-- | @Selector@ for @makeFirstResponder:@
makeFirstResponderSelector :: Selector
makeFirstResponderSelector = mkSelector "makeFirstResponder:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @miniaturize:@
miniaturizeSelector :: Selector
miniaturizeSelector = mkSelector "miniaturize:"

-- | @Selector@ for @deminiaturize:@
deminiaturizeSelector :: Selector
deminiaturizeSelector = mkSelector "deminiaturize:"

-- | @Selector@ for @zoom:@
zoomSelector :: Selector
zoomSelector = mkSelector "zoom:"

-- | @Selector@ for @tryToPerform:with:@
tryToPerform_withSelector :: Selector
tryToPerform_withSelector = mkSelector "tryToPerform:with:"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @setContentBorderThickness:forEdge:@
setContentBorderThickness_forEdgeSelector :: Selector
setContentBorderThickness_forEdgeSelector = mkSelector "setContentBorderThickness:forEdge:"

-- | @Selector@ for @contentBorderThicknessForEdge:@
contentBorderThicknessForEdgeSelector :: Selector
contentBorderThicknessForEdgeSelector = mkSelector "contentBorderThicknessForEdge:"

-- | @Selector@ for @setAutorecalculatesContentBorderThickness:forEdge:@
setAutorecalculatesContentBorderThickness_forEdgeSelector :: Selector
setAutorecalculatesContentBorderThickness_forEdgeSelector = mkSelector "setAutorecalculatesContentBorderThickness:forEdge:"

-- | @Selector@ for @autorecalculatesContentBorderThicknessForEdge:@
autorecalculatesContentBorderThicknessForEdgeSelector :: Selector
autorecalculatesContentBorderThicknessForEdgeSelector = mkSelector "autorecalculatesContentBorderThicknessForEdge:"

-- | @Selector@ for @center@
centerSelector :: Selector
centerSelector = mkSelector "center"

-- | @Selector@ for @makeKeyAndOrderFront:@
makeKeyAndOrderFrontSelector :: Selector
makeKeyAndOrderFrontSelector = mkSelector "makeKeyAndOrderFront:"

-- | @Selector@ for @orderFront:@
orderFrontSelector :: Selector
orderFrontSelector = mkSelector "orderFront:"

-- | @Selector@ for @orderBack:@
orderBackSelector :: Selector
orderBackSelector = mkSelector "orderBack:"

-- | @Selector@ for @orderOut:@
orderOutSelector :: Selector
orderOutSelector = mkSelector "orderOut:"

-- | @Selector@ for @orderWindow:relativeTo:@
orderWindow_relativeToSelector :: Selector
orderWindow_relativeToSelector = mkSelector "orderWindow:relativeTo:"

-- | @Selector@ for @orderFrontRegardless@
orderFrontRegardlessSelector :: Selector
orderFrontRegardlessSelector = mkSelector "orderFrontRegardless"

-- | @Selector@ for @makeKeyWindow@
makeKeyWindowSelector :: Selector
makeKeyWindowSelector = mkSelector "makeKeyWindow"

-- | @Selector@ for @makeMainWindow@
makeMainWindowSelector :: Selector
makeMainWindowSelector = mkSelector "makeMainWindow"

-- | @Selector@ for @becomeKeyWindow@
becomeKeyWindowSelector :: Selector
becomeKeyWindowSelector = mkSelector "becomeKeyWindow"

-- | @Selector@ for @resignKeyWindow@
resignKeyWindowSelector :: Selector
resignKeyWindowSelector = mkSelector "resignKeyWindow"

-- | @Selector@ for @becomeMainWindow@
becomeMainWindowSelector :: Selector
becomeMainWindowSelector = mkSelector "becomeMainWindow"

-- | @Selector@ for @resignMainWindow@
resignMainWindowSelector :: Selector
resignMainWindowSelector = mkSelector "resignMainWindow"

-- | @Selector@ for @convertRectToScreen:@
convertRectToScreenSelector :: Selector
convertRectToScreenSelector = mkSelector "convertRectToScreen:"

-- | @Selector@ for @convertRectFromScreen:@
convertRectFromScreenSelector :: Selector
convertRectFromScreenSelector = mkSelector "convertRectFromScreen:"

-- | @Selector@ for @convertPointToScreen:@
convertPointToScreenSelector :: Selector
convertPointToScreenSelector = mkSelector "convertPointToScreen:"

-- | @Selector@ for @convertPointFromScreen:@
convertPointFromScreenSelector :: Selector
convertPointFromScreenSelector = mkSelector "convertPointFromScreen:"

-- | @Selector@ for @convertRectToBacking:@
convertRectToBackingSelector :: Selector
convertRectToBackingSelector = mkSelector "convertRectToBacking:"

-- | @Selector@ for @convertRectFromBacking:@
convertRectFromBackingSelector :: Selector
convertRectFromBackingSelector = mkSelector "convertRectFromBacking:"

-- | @Selector@ for @convertPointToBacking:@
convertPointToBackingSelector :: Selector
convertPointToBackingSelector = mkSelector "convertPointToBacking:"

-- | @Selector@ for @convertPointFromBacking:@
convertPointFromBackingSelector :: Selector
convertPointFromBackingSelector = mkSelector "convertPointFromBacking:"

-- | @Selector@ for @backingAlignedRect:options:@
backingAlignedRect_optionsSelector :: Selector
backingAlignedRect_optionsSelector = mkSelector "backingAlignedRect:options:"

-- | @Selector@ for @performClose:@
performCloseSelector :: Selector
performCloseSelector = mkSelector "performClose:"

-- | @Selector@ for @performMiniaturize:@
performMiniaturizeSelector :: Selector
performMiniaturizeSelector = mkSelector "performMiniaturize:"

-- | @Selector@ for @performZoom:@
performZoomSelector :: Selector
performZoomSelector = mkSelector "performZoom:"

-- | @Selector@ for @dataWithEPSInsideRect:@
dataWithEPSInsideRectSelector :: Selector
dataWithEPSInsideRectSelector = mkSelector "dataWithEPSInsideRect:"

-- | @Selector@ for @dataWithPDFInsideRect:@
dataWithPDFInsideRectSelector :: Selector
dataWithPDFInsideRectSelector = mkSelector "dataWithPDFInsideRect:"

-- | @Selector@ for @print:@
printSelector :: Selector
printSelector = mkSelector "print:"

-- | @Selector@ for @setDynamicDepthLimit:@
setDynamicDepthLimitSelector :: Selector
setDynamicDepthLimitSelector = mkSelector "setDynamicDepthLimit:"

-- | @Selector@ for @invalidateShadow@
invalidateShadowSelector :: Selector
invalidateShadowSelector = mkSelector "invalidateShadow"

-- | @Selector@ for @toggleFullScreen:@
toggleFullScreenSelector :: Selector
toggleFullScreenSelector = mkSelector "toggleFullScreen:"

-- | @Selector@ for @setFrameFromString:@
setFrameFromStringSelector :: Selector
setFrameFromStringSelector = mkSelector "setFrameFromString:"

-- | @Selector@ for @saveFrameUsingName:@
saveFrameUsingNameSelector :: Selector
saveFrameUsingNameSelector = mkSelector "saveFrameUsingName:"

-- | @Selector@ for @setFrameUsingName:force:@
setFrameUsingName_forceSelector :: Selector
setFrameUsingName_forceSelector = mkSelector "setFrameUsingName:force:"

-- | @Selector@ for @setFrameUsingName:@
setFrameUsingNameSelector :: Selector
setFrameUsingNameSelector = mkSelector "setFrameUsingName:"

-- | @Selector@ for @setFrameAutosaveName:@
setFrameAutosaveNameSelector :: Selector
setFrameAutosaveNameSelector = mkSelector "setFrameAutosaveName:"

-- | @Selector@ for @removeFrameUsingName:@
removeFrameUsingNameSelector :: Selector
removeFrameUsingNameSelector = mkSelector "removeFrameUsingName:"

-- | @Selector@ for @beginSheet:completionHandler:@
beginSheet_completionHandlerSelector :: Selector
beginSheet_completionHandlerSelector = mkSelector "beginSheet:completionHandler:"

-- | @Selector@ for @beginCriticalSheet:completionHandler:@
beginCriticalSheet_completionHandlerSelector :: Selector
beginCriticalSheet_completionHandlerSelector = mkSelector "beginCriticalSheet:completionHandler:"

-- | @Selector@ for @endSheet:@
endSheetSelector :: Selector
endSheetSelector = mkSelector "endSheet:"

-- | @Selector@ for @endSheet:returnCode:@
endSheet_returnCodeSelector :: Selector
endSheet_returnCodeSelector = mkSelector "endSheet:returnCode:"

-- | @Selector@ for @standardWindowButton:forStyleMask:@
standardWindowButton_forStyleMaskSelector :: Selector
standardWindowButton_forStyleMaskSelector = mkSelector "standardWindowButton:forStyleMask:"

-- | @Selector@ for @standardWindowButton:@
standardWindowButtonSelector :: Selector
standardWindowButtonSelector = mkSelector "standardWindowButton:"

-- | @Selector@ for @addChildWindow:ordered:@
addChildWindow_orderedSelector :: Selector
addChildWindow_orderedSelector = mkSelector "addChildWindow:ordered:"

-- | @Selector@ for @removeChildWindow:@
removeChildWindowSelector :: Selector
removeChildWindowSelector = mkSelector "removeChildWindow:"

-- | @Selector@ for @canRepresentDisplayGamut:@
canRepresentDisplayGamutSelector :: Selector
canRepresentDisplayGamutSelector = mkSelector "canRepresentDisplayGamut:"

-- | @Selector@ for @windowNumbersWithOptions:@
windowNumbersWithOptionsSelector :: Selector
windowNumbersWithOptionsSelector = mkSelector "windowNumbersWithOptions:"

-- | @Selector@ for @windowNumberAtPoint:belowWindowWithWindowNumber:@
windowNumberAtPoint_belowWindowWithWindowNumberSelector :: Selector
windowNumberAtPoint_belowWindowWithWindowNumberSelector = mkSelector "windowNumberAtPoint:belowWindowWithWindowNumber:"

-- | @Selector@ for @windowWithContentViewController:@
windowWithContentViewControllerSelector :: Selector
windowWithContentViewControllerSelector = mkSelector "windowWithContentViewController:"

-- | @Selector@ for @performWindowDragWithEvent:@
performWindowDragWithEventSelector :: Selector
performWindowDragWithEventSelector = mkSelector "performWindowDragWithEvent:"

-- | @Selector@ for @selectNextKeyView:@
selectNextKeyViewSelector :: Selector
selectNextKeyViewSelector = mkSelector "selectNextKeyView:"

-- | @Selector@ for @selectPreviousKeyView:@
selectPreviousKeyViewSelector :: Selector
selectPreviousKeyViewSelector = mkSelector "selectPreviousKeyView:"

-- | @Selector@ for @selectKeyViewFollowingView:@
selectKeyViewFollowingViewSelector :: Selector
selectKeyViewFollowingViewSelector = mkSelector "selectKeyViewFollowingView:"

-- | @Selector@ for @selectKeyViewPrecedingView:@
selectKeyViewPrecedingViewSelector :: Selector
selectKeyViewPrecedingViewSelector = mkSelector "selectKeyViewPrecedingView:"

-- | @Selector@ for @disableKeyEquivalentForDefaultButtonCell@
disableKeyEquivalentForDefaultButtonCellSelector :: Selector
disableKeyEquivalentForDefaultButtonCellSelector = mkSelector "disableKeyEquivalentForDefaultButtonCell"

-- | @Selector@ for @enableKeyEquivalentForDefaultButtonCell@
enableKeyEquivalentForDefaultButtonCellSelector :: Selector
enableKeyEquivalentForDefaultButtonCellSelector = mkSelector "enableKeyEquivalentForDefaultButtonCell"

-- | @Selector@ for @recalculateKeyViewLoop@
recalculateKeyViewLoopSelector :: Selector
recalculateKeyViewLoopSelector = mkSelector "recalculateKeyViewLoop"

-- | @Selector@ for @toggleToolbarShown:@
toggleToolbarShownSelector :: Selector
toggleToolbarShownSelector = mkSelector "toggleToolbarShown:"

-- | @Selector@ for @runToolbarCustomizationPalette:@
runToolbarCustomizationPaletteSelector :: Selector
runToolbarCustomizationPaletteSelector = mkSelector "runToolbarCustomizationPalette:"

-- | @Selector@ for @selectNextTab:@
selectNextTabSelector :: Selector
selectNextTabSelector = mkSelector "selectNextTab:"

-- | @Selector@ for @selectPreviousTab:@
selectPreviousTabSelector :: Selector
selectPreviousTabSelector = mkSelector "selectPreviousTab:"

-- | @Selector@ for @moveTabToNewWindow:@
moveTabToNewWindowSelector :: Selector
moveTabToNewWindowSelector = mkSelector "moveTabToNewWindow:"

-- | @Selector@ for @mergeAllWindows:@
mergeAllWindowsSelector :: Selector
mergeAllWindowsSelector = mkSelector "mergeAllWindows:"

-- | @Selector@ for @toggleTabBar:@
toggleTabBarSelector :: Selector
toggleTabBarSelector = mkSelector "toggleTabBar:"

-- | @Selector@ for @toggleTabOverview:@
toggleTabOverviewSelector :: Selector
toggleTabOverviewSelector = mkSelector "toggleTabOverview:"

-- | @Selector@ for @addTabbedWindow:ordered:@
addTabbedWindow_orderedSelector :: Selector
addTabbedWindow_orderedSelector = mkSelector "addTabbedWindow:ordered:"

-- | @Selector@ for @transferWindowSharingToWindow:completionHandler:@
transferWindowSharingToWindow_completionHandlerSelector :: Selector
transferWindowSharingToWindow_completionHandlerSelector = mkSelector "transferWindowSharingToWindow:completionHandler:"

-- | @Selector@ for @requestSharingOfWindow:completionHandler:@
requestSharingOfWindow_completionHandlerSelector :: Selector
requestSharingOfWindow_completionHandlerSelector = mkSelector "requestSharingOfWindow:completionHandler:"

-- | @Selector@ for @requestSharingOfWindowUsingPreview:title:completionHandler:@
requestSharingOfWindowUsingPreview_title_completionHandlerSelector :: Selector
requestSharingOfWindowUsingPreview_title_completionHandlerSelector = mkSelector "requestSharingOfWindowUsingPreview:title:completionHandler:"

-- | @Selector@ for @disableSnapshotRestoration@
disableSnapshotRestorationSelector :: Selector
disableSnapshotRestorationSelector = mkSelector "disableSnapshotRestoration"

-- | @Selector@ for @enableSnapshotRestoration@
enableSnapshotRestorationSelector :: Selector
enableSnapshotRestorationSelector = mkSelector "enableSnapshotRestoration"

-- | @Selector@ for @setIsMiniaturized:@
setIsMiniaturizedSelector :: Selector
setIsMiniaturizedSelector = mkSelector "setIsMiniaturized:"

-- | @Selector@ for @setIsVisible:@
setIsVisibleSelector :: Selector
setIsVisibleSelector = mkSelector "setIsVisible:"

-- | @Selector@ for @setIsZoomed:@
setIsZoomedSelector :: Selector
setIsZoomedSelector = mkSelector "setIsZoomed:"

-- | @Selector@ for @handleCloseScriptCommand:@
handleCloseScriptCommandSelector :: Selector
handleCloseScriptCommandSelector = mkSelector "handleCloseScriptCommand:"

-- | @Selector@ for @handlePrintScriptCommand:@
handlePrintScriptCommandSelector :: Selector
handlePrintScriptCommandSelector = mkSelector "handlePrintScriptCommand:"

-- | @Selector@ for @handleSaveScriptCommand:@
handleSaveScriptCommandSelector :: Selector
handleSaveScriptCommandSelector = mkSelector "handleSaveScriptCommand:"

-- | @Selector@ for @visualizeConstraints:@
visualizeConstraintsSelector :: Selector
visualizeConstraintsSelector = mkSelector "visualizeConstraints:"

-- | @Selector@ for @anchorAttributeForOrientation:@
anchorAttributeForOrientationSelector :: Selector
anchorAttributeForOrientationSelector = mkSelector "anchorAttributeForOrientation:"

-- | @Selector@ for @setAnchorAttribute:forOrientation:@
setAnchorAttribute_forOrientationSelector :: Selector
setAnchorAttribute_forOrientationSelector = mkSelector "setAnchorAttribute:forOrientation:"

-- | @Selector@ for @updateConstraintsIfNeeded@
updateConstraintsIfNeededSelector :: Selector
updateConstraintsIfNeededSelector = mkSelector "updateConstraintsIfNeeded"

-- | @Selector@ for @layoutIfNeeded@
layoutIfNeededSelector :: Selector
layoutIfNeededSelector = mkSelector "layoutIfNeeded"

-- | @Selector@ for @cacheImageInRect:@
cacheImageInRectSelector :: Selector
cacheImageInRectSelector = mkSelector "cacheImageInRect:"

-- | @Selector@ for @restoreCachedImage@
restoreCachedImageSelector :: Selector
restoreCachedImageSelector = mkSelector "restoreCachedImage"

-- | @Selector@ for @discardCachedImage@
discardCachedImageSelector :: Selector
discardCachedImageSelector = mkSelector "discardCachedImage"

-- | @Selector@ for @menuChanged:@
menuChangedSelector :: Selector
menuChangedSelector = mkSelector "menuChanged:"

-- | @Selector@ for @gState@
gStateSelector :: Selector
gStateSelector = mkSelector "gState"

-- | @Selector@ for @convertBaseToScreen:@
convertBaseToScreenSelector :: Selector
convertBaseToScreenSelector = mkSelector "convertBaseToScreen:"

-- | @Selector@ for @convertScreenToBase:@
convertScreenToBaseSelector :: Selector
convertScreenToBaseSelector = mkSelector "convertScreenToBase:"

-- | @Selector@ for @userSpaceScaleFactor@
userSpaceScaleFactorSelector :: Selector
userSpaceScaleFactorSelector = mkSelector "userSpaceScaleFactor"

-- | @Selector@ for @useOptimizedDrawing:@
useOptimizedDrawingSelector :: Selector
useOptimizedDrawingSelector = mkSelector "useOptimizedDrawing:"

-- | @Selector@ for @canStoreColor@
canStoreColorSelector :: Selector
canStoreColorSelector = mkSelector "canStoreColor"

-- | @Selector@ for @disableFlushWindow@
disableFlushWindowSelector :: Selector
disableFlushWindowSelector = mkSelector "disableFlushWindow"

-- | @Selector@ for @enableFlushWindow@
enableFlushWindowSelector :: Selector
enableFlushWindowSelector = mkSelector "enableFlushWindow"

-- | @Selector@ for @flushWindow@
flushWindowSelector :: Selector
flushWindowSelector = mkSelector "flushWindow"

-- | @Selector@ for @flushWindowIfNeeded@
flushWindowIfNeededSelector :: Selector
flushWindowIfNeededSelector = mkSelector "flushWindowIfNeeded"

-- | @Selector@ for @initWithWindowRef:@
initWithWindowRefSelector :: Selector
initWithWindowRefSelector = mkSelector "initWithWindowRef:"

-- | @Selector@ for @disableScreenUpdatesUntilFlush@
disableScreenUpdatesUntilFlushSelector :: Selector
disableScreenUpdatesUntilFlushSelector = mkSelector "disableScreenUpdatesUntilFlush"

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @beginDraggingSessionWithItems:event:source:@
beginDraggingSessionWithItems_event_sourceSelector :: Selector
beginDraggingSessionWithItems_event_sourceSelector = mkSelector "beginDraggingSessionWithItems:event:source:"

-- | @Selector@ for @dragImage:at:offset:event:pasteboard:source:slideBack:@
dragImage_at_offset_event_pasteboard_source_slideBackSelector :: Selector
dragImage_at_offset_event_pasteboard_source_slideBackSelector = mkSelector "dragImage:at:offset:event:pasteboard:source:slideBack:"

-- | @Selector@ for @registerForDraggedTypes:@
registerForDraggedTypesSelector :: Selector
registerForDraggedTypesSelector = mkSelector "registerForDraggedTypes:"

-- | @Selector@ for @unregisterDraggedTypes@
unregisterDraggedTypesSelector :: Selector
unregisterDraggedTypesSelector = mkSelector "unregisterDraggedTypes"

-- | @Selector@ for @disableCursorRects@
disableCursorRectsSelector :: Selector
disableCursorRectsSelector = mkSelector "disableCursorRects"

-- | @Selector@ for @enableCursorRects@
enableCursorRectsSelector :: Selector
enableCursorRectsSelector = mkSelector "enableCursorRects"

-- | @Selector@ for @discardCursorRects@
discardCursorRectsSelector :: Selector
discardCursorRectsSelector = mkSelector "discardCursorRects"

-- | @Selector@ for @invalidateCursorRectsForView:@
invalidateCursorRectsForViewSelector :: Selector
invalidateCursorRectsForViewSelector = mkSelector "invalidateCursorRectsForView:"

-- | @Selector@ for @resetCursorRects@
resetCursorRectsSelector :: Selector
resetCursorRectsSelector = mkSelector "resetCursorRects"

-- | @Selector@ for @trackEventsMatchingMask:timeout:mode:handler:@
trackEventsMatchingMask_timeout_mode_handlerSelector :: Selector
trackEventsMatchingMask_timeout_mode_handlerSelector = mkSelector "trackEventsMatchingMask:timeout:mode:handler:"

-- | @Selector@ for @nextEventMatchingMask:@
nextEventMatchingMaskSelector :: Selector
nextEventMatchingMaskSelector = mkSelector "nextEventMatchingMask:"

-- | @Selector@ for @nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeueSelector :: Selector
nextEventMatchingMask_untilDate_inMode_dequeueSelector = mkSelector "nextEventMatchingMask:untilDate:inMode:dequeue:"

-- | @Selector@ for @discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEventSelector :: Selector
discardEventsMatchingMask_beforeEventSelector = mkSelector "discardEventsMatchingMask:beforeEvent:"

-- | @Selector@ for @postEvent:atStart:@
postEvent_atStartSelector :: Selector
postEvent_atStartSelector = mkSelector "postEvent:atStart:"

-- | @Selector@ for @sendEvent:@
sendEventSelector :: Selector
sendEventSelector = mkSelector "sendEvent:"

-- | @Selector@ for @defaultDepthLimit@
defaultDepthLimitSelector :: Selector
defaultDepthLimitSelector = mkSelector "defaultDepthLimit"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @titleVisibility@
titleVisibilitySelector :: Selector
titleVisibilitySelector = mkSelector "titleVisibility"

-- | @Selector@ for @setTitleVisibility:@
setTitleVisibilitySelector :: Selector
setTitleVisibilitySelector = mkSelector "setTitleVisibility:"

-- | @Selector@ for @titlebarAppearsTransparent@
titlebarAppearsTransparentSelector :: Selector
titlebarAppearsTransparentSelector = mkSelector "titlebarAppearsTransparent"

-- | @Selector@ for @setTitlebarAppearsTransparent:@
setTitlebarAppearsTransparentSelector :: Selector
setTitlebarAppearsTransparentSelector = mkSelector "setTitlebarAppearsTransparent:"

-- | @Selector@ for @toolbarStyle@
toolbarStyleSelector :: Selector
toolbarStyleSelector = mkSelector "toolbarStyle"

-- | @Selector@ for @setToolbarStyle:@
setToolbarStyleSelector :: Selector
setToolbarStyleSelector = mkSelector "setToolbarStyle:"

-- | @Selector@ for @contentLayoutRect@
contentLayoutRectSelector :: Selector
contentLayoutRectSelector = mkSelector "contentLayoutRect"

-- | @Selector@ for @contentLayoutGuide@
contentLayoutGuideSelector :: Selector
contentLayoutGuideSelector = mkSelector "contentLayoutGuide"

-- | @Selector@ for @titlebarAccessoryViewControllers@
titlebarAccessoryViewControllersSelector :: Selector
titlebarAccessoryViewControllersSelector = mkSelector "titlebarAccessoryViewControllers"

-- | @Selector@ for @setTitlebarAccessoryViewControllers:@
setTitlebarAccessoryViewControllersSelector :: Selector
setTitlebarAccessoryViewControllersSelector = mkSelector "setTitlebarAccessoryViewControllers:"

-- | @Selector@ for @representedURL@
representedURLSelector :: Selector
representedURLSelector = mkSelector "representedURL"

-- | @Selector@ for @setRepresentedURL:@
setRepresentedURLSelector :: Selector
setRepresentedURLSelector = mkSelector "setRepresentedURL:"

-- | @Selector@ for @representedFilename@
representedFilenameSelector :: Selector
representedFilenameSelector = mkSelector "representedFilename"

-- | @Selector@ for @setRepresentedFilename:@
setRepresentedFilenameSelector :: Selector
setRepresentedFilenameSelector = mkSelector "setRepresentedFilename:"

-- | @Selector@ for @excludedFromWindowsMenu@
excludedFromWindowsMenuSelector :: Selector
excludedFromWindowsMenuSelector = mkSelector "excludedFromWindowsMenu"

-- | @Selector@ for @setExcludedFromWindowsMenu:@
setExcludedFromWindowsMenuSelector :: Selector
setExcludedFromWindowsMenuSelector = mkSelector "setExcludedFromWindowsMenu:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @windowNumber@
windowNumberSelector :: Selector
windowNumberSelector = mkSelector "windowNumber"

-- | @Selector@ for @styleMask@
styleMaskSelector :: Selector
styleMaskSelector = mkSelector "styleMask"

-- | @Selector@ for @setStyleMask:@
setStyleMaskSelector :: Selector
setStyleMaskSelector = mkSelector "setStyleMask:"

-- | @Selector@ for @cascadingReferenceFrame@
cascadingReferenceFrameSelector :: Selector
cascadingReferenceFrameSelector = mkSelector "cascadingReferenceFrame"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @inLiveResize@
inLiveResizeSelector :: Selector
inLiveResizeSelector = mkSelector "inLiveResize"

-- | @Selector@ for @resizeIncrements@
resizeIncrementsSelector :: Selector
resizeIncrementsSelector = mkSelector "resizeIncrements"

-- | @Selector@ for @setResizeIncrements:@
setResizeIncrementsSelector :: Selector
setResizeIncrementsSelector = mkSelector "setResizeIncrements:"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector
aspectRatioSelector = mkSelector "aspectRatio"

-- | @Selector@ for @setAspectRatio:@
setAspectRatioSelector :: Selector
setAspectRatioSelector = mkSelector "setAspectRatio:"

-- | @Selector@ for @contentResizeIncrements@
contentResizeIncrementsSelector :: Selector
contentResizeIncrementsSelector = mkSelector "contentResizeIncrements"

-- | @Selector@ for @setContentResizeIncrements:@
setContentResizeIncrementsSelector :: Selector
setContentResizeIncrementsSelector = mkSelector "setContentResizeIncrements:"

-- | @Selector@ for @contentAspectRatio@
contentAspectRatioSelector :: Selector
contentAspectRatioSelector = mkSelector "contentAspectRatio"

-- | @Selector@ for @setContentAspectRatio:@
setContentAspectRatioSelector :: Selector
setContentAspectRatioSelector = mkSelector "setContentAspectRatio:"

-- | @Selector@ for @viewsNeedDisplay@
viewsNeedDisplaySelector :: Selector
viewsNeedDisplaySelector = mkSelector "viewsNeedDisplay"

-- | @Selector@ for @setViewsNeedDisplay:@
setViewsNeedDisplaySelector :: Selector
setViewsNeedDisplaySelector = mkSelector "setViewsNeedDisplay:"

-- | @Selector@ for @preservesContentDuringLiveResize@
preservesContentDuringLiveResizeSelector :: Selector
preservesContentDuringLiveResizeSelector = mkSelector "preservesContentDuringLiveResize"

-- | @Selector@ for @setPreservesContentDuringLiveResize:@
setPreservesContentDuringLiveResizeSelector :: Selector
setPreservesContentDuringLiveResizeSelector = mkSelector "setPreservesContentDuringLiveResize:"

-- | @Selector@ for @firstResponder@
firstResponderSelector :: Selector
firstResponderSelector = mkSelector "firstResponder"

-- | @Selector@ for @resizeFlags@
resizeFlagsSelector :: Selector
resizeFlagsSelector = mkSelector "resizeFlags"

-- | @Selector@ for @releasedWhenClosed@
releasedWhenClosedSelector :: Selector
releasedWhenClosedSelector = mkSelector "releasedWhenClosed"

-- | @Selector@ for @setReleasedWhenClosed:@
setReleasedWhenClosedSelector :: Selector
setReleasedWhenClosedSelector = mkSelector "setReleasedWhenClosed:"

-- | @Selector@ for @zoomed@
zoomedSelector :: Selector
zoomedSelector = mkSelector "zoomed"

-- | @Selector@ for @miniaturized@
miniaturizedSelector :: Selector
miniaturizedSelector = mkSelector "miniaturized"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @movable@
movableSelector :: Selector
movableSelector = mkSelector "movable"

-- | @Selector@ for @setMovable:@
setMovableSelector :: Selector
setMovableSelector = mkSelector "setMovable:"

-- | @Selector@ for @movableByWindowBackground@
movableByWindowBackgroundSelector :: Selector
movableByWindowBackgroundSelector = mkSelector "movableByWindowBackground"

-- | @Selector@ for @setMovableByWindowBackground:@
setMovableByWindowBackgroundSelector :: Selector
setMovableByWindowBackgroundSelector = mkSelector "setMovableByWindowBackground:"

-- | @Selector@ for @hidesOnDeactivate@
hidesOnDeactivateSelector :: Selector
hidesOnDeactivateSelector = mkSelector "hidesOnDeactivate"

-- | @Selector@ for @setHidesOnDeactivate:@
setHidesOnDeactivateSelector :: Selector
setHidesOnDeactivateSelector = mkSelector "setHidesOnDeactivate:"

-- | @Selector@ for @canHide@
canHideSelector :: Selector
canHideSelector = mkSelector "canHide"

-- | @Selector@ for @setCanHide:@
setCanHideSelector :: Selector
setCanHideSelector = mkSelector "setCanHide:"

-- | @Selector@ for @miniwindowImage@
miniwindowImageSelector :: Selector
miniwindowImageSelector = mkSelector "miniwindowImage"

-- | @Selector@ for @setMiniwindowImage:@
setMiniwindowImageSelector :: Selector
setMiniwindowImageSelector = mkSelector "setMiniwindowImage:"

-- | @Selector@ for @miniwindowTitle@
miniwindowTitleSelector :: Selector
miniwindowTitleSelector = mkSelector "miniwindowTitle"

-- | @Selector@ for @setMiniwindowTitle:@
setMiniwindowTitleSelector :: Selector
setMiniwindowTitleSelector = mkSelector "setMiniwindowTitle:"

-- | @Selector@ for @dockTile@
dockTileSelector :: Selector
dockTileSelector = mkSelector "dockTile"

-- | @Selector@ for @documentEdited@
documentEditedSelector :: Selector
documentEditedSelector = mkSelector "documentEdited"

-- | @Selector@ for @setDocumentEdited:@
setDocumentEditedSelector :: Selector
setDocumentEditedSelector = mkSelector "setDocumentEdited:"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @keyWindow@
keyWindowSelector :: Selector
keyWindowSelector = mkSelector "keyWindow"

-- | @Selector@ for @mainWindow@
mainWindowSelector :: Selector
mainWindowSelector = mkSelector "mainWindow"

-- | @Selector@ for @canBecomeKeyWindow@
canBecomeKeyWindowSelector :: Selector
canBecomeKeyWindowSelector = mkSelector "canBecomeKeyWindow"

-- | @Selector@ for @canBecomeMainWindow@
canBecomeMainWindowSelector :: Selector
canBecomeMainWindowSelector = mkSelector "canBecomeMainWindow"

-- | @Selector@ for @worksWhenModal@
worksWhenModalSelector :: Selector
worksWhenModalSelector = mkSelector "worksWhenModal"

-- | @Selector@ for @preventsApplicationTerminationWhenModal@
preventsApplicationTerminationWhenModalSelector :: Selector
preventsApplicationTerminationWhenModalSelector = mkSelector "preventsApplicationTerminationWhenModal"

-- | @Selector@ for @setPreventsApplicationTerminationWhenModal:@
setPreventsApplicationTerminationWhenModalSelector :: Selector
setPreventsApplicationTerminationWhenModalSelector = mkSelector "setPreventsApplicationTerminationWhenModal:"

-- | @Selector@ for @backingScaleFactor@
backingScaleFactorSelector :: Selector
backingScaleFactorSelector = mkSelector "backingScaleFactor"

-- | @Selector@ for @allowsToolTipsWhenApplicationIsInactive@
allowsToolTipsWhenApplicationIsInactiveSelector :: Selector
allowsToolTipsWhenApplicationIsInactiveSelector = mkSelector "allowsToolTipsWhenApplicationIsInactive"

-- | @Selector@ for @setAllowsToolTipsWhenApplicationIsInactive:@
setAllowsToolTipsWhenApplicationIsInactiveSelector :: Selector
setAllowsToolTipsWhenApplicationIsInactiveSelector = mkSelector "setAllowsToolTipsWhenApplicationIsInactive:"

-- | @Selector@ for @backingType@
backingTypeSelector :: Selector
backingTypeSelector = mkSelector "backingType"

-- | @Selector@ for @setBackingType:@
setBackingTypeSelector :: Selector
setBackingTypeSelector = mkSelector "setBackingType:"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @depthLimit@
depthLimitSelector :: Selector
depthLimitSelector = mkSelector "depthLimit"

-- | @Selector@ for @setDepthLimit:@
setDepthLimitSelector :: Selector
setDepthLimitSelector = mkSelector "setDepthLimit:"

-- | @Selector@ for @hasDynamicDepthLimit@
hasDynamicDepthLimitSelector :: Selector
hasDynamicDepthLimitSelector = mkSelector "hasDynamicDepthLimit"

-- | @Selector@ for @screen@
screenSelector :: Selector
screenSelector = mkSelector "screen"

-- | @Selector@ for @deepestScreen@
deepestScreenSelector :: Selector
deepestScreenSelector = mkSelector "deepestScreen"

-- | @Selector@ for @hasShadow@
hasShadowSelector :: Selector
hasShadowSelector = mkSelector "hasShadow"

-- | @Selector@ for @setHasShadow:@
setHasShadowSelector :: Selector
setHasShadowSelector = mkSelector "setHasShadow:"

-- | @Selector@ for @alphaValue@
alphaValueSelector :: Selector
alphaValueSelector = mkSelector "alphaValue"

-- | @Selector@ for @setAlphaValue:@
setAlphaValueSelector :: Selector
setAlphaValueSelector = mkSelector "setAlphaValue:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @sharingType@
sharingTypeSelector :: Selector
sharingTypeSelector = mkSelector "sharingType"

-- | @Selector@ for @setSharingType:@
setSharingTypeSelector :: Selector
setSharingTypeSelector = mkSelector "setSharingType:"

-- | @Selector@ for @allowsConcurrentViewDrawing@
allowsConcurrentViewDrawingSelector :: Selector
allowsConcurrentViewDrawingSelector = mkSelector "allowsConcurrentViewDrawing"

-- | @Selector@ for @setAllowsConcurrentViewDrawing:@
setAllowsConcurrentViewDrawingSelector :: Selector
setAllowsConcurrentViewDrawingSelector = mkSelector "setAllowsConcurrentViewDrawing:"

-- | @Selector@ for @displaysWhenScreenProfileChanges@
displaysWhenScreenProfileChangesSelector :: Selector
displaysWhenScreenProfileChangesSelector = mkSelector "displaysWhenScreenProfileChanges"

-- | @Selector@ for @setDisplaysWhenScreenProfileChanges:@
setDisplaysWhenScreenProfileChangesSelector :: Selector
setDisplaysWhenScreenProfileChangesSelector = mkSelector "setDisplaysWhenScreenProfileChanges:"

-- | @Selector@ for @canBecomeVisibleWithoutLogin@
canBecomeVisibleWithoutLoginSelector :: Selector
canBecomeVisibleWithoutLoginSelector = mkSelector "canBecomeVisibleWithoutLogin"

-- | @Selector@ for @setCanBecomeVisibleWithoutLogin:@
setCanBecomeVisibleWithoutLoginSelector :: Selector
setCanBecomeVisibleWithoutLoginSelector = mkSelector "setCanBecomeVisibleWithoutLogin:"

-- | @Selector@ for @collectionBehavior@
collectionBehaviorSelector :: Selector
collectionBehaviorSelector = mkSelector "collectionBehavior"

-- | @Selector@ for @setCollectionBehavior:@
setCollectionBehaviorSelector :: Selector
setCollectionBehaviorSelector = mkSelector "setCollectionBehavior:"

-- | @Selector@ for @animationBehavior@
animationBehaviorSelector :: Selector
animationBehaviorSelector = mkSelector "animationBehavior"

-- | @Selector@ for @setAnimationBehavior:@
setAnimationBehaviorSelector :: Selector
setAnimationBehaviorSelector = mkSelector "setAnimationBehavior:"

-- | @Selector@ for @onActiveSpace@
onActiveSpaceSelector :: Selector
onActiveSpaceSelector = mkSelector "onActiveSpace"

-- | @Selector@ for @stringWithSavedFrame@
stringWithSavedFrameSelector :: Selector
stringWithSavedFrameSelector = mkSelector "stringWithSavedFrame"

-- | @Selector@ for @frameAutosaveName@
frameAutosaveNameSelector :: Selector
frameAutosaveNameSelector = mkSelector "frameAutosaveName"

-- | @Selector@ for @minSize@
minSizeSelector :: Selector
minSizeSelector = mkSelector "minSize"

-- | @Selector@ for @setMinSize:@
setMinSizeSelector :: Selector
setMinSizeSelector = mkSelector "setMinSize:"

-- | @Selector@ for @maxSize@
maxSizeSelector :: Selector
maxSizeSelector = mkSelector "maxSize"

-- | @Selector@ for @setMaxSize:@
setMaxSizeSelector :: Selector
setMaxSizeSelector = mkSelector "setMaxSize:"

-- | @Selector@ for @contentMinSize@
contentMinSizeSelector :: Selector
contentMinSizeSelector = mkSelector "contentMinSize"

-- | @Selector@ for @setContentMinSize:@
setContentMinSizeSelector :: Selector
setContentMinSizeSelector = mkSelector "setContentMinSize:"

-- | @Selector@ for @contentMaxSize@
contentMaxSizeSelector :: Selector
contentMaxSizeSelector = mkSelector "contentMaxSize"

-- | @Selector@ for @setContentMaxSize:@
setContentMaxSizeSelector :: Selector
setContentMaxSizeSelector = mkSelector "setContentMaxSize:"

-- | @Selector@ for @minFullScreenContentSize@
minFullScreenContentSizeSelector :: Selector
minFullScreenContentSizeSelector = mkSelector "minFullScreenContentSize"

-- | @Selector@ for @setMinFullScreenContentSize:@
setMinFullScreenContentSizeSelector :: Selector
setMinFullScreenContentSizeSelector = mkSelector "setMinFullScreenContentSize:"

-- | @Selector@ for @maxFullScreenContentSize@
maxFullScreenContentSizeSelector :: Selector
maxFullScreenContentSizeSelector = mkSelector "maxFullScreenContentSize"

-- | @Selector@ for @setMaxFullScreenContentSize:@
setMaxFullScreenContentSizeSelector :: Selector
setMaxFullScreenContentSizeSelector = mkSelector "setMaxFullScreenContentSize:"

-- | @Selector@ for @deviceDescription@
deviceDescriptionSelector :: Selector
deviceDescriptionSelector = mkSelector "deviceDescription"

-- | @Selector@ for @windowController@
windowControllerSelector :: Selector
windowControllerSelector = mkSelector "windowController"

-- | @Selector@ for @setWindowController:@
setWindowControllerSelector :: Selector
setWindowControllerSelector = mkSelector "setWindowController:"

-- | @Selector@ for @sheets@
sheetsSelector :: Selector
sheetsSelector = mkSelector "sheets"

-- | @Selector@ for @attachedSheet@
attachedSheetSelector :: Selector
attachedSheetSelector = mkSelector "attachedSheet"

-- | @Selector@ for @sheet@
sheetSelector :: Selector
sheetSelector = mkSelector "sheet"

-- | @Selector@ for @sheetParent@
sheetParentSelector :: Selector
sheetParentSelector = mkSelector "sheetParent"

-- | @Selector@ for @childWindows@
childWindowsSelector :: Selector
childWindowsSelector = mkSelector "childWindows"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector
setParentWindowSelector = mkSelector "setParentWindow:"

-- | @Selector@ for @appearanceSource@
appearanceSourceSelector :: Selector
appearanceSourceSelector = mkSelector "appearanceSource"

-- | @Selector@ for @setAppearanceSource:@
setAppearanceSourceSelector :: Selector
setAppearanceSourceSelector = mkSelector "setAppearanceSource:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector
setColorSpaceSelector = mkSelector "setColorSpace:"

-- | @Selector@ for @occlusionState@
occlusionStateSelector :: Selector
occlusionStateSelector = mkSelector "occlusionState"

-- | @Selector@ for @titlebarSeparatorStyle@
titlebarSeparatorStyleSelector :: Selector
titlebarSeparatorStyleSelector = mkSelector "titlebarSeparatorStyle"

-- | @Selector@ for @setTitlebarSeparatorStyle:@
setTitlebarSeparatorStyleSelector :: Selector
setTitlebarSeparatorStyleSelector = mkSelector "setTitlebarSeparatorStyle:"

-- | @Selector@ for @contentViewController@
contentViewControllerSelector :: Selector
contentViewControllerSelector = mkSelector "contentViewController"

-- | @Selector@ for @setContentViewController:@
setContentViewControllerSelector :: Selector
setContentViewControllerSelector = mkSelector "setContentViewController:"

-- | @Selector@ for @initialFirstResponder@
initialFirstResponderSelector :: Selector
initialFirstResponderSelector = mkSelector "initialFirstResponder"

-- | @Selector@ for @setInitialFirstResponder:@
setInitialFirstResponderSelector :: Selector
setInitialFirstResponderSelector = mkSelector "setInitialFirstResponder:"

-- | @Selector@ for @keyViewSelectionDirection@
keyViewSelectionDirectionSelector :: Selector
keyViewSelectionDirectionSelector = mkSelector "keyViewSelectionDirection"

-- | @Selector@ for @defaultButtonCell@
defaultButtonCellSelector :: Selector
defaultButtonCellSelector = mkSelector "defaultButtonCell"

-- | @Selector@ for @setDefaultButtonCell:@
setDefaultButtonCellSelector :: Selector
setDefaultButtonCellSelector = mkSelector "setDefaultButtonCell:"

-- | @Selector@ for @autorecalculatesKeyViewLoop@
autorecalculatesKeyViewLoopSelector :: Selector
autorecalculatesKeyViewLoopSelector = mkSelector "autorecalculatesKeyViewLoop"

-- | @Selector@ for @setAutorecalculatesKeyViewLoop:@
setAutorecalculatesKeyViewLoopSelector :: Selector
setAutorecalculatesKeyViewLoopSelector = mkSelector "setAutorecalculatesKeyViewLoop:"

-- | @Selector@ for @toolbar@
toolbarSelector :: Selector
toolbarSelector = mkSelector "toolbar"

-- | @Selector@ for @setToolbar:@
setToolbarSelector :: Selector
setToolbarSelector = mkSelector "setToolbar:"

-- | @Selector@ for @showsToolbarButton@
showsToolbarButtonSelector :: Selector
showsToolbarButtonSelector = mkSelector "showsToolbarButton"

-- | @Selector@ for @setShowsToolbarButton:@
setShowsToolbarButtonSelector :: Selector
setShowsToolbarButtonSelector = mkSelector "setShowsToolbarButton:"

-- | @Selector@ for @allowsAutomaticWindowTabbing@
allowsAutomaticWindowTabbingSelector :: Selector
allowsAutomaticWindowTabbingSelector = mkSelector "allowsAutomaticWindowTabbing"

-- | @Selector@ for @setAllowsAutomaticWindowTabbing:@
setAllowsAutomaticWindowTabbingSelector :: Selector
setAllowsAutomaticWindowTabbingSelector = mkSelector "setAllowsAutomaticWindowTabbing:"

-- | @Selector@ for @userTabbingPreference@
userTabbingPreferenceSelector :: Selector
userTabbingPreferenceSelector = mkSelector "userTabbingPreference"

-- | @Selector@ for @tabbingMode@
tabbingModeSelector :: Selector
tabbingModeSelector = mkSelector "tabbingMode"

-- | @Selector@ for @setTabbingMode:@
setTabbingModeSelector :: Selector
setTabbingModeSelector = mkSelector "setTabbingMode:"

-- | @Selector@ for @tabbingIdentifier@
tabbingIdentifierSelector :: Selector
tabbingIdentifierSelector = mkSelector "tabbingIdentifier"

-- | @Selector@ for @setTabbingIdentifier:@
setTabbingIdentifierSelector :: Selector
setTabbingIdentifierSelector = mkSelector "setTabbingIdentifier:"

-- | @Selector@ for @tabbedWindows@
tabbedWindowsSelector :: Selector
tabbedWindowsSelector = mkSelector "tabbedWindows"

-- | @Selector@ for @tab@
tabSelector :: Selector
tabSelector = mkSelector "tab"

-- | @Selector@ for @tabGroup@
tabGroupSelector :: Selector
tabGroupSelector = mkSelector "tabGroup"

-- | @Selector@ for @hasActiveWindowSharingSession@
hasActiveWindowSharingSessionSelector :: Selector
hasActiveWindowSharingSessionSelector = mkSelector "hasActiveWindowSharingSession"

-- | @Selector@ for @windowTitlebarLayoutDirection@
windowTitlebarLayoutDirectionSelector :: Selector
windowTitlebarLayoutDirectionSelector = mkSelector "windowTitlebarLayoutDirection"

-- | @Selector@ for @restorable@
restorableSelector :: Selector
restorableSelector = mkSelector "restorable"

-- | @Selector@ for @setRestorable:@
setRestorableSelector :: Selector
setRestorableSelector = mkSelector "setRestorable:"

-- | @Selector@ for @restorationClass@
restorationClassSelector :: Selector
restorationClassSelector = mkSelector "restorationClass"

-- | @Selector@ for @setRestorationClass:@
setRestorationClassSelector :: Selector
setRestorationClassSelector = mkSelector "setRestorationClass:"

-- | @Selector@ for @hasCloseBox@
hasCloseBoxSelector :: Selector
hasCloseBoxSelector = mkSelector "hasCloseBox"

-- | @Selector@ for @hasTitleBar@
hasTitleBarSelector :: Selector
hasTitleBarSelector = mkSelector "hasTitleBar"

-- | @Selector@ for @floatingPanel@
floatingPanelSelector :: Selector
floatingPanelSelector = mkSelector "floatingPanel"

-- | @Selector@ for @miniaturizable@
miniaturizableSelector :: Selector
miniaturizableSelector = mkSelector "miniaturizable"

-- | @Selector@ for @modalPanel@
modalPanelSelector :: Selector
modalPanelSelector = mkSelector "modalPanel"

-- | @Selector@ for @resizable@
resizableSelector :: Selector
resizableSelector = mkSelector "resizable"

-- | @Selector@ for @zoomable@
zoomableSelector :: Selector
zoomableSelector = mkSelector "zoomable"

-- | @Selector@ for @orderedIndex@
orderedIndexSelector :: Selector
orderedIndexSelector = mkSelector "orderedIndex"

-- | @Selector@ for @setOrderedIndex:@
setOrderedIndexSelector :: Selector
setOrderedIndexSelector = mkSelector "setOrderedIndex:"

-- | @Selector@ for @drawers@
drawersSelector :: Selector
drawersSelector = mkSelector "drawers"

-- | @Selector@ for @flushWindowDisabled@
flushWindowDisabledSelector :: Selector
flushWindowDisabledSelector = mkSelector "flushWindowDisabled"

-- | @Selector@ for @autodisplay@
autodisplaySelector :: Selector
autodisplaySelector = mkSelector "autodisplay"

-- | @Selector@ for @setAutodisplay:@
setAutodisplaySelector :: Selector
setAutodisplaySelector = mkSelector "setAutodisplay:"

-- | @Selector@ for @graphicsContext@
graphicsContextSelector :: Selector
graphicsContextSelector = mkSelector "graphicsContext"

-- | @Selector@ for @oneShot@
oneShotSelector :: Selector
oneShotSelector = mkSelector "oneShot"

-- | @Selector@ for @setOneShot:@
setOneShotSelector :: Selector
setOneShotSelector = mkSelector "setOneShot:"

-- | @Selector@ for @preferredBackingLocation@
preferredBackingLocationSelector :: Selector
preferredBackingLocationSelector = mkSelector "preferredBackingLocation"

-- | @Selector@ for @setPreferredBackingLocation:@
setPreferredBackingLocationSelector :: Selector
setPreferredBackingLocationSelector = mkSelector "setPreferredBackingLocation:"

-- | @Selector@ for @backingLocation@
backingLocationSelector :: Selector
backingLocationSelector = mkSelector "backingLocation"

-- | @Selector@ for @showsResizeIndicator@
showsResizeIndicatorSelector :: Selector
showsResizeIndicatorSelector = mkSelector "showsResizeIndicator"

-- | @Selector@ for @setShowsResizeIndicator:@
setShowsResizeIndicatorSelector :: Selector
setShowsResizeIndicatorSelector = mkSelector "setShowsResizeIndicator:"

-- | @Selector@ for @windowRef@
windowRefSelector :: Selector
windowRefSelector = mkSelector "windowRef"

-- | @Selector@ for @areCursorRectsEnabled@
areCursorRectsEnabledSelector :: Selector
areCursorRectsEnabledSelector = mkSelector "areCursorRectsEnabled"

-- | @Selector@ for @currentEvent@
currentEventSelector :: Selector
currentEventSelector = mkSelector "currentEvent"

-- | @Selector@ for @acceptsMouseMovedEvents@
acceptsMouseMovedEventsSelector :: Selector
acceptsMouseMovedEventsSelector = mkSelector "acceptsMouseMovedEvents"

-- | @Selector@ for @setAcceptsMouseMovedEvents:@
setAcceptsMouseMovedEventsSelector :: Selector
setAcceptsMouseMovedEventsSelector = mkSelector "setAcceptsMouseMovedEvents:"

-- | @Selector@ for @ignoresMouseEvents@
ignoresMouseEventsSelector :: Selector
ignoresMouseEventsSelector = mkSelector "ignoresMouseEvents"

-- | @Selector@ for @setIgnoresMouseEvents:@
setIgnoresMouseEventsSelector :: Selector
setIgnoresMouseEventsSelector = mkSelector "setIgnoresMouseEvents:"

-- | @Selector@ for @mouseLocationOutsideOfEventStream@
mouseLocationOutsideOfEventStreamSelector :: Selector
mouseLocationOutsideOfEventStreamSelector = mkSelector "mouseLocationOutsideOfEventStream"

