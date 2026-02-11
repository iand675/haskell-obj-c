{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCell@.
module ObjC.AppKit.NSCell
  ( NSCell
  , IsNSCell(..)
  , init_
  , initTextCell
  , initImageCell
  , initWithCoder
  , sendActionOn
  , compare_
  , takeIntValueFrom
  , takeFloatValueFrom
  , takeDoubleValueFrom
  , takeStringValueFrom
  , takeObjectValueFrom
  , takeIntegerValueFrom
  , cellAttribute
  , setCellAttribute_to
  , imageRectForBounds
  , titleRectForBounds
  , drawingRectForBounds
  , _bulletStringForString_bulletCharacter
  , cellSizeForBounds
  , highlightColorWithFrame_inView
  , calcDrawInfo
  , setUpFieldEditorAttributes
  , drawInteriorWithFrame_inView
  , drawWithFrame_inView
  , highlight_withFrame_inView
  , getPeriodicDelay_interval
  , startTrackingAt_inView
  , continueTracking_at_inView
  , stopTracking_at_inView_mouseIsUp
  , trackMouse_inRect_ofView_untilMouseUp
  , editWithFrame_inView_editor_delegate_event
  , selectWithFrame_inView_editor_delegate_start_length
  , endEditing
  , resetCursorRect_inView
  , menuForEvent_inRect_ofView
  , fieldEditorForView
  , draggingImageComponentsWithFrame_inView
  , entryType
  , setEntryType
  , isEntryAcceptable
  , setFloatingPointFormat_left_right
  , setMnemonicLocation
  , mnemonicLocation
  , mnemonic
  , setTitleWithMnemonic
  , expansionFrameWithFrame_inView
  , drawWithExpansionFrame_inView
  , hitTestForEvent_inRect_ofView
  , setNextState
  , performClick
  , drawFocusRingMaskWithFrame_inView
  , focusRingMaskBoundsForFrame_inView
  , prefersTrackingUntilMouseUp
  , controlView
  , setControlView
  , type_
  , setType
  , state
  , setState
  , target
  , setTarget
  , action
  , setAction
  , tag
  , setTag
  , title
  , setTitle
  , opaque
  , enabled
  , setEnabled
  , continuous
  , setContinuous
  , editable
  , setEditable
  , selectable
  , setSelectable
  , bordered
  , setBordered
  , bezeled
  , setBezeled
  , scrollable
  , setScrollable
  , highlighted
  , setHighlighted
  , alignment
  , setAlignment
  , wraps
  , setWraps
  , font
  , setFont
  , keyEquivalent
  , formatter
  , setFormatter
  , objectValue
  , setObjectValue
  , hasValidObjectValue
  , stringValue
  , setStringValue
  , intValue
  , setIntValue
  , floatValue
  , setFloatValue
  , doubleValue
  , setDoubleValue
  , integerValue
  , setIntegerValue
  , image
  , setImage
  , controlSize
  , setControlSize
  , representedObject
  , setRepresentedObject
  , cellSize
  , mouseDownFlags
  , menu
  , setMenu
  , defaultMenu
  , sendsActionOnEndEditing
  , setSendsActionOnEndEditing
  , baseWritingDirection
  , setBaseWritingDirection
  , lineBreakMode
  , setLineBreakMode
  , allowsUndo
  , setAllowsUndo
  , truncatesLastVisibleLine
  , setTruncatesLastVisibleLine
  , userInterfaceLayoutDirection
  , setUserInterfaceLayoutDirection
  , usesSingleLineMode
  , setUsesSingleLineMode
  , controlTint
  , setControlTint
  , backgroundStyle
  , setBackgroundStyle
  , interiorBackgroundStyle
  , allowsMixedState
  , setAllowsMixedState
  , nextState
  , attributedStringValue
  , setAttributedStringValue
  , allowsEditingTextAttributes
  , setAllowsEditingTextAttributes
  , importsGraphics
  , setImportsGraphics
  , refusesFirstResponder
  , setRefusesFirstResponder
  , acceptsFirstResponder
  , showsFirstResponder
  , setShowsFirstResponder
  , focusRingType
  , setFocusRingType
  , defaultFocusRingType
  , wantsNotificationForMarkedText
  , initSelector
  , initTextCellSelector
  , initImageCellSelector
  , initWithCoderSelector
  , sendActionOnSelector
  , compareSelector
  , takeIntValueFromSelector
  , takeFloatValueFromSelector
  , takeDoubleValueFromSelector
  , takeStringValueFromSelector
  , takeObjectValueFromSelector
  , takeIntegerValueFromSelector
  , cellAttributeSelector
  , setCellAttribute_toSelector
  , imageRectForBoundsSelector
  , titleRectForBoundsSelector
  , drawingRectForBoundsSelector
  , _bulletStringForString_bulletCharacterSelector
  , cellSizeForBoundsSelector
  , highlightColorWithFrame_inViewSelector
  , calcDrawInfoSelector
  , setUpFieldEditorAttributesSelector
  , drawInteriorWithFrame_inViewSelector
  , drawWithFrame_inViewSelector
  , highlight_withFrame_inViewSelector
  , getPeriodicDelay_intervalSelector
  , startTrackingAt_inViewSelector
  , continueTracking_at_inViewSelector
  , stopTracking_at_inView_mouseIsUpSelector
  , trackMouse_inRect_ofView_untilMouseUpSelector
  , editWithFrame_inView_editor_delegate_eventSelector
  , selectWithFrame_inView_editor_delegate_start_lengthSelector
  , endEditingSelector
  , resetCursorRect_inViewSelector
  , menuForEvent_inRect_ofViewSelector
  , fieldEditorForViewSelector
  , draggingImageComponentsWithFrame_inViewSelector
  , entryTypeSelector
  , setEntryTypeSelector
  , isEntryAcceptableSelector
  , setFloatingPointFormat_left_rightSelector
  , setMnemonicLocationSelector
  , mnemonicLocationSelector
  , mnemonicSelector
  , setTitleWithMnemonicSelector
  , expansionFrameWithFrame_inViewSelector
  , drawWithExpansionFrame_inViewSelector
  , hitTestForEvent_inRect_ofViewSelector
  , setNextStateSelector
  , performClickSelector
  , drawFocusRingMaskWithFrame_inViewSelector
  , focusRingMaskBoundsForFrame_inViewSelector
  , prefersTrackingUntilMouseUpSelector
  , controlViewSelector
  , setControlViewSelector
  , typeSelector
  , setTypeSelector
  , stateSelector
  , setStateSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , tagSelector
  , setTagSelector
  , titleSelector
  , setTitleSelector
  , opaqueSelector
  , enabledSelector
  , setEnabledSelector
  , continuousSelector
  , setContinuousSelector
  , editableSelector
  , setEditableSelector
  , selectableSelector
  , setSelectableSelector
  , borderedSelector
  , setBorderedSelector
  , bezeledSelector
  , setBezeledSelector
  , scrollableSelector
  , setScrollableSelector
  , highlightedSelector
  , setHighlightedSelector
  , alignmentSelector
  , setAlignmentSelector
  , wrapsSelector
  , setWrapsSelector
  , fontSelector
  , setFontSelector
  , keyEquivalentSelector
  , formatterSelector
  , setFormatterSelector
  , objectValueSelector
  , setObjectValueSelector
  , hasValidObjectValueSelector
  , stringValueSelector
  , setStringValueSelector
  , intValueSelector
  , setIntValueSelector
  , floatValueSelector
  , setFloatValueSelector
  , doubleValueSelector
  , setDoubleValueSelector
  , integerValueSelector
  , setIntegerValueSelector
  , imageSelector
  , setImageSelector
  , controlSizeSelector
  , setControlSizeSelector
  , representedObjectSelector
  , setRepresentedObjectSelector
  , cellSizeSelector
  , mouseDownFlagsSelector
  , menuSelector
  , setMenuSelector
  , defaultMenuSelector
  , sendsActionOnEndEditingSelector
  , setSendsActionOnEndEditingSelector
  , baseWritingDirectionSelector
  , setBaseWritingDirectionSelector
  , lineBreakModeSelector
  , setLineBreakModeSelector
  , allowsUndoSelector
  , setAllowsUndoSelector
  , truncatesLastVisibleLineSelector
  , setTruncatesLastVisibleLineSelector
  , userInterfaceLayoutDirectionSelector
  , setUserInterfaceLayoutDirectionSelector
  , usesSingleLineModeSelector
  , setUsesSingleLineModeSelector
  , controlTintSelector
  , setControlTintSelector
  , backgroundStyleSelector
  , setBackgroundStyleSelector
  , interiorBackgroundStyleSelector
  , allowsMixedStateSelector
  , setAllowsMixedStateSelector
  , nextStateSelector
  , attributedStringValueSelector
  , setAttributedStringValueSelector
  , allowsEditingTextAttributesSelector
  , setAllowsEditingTextAttributesSelector
  , importsGraphicsSelector
  , setImportsGraphicsSelector
  , refusesFirstResponderSelector
  , setRefusesFirstResponderSelector
  , acceptsFirstResponderSelector
  , showsFirstResponderSelector
  , setShowsFirstResponderSelector
  , focusRingTypeSelector
  , setFocusRingTypeSelector
  , defaultFocusRingTypeSelector
  , wantsNotificationForMarkedTextSelector

  -- * Enum types
  , NSBackgroundStyle(NSBackgroundStyle)
  , pattern NSBackgroundStyleNormal
  , pattern NSBackgroundStyleEmphasized
  , pattern NSBackgroundStyleRaised
  , pattern NSBackgroundStyleLowered
  , NSCellAttribute(NSCellAttribute)
  , pattern NSCellDisabled
  , pattern NSCellState
  , pattern NSPushInCell
  , pattern NSCellEditable
  , pattern NSChangeGrayCell
  , pattern NSCellHighlighted
  , pattern NSCellLightsByContents
  , pattern NSCellLightsByGray
  , pattern NSChangeBackgroundCell
  , pattern NSCellLightsByBackground
  , pattern NSCellIsBordered
  , pattern NSCellHasOverlappingImage
  , pattern NSCellHasImageHorizontal
  , pattern NSCellHasImageOnLeftOrBottom
  , pattern NSCellChangesContents
  , pattern NSCellIsInsetButton
  , pattern NSCellAllowsMixedState
  , NSCellHitResult(NSCellHitResult)
  , pattern NSCellHitNone
  , pattern NSCellHitContentArea
  , pattern NSCellHitEditableTextArea
  , pattern NSCellHitTrackableArea
  , NSCellType(NSCellType)
  , pattern NSNullCellType
  , pattern NSTextCellType
  , pattern NSImageCellType
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge
  , NSControlTint(NSControlTint)
  , pattern NSDefaultControlTint
  , pattern NSBlueControlTint
  , pattern NSGraphiteControlTint
  , pattern NSClearControlTint
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
  , NSFocusRingType(NSFocusRingType)
  , pattern NSFocusRingTypeDefault
  , pattern NSFocusRingTypeNone
  , pattern NSFocusRingTypeExterior
  , NSLineBreakMode(NSLineBreakMode)
  , pattern NSLineBreakByWordWrapping
  , pattern NSLineBreakByCharWrapping
  , pattern NSLineBreakByClipping
  , pattern NSLineBreakByTruncatingHead
  , pattern NSLineBreakByTruncatingTail
  , pattern NSLineBreakByTruncatingMiddle
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft
  , NSWritingDirection(NSWritingDirection)
  , pattern NSWritingDirectionNatural
  , pattern NSWritingDirectionLeftToRight
  , pattern NSWritingDirectionRightToLeft

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

-- | @- init@
init_ :: IsNSCell nsCell => nsCell -> IO (Id NSCell)
init_ nsCell  =
  sendMsg nsCell (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initTextCell:@
initTextCell :: (IsNSCell nsCell, IsNSString string) => nsCell -> string -> IO (Id NSCell)
initTextCell nsCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSCell nsCell, IsNSImage image) => nsCell -> image -> IO (Id NSCell)
initImageCell nsCell  image =
withObjCPtr image $ \raw_image ->
    sendMsg nsCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSCell nsCell, IsNSCoder coder) => nsCell -> coder -> IO (Id NSCell)
initWithCoder nsCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- sendActionOn:@
sendActionOn :: IsNSCell nsCell => nsCell -> NSEventMask -> IO CLong
sendActionOn nsCell  mask =
  sendMsg nsCell (mkSelector "sendActionOn:") retCLong [argCULong (coerce mask)]

-- | @- compare:@
compare_ :: IsNSCell nsCell => nsCell -> RawId -> IO NSComparisonResult
compare_ nsCell  otherCell =
  fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsCell (mkSelector "compare:") retCLong [argPtr (castPtr (unRawId otherCell) :: Ptr ())]

-- | @- takeIntValueFrom:@
takeIntValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeIntValueFrom nsCell  sender =
  sendMsg nsCell (mkSelector "takeIntValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeFloatValueFrom:@
takeFloatValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeFloatValueFrom nsCell  sender =
  sendMsg nsCell (mkSelector "takeFloatValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeDoubleValueFrom:@
takeDoubleValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeDoubleValueFrom nsCell  sender =
  sendMsg nsCell (mkSelector "takeDoubleValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeStringValueFrom:@
takeStringValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeStringValueFrom nsCell  sender =
  sendMsg nsCell (mkSelector "takeStringValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeObjectValueFrom:@
takeObjectValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeObjectValueFrom nsCell  sender =
  sendMsg nsCell (mkSelector "takeObjectValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeIntegerValueFrom:@
takeIntegerValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeIntegerValueFrom nsCell  sender =
  sendMsg nsCell (mkSelector "takeIntegerValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- cellAttribute:@
cellAttribute :: IsNSCell nsCell => nsCell -> NSCellAttribute -> IO CLong
cellAttribute nsCell  parameter =
  sendMsg nsCell (mkSelector "cellAttribute:") retCLong [argCULong (coerce parameter)]

-- | @- setCellAttribute:to:@
setCellAttribute_to :: IsNSCell nsCell => nsCell -> NSCellAttribute -> CLong -> IO ()
setCellAttribute_to nsCell  parameter value =
  sendMsg nsCell (mkSelector "setCellAttribute:to:") retVoid [argCULong (coerce parameter), argCLong (fromIntegral value)]

-- | @- imageRectForBounds:@
imageRectForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSRect
imageRectForBounds nsCell  rect =
  sendMsgStret nsCell (mkSelector "imageRectForBounds:") retNSRect [argNSRect rect]

-- | @- titleRectForBounds:@
titleRectForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSRect
titleRectForBounds nsCell  rect =
  sendMsgStret nsCell (mkSelector "titleRectForBounds:") retNSRect [argNSRect rect]

-- | @- drawingRectForBounds:@
drawingRectForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSRect
drawingRectForBounds nsCell  rect =
  sendMsgStret nsCell (mkSelector "drawingRectForBounds:") retNSRect [argNSRect rect]

-- | @+ _bulletStringForString:bulletCharacter:@
_bulletStringForString_bulletCharacter :: IsNSString string => string -> CUShort -> IO (Id NSString)
_bulletStringForString_bulletCharacter string bulletChar =
  do
    cls' <- getRequiredClass "NSCell"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "_bulletStringForString:bulletCharacter:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCUInt (fromIntegral bulletChar)] >>= retainedObject . castPtr

-- | @- cellSizeForBounds:@
cellSizeForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSSize
cellSizeForBounds nsCell  rect =
  sendMsgStret nsCell (mkSelector "cellSizeForBounds:") retNSSize [argNSRect rect]

-- | @- highlightColorWithFrame:inView:@
highlightColorWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO (Id NSColor)
highlightColorWithFrame_inView nsCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "highlightColorWithFrame:inView:") (retPtr retVoid) [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())] >>= retainedObject . castPtr

-- | @- calcDrawInfo:@
calcDrawInfo :: IsNSCell nsCell => nsCell -> NSRect -> IO ()
calcDrawInfo nsCell  rect =
  sendMsg nsCell (mkSelector "calcDrawInfo:") retVoid [argNSRect rect]

-- | @- setUpFieldEditorAttributes:@
setUpFieldEditorAttributes :: (IsNSCell nsCell, IsNSText textObj) => nsCell -> textObj -> IO (Id NSText)
setUpFieldEditorAttributes nsCell  textObj =
withObjCPtr textObj $ \raw_textObj ->
    sendMsg nsCell (mkSelector "setUpFieldEditorAttributes:") (retPtr retVoid) [argPtr (castPtr raw_textObj :: Ptr ())] >>= retainedObject . castPtr

-- | @- drawInteriorWithFrame:inView:@
drawInteriorWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
drawInteriorWithFrame_inView nsCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "drawInteriorWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawWithFrame:inView:@
drawWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
drawWithFrame_inView nsCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "drawWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- highlight:withFrame:inView:@
highlight_withFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> Bool -> NSRect -> controlView -> IO ()
highlight_withFrame_inView nsCell  flag cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "highlight:withFrame:inView:") retVoid [argCULong (if flag then 1 else 0), argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- getPeriodicDelay:interval:@
getPeriodicDelay_interval :: IsNSCell nsCell => nsCell -> Ptr CFloat -> Ptr CFloat -> IO ()
getPeriodicDelay_interval nsCell  delay interval =
  sendMsg nsCell (mkSelector "getPeriodicDelay:interval:") retVoid [argPtr delay, argPtr interval]

-- | @- startTrackingAt:inView:@
startTrackingAt_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSPoint -> controlView -> IO Bool
startTrackingAt_inView nsCell  startPoint controlView =
withObjCPtr controlView $ \raw_controlView ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "startTrackingAt:inView:") retCULong [argNSPoint startPoint, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- continueTracking:at:inView:@
continueTracking_at_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSPoint -> NSPoint -> controlView -> IO Bool
continueTracking_at_inView nsCell  lastPoint currentPoint controlView =
withObjCPtr controlView $ \raw_controlView ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "continueTracking:at:inView:") retCULong [argNSPoint lastPoint, argNSPoint currentPoint, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- stopTracking:at:inView:mouseIsUp:@
stopTracking_at_inView_mouseIsUp :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSPoint -> NSPoint -> controlView -> Bool -> IO ()
stopTracking_at_inView_mouseIsUp nsCell  lastPoint stopPoint controlView flag =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "stopTracking:at:inView:mouseIsUp:") retVoid [argNSPoint lastPoint, argNSPoint stopPoint, argPtr (castPtr raw_controlView :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- trackMouse:inRect:ofView:untilMouseUp:@
trackMouse_inRect_ofView_untilMouseUp :: (IsNSCell nsCell, IsNSEvent event, IsNSView controlView) => nsCell -> event -> NSRect -> controlView -> Bool -> IO Bool
trackMouse_inRect_ofView_untilMouseUp nsCell  event cellFrame controlView flag =
withObjCPtr event $ \raw_event ->
  withObjCPtr controlView $ \raw_controlView ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "trackMouse:inRect:ofView:untilMouseUp:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- editWithFrame:inView:editor:delegate:event:@
editWithFrame_inView_editor_delegate_event :: (IsNSCell nsCell, IsNSView controlView, IsNSText textObj, IsNSEvent event) => nsCell -> NSRect -> controlView -> textObj -> RawId -> event -> IO ()
editWithFrame_inView_editor_delegate_event nsCell  rect controlView textObj delegate event =
withObjCPtr controlView $ \raw_controlView ->
  withObjCPtr textObj $ \raw_textObj ->
    withObjCPtr event $ \raw_event ->
        sendMsg nsCell (mkSelector "editWithFrame:inView:editor:delegate:event:") retVoid [argNSRect rect, argPtr (castPtr raw_controlView :: Ptr ()), argPtr (castPtr raw_textObj :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | @- selectWithFrame:inView:editor:delegate:start:length:@
selectWithFrame_inView_editor_delegate_start_length :: (IsNSCell nsCell, IsNSView controlView, IsNSText textObj) => nsCell -> NSRect -> controlView -> textObj -> RawId -> CLong -> CLong -> IO ()
selectWithFrame_inView_editor_delegate_start_length nsCell  rect controlView textObj delegate selStart selLength =
withObjCPtr controlView $ \raw_controlView ->
  withObjCPtr textObj $ \raw_textObj ->
      sendMsg nsCell (mkSelector "selectWithFrame:inView:editor:delegate:start:length:") retVoid [argNSRect rect, argPtr (castPtr raw_controlView :: Ptr ()), argPtr (castPtr raw_textObj :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argCLong (fromIntegral selStart), argCLong (fromIntegral selLength)]

-- | @- endEditing:@
endEditing :: (IsNSCell nsCell, IsNSText textObj) => nsCell -> textObj -> IO ()
endEditing nsCell  textObj =
withObjCPtr textObj $ \raw_textObj ->
    sendMsg nsCell (mkSelector "endEditing:") retVoid [argPtr (castPtr raw_textObj :: Ptr ())]

-- | @- resetCursorRect:inView:@
resetCursorRect_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
resetCursorRect_inView nsCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "resetCursorRect:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- menuForEvent:inRect:ofView:@
menuForEvent_inRect_ofView :: (IsNSCell nsCell, IsNSEvent event, IsNSView view) => nsCell -> event -> NSRect -> view -> IO (Id NSMenu)
menuForEvent_inRect_ofView nsCell  event cellFrame view =
withObjCPtr event $ \raw_event ->
  withObjCPtr view $ \raw_view ->
      sendMsg nsCell (mkSelector "menuForEvent:inRect:ofView:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ()), argNSRect cellFrame, argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- fieldEditorForView:@
fieldEditorForView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> controlView -> IO (Id NSTextView)
fieldEditorForView nsCell  controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "fieldEditorForView:") (retPtr retVoid) [argPtr (castPtr raw_controlView :: Ptr ())] >>= retainedObject . castPtr

-- | @- draggingImageComponentsWithFrame:inView:@
draggingImageComponentsWithFrame_inView :: (IsNSCell nsCell, IsNSView view) => nsCell -> NSRect -> view -> IO (Id NSArray)
draggingImageComponentsWithFrame_inView nsCell  frame view =
withObjCPtr view $ \raw_view ->
    sendMsg nsCell (mkSelector "draggingImageComponentsWithFrame:inView:") (retPtr retVoid) [argNSRect frame, argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- entryType@
entryType :: IsNSCell nsCell => nsCell -> IO CLong
entryType nsCell  =
  sendMsg nsCell (mkSelector "entryType") retCLong []

-- | @- setEntryType:@
setEntryType :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setEntryType nsCell  type_ =
  sendMsg nsCell (mkSelector "setEntryType:") retVoid [argCLong (fromIntegral type_)]

-- | @- isEntryAcceptable:@
isEntryAcceptable :: (IsNSCell nsCell, IsNSString string) => nsCell -> string -> IO Bool
isEntryAcceptable nsCell  string =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "isEntryAcceptable:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_right :: IsNSCell nsCell => nsCell -> Bool -> CULong -> CULong -> IO ()
setFloatingPointFormat_left_right nsCell  autoRange leftDigits rightDigits =
  sendMsg nsCell (mkSelector "setFloatingPointFormat:left:right:") retVoid [argCULong (if autoRange then 1 else 0), argCULong (fromIntegral leftDigits), argCULong (fromIntegral rightDigits)]

-- | @- setMnemonicLocation:@
setMnemonicLocation :: IsNSCell nsCell => nsCell -> CULong -> IO ()
setMnemonicLocation nsCell  location =
  sendMsg nsCell (mkSelector "setMnemonicLocation:") retVoid [argCULong (fromIntegral location)]

-- | @- mnemonicLocation@
mnemonicLocation :: IsNSCell nsCell => nsCell -> IO CULong
mnemonicLocation nsCell  =
  sendMsg nsCell (mkSelector "mnemonicLocation") retCULong []

-- | @- mnemonic@
mnemonic :: IsNSCell nsCell => nsCell -> IO (Id NSString)
mnemonic nsCell  =
  sendMsg nsCell (mkSelector "mnemonic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSCell nsCell, IsNSString stringWithAmpersand) => nsCell -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsCell  stringWithAmpersand =
withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
    sendMsg nsCell (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | @- expansionFrameWithFrame:inView:@
expansionFrameWithFrame_inView :: (IsNSCell nsCell, IsNSView view) => nsCell -> NSRect -> view -> IO NSRect
expansionFrameWithFrame_inView nsCell  cellFrame view =
withObjCPtr view $ \raw_view ->
    sendMsgStret nsCell (mkSelector "expansionFrameWithFrame:inView:") retNSRect [argNSRect cellFrame, argPtr (castPtr raw_view :: Ptr ())]

-- | @- drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inView :: (IsNSCell nsCell, IsNSView view) => nsCell -> NSRect -> view -> IO ()
drawWithExpansionFrame_inView nsCell  cellFrame view =
withObjCPtr view $ \raw_view ->
    sendMsg nsCell (mkSelector "drawWithExpansionFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_view :: Ptr ())]

-- | @- hitTestForEvent:inRect:ofView:@
hitTestForEvent_inRect_ofView :: (IsNSCell nsCell, IsNSEvent event, IsNSView controlView) => nsCell -> event -> NSRect -> controlView -> IO NSCellHitResult
hitTestForEvent_inRect_ofView nsCell  event cellFrame controlView =
withObjCPtr event $ \raw_event ->
  withObjCPtr controlView $ \raw_controlView ->
      fmap (coerce :: CULong -> NSCellHitResult) $ sendMsg nsCell (mkSelector "hitTestForEvent:inRect:ofView:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- setNextState@
setNextState :: IsNSCell nsCell => nsCell -> IO ()
setNextState nsCell  =
  sendMsg nsCell (mkSelector "setNextState") retVoid []

-- | @- performClick:@
performClick :: IsNSCell nsCell => nsCell -> RawId -> IO ()
performClick nsCell  sender =
  sendMsg nsCell (mkSelector "performClick:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- drawFocusRingMaskWithFrame:inView:@
drawFocusRingMaskWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
drawFocusRingMaskWithFrame_inView nsCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsCell (mkSelector "drawFocusRingMaskWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- focusRingMaskBoundsForFrame:inView:@
focusRingMaskBoundsForFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO NSRect
focusRingMaskBoundsForFrame_inView nsCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsgStret nsCell (mkSelector "focusRingMaskBoundsForFrame:inView:") retNSRect [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @+ prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUp :: IO Bool
prefersTrackingUntilMouseUp  =
  do
    cls' <- getRequiredClass "NSCell"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "prefersTrackingUntilMouseUp") retCULong []

-- | @- controlView@
controlView :: IsNSCell nsCell => nsCell -> IO (Id NSView)
controlView nsCell  =
  sendMsg nsCell (mkSelector "controlView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setControlView:@
setControlView :: (IsNSCell nsCell, IsNSView value) => nsCell -> value -> IO ()
setControlView nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setControlView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsNSCell nsCell => nsCell -> IO NSCellType
type_ nsCell  =
  fmap (coerce :: CULong -> NSCellType) $ sendMsg nsCell (mkSelector "type") retCULong []

-- | @- setType:@
setType :: IsNSCell nsCell => nsCell -> NSCellType -> IO ()
setType nsCell  value =
  sendMsg nsCell (mkSelector "setType:") retVoid [argCULong (coerce value)]

-- | @- state@
state :: IsNSCell nsCell => nsCell -> IO CLong
state nsCell  =
  sendMsg nsCell (mkSelector "state") retCLong []

-- | @- setState:@
setState :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setState nsCell  value =
  sendMsg nsCell (mkSelector "setState:") retVoid [argCLong (fromIntegral value)]

-- | @- target@
target :: IsNSCell nsCell => nsCell -> IO RawId
target nsCell  =
  fmap (RawId . castPtr) $ sendMsg nsCell (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSCell nsCell => nsCell -> RawId -> IO ()
setTarget nsCell  value =
  sendMsg nsCell (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSCell nsCell => nsCell -> IO Selector
action nsCell  =
  fmap (Selector . castPtr) $ sendMsg nsCell (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSCell nsCell => nsCell -> Selector -> IO ()
setAction nsCell  value =
  sendMsg nsCell (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- tag@
tag :: IsNSCell nsCell => nsCell -> IO CLong
tag nsCell  =
  sendMsg nsCell (mkSelector "tag") retCLong []

-- | @- setTag:@
setTag :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setTag nsCell  value =
  sendMsg nsCell (mkSelector "setTag:") retVoid [argCLong (fromIntegral value)]

-- | @- title@
title :: IsNSCell nsCell => nsCell -> IO (Id NSString)
title nsCell  =
  sendMsg nsCell (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSCell nsCell, IsNSString value) => nsCell -> value -> IO ()
setTitle nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- opaque@
opaque :: IsNSCell nsCell => nsCell -> IO Bool
opaque nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "opaque") retCULong []

-- | @- enabled@
enabled :: IsNSCell nsCell => nsCell -> IO Bool
enabled nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setEnabled nsCell  value =
  sendMsg nsCell (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- continuous@
continuous :: IsNSCell nsCell => nsCell -> IO Bool
continuous nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "continuous") retCULong []

-- | @- setContinuous:@
setContinuous :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setContinuous nsCell  value =
  sendMsg nsCell (mkSelector "setContinuous:") retVoid [argCULong (if value then 1 else 0)]

-- | @- editable@
editable :: IsNSCell nsCell => nsCell -> IO Bool
editable nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setEditable nsCell  value =
  sendMsg nsCell (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectable@
selectable :: IsNSCell nsCell => nsCell -> IO Bool
selectable nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "selectable") retCULong []

-- | @- setSelectable:@
setSelectable :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setSelectable nsCell  value =
  sendMsg nsCell (mkSelector "setSelectable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- bordered@
bordered :: IsNSCell nsCell => nsCell -> IO Bool
bordered nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "bordered") retCULong []

-- | @- setBordered:@
setBordered :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setBordered nsCell  value =
  sendMsg nsCell (mkSelector "setBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- bezeled@
bezeled :: IsNSCell nsCell => nsCell -> IO Bool
bezeled nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "bezeled") retCULong []

-- | @- setBezeled:@
setBezeled :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setBezeled nsCell  value =
  sendMsg nsCell (mkSelector "setBezeled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scrollable@
scrollable :: IsNSCell nsCell => nsCell -> IO Bool
scrollable nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "scrollable") retCULong []

-- | @- setScrollable:@
setScrollable :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setScrollable nsCell  value =
  sendMsg nsCell (mkSelector "setScrollable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- highlighted@
highlighted :: IsNSCell nsCell => nsCell -> IO Bool
highlighted nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "highlighted") retCULong []

-- | @- setHighlighted:@
setHighlighted :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setHighlighted nsCell  value =
  sendMsg nsCell (mkSelector "setHighlighted:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alignment@
alignment :: IsNSCell nsCell => nsCell -> IO NSTextAlignment
alignment nsCell  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsCell (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsNSCell nsCell => nsCell -> NSTextAlignment -> IO ()
setAlignment nsCell  value =
  sendMsg nsCell (mkSelector "setAlignment:") retVoid [argCLong (coerce value)]

-- | @- wraps@
wraps :: IsNSCell nsCell => nsCell -> IO Bool
wraps nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "wraps") retCULong []

-- | @- setWraps:@
setWraps :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setWraps nsCell  value =
  sendMsg nsCell (mkSelector "setWraps:") retVoid [argCULong (if value then 1 else 0)]

-- | @- font@
font :: IsNSCell nsCell => nsCell -> IO (Id NSFont)
font nsCell  =
  sendMsg nsCell (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsNSCell nsCell, IsNSFont value) => nsCell -> value -> IO ()
setFont nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyEquivalent@
keyEquivalent :: IsNSCell nsCell => nsCell -> IO (Id NSString)
keyEquivalent nsCell  =
  sendMsg nsCell (mkSelector "keyEquivalent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- formatter@
formatter :: IsNSCell nsCell => nsCell -> IO (Id NSFormatter)
formatter nsCell  =
  sendMsg nsCell (mkSelector "formatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFormatter:@
setFormatter :: (IsNSCell nsCell, IsNSFormatter value) => nsCell -> value -> IO ()
setFormatter nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- objectValue@
objectValue :: IsNSCell nsCell => nsCell -> IO RawId
objectValue nsCell  =
  fmap (RawId . castPtr) $ sendMsg nsCell (mkSelector "objectValue") (retPtr retVoid) []

-- | @- setObjectValue:@
setObjectValue :: IsNSCell nsCell => nsCell -> RawId -> IO ()
setObjectValue nsCell  value =
  sendMsg nsCell (mkSelector "setObjectValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- hasValidObjectValue@
hasValidObjectValue :: IsNSCell nsCell => nsCell -> IO Bool
hasValidObjectValue nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "hasValidObjectValue") retCULong []

-- | @- stringValue@
stringValue :: IsNSCell nsCell => nsCell -> IO (Id NSString)
stringValue nsCell  =
  sendMsg nsCell (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStringValue:@
setStringValue :: (IsNSCell nsCell, IsNSString value) => nsCell -> value -> IO ()
setStringValue nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- intValue@
intValue :: IsNSCell nsCell => nsCell -> IO CInt
intValue nsCell  =
  sendMsg nsCell (mkSelector "intValue") retCInt []

-- | @- setIntValue:@
setIntValue :: IsNSCell nsCell => nsCell -> CInt -> IO ()
setIntValue nsCell  value =
  sendMsg nsCell (mkSelector "setIntValue:") retVoid [argCInt (fromIntegral value)]

-- | @- floatValue@
floatValue :: IsNSCell nsCell => nsCell -> IO CFloat
floatValue nsCell  =
  sendMsg nsCell (mkSelector "floatValue") retCFloat []

-- | @- setFloatValue:@
setFloatValue :: IsNSCell nsCell => nsCell -> CFloat -> IO ()
setFloatValue nsCell  value =
  sendMsg nsCell (mkSelector "setFloatValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- doubleValue@
doubleValue :: IsNSCell nsCell => nsCell -> IO CDouble
doubleValue nsCell  =
  sendMsg nsCell (mkSelector "doubleValue") retCDouble []

-- | @- setDoubleValue:@
setDoubleValue :: IsNSCell nsCell => nsCell -> CDouble -> IO ()
setDoubleValue nsCell  value =
  sendMsg nsCell (mkSelector "setDoubleValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- integerValue@
integerValue :: IsNSCell nsCell => nsCell -> IO CLong
integerValue nsCell  =
  sendMsg nsCell (mkSelector "integerValue") retCLong []

-- | @- setIntegerValue:@
setIntegerValue :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setIntegerValue nsCell  value =
  sendMsg nsCell (mkSelector "setIntegerValue:") retVoid [argCLong (fromIntegral value)]

-- | @- image@
image :: IsNSCell nsCell => nsCell -> IO (Id NSImage)
image nsCell  =
  sendMsg nsCell (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSCell nsCell, IsNSImage value) => nsCell -> value -> IO ()
setImage nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- controlSize@
controlSize :: IsNSCell nsCell => nsCell -> IO NSControlSize
controlSize nsCell  =
  fmap (coerce :: CULong -> NSControlSize) $ sendMsg nsCell (mkSelector "controlSize") retCULong []

-- | @- setControlSize:@
setControlSize :: IsNSCell nsCell => nsCell -> NSControlSize -> IO ()
setControlSize nsCell  value =
  sendMsg nsCell (mkSelector "setControlSize:") retVoid [argCULong (coerce value)]

-- | @- representedObject@
representedObject :: IsNSCell nsCell => nsCell -> IO RawId
representedObject nsCell  =
  fmap (RawId . castPtr) $ sendMsg nsCell (mkSelector "representedObject") (retPtr retVoid) []

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSCell nsCell => nsCell -> RawId -> IO ()
setRepresentedObject nsCell  value =
  sendMsg nsCell (mkSelector "setRepresentedObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- cellSize@
cellSize :: IsNSCell nsCell => nsCell -> IO NSSize
cellSize nsCell  =
  sendMsgStret nsCell (mkSelector "cellSize") retNSSize []

-- | @- mouseDownFlags@
mouseDownFlags :: IsNSCell nsCell => nsCell -> IO CLong
mouseDownFlags nsCell  =
  sendMsg nsCell (mkSelector "mouseDownFlags") retCLong []

-- | @- menu@
menu :: IsNSCell nsCell => nsCell -> IO (Id NSMenu)
menu nsCell  =
  sendMsg nsCell (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMenu:@
setMenu :: (IsNSCell nsCell, IsNSMenu value) => nsCell -> value -> IO ()
setMenu nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ defaultMenu@
defaultMenu :: IO (Id NSMenu)
defaultMenu  =
  do
    cls' <- getRequiredClass "NSCell"
    sendClassMsg cls' (mkSelector "defaultMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sendsActionOnEndEditing@
sendsActionOnEndEditing :: IsNSCell nsCell => nsCell -> IO Bool
sendsActionOnEndEditing nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "sendsActionOnEndEditing") retCULong []

-- | @- setSendsActionOnEndEditing:@
setSendsActionOnEndEditing :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setSendsActionOnEndEditing nsCell  value =
  sendMsg nsCell (mkSelector "setSendsActionOnEndEditing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSCell nsCell => nsCell -> IO NSWritingDirection
baseWritingDirection nsCell  =
  fmap (coerce :: CLong -> NSWritingDirection) $ sendMsg nsCell (mkSelector "baseWritingDirection") retCLong []

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSCell nsCell => nsCell -> NSWritingDirection -> IO ()
setBaseWritingDirection nsCell  value =
  sendMsg nsCell (mkSelector "setBaseWritingDirection:") retVoid [argCLong (coerce value)]

-- | @- lineBreakMode@
lineBreakMode :: IsNSCell nsCell => nsCell -> IO NSLineBreakMode
lineBreakMode nsCell  =
  fmap (coerce :: CULong -> NSLineBreakMode) $ sendMsg nsCell (mkSelector "lineBreakMode") retCULong []

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSCell nsCell => nsCell -> NSLineBreakMode -> IO ()
setLineBreakMode nsCell  value =
  sendMsg nsCell (mkSelector "setLineBreakMode:") retVoid [argCULong (coerce value)]

-- | @- allowsUndo@
allowsUndo :: IsNSCell nsCell => nsCell -> IO Bool
allowsUndo nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "allowsUndo") retCULong []

-- | @- setAllowsUndo:@
setAllowsUndo :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setAllowsUndo nsCell  value =
  sendMsg nsCell (mkSelector "setAllowsUndo:") retVoid [argCULong (if value then 1 else 0)]

-- | @- truncatesLastVisibleLine@
truncatesLastVisibleLine :: IsNSCell nsCell => nsCell -> IO Bool
truncatesLastVisibleLine nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "truncatesLastVisibleLine") retCULong []

-- | @- setTruncatesLastVisibleLine:@
setTruncatesLastVisibleLine :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setTruncatesLastVisibleLine nsCell  value =
  sendMsg nsCell (mkSelector "setTruncatesLastVisibleLine:") retVoid [argCULong (if value then 1 else 0)]

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSCell nsCell => nsCell -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsCell  =
  fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsCell (mkSelector "userInterfaceLayoutDirection") retCLong []

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSCell nsCell => nsCell -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsCell  value =
  sendMsg nsCell (mkSelector "setUserInterfaceLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @- usesSingleLineMode@
usesSingleLineMode :: IsNSCell nsCell => nsCell -> IO Bool
usesSingleLineMode nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "usesSingleLineMode") retCULong []

-- | @- setUsesSingleLineMode:@
setUsesSingleLineMode :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setUsesSingleLineMode nsCell  value =
  sendMsg nsCell (mkSelector "setUsesSingleLineMode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- controlTint@
controlTint :: IsNSCell nsCell => nsCell -> IO NSControlTint
controlTint nsCell  =
  fmap (coerce :: CULong -> NSControlTint) $ sendMsg nsCell (mkSelector "controlTint") retCULong []

-- | @- setControlTint:@
setControlTint :: IsNSCell nsCell => nsCell -> NSControlTint -> IO ()
setControlTint nsCell  value =
  sendMsg nsCell (mkSelector "setControlTint:") retVoid [argCULong (coerce value)]

-- | @- backgroundStyle@
backgroundStyle :: IsNSCell nsCell => nsCell -> IO NSBackgroundStyle
backgroundStyle nsCell  =
  fmap (coerce :: CLong -> NSBackgroundStyle) $ sendMsg nsCell (mkSelector "backgroundStyle") retCLong []

-- | @- setBackgroundStyle:@
setBackgroundStyle :: IsNSCell nsCell => nsCell -> NSBackgroundStyle -> IO ()
setBackgroundStyle nsCell  value =
  sendMsg nsCell (mkSelector "setBackgroundStyle:") retVoid [argCLong (coerce value)]

-- | @- interiorBackgroundStyle@
interiorBackgroundStyle :: IsNSCell nsCell => nsCell -> IO NSBackgroundStyle
interiorBackgroundStyle nsCell  =
  fmap (coerce :: CLong -> NSBackgroundStyle) $ sendMsg nsCell (mkSelector "interiorBackgroundStyle") retCLong []

-- | @- allowsMixedState@
allowsMixedState :: IsNSCell nsCell => nsCell -> IO Bool
allowsMixedState nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "allowsMixedState") retCULong []

-- | @- setAllowsMixedState:@
setAllowsMixedState :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setAllowsMixedState nsCell  value =
  sendMsg nsCell (mkSelector "setAllowsMixedState:") retVoid [argCULong (if value then 1 else 0)]

-- | @- nextState@
nextState :: IsNSCell nsCell => nsCell -> IO CLong
nextState nsCell  =
  sendMsg nsCell (mkSelector "nextState") retCLong []

-- | @- attributedStringValue@
attributedStringValue :: IsNSCell nsCell => nsCell -> IO (Id NSAttributedString)
attributedStringValue nsCell  =
  sendMsg nsCell (mkSelector "attributedStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedStringValue:@
setAttributedStringValue :: (IsNSCell nsCell, IsNSAttributedString value) => nsCell -> value -> IO ()
setAttributedStringValue nsCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCell (mkSelector "setAttributedStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsEditingTextAttributes@
allowsEditingTextAttributes :: IsNSCell nsCell => nsCell -> IO Bool
allowsEditingTextAttributes nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "allowsEditingTextAttributes") retCULong []

-- | @- setAllowsEditingTextAttributes:@
setAllowsEditingTextAttributes :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setAllowsEditingTextAttributes nsCell  value =
  sendMsg nsCell (mkSelector "setAllowsEditingTextAttributes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- importsGraphics@
importsGraphics :: IsNSCell nsCell => nsCell -> IO Bool
importsGraphics nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "importsGraphics") retCULong []

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setImportsGraphics nsCell  value =
  sendMsg nsCell (mkSelector "setImportsGraphics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- refusesFirstResponder@
refusesFirstResponder :: IsNSCell nsCell => nsCell -> IO Bool
refusesFirstResponder nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "refusesFirstResponder") retCULong []

-- | @- setRefusesFirstResponder:@
setRefusesFirstResponder :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setRefusesFirstResponder nsCell  value =
  sendMsg nsCell (mkSelector "setRefusesFirstResponder:") retVoid [argCULong (if value then 1 else 0)]

-- | @- acceptsFirstResponder@
acceptsFirstResponder :: IsNSCell nsCell => nsCell -> IO Bool
acceptsFirstResponder nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "acceptsFirstResponder") retCULong []

-- | @- showsFirstResponder@
showsFirstResponder :: IsNSCell nsCell => nsCell -> IO Bool
showsFirstResponder nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "showsFirstResponder") retCULong []

-- | @- setShowsFirstResponder:@
setShowsFirstResponder :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setShowsFirstResponder nsCell  value =
  sendMsg nsCell (mkSelector "setShowsFirstResponder:") retVoid [argCULong (if value then 1 else 0)]

-- | @- focusRingType@
focusRingType :: IsNSCell nsCell => nsCell -> IO NSFocusRingType
focusRingType nsCell  =
  fmap (coerce :: CULong -> NSFocusRingType) $ sendMsg nsCell (mkSelector "focusRingType") retCULong []

-- | @- setFocusRingType:@
setFocusRingType :: IsNSCell nsCell => nsCell -> NSFocusRingType -> IO ()
setFocusRingType nsCell  value =
  sendMsg nsCell (mkSelector "setFocusRingType:") retVoid [argCULong (coerce value)]

-- | @+ defaultFocusRingType@
defaultFocusRingType :: IO NSFocusRingType
defaultFocusRingType  =
  do
    cls' <- getRequiredClass "NSCell"
    fmap (coerce :: CULong -> NSFocusRingType) $ sendClassMsg cls' (mkSelector "defaultFocusRingType") retCULong []

-- | @- wantsNotificationForMarkedText@
wantsNotificationForMarkedText :: IsNSCell nsCell => nsCell -> IO Bool
wantsNotificationForMarkedText nsCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCell (mkSelector "wantsNotificationForMarkedText") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sendActionOn:@
sendActionOnSelector :: Selector
sendActionOnSelector = mkSelector "sendActionOn:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @takeIntValueFrom:@
takeIntValueFromSelector :: Selector
takeIntValueFromSelector = mkSelector "takeIntValueFrom:"

-- | @Selector@ for @takeFloatValueFrom:@
takeFloatValueFromSelector :: Selector
takeFloatValueFromSelector = mkSelector "takeFloatValueFrom:"

-- | @Selector@ for @takeDoubleValueFrom:@
takeDoubleValueFromSelector :: Selector
takeDoubleValueFromSelector = mkSelector "takeDoubleValueFrom:"

-- | @Selector@ for @takeStringValueFrom:@
takeStringValueFromSelector :: Selector
takeStringValueFromSelector = mkSelector "takeStringValueFrom:"

-- | @Selector@ for @takeObjectValueFrom:@
takeObjectValueFromSelector :: Selector
takeObjectValueFromSelector = mkSelector "takeObjectValueFrom:"

-- | @Selector@ for @takeIntegerValueFrom:@
takeIntegerValueFromSelector :: Selector
takeIntegerValueFromSelector = mkSelector "takeIntegerValueFrom:"

-- | @Selector@ for @cellAttribute:@
cellAttributeSelector :: Selector
cellAttributeSelector = mkSelector "cellAttribute:"

-- | @Selector@ for @setCellAttribute:to:@
setCellAttribute_toSelector :: Selector
setCellAttribute_toSelector = mkSelector "setCellAttribute:to:"

-- | @Selector@ for @imageRectForBounds:@
imageRectForBoundsSelector :: Selector
imageRectForBoundsSelector = mkSelector "imageRectForBounds:"

-- | @Selector@ for @titleRectForBounds:@
titleRectForBoundsSelector :: Selector
titleRectForBoundsSelector = mkSelector "titleRectForBounds:"

-- | @Selector@ for @drawingRectForBounds:@
drawingRectForBoundsSelector :: Selector
drawingRectForBoundsSelector = mkSelector "drawingRectForBounds:"

-- | @Selector@ for @_bulletStringForString:bulletCharacter:@
_bulletStringForString_bulletCharacterSelector :: Selector
_bulletStringForString_bulletCharacterSelector = mkSelector "_bulletStringForString:bulletCharacter:"

-- | @Selector@ for @cellSizeForBounds:@
cellSizeForBoundsSelector :: Selector
cellSizeForBoundsSelector = mkSelector "cellSizeForBounds:"

-- | @Selector@ for @highlightColorWithFrame:inView:@
highlightColorWithFrame_inViewSelector :: Selector
highlightColorWithFrame_inViewSelector = mkSelector "highlightColorWithFrame:inView:"

-- | @Selector@ for @calcDrawInfo:@
calcDrawInfoSelector :: Selector
calcDrawInfoSelector = mkSelector "calcDrawInfo:"

-- | @Selector@ for @setUpFieldEditorAttributes:@
setUpFieldEditorAttributesSelector :: Selector
setUpFieldEditorAttributesSelector = mkSelector "setUpFieldEditorAttributes:"

-- | @Selector@ for @drawInteriorWithFrame:inView:@
drawInteriorWithFrame_inViewSelector :: Selector
drawInteriorWithFrame_inViewSelector = mkSelector "drawInteriorWithFrame:inView:"

-- | @Selector@ for @drawWithFrame:inView:@
drawWithFrame_inViewSelector :: Selector
drawWithFrame_inViewSelector = mkSelector "drawWithFrame:inView:"

-- | @Selector@ for @highlight:withFrame:inView:@
highlight_withFrame_inViewSelector :: Selector
highlight_withFrame_inViewSelector = mkSelector "highlight:withFrame:inView:"

-- | @Selector@ for @getPeriodicDelay:interval:@
getPeriodicDelay_intervalSelector :: Selector
getPeriodicDelay_intervalSelector = mkSelector "getPeriodicDelay:interval:"

-- | @Selector@ for @startTrackingAt:inView:@
startTrackingAt_inViewSelector :: Selector
startTrackingAt_inViewSelector = mkSelector "startTrackingAt:inView:"

-- | @Selector@ for @continueTracking:at:inView:@
continueTracking_at_inViewSelector :: Selector
continueTracking_at_inViewSelector = mkSelector "continueTracking:at:inView:"

-- | @Selector@ for @stopTracking:at:inView:mouseIsUp:@
stopTracking_at_inView_mouseIsUpSelector :: Selector
stopTracking_at_inView_mouseIsUpSelector = mkSelector "stopTracking:at:inView:mouseIsUp:"

-- | @Selector@ for @trackMouse:inRect:ofView:untilMouseUp:@
trackMouse_inRect_ofView_untilMouseUpSelector :: Selector
trackMouse_inRect_ofView_untilMouseUpSelector = mkSelector "trackMouse:inRect:ofView:untilMouseUp:"

-- | @Selector@ for @editWithFrame:inView:editor:delegate:event:@
editWithFrame_inView_editor_delegate_eventSelector :: Selector
editWithFrame_inView_editor_delegate_eventSelector = mkSelector "editWithFrame:inView:editor:delegate:event:"

-- | @Selector@ for @selectWithFrame:inView:editor:delegate:start:length:@
selectWithFrame_inView_editor_delegate_start_lengthSelector :: Selector
selectWithFrame_inView_editor_delegate_start_lengthSelector = mkSelector "selectWithFrame:inView:editor:delegate:start:length:"

-- | @Selector@ for @endEditing:@
endEditingSelector :: Selector
endEditingSelector = mkSelector "endEditing:"

-- | @Selector@ for @resetCursorRect:inView:@
resetCursorRect_inViewSelector :: Selector
resetCursorRect_inViewSelector = mkSelector "resetCursorRect:inView:"

-- | @Selector@ for @menuForEvent:inRect:ofView:@
menuForEvent_inRect_ofViewSelector :: Selector
menuForEvent_inRect_ofViewSelector = mkSelector "menuForEvent:inRect:ofView:"

-- | @Selector@ for @fieldEditorForView:@
fieldEditorForViewSelector :: Selector
fieldEditorForViewSelector = mkSelector "fieldEditorForView:"

-- | @Selector@ for @draggingImageComponentsWithFrame:inView:@
draggingImageComponentsWithFrame_inViewSelector :: Selector
draggingImageComponentsWithFrame_inViewSelector = mkSelector "draggingImageComponentsWithFrame:inView:"

-- | @Selector@ for @entryType@
entryTypeSelector :: Selector
entryTypeSelector = mkSelector "entryType"

-- | @Selector@ for @setEntryType:@
setEntryTypeSelector :: Selector
setEntryTypeSelector = mkSelector "setEntryType:"

-- | @Selector@ for @isEntryAcceptable:@
isEntryAcceptableSelector :: Selector
isEntryAcceptableSelector = mkSelector "isEntryAcceptable:"

-- | @Selector@ for @setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_rightSelector :: Selector
setFloatingPointFormat_left_rightSelector = mkSelector "setFloatingPointFormat:left:right:"

-- | @Selector@ for @setMnemonicLocation:@
setMnemonicLocationSelector :: Selector
setMnemonicLocationSelector = mkSelector "setMnemonicLocation:"

-- | @Selector@ for @mnemonicLocation@
mnemonicLocationSelector :: Selector
mnemonicLocationSelector = mkSelector "mnemonicLocation"

-- | @Selector@ for @mnemonic@
mnemonicSelector :: Selector
mnemonicSelector = mkSelector "mnemonic"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @expansionFrameWithFrame:inView:@
expansionFrameWithFrame_inViewSelector :: Selector
expansionFrameWithFrame_inViewSelector = mkSelector "expansionFrameWithFrame:inView:"

-- | @Selector@ for @drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inViewSelector :: Selector
drawWithExpansionFrame_inViewSelector = mkSelector "drawWithExpansionFrame:inView:"

-- | @Selector@ for @hitTestForEvent:inRect:ofView:@
hitTestForEvent_inRect_ofViewSelector :: Selector
hitTestForEvent_inRect_ofViewSelector = mkSelector "hitTestForEvent:inRect:ofView:"

-- | @Selector@ for @setNextState@
setNextStateSelector :: Selector
setNextStateSelector = mkSelector "setNextState"

-- | @Selector@ for @performClick:@
performClickSelector :: Selector
performClickSelector = mkSelector "performClick:"

-- | @Selector@ for @drawFocusRingMaskWithFrame:inView:@
drawFocusRingMaskWithFrame_inViewSelector :: Selector
drawFocusRingMaskWithFrame_inViewSelector = mkSelector "drawFocusRingMaskWithFrame:inView:"

-- | @Selector@ for @focusRingMaskBoundsForFrame:inView:@
focusRingMaskBoundsForFrame_inViewSelector :: Selector
focusRingMaskBoundsForFrame_inViewSelector = mkSelector "focusRingMaskBoundsForFrame:inView:"

-- | @Selector@ for @prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUpSelector :: Selector
prefersTrackingUntilMouseUpSelector = mkSelector "prefersTrackingUntilMouseUp"

-- | @Selector@ for @controlView@
controlViewSelector :: Selector
controlViewSelector = mkSelector "controlView"

-- | @Selector@ for @setControlView:@
setControlViewSelector :: Selector
setControlViewSelector = mkSelector "setControlView:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @scrollable@
scrollableSelector :: Selector
scrollableSelector = mkSelector "scrollable"

-- | @Selector@ for @setScrollable:@
setScrollableSelector :: Selector
setScrollableSelector = mkSelector "setScrollable:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @wraps@
wrapsSelector :: Selector
wrapsSelector = mkSelector "wraps"

-- | @Selector@ for @setWraps:@
setWrapsSelector :: Selector
setWrapsSelector = mkSelector "setWraps:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @keyEquivalent@
keyEquivalentSelector :: Selector
keyEquivalentSelector = mkSelector "keyEquivalent"

-- | @Selector@ for @formatter@
formatterSelector :: Selector
formatterSelector = mkSelector "formatter"

-- | @Selector@ for @setFormatter:@
setFormatterSelector :: Selector
setFormatterSelector = mkSelector "setFormatter:"

-- | @Selector@ for @objectValue@
objectValueSelector :: Selector
objectValueSelector = mkSelector "objectValue"

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @hasValidObjectValue@
hasValidObjectValueSelector :: Selector
hasValidObjectValueSelector = mkSelector "hasValidObjectValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @intValue@
intValueSelector :: Selector
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @setIntValue:@
setIntValueSelector :: Selector
setIntValueSelector = mkSelector "setIntValue:"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector
setFloatValueSelector = mkSelector "setFloatValue:"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @setDoubleValue:@
setDoubleValueSelector :: Selector
setDoubleValueSelector = mkSelector "setDoubleValue:"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @setIntegerValue:@
setIntegerValueSelector :: Selector
setIntegerValueSelector = mkSelector "setIntegerValue:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @cellSize@
cellSizeSelector :: Selector
cellSizeSelector = mkSelector "cellSize"

-- | @Selector@ for @mouseDownFlags@
mouseDownFlagsSelector :: Selector
mouseDownFlagsSelector = mkSelector "mouseDownFlags"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @defaultMenu@
defaultMenuSelector :: Selector
defaultMenuSelector = mkSelector "defaultMenu"

-- | @Selector@ for @sendsActionOnEndEditing@
sendsActionOnEndEditingSelector :: Selector
sendsActionOnEndEditingSelector = mkSelector "sendsActionOnEndEditing"

-- | @Selector@ for @setSendsActionOnEndEditing:@
setSendsActionOnEndEditingSelector :: Selector
setSendsActionOnEndEditingSelector = mkSelector "setSendsActionOnEndEditing:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @allowsUndo@
allowsUndoSelector :: Selector
allowsUndoSelector = mkSelector "allowsUndo"

-- | @Selector@ for @setAllowsUndo:@
setAllowsUndoSelector :: Selector
setAllowsUndoSelector = mkSelector "setAllowsUndo:"

-- | @Selector@ for @truncatesLastVisibleLine@
truncatesLastVisibleLineSelector :: Selector
truncatesLastVisibleLineSelector = mkSelector "truncatesLastVisibleLine"

-- | @Selector@ for @setTruncatesLastVisibleLine:@
setTruncatesLastVisibleLineSelector :: Selector
setTruncatesLastVisibleLineSelector = mkSelector "setTruncatesLastVisibleLine:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @usesSingleLineMode@
usesSingleLineModeSelector :: Selector
usesSingleLineModeSelector = mkSelector "usesSingleLineMode"

-- | @Selector@ for @setUsesSingleLineMode:@
setUsesSingleLineModeSelector :: Selector
setUsesSingleLineModeSelector = mkSelector "setUsesSingleLineMode:"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector
setControlTintSelector = mkSelector "setControlTint:"

-- | @Selector@ for @backgroundStyle@
backgroundStyleSelector :: Selector
backgroundStyleSelector = mkSelector "backgroundStyle"

-- | @Selector@ for @setBackgroundStyle:@
setBackgroundStyleSelector :: Selector
setBackgroundStyleSelector = mkSelector "setBackgroundStyle:"

-- | @Selector@ for @interiorBackgroundStyle@
interiorBackgroundStyleSelector :: Selector
interiorBackgroundStyleSelector = mkSelector "interiorBackgroundStyle"

-- | @Selector@ for @allowsMixedState@
allowsMixedStateSelector :: Selector
allowsMixedStateSelector = mkSelector "allowsMixedState"

-- | @Selector@ for @setAllowsMixedState:@
setAllowsMixedStateSelector :: Selector
setAllowsMixedStateSelector = mkSelector "setAllowsMixedState:"

-- | @Selector@ for @nextState@
nextStateSelector :: Selector
nextStateSelector = mkSelector "nextState"

-- | @Selector@ for @attributedStringValue@
attributedStringValueSelector :: Selector
attributedStringValueSelector = mkSelector "attributedStringValue"

-- | @Selector@ for @setAttributedStringValue:@
setAttributedStringValueSelector :: Selector
setAttributedStringValueSelector = mkSelector "setAttributedStringValue:"

-- | @Selector@ for @allowsEditingTextAttributes@
allowsEditingTextAttributesSelector :: Selector
allowsEditingTextAttributesSelector = mkSelector "allowsEditingTextAttributes"

-- | @Selector@ for @setAllowsEditingTextAttributes:@
setAllowsEditingTextAttributesSelector :: Selector
setAllowsEditingTextAttributesSelector = mkSelector "setAllowsEditingTextAttributes:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @refusesFirstResponder@
refusesFirstResponderSelector :: Selector
refusesFirstResponderSelector = mkSelector "refusesFirstResponder"

-- | @Selector@ for @setRefusesFirstResponder:@
setRefusesFirstResponderSelector :: Selector
setRefusesFirstResponderSelector = mkSelector "setRefusesFirstResponder:"

-- | @Selector@ for @acceptsFirstResponder@
acceptsFirstResponderSelector :: Selector
acceptsFirstResponderSelector = mkSelector "acceptsFirstResponder"

-- | @Selector@ for @showsFirstResponder@
showsFirstResponderSelector :: Selector
showsFirstResponderSelector = mkSelector "showsFirstResponder"

-- | @Selector@ for @setShowsFirstResponder:@
setShowsFirstResponderSelector :: Selector
setShowsFirstResponderSelector = mkSelector "setShowsFirstResponder:"

-- | @Selector@ for @focusRingType@
focusRingTypeSelector :: Selector
focusRingTypeSelector = mkSelector "focusRingType"

-- | @Selector@ for @setFocusRingType:@
setFocusRingTypeSelector :: Selector
setFocusRingTypeSelector = mkSelector "setFocusRingType:"

-- | @Selector@ for @defaultFocusRingType@
defaultFocusRingTypeSelector :: Selector
defaultFocusRingTypeSelector = mkSelector "defaultFocusRingType"

-- | @Selector@ for @wantsNotificationForMarkedText@
wantsNotificationForMarkedTextSelector :: Selector
wantsNotificationForMarkedTextSelector = mkSelector "wantsNotificationForMarkedText"

