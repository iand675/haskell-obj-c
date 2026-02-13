{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , _bulletStringForString_bulletCharacterSelector
  , acceptsFirstResponderSelector
  , actionSelector
  , alignmentSelector
  , allowsEditingTextAttributesSelector
  , allowsMixedStateSelector
  , allowsUndoSelector
  , attributedStringValueSelector
  , backgroundStyleSelector
  , baseWritingDirectionSelector
  , bezeledSelector
  , borderedSelector
  , calcDrawInfoSelector
  , cellAttributeSelector
  , cellSizeForBoundsSelector
  , cellSizeSelector
  , compareSelector
  , continueTracking_at_inViewSelector
  , continuousSelector
  , controlSizeSelector
  , controlTintSelector
  , controlViewSelector
  , defaultFocusRingTypeSelector
  , defaultMenuSelector
  , doubleValueSelector
  , draggingImageComponentsWithFrame_inViewSelector
  , drawFocusRingMaskWithFrame_inViewSelector
  , drawInteriorWithFrame_inViewSelector
  , drawWithExpansionFrame_inViewSelector
  , drawWithFrame_inViewSelector
  , drawingRectForBoundsSelector
  , editWithFrame_inView_editor_delegate_eventSelector
  , editableSelector
  , enabledSelector
  , endEditingSelector
  , entryTypeSelector
  , expansionFrameWithFrame_inViewSelector
  , fieldEditorForViewSelector
  , floatValueSelector
  , focusRingMaskBoundsForFrame_inViewSelector
  , focusRingTypeSelector
  , fontSelector
  , formatterSelector
  , getPeriodicDelay_intervalSelector
  , hasValidObjectValueSelector
  , highlightColorWithFrame_inViewSelector
  , highlight_withFrame_inViewSelector
  , highlightedSelector
  , hitTestForEvent_inRect_ofViewSelector
  , imageRectForBoundsSelector
  , imageSelector
  , importsGraphicsSelector
  , initImageCellSelector
  , initSelector
  , initTextCellSelector
  , initWithCoderSelector
  , intValueSelector
  , integerValueSelector
  , interiorBackgroundStyleSelector
  , isEntryAcceptableSelector
  , keyEquivalentSelector
  , lineBreakModeSelector
  , menuForEvent_inRect_ofViewSelector
  , menuSelector
  , mnemonicLocationSelector
  , mnemonicSelector
  , mouseDownFlagsSelector
  , nextStateSelector
  , objectValueSelector
  , opaqueSelector
  , performClickSelector
  , prefersTrackingUntilMouseUpSelector
  , refusesFirstResponderSelector
  , representedObjectSelector
  , resetCursorRect_inViewSelector
  , scrollableSelector
  , selectWithFrame_inView_editor_delegate_start_lengthSelector
  , selectableSelector
  , sendActionOnSelector
  , sendsActionOnEndEditingSelector
  , setActionSelector
  , setAlignmentSelector
  , setAllowsEditingTextAttributesSelector
  , setAllowsMixedStateSelector
  , setAllowsUndoSelector
  , setAttributedStringValueSelector
  , setBackgroundStyleSelector
  , setBaseWritingDirectionSelector
  , setBezeledSelector
  , setBorderedSelector
  , setCellAttribute_toSelector
  , setContinuousSelector
  , setControlSizeSelector
  , setControlTintSelector
  , setControlViewSelector
  , setDoubleValueSelector
  , setEditableSelector
  , setEnabledSelector
  , setEntryTypeSelector
  , setFloatValueSelector
  , setFloatingPointFormat_left_rightSelector
  , setFocusRingTypeSelector
  , setFontSelector
  , setFormatterSelector
  , setHighlightedSelector
  , setImageSelector
  , setImportsGraphicsSelector
  , setIntValueSelector
  , setIntegerValueSelector
  , setLineBreakModeSelector
  , setMenuSelector
  , setMnemonicLocationSelector
  , setNextStateSelector
  , setObjectValueSelector
  , setRefusesFirstResponderSelector
  , setRepresentedObjectSelector
  , setScrollableSelector
  , setSelectableSelector
  , setSendsActionOnEndEditingSelector
  , setShowsFirstResponderSelector
  , setStateSelector
  , setStringValueSelector
  , setTagSelector
  , setTargetSelector
  , setTitleSelector
  , setTitleWithMnemonicSelector
  , setTruncatesLastVisibleLineSelector
  , setTypeSelector
  , setUpFieldEditorAttributesSelector
  , setUserInterfaceLayoutDirectionSelector
  , setUsesSingleLineModeSelector
  , setWrapsSelector
  , showsFirstResponderSelector
  , startTrackingAt_inViewSelector
  , stateSelector
  , stopTracking_at_inView_mouseIsUpSelector
  , stringValueSelector
  , tagSelector
  , takeDoubleValueFromSelector
  , takeFloatValueFromSelector
  , takeIntValueFromSelector
  , takeIntegerValueFromSelector
  , takeObjectValueFromSelector
  , takeStringValueFromSelector
  , targetSelector
  , titleRectForBoundsSelector
  , titleSelector
  , trackMouse_inRect_ofView_untilMouseUpSelector
  , truncatesLastVisibleLineSelector
  , typeSelector
  , userInterfaceLayoutDirectionSelector
  , usesSingleLineModeSelector
  , wantsNotificationForMarkedTextSelector
  , wrapsSelector

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

-- | @- init@
init_ :: IsNSCell nsCell => nsCell -> IO (Id NSCell)
init_ nsCell =
  sendOwnedMessage nsCell initSelector

-- | @- initTextCell:@
initTextCell :: (IsNSCell nsCell, IsNSString string) => nsCell -> string -> IO (Id NSCell)
initTextCell nsCell string =
  sendOwnedMessage nsCell initTextCellSelector (toNSString string)

-- | @- initImageCell:@
initImageCell :: (IsNSCell nsCell, IsNSImage image) => nsCell -> image -> IO (Id NSCell)
initImageCell nsCell image =
  sendOwnedMessage nsCell initImageCellSelector (toNSImage image)

-- | @- initWithCoder:@
initWithCoder :: (IsNSCell nsCell, IsNSCoder coder) => nsCell -> coder -> IO (Id NSCell)
initWithCoder nsCell coder =
  sendOwnedMessage nsCell initWithCoderSelector (toNSCoder coder)

-- | @- sendActionOn:@
sendActionOn :: IsNSCell nsCell => nsCell -> NSEventMask -> IO CLong
sendActionOn nsCell mask =
  sendMessage nsCell sendActionOnSelector mask

-- | @- compare:@
compare_ :: IsNSCell nsCell => nsCell -> RawId -> IO NSComparisonResult
compare_ nsCell otherCell =
  sendMessage nsCell compareSelector otherCell

-- | @- takeIntValueFrom:@
takeIntValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeIntValueFrom nsCell sender =
  sendMessage nsCell takeIntValueFromSelector sender

-- | @- takeFloatValueFrom:@
takeFloatValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeFloatValueFrom nsCell sender =
  sendMessage nsCell takeFloatValueFromSelector sender

-- | @- takeDoubleValueFrom:@
takeDoubleValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeDoubleValueFrom nsCell sender =
  sendMessage nsCell takeDoubleValueFromSelector sender

-- | @- takeStringValueFrom:@
takeStringValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeStringValueFrom nsCell sender =
  sendMessage nsCell takeStringValueFromSelector sender

-- | @- takeObjectValueFrom:@
takeObjectValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeObjectValueFrom nsCell sender =
  sendMessage nsCell takeObjectValueFromSelector sender

-- | @- takeIntegerValueFrom:@
takeIntegerValueFrom :: IsNSCell nsCell => nsCell -> RawId -> IO ()
takeIntegerValueFrom nsCell sender =
  sendMessage nsCell takeIntegerValueFromSelector sender

-- | @- cellAttribute:@
cellAttribute :: IsNSCell nsCell => nsCell -> NSCellAttribute -> IO CLong
cellAttribute nsCell parameter =
  sendMessage nsCell cellAttributeSelector parameter

-- | @- setCellAttribute:to:@
setCellAttribute_to :: IsNSCell nsCell => nsCell -> NSCellAttribute -> CLong -> IO ()
setCellAttribute_to nsCell parameter value =
  sendMessage nsCell setCellAttribute_toSelector parameter value

-- | @- imageRectForBounds:@
imageRectForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSRect
imageRectForBounds nsCell rect =
  sendMessage nsCell imageRectForBoundsSelector rect

-- | @- titleRectForBounds:@
titleRectForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSRect
titleRectForBounds nsCell rect =
  sendMessage nsCell titleRectForBoundsSelector rect

-- | @- drawingRectForBounds:@
drawingRectForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSRect
drawingRectForBounds nsCell rect =
  sendMessage nsCell drawingRectForBoundsSelector rect

-- | @+ _bulletStringForString:bulletCharacter:@
_bulletStringForString_bulletCharacter :: IsNSString string => string -> CUShort -> IO (Id NSString)
_bulletStringForString_bulletCharacter string bulletChar =
  do
    cls' <- getRequiredClass "NSCell"
    sendClassMessage cls' _bulletStringForString_bulletCharacterSelector (toNSString string) bulletChar

-- | @- cellSizeForBounds:@
cellSizeForBounds :: IsNSCell nsCell => nsCell -> NSRect -> IO NSSize
cellSizeForBounds nsCell rect =
  sendMessage nsCell cellSizeForBoundsSelector rect

-- | @- highlightColorWithFrame:inView:@
highlightColorWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO (Id NSColor)
highlightColorWithFrame_inView nsCell cellFrame controlView =
  sendMessage nsCell highlightColorWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- calcDrawInfo:@
calcDrawInfo :: IsNSCell nsCell => nsCell -> NSRect -> IO ()
calcDrawInfo nsCell rect =
  sendMessage nsCell calcDrawInfoSelector rect

-- | @- setUpFieldEditorAttributes:@
setUpFieldEditorAttributes :: (IsNSCell nsCell, IsNSText textObj) => nsCell -> textObj -> IO (Id NSText)
setUpFieldEditorAttributes nsCell textObj =
  sendMessage nsCell setUpFieldEditorAttributesSelector (toNSText textObj)

-- | @- drawInteriorWithFrame:inView:@
drawInteriorWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
drawInteriorWithFrame_inView nsCell cellFrame controlView =
  sendMessage nsCell drawInteriorWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- drawWithFrame:inView:@
drawWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
drawWithFrame_inView nsCell cellFrame controlView =
  sendMessage nsCell drawWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- highlight:withFrame:inView:@
highlight_withFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> Bool -> NSRect -> controlView -> IO ()
highlight_withFrame_inView nsCell flag cellFrame controlView =
  sendMessage nsCell highlight_withFrame_inViewSelector flag cellFrame (toNSView controlView)

-- | @- getPeriodicDelay:interval:@
getPeriodicDelay_interval :: IsNSCell nsCell => nsCell -> Ptr CFloat -> Ptr CFloat -> IO ()
getPeriodicDelay_interval nsCell delay interval =
  sendMessage nsCell getPeriodicDelay_intervalSelector delay interval

-- | @- startTrackingAt:inView:@
startTrackingAt_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSPoint -> controlView -> IO Bool
startTrackingAt_inView nsCell startPoint controlView =
  sendMessage nsCell startTrackingAt_inViewSelector startPoint (toNSView controlView)

-- | @- continueTracking:at:inView:@
continueTracking_at_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSPoint -> NSPoint -> controlView -> IO Bool
continueTracking_at_inView nsCell lastPoint currentPoint controlView =
  sendMessage nsCell continueTracking_at_inViewSelector lastPoint currentPoint (toNSView controlView)

-- | @- stopTracking:at:inView:mouseIsUp:@
stopTracking_at_inView_mouseIsUp :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSPoint -> NSPoint -> controlView -> Bool -> IO ()
stopTracking_at_inView_mouseIsUp nsCell lastPoint stopPoint controlView flag =
  sendMessage nsCell stopTracking_at_inView_mouseIsUpSelector lastPoint stopPoint (toNSView controlView) flag

-- | @- trackMouse:inRect:ofView:untilMouseUp:@
trackMouse_inRect_ofView_untilMouseUp :: (IsNSCell nsCell, IsNSEvent event, IsNSView controlView) => nsCell -> event -> NSRect -> controlView -> Bool -> IO Bool
trackMouse_inRect_ofView_untilMouseUp nsCell event cellFrame controlView flag =
  sendMessage nsCell trackMouse_inRect_ofView_untilMouseUpSelector (toNSEvent event) cellFrame (toNSView controlView) flag

-- | @- editWithFrame:inView:editor:delegate:event:@
editWithFrame_inView_editor_delegate_event :: (IsNSCell nsCell, IsNSView controlView, IsNSText textObj, IsNSEvent event) => nsCell -> NSRect -> controlView -> textObj -> RawId -> event -> IO ()
editWithFrame_inView_editor_delegate_event nsCell rect controlView textObj delegate event =
  sendMessage nsCell editWithFrame_inView_editor_delegate_eventSelector rect (toNSView controlView) (toNSText textObj) delegate (toNSEvent event)

-- | @- selectWithFrame:inView:editor:delegate:start:length:@
selectWithFrame_inView_editor_delegate_start_length :: (IsNSCell nsCell, IsNSView controlView, IsNSText textObj) => nsCell -> NSRect -> controlView -> textObj -> RawId -> CLong -> CLong -> IO ()
selectWithFrame_inView_editor_delegate_start_length nsCell rect controlView textObj delegate selStart selLength =
  sendMessage nsCell selectWithFrame_inView_editor_delegate_start_lengthSelector rect (toNSView controlView) (toNSText textObj) delegate selStart selLength

-- | @- endEditing:@
endEditing :: (IsNSCell nsCell, IsNSText textObj) => nsCell -> textObj -> IO ()
endEditing nsCell textObj =
  sendMessage nsCell endEditingSelector (toNSText textObj)

-- | @- resetCursorRect:inView:@
resetCursorRect_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
resetCursorRect_inView nsCell cellFrame controlView =
  sendMessage nsCell resetCursorRect_inViewSelector cellFrame (toNSView controlView)

-- | @- menuForEvent:inRect:ofView:@
menuForEvent_inRect_ofView :: (IsNSCell nsCell, IsNSEvent event, IsNSView view) => nsCell -> event -> NSRect -> view -> IO (Id NSMenu)
menuForEvent_inRect_ofView nsCell event cellFrame view =
  sendMessage nsCell menuForEvent_inRect_ofViewSelector (toNSEvent event) cellFrame (toNSView view)

-- | @- fieldEditorForView:@
fieldEditorForView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> controlView -> IO (Id NSTextView)
fieldEditorForView nsCell controlView =
  sendMessage nsCell fieldEditorForViewSelector (toNSView controlView)

-- | @- draggingImageComponentsWithFrame:inView:@
draggingImageComponentsWithFrame_inView :: (IsNSCell nsCell, IsNSView view) => nsCell -> NSRect -> view -> IO (Id NSArray)
draggingImageComponentsWithFrame_inView nsCell frame view =
  sendMessage nsCell draggingImageComponentsWithFrame_inViewSelector frame (toNSView view)

-- | @- entryType@
entryType :: IsNSCell nsCell => nsCell -> IO CLong
entryType nsCell =
  sendMessage nsCell entryTypeSelector

-- | @- setEntryType:@
setEntryType :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setEntryType nsCell type_ =
  sendMessage nsCell setEntryTypeSelector type_

-- | @- isEntryAcceptable:@
isEntryAcceptable :: (IsNSCell nsCell, IsNSString string) => nsCell -> string -> IO Bool
isEntryAcceptable nsCell string =
  sendMessage nsCell isEntryAcceptableSelector (toNSString string)

-- | @- setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_right :: IsNSCell nsCell => nsCell -> Bool -> CULong -> CULong -> IO ()
setFloatingPointFormat_left_right nsCell autoRange leftDigits rightDigits =
  sendMessage nsCell setFloatingPointFormat_left_rightSelector autoRange leftDigits rightDigits

-- | @- setMnemonicLocation:@
setMnemonicLocation :: IsNSCell nsCell => nsCell -> CULong -> IO ()
setMnemonicLocation nsCell location =
  sendMessage nsCell setMnemonicLocationSelector location

-- | @- mnemonicLocation@
mnemonicLocation :: IsNSCell nsCell => nsCell -> IO CULong
mnemonicLocation nsCell =
  sendMessage nsCell mnemonicLocationSelector

-- | @- mnemonic@
mnemonic :: IsNSCell nsCell => nsCell -> IO (Id NSString)
mnemonic nsCell =
  sendMessage nsCell mnemonicSelector

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSCell nsCell, IsNSString stringWithAmpersand) => nsCell -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsCell stringWithAmpersand =
  sendMessage nsCell setTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | @- expansionFrameWithFrame:inView:@
expansionFrameWithFrame_inView :: (IsNSCell nsCell, IsNSView view) => nsCell -> NSRect -> view -> IO NSRect
expansionFrameWithFrame_inView nsCell cellFrame view =
  sendMessage nsCell expansionFrameWithFrame_inViewSelector cellFrame (toNSView view)

-- | @- drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inView :: (IsNSCell nsCell, IsNSView view) => nsCell -> NSRect -> view -> IO ()
drawWithExpansionFrame_inView nsCell cellFrame view =
  sendMessage nsCell drawWithExpansionFrame_inViewSelector cellFrame (toNSView view)

-- | @- hitTestForEvent:inRect:ofView:@
hitTestForEvent_inRect_ofView :: (IsNSCell nsCell, IsNSEvent event, IsNSView controlView) => nsCell -> event -> NSRect -> controlView -> IO NSCellHitResult
hitTestForEvent_inRect_ofView nsCell event cellFrame controlView =
  sendMessage nsCell hitTestForEvent_inRect_ofViewSelector (toNSEvent event) cellFrame (toNSView controlView)

-- | @- setNextState@
setNextState :: IsNSCell nsCell => nsCell -> IO ()
setNextState nsCell =
  sendMessage nsCell setNextStateSelector

-- | @- performClick:@
performClick :: IsNSCell nsCell => nsCell -> RawId -> IO ()
performClick nsCell sender =
  sendMessage nsCell performClickSelector sender

-- | @- drawFocusRingMaskWithFrame:inView:@
drawFocusRingMaskWithFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO ()
drawFocusRingMaskWithFrame_inView nsCell cellFrame controlView =
  sendMessage nsCell drawFocusRingMaskWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- focusRingMaskBoundsForFrame:inView:@
focusRingMaskBoundsForFrame_inView :: (IsNSCell nsCell, IsNSView controlView) => nsCell -> NSRect -> controlView -> IO NSRect
focusRingMaskBoundsForFrame_inView nsCell cellFrame controlView =
  sendMessage nsCell focusRingMaskBoundsForFrame_inViewSelector cellFrame (toNSView controlView)

-- | @+ prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUp :: IO Bool
prefersTrackingUntilMouseUp  =
  do
    cls' <- getRequiredClass "NSCell"
    sendClassMessage cls' prefersTrackingUntilMouseUpSelector

-- | @- controlView@
controlView :: IsNSCell nsCell => nsCell -> IO (Id NSView)
controlView nsCell =
  sendMessage nsCell controlViewSelector

-- | @- setControlView:@
setControlView :: (IsNSCell nsCell, IsNSView value) => nsCell -> value -> IO ()
setControlView nsCell value =
  sendMessage nsCell setControlViewSelector (toNSView value)

-- | @- type@
type_ :: IsNSCell nsCell => nsCell -> IO NSCellType
type_ nsCell =
  sendMessage nsCell typeSelector

-- | @- setType:@
setType :: IsNSCell nsCell => nsCell -> NSCellType -> IO ()
setType nsCell value =
  sendMessage nsCell setTypeSelector value

-- | @- state@
state :: IsNSCell nsCell => nsCell -> IO CLong
state nsCell =
  sendMessage nsCell stateSelector

-- | @- setState:@
setState :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setState nsCell value =
  sendMessage nsCell setStateSelector value

-- | @- target@
target :: IsNSCell nsCell => nsCell -> IO RawId
target nsCell =
  sendMessage nsCell targetSelector

-- | @- setTarget:@
setTarget :: IsNSCell nsCell => nsCell -> RawId -> IO ()
setTarget nsCell value =
  sendMessage nsCell setTargetSelector value

-- | @- action@
action :: IsNSCell nsCell => nsCell -> IO Sel
action nsCell =
  sendMessage nsCell actionSelector

-- | @- setAction:@
setAction :: IsNSCell nsCell => nsCell -> Sel -> IO ()
setAction nsCell value =
  sendMessage nsCell setActionSelector value

-- | @- tag@
tag :: IsNSCell nsCell => nsCell -> IO CLong
tag nsCell =
  sendMessage nsCell tagSelector

-- | @- setTag:@
setTag :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setTag nsCell value =
  sendMessage nsCell setTagSelector value

-- | @- title@
title :: IsNSCell nsCell => nsCell -> IO (Id NSString)
title nsCell =
  sendMessage nsCell titleSelector

-- | @- setTitle:@
setTitle :: (IsNSCell nsCell, IsNSString value) => nsCell -> value -> IO ()
setTitle nsCell value =
  sendMessage nsCell setTitleSelector (toNSString value)

-- | @- opaque@
opaque :: IsNSCell nsCell => nsCell -> IO Bool
opaque nsCell =
  sendMessage nsCell opaqueSelector

-- | @- enabled@
enabled :: IsNSCell nsCell => nsCell -> IO Bool
enabled nsCell =
  sendMessage nsCell enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setEnabled nsCell value =
  sendMessage nsCell setEnabledSelector value

-- | @- continuous@
continuous :: IsNSCell nsCell => nsCell -> IO Bool
continuous nsCell =
  sendMessage nsCell continuousSelector

-- | @- setContinuous:@
setContinuous :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setContinuous nsCell value =
  sendMessage nsCell setContinuousSelector value

-- | @- editable@
editable :: IsNSCell nsCell => nsCell -> IO Bool
editable nsCell =
  sendMessage nsCell editableSelector

-- | @- setEditable:@
setEditable :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setEditable nsCell value =
  sendMessage nsCell setEditableSelector value

-- | @- selectable@
selectable :: IsNSCell nsCell => nsCell -> IO Bool
selectable nsCell =
  sendMessage nsCell selectableSelector

-- | @- setSelectable:@
setSelectable :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setSelectable nsCell value =
  sendMessage nsCell setSelectableSelector value

-- | @- bordered@
bordered :: IsNSCell nsCell => nsCell -> IO Bool
bordered nsCell =
  sendMessage nsCell borderedSelector

-- | @- setBordered:@
setBordered :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setBordered nsCell value =
  sendMessage nsCell setBorderedSelector value

-- | @- bezeled@
bezeled :: IsNSCell nsCell => nsCell -> IO Bool
bezeled nsCell =
  sendMessage nsCell bezeledSelector

-- | @- setBezeled:@
setBezeled :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setBezeled nsCell value =
  sendMessage nsCell setBezeledSelector value

-- | @- scrollable@
scrollable :: IsNSCell nsCell => nsCell -> IO Bool
scrollable nsCell =
  sendMessage nsCell scrollableSelector

-- | @- setScrollable:@
setScrollable :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setScrollable nsCell value =
  sendMessage nsCell setScrollableSelector value

-- | @- highlighted@
highlighted :: IsNSCell nsCell => nsCell -> IO Bool
highlighted nsCell =
  sendMessage nsCell highlightedSelector

-- | @- setHighlighted:@
setHighlighted :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setHighlighted nsCell value =
  sendMessage nsCell setHighlightedSelector value

-- | @- alignment@
alignment :: IsNSCell nsCell => nsCell -> IO NSTextAlignment
alignment nsCell =
  sendMessage nsCell alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsNSCell nsCell => nsCell -> NSTextAlignment -> IO ()
setAlignment nsCell value =
  sendMessage nsCell setAlignmentSelector value

-- | @- wraps@
wraps :: IsNSCell nsCell => nsCell -> IO Bool
wraps nsCell =
  sendMessage nsCell wrapsSelector

-- | @- setWraps:@
setWraps :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setWraps nsCell value =
  sendMessage nsCell setWrapsSelector value

-- | @- font@
font :: IsNSCell nsCell => nsCell -> IO (Id NSFont)
font nsCell =
  sendMessage nsCell fontSelector

-- | @- setFont:@
setFont :: (IsNSCell nsCell, IsNSFont value) => nsCell -> value -> IO ()
setFont nsCell value =
  sendMessage nsCell setFontSelector (toNSFont value)

-- | @- keyEquivalent@
keyEquivalent :: IsNSCell nsCell => nsCell -> IO (Id NSString)
keyEquivalent nsCell =
  sendMessage nsCell keyEquivalentSelector

-- | @- formatter@
formatter :: IsNSCell nsCell => nsCell -> IO (Id NSFormatter)
formatter nsCell =
  sendMessage nsCell formatterSelector

-- | @- setFormatter:@
setFormatter :: (IsNSCell nsCell, IsNSFormatter value) => nsCell -> value -> IO ()
setFormatter nsCell value =
  sendMessage nsCell setFormatterSelector (toNSFormatter value)

-- | @- objectValue@
objectValue :: IsNSCell nsCell => nsCell -> IO RawId
objectValue nsCell =
  sendMessage nsCell objectValueSelector

-- | @- setObjectValue:@
setObjectValue :: IsNSCell nsCell => nsCell -> RawId -> IO ()
setObjectValue nsCell value =
  sendMessage nsCell setObjectValueSelector value

-- | @- hasValidObjectValue@
hasValidObjectValue :: IsNSCell nsCell => nsCell -> IO Bool
hasValidObjectValue nsCell =
  sendMessage nsCell hasValidObjectValueSelector

-- | @- stringValue@
stringValue :: IsNSCell nsCell => nsCell -> IO (Id NSString)
stringValue nsCell =
  sendMessage nsCell stringValueSelector

-- | @- setStringValue:@
setStringValue :: (IsNSCell nsCell, IsNSString value) => nsCell -> value -> IO ()
setStringValue nsCell value =
  sendMessage nsCell setStringValueSelector (toNSString value)

-- | @- intValue@
intValue :: IsNSCell nsCell => nsCell -> IO CInt
intValue nsCell =
  sendMessage nsCell intValueSelector

-- | @- setIntValue:@
setIntValue :: IsNSCell nsCell => nsCell -> CInt -> IO ()
setIntValue nsCell value =
  sendMessage nsCell setIntValueSelector value

-- | @- floatValue@
floatValue :: IsNSCell nsCell => nsCell -> IO CFloat
floatValue nsCell =
  sendMessage nsCell floatValueSelector

-- | @- setFloatValue:@
setFloatValue :: IsNSCell nsCell => nsCell -> CFloat -> IO ()
setFloatValue nsCell value =
  sendMessage nsCell setFloatValueSelector value

-- | @- doubleValue@
doubleValue :: IsNSCell nsCell => nsCell -> IO CDouble
doubleValue nsCell =
  sendMessage nsCell doubleValueSelector

-- | @- setDoubleValue:@
setDoubleValue :: IsNSCell nsCell => nsCell -> CDouble -> IO ()
setDoubleValue nsCell value =
  sendMessage nsCell setDoubleValueSelector value

-- | @- integerValue@
integerValue :: IsNSCell nsCell => nsCell -> IO CLong
integerValue nsCell =
  sendMessage nsCell integerValueSelector

-- | @- setIntegerValue:@
setIntegerValue :: IsNSCell nsCell => nsCell -> CLong -> IO ()
setIntegerValue nsCell value =
  sendMessage nsCell setIntegerValueSelector value

-- | @- image@
image :: IsNSCell nsCell => nsCell -> IO (Id NSImage)
image nsCell =
  sendMessage nsCell imageSelector

-- | @- setImage:@
setImage :: (IsNSCell nsCell, IsNSImage value) => nsCell -> value -> IO ()
setImage nsCell value =
  sendMessage nsCell setImageSelector (toNSImage value)

-- | @- controlSize@
controlSize :: IsNSCell nsCell => nsCell -> IO NSControlSize
controlSize nsCell =
  sendMessage nsCell controlSizeSelector

-- | @- setControlSize:@
setControlSize :: IsNSCell nsCell => nsCell -> NSControlSize -> IO ()
setControlSize nsCell value =
  sendMessage nsCell setControlSizeSelector value

-- | @- representedObject@
representedObject :: IsNSCell nsCell => nsCell -> IO RawId
representedObject nsCell =
  sendMessage nsCell representedObjectSelector

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSCell nsCell => nsCell -> RawId -> IO ()
setRepresentedObject nsCell value =
  sendMessage nsCell setRepresentedObjectSelector value

-- | @- cellSize@
cellSize :: IsNSCell nsCell => nsCell -> IO NSSize
cellSize nsCell =
  sendMessage nsCell cellSizeSelector

-- | @- mouseDownFlags@
mouseDownFlags :: IsNSCell nsCell => nsCell -> IO CLong
mouseDownFlags nsCell =
  sendMessage nsCell mouseDownFlagsSelector

-- | @- menu@
menu :: IsNSCell nsCell => nsCell -> IO (Id NSMenu)
menu nsCell =
  sendMessage nsCell menuSelector

-- | @- setMenu:@
setMenu :: (IsNSCell nsCell, IsNSMenu value) => nsCell -> value -> IO ()
setMenu nsCell value =
  sendMessage nsCell setMenuSelector (toNSMenu value)

-- | @+ defaultMenu@
defaultMenu :: IO (Id NSMenu)
defaultMenu  =
  do
    cls' <- getRequiredClass "NSCell"
    sendClassMessage cls' defaultMenuSelector

-- | @- sendsActionOnEndEditing@
sendsActionOnEndEditing :: IsNSCell nsCell => nsCell -> IO Bool
sendsActionOnEndEditing nsCell =
  sendMessage nsCell sendsActionOnEndEditingSelector

-- | @- setSendsActionOnEndEditing:@
setSendsActionOnEndEditing :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setSendsActionOnEndEditing nsCell value =
  sendMessage nsCell setSendsActionOnEndEditingSelector value

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSCell nsCell => nsCell -> IO NSWritingDirection
baseWritingDirection nsCell =
  sendMessage nsCell baseWritingDirectionSelector

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSCell nsCell => nsCell -> NSWritingDirection -> IO ()
setBaseWritingDirection nsCell value =
  sendMessage nsCell setBaseWritingDirectionSelector value

-- | @- lineBreakMode@
lineBreakMode :: IsNSCell nsCell => nsCell -> IO NSLineBreakMode
lineBreakMode nsCell =
  sendMessage nsCell lineBreakModeSelector

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSCell nsCell => nsCell -> NSLineBreakMode -> IO ()
setLineBreakMode nsCell value =
  sendMessage nsCell setLineBreakModeSelector value

-- | @- allowsUndo@
allowsUndo :: IsNSCell nsCell => nsCell -> IO Bool
allowsUndo nsCell =
  sendMessage nsCell allowsUndoSelector

-- | @- setAllowsUndo:@
setAllowsUndo :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setAllowsUndo nsCell value =
  sendMessage nsCell setAllowsUndoSelector value

-- | @- truncatesLastVisibleLine@
truncatesLastVisibleLine :: IsNSCell nsCell => nsCell -> IO Bool
truncatesLastVisibleLine nsCell =
  sendMessage nsCell truncatesLastVisibleLineSelector

-- | @- setTruncatesLastVisibleLine:@
setTruncatesLastVisibleLine :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setTruncatesLastVisibleLine nsCell value =
  sendMessage nsCell setTruncatesLastVisibleLineSelector value

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSCell nsCell => nsCell -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsCell =
  sendMessage nsCell userInterfaceLayoutDirectionSelector

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSCell nsCell => nsCell -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsCell value =
  sendMessage nsCell setUserInterfaceLayoutDirectionSelector value

-- | @- usesSingleLineMode@
usesSingleLineMode :: IsNSCell nsCell => nsCell -> IO Bool
usesSingleLineMode nsCell =
  sendMessage nsCell usesSingleLineModeSelector

-- | @- setUsesSingleLineMode:@
setUsesSingleLineMode :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setUsesSingleLineMode nsCell value =
  sendMessage nsCell setUsesSingleLineModeSelector value

-- | @- controlTint@
controlTint :: IsNSCell nsCell => nsCell -> IO NSControlTint
controlTint nsCell =
  sendMessage nsCell controlTintSelector

-- | @- setControlTint:@
setControlTint :: IsNSCell nsCell => nsCell -> NSControlTint -> IO ()
setControlTint nsCell value =
  sendMessage nsCell setControlTintSelector value

-- | @- backgroundStyle@
backgroundStyle :: IsNSCell nsCell => nsCell -> IO NSBackgroundStyle
backgroundStyle nsCell =
  sendMessage nsCell backgroundStyleSelector

-- | @- setBackgroundStyle:@
setBackgroundStyle :: IsNSCell nsCell => nsCell -> NSBackgroundStyle -> IO ()
setBackgroundStyle nsCell value =
  sendMessage nsCell setBackgroundStyleSelector value

-- | @- interiorBackgroundStyle@
interiorBackgroundStyle :: IsNSCell nsCell => nsCell -> IO NSBackgroundStyle
interiorBackgroundStyle nsCell =
  sendMessage nsCell interiorBackgroundStyleSelector

-- | @- allowsMixedState@
allowsMixedState :: IsNSCell nsCell => nsCell -> IO Bool
allowsMixedState nsCell =
  sendMessage nsCell allowsMixedStateSelector

-- | @- setAllowsMixedState:@
setAllowsMixedState :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setAllowsMixedState nsCell value =
  sendMessage nsCell setAllowsMixedStateSelector value

-- | @- nextState@
nextState :: IsNSCell nsCell => nsCell -> IO CLong
nextState nsCell =
  sendMessage nsCell nextStateSelector

-- | @- attributedStringValue@
attributedStringValue :: IsNSCell nsCell => nsCell -> IO (Id NSAttributedString)
attributedStringValue nsCell =
  sendMessage nsCell attributedStringValueSelector

-- | @- setAttributedStringValue:@
setAttributedStringValue :: (IsNSCell nsCell, IsNSAttributedString value) => nsCell -> value -> IO ()
setAttributedStringValue nsCell value =
  sendMessage nsCell setAttributedStringValueSelector (toNSAttributedString value)

-- | @- allowsEditingTextAttributes@
allowsEditingTextAttributes :: IsNSCell nsCell => nsCell -> IO Bool
allowsEditingTextAttributes nsCell =
  sendMessage nsCell allowsEditingTextAttributesSelector

-- | @- setAllowsEditingTextAttributes:@
setAllowsEditingTextAttributes :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setAllowsEditingTextAttributes nsCell value =
  sendMessage nsCell setAllowsEditingTextAttributesSelector value

-- | @- importsGraphics@
importsGraphics :: IsNSCell nsCell => nsCell -> IO Bool
importsGraphics nsCell =
  sendMessage nsCell importsGraphicsSelector

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setImportsGraphics nsCell value =
  sendMessage nsCell setImportsGraphicsSelector value

-- | @- refusesFirstResponder@
refusesFirstResponder :: IsNSCell nsCell => nsCell -> IO Bool
refusesFirstResponder nsCell =
  sendMessage nsCell refusesFirstResponderSelector

-- | @- setRefusesFirstResponder:@
setRefusesFirstResponder :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setRefusesFirstResponder nsCell value =
  sendMessage nsCell setRefusesFirstResponderSelector value

-- | @- acceptsFirstResponder@
acceptsFirstResponder :: IsNSCell nsCell => nsCell -> IO Bool
acceptsFirstResponder nsCell =
  sendMessage nsCell acceptsFirstResponderSelector

-- | @- showsFirstResponder@
showsFirstResponder :: IsNSCell nsCell => nsCell -> IO Bool
showsFirstResponder nsCell =
  sendMessage nsCell showsFirstResponderSelector

-- | @- setShowsFirstResponder:@
setShowsFirstResponder :: IsNSCell nsCell => nsCell -> Bool -> IO ()
setShowsFirstResponder nsCell value =
  sendMessage nsCell setShowsFirstResponderSelector value

-- | @- focusRingType@
focusRingType :: IsNSCell nsCell => nsCell -> IO NSFocusRingType
focusRingType nsCell =
  sendMessage nsCell focusRingTypeSelector

-- | @- setFocusRingType:@
setFocusRingType :: IsNSCell nsCell => nsCell -> NSFocusRingType -> IO ()
setFocusRingType nsCell value =
  sendMessage nsCell setFocusRingTypeSelector value

-- | @+ defaultFocusRingType@
defaultFocusRingType :: IO NSFocusRingType
defaultFocusRingType  =
  do
    cls' <- getRequiredClass "NSCell"
    sendClassMessage cls' defaultFocusRingTypeSelector

-- | @- wantsNotificationForMarkedText@
wantsNotificationForMarkedText :: IsNSCell nsCell => nsCell -> IO Bool
wantsNotificationForMarkedText nsCell =
  sendMessage nsCell wantsNotificationForMarkedTextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCell)
initSelector = mkSelector "init"

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sendActionOn:@
sendActionOnSelector :: Selector '[NSEventMask] CLong
sendActionOnSelector = mkSelector "sendActionOn:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[RawId] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @takeIntValueFrom:@
takeIntValueFromSelector :: Selector '[RawId] ()
takeIntValueFromSelector = mkSelector "takeIntValueFrom:"

-- | @Selector@ for @takeFloatValueFrom:@
takeFloatValueFromSelector :: Selector '[RawId] ()
takeFloatValueFromSelector = mkSelector "takeFloatValueFrom:"

-- | @Selector@ for @takeDoubleValueFrom:@
takeDoubleValueFromSelector :: Selector '[RawId] ()
takeDoubleValueFromSelector = mkSelector "takeDoubleValueFrom:"

-- | @Selector@ for @takeStringValueFrom:@
takeStringValueFromSelector :: Selector '[RawId] ()
takeStringValueFromSelector = mkSelector "takeStringValueFrom:"

-- | @Selector@ for @takeObjectValueFrom:@
takeObjectValueFromSelector :: Selector '[RawId] ()
takeObjectValueFromSelector = mkSelector "takeObjectValueFrom:"

-- | @Selector@ for @takeIntegerValueFrom:@
takeIntegerValueFromSelector :: Selector '[RawId] ()
takeIntegerValueFromSelector = mkSelector "takeIntegerValueFrom:"

-- | @Selector@ for @cellAttribute:@
cellAttributeSelector :: Selector '[NSCellAttribute] CLong
cellAttributeSelector = mkSelector "cellAttribute:"

-- | @Selector@ for @setCellAttribute:to:@
setCellAttribute_toSelector :: Selector '[NSCellAttribute, CLong] ()
setCellAttribute_toSelector = mkSelector "setCellAttribute:to:"

-- | @Selector@ for @imageRectForBounds:@
imageRectForBoundsSelector :: Selector '[NSRect] NSRect
imageRectForBoundsSelector = mkSelector "imageRectForBounds:"

-- | @Selector@ for @titleRectForBounds:@
titleRectForBoundsSelector :: Selector '[NSRect] NSRect
titleRectForBoundsSelector = mkSelector "titleRectForBounds:"

-- | @Selector@ for @drawingRectForBounds:@
drawingRectForBoundsSelector :: Selector '[NSRect] NSRect
drawingRectForBoundsSelector = mkSelector "drawingRectForBounds:"

-- | @Selector@ for @_bulletStringForString:bulletCharacter:@
_bulletStringForString_bulletCharacterSelector :: Selector '[Id NSString, CUShort] (Id NSString)
_bulletStringForString_bulletCharacterSelector = mkSelector "_bulletStringForString:bulletCharacter:"

-- | @Selector@ for @cellSizeForBounds:@
cellSizeForBoundsSelector :: Selector '[NSRect] NSSize
cellSizeForBoundsSelector = mkSelector "cellSizeForBounds:"

-- | @Selector@ for @highlightColorWithFrame:inView:@
highlightColorWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] (Id NSColor)
highlightColorWithFrame_inViewSelector = mkSelector "highlightColorWithFrame:inView:"

-- | @Selector@ for @calcDrawInfo:@
calcDrawInfoSelector :: Selector '[NSRect] ()
calcDrawInfoSelector = mkSelector "calcDrawInfo:"

-- | @Selector@ for @setUpFieldEditorAttributes:@
setUpFieldEditorAttributesSelector :: Selector '[Id NSText] (Id NSText)
setUpFieldEditorAttributesSelector = mkSelector "setUpFieldEditorAttributes:"

-- | @Selector@ for @drawInteriorWithFrame:inView:@
drawInteriorWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawInteriorWithFrame_inViewSelector = mkSelector "drawInteriorWithFrame:inView:"

-- | @Selector@ for @drawWithFrame:inView:@
drawWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawWithFrame_inViewSelector = mkSelector "drawWithFrame:inView:"

-- | @Selector@ for @highlight:withFrame:inView:@
highlight_withFrame_inViewSelector :: Selector '[Bool, NSRect, Id NSView] ()
highlight_withFrame_inViewSelector = mkSelector "highlight:withFrame:inView:"

-- | @Selector@ for @getPeriodicDelay:interval:@
getPeriodicDelay_intervalSelector :: Selector '[Ptr CFloat, Ptr CFloat] ()
getPeriodicDelay_intervalSelector = mkSelector "getPeriodicDelay:interval:"

-- | @Selector@ for @startTrackingAt:inView:@
startTrackingAt_inViewSelector :: Selector '[NSPoint, Id NSView] Bool
startTrackingAt_inViewSelector = mkSelector "startTrackingAt:inView:"

-- | @Selector@ for @continueTracking:at:inView:@
continueTracking_at_inViewSelector :: Selector '[NSPoint, NSPoint, Id NSView] Bool
continueTracking_at_inViewSelector = mkSelector "continueTracking:at:inView:"

-- | @Selector@ for @stopTracking:at:inView:mouseIsUp:@
stopTracking_at_inView_mouseIsUpSelector :: Selector '[NSPoint, NSPoint, Id NSView, Bool] ()
stopTracking_at_inView_mouseIsUpSelector = mkSelector "stopTracking:at:inView:mouseIsUp:"

-- | @Selector@ for @trackMouse:inRect:ofView:untilMouseUp:@
trackMouse_inRect_ofView_untilMouseUpSelector :: Selector '[Id NSEvent, NSRect, Id NSView, Bool] Bool
trackMouse_inRect_ofView_untilMouseUpSelector = mkSelector "trackMouse:inRect:ofView:untilMouseUp:"

-- | @Selector@ for @editWithFrame:inView:editor:delegate:event:@
editWithFrame_inView_editor_delegate_eventSelector :: Selector '[NSRect, Id NSView, Id NSText, RawId, Id NSEvent] ()
editWithFrame_inView_editor_delegate_eventSelector = mkSelector "editWithFrame:inView:editor:delegate:event:"

-- | @Selector@ for @selectWithFrame:inView:editor:delegate:start:length:@
selectWithFrame_inView_editor_delegate_start_lengthSelector :: Selector '[NSRect, Id NSView, Id NSText, RawId, CLong, CLong] ()
selectWithFrame_inView_editor_delegate_start_lengthSelector = mkSelector "selectWithFrame:inView:editor:delegate:start:length:"

-- | @Selector@ for @endEditing:@
endEditingSelector :: Selector '[Id NSText] ()
endEditingSelector = mkSelector "endEditing:"

-- | @Selector@ for @resetCursorRect:inView:@
resetCursorRect_inViewSelector :: Selector '[NSRect, Id NSView] ()
resetCursorRect_inViewSelector = mkSelector "resetCursorRect:inView:"

-- | @Selector@ for @menuForEvent:inRect:ofView:@
menuForEvent_inRect_ofViewSelector :: Selector '[Id NSEvent, NSRect, Id NSView] (Id NSMenu)
menuForEvent_inRect_ofViewSelector = mkSelector "menuForEvent:inRect:ofView:"

-- | @Selector@ for @fieldEditorForView:@
fieldEditorForViewSelector :: Selector '[Id NSView] (Id NSTextView)
fieldEditorForViewSelector = mkSelector "fieldEditorForView:"

-- | @Selector@ for @draggingImageComponentsWithFrame:inView:@
draggingImageComponentsWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] (Id NSArray)
draggingImageComponentsWithFrame_inViewSelector = mkSelector "draggingImageComponentsWithFrame:inView:"

-- | @Selector@ for @entryType@
entryTypeSelector :: Selector '[] CLong
entryTypeSelector = mkSelector "entryType"

-- | @Selector@ for @setEntryType:@
setEntryTypeSelector :: Selector '[CLong] ()
setEntryTypeSelector = mkSelector "setEntryType:"

-- | @Selector@ for @isEntryAcceptable:@
isEntryAcceptableSelector :: Selector '[Id NSString] Bool
isEntryAcceptableSelector = mkSelector "isEntryAcceptable:"

-- | @Selector@ for @setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_rightSelector :: Selector '[Bool, CULong, CULong] ()
setFloatingPointFormat_left_rightSelector = mkSelector "setFloatingPointFormat:left:right:"

-- | @Selector@ for @setMnemonicLocation:@
setMnemonicLocationSelector :: Selector '[CULong] ()
setMnemonicLocationSelector = mkSelector "setMnemonicLocation:"

-- | @Selector@ for @mnemonicLocation@
mnemonicLocationSelector :: Selector '[] CULong
mnemonicLocationSelector = mkSelector "mnemonicLocation"

-- | @Selector@ for @mnemonic@
mnemonicSelector :: Selector '[] (Id NSString)
mnemonicSelector = mkSelector "mnemonic"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @expansionFrameWithFrame:inView:@
expansionFrameWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] NSRect
expansionFrameWithFrame_inViewSelector = mkSelector "expansionFrameWithFrame:inView:"

-- | @Selector@ for @drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawWithExpansionFrame_inViewSelector = mkSelector "drawWithExpansionFrame:inView:"

-- | @Selector@ for @hitTestForEvent:inRect:ofView:@
hitTestForEvent_inRect_ofViewSelector :: Selector '[Id NSEvent, NSRect, Id NSView] NSCellHitResult
hitTestForEvent_inRect_ofViewSelector = mkSelector "hitTestForEvent:inRect:ofView:"

-- | @Selector@ for @setNextState@
setNextStateSelector :: Selector '[] ()
setNextStateSelector = mkSelector "setNextState"

-- | @Selector@ for @performClick:@
performClickSelector :: Selector '[RawId] ()
performClickSelector = mkSelector "performClick:"

-- | @Selector@ for @drawFocusRingMaskWithFrame:inView:@
drawFocusRingMaskWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawFocusRingMaskWithFrame_inViewSelector = mkSelector "drawFocusRingMaskWithFrame:inView:"

-- | @Selector@ for @focusRingMaskBoundsForFrame:inView:@
focusRingMaskBoundsForFrame_inViewSelector :: Selector '[NSRect, Id NSView] NSRect
focusRingMaskBoundsForFrame_inViewSelector = mkSelector "focusRingMaskBoundsForFrame:inView:"

-- | @Selector@ for @prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUpSelector :: Selector '[] Bool
prefersTrackingUntilMouseUpSelector = mkSelector "prefersTrackingUntilMouseUp"

-- | @Selector@ for @controlView@
controlViewSelector :: Selector '[] (Id NSView)
controlViewSelector = mkSelector "controlView"

-- | @Selector@ for @setControlView:@
setControlViewSelector :: Selector '[Id NSView] ()
setControlViewSelector = mkSelector "setControlView:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSCellType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[NSCellType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CLong
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[CLong] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CLong
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[CLong] ()
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector '[] Bool
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector '[Bool] ()
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector '[] Bool
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector '[Bool] ()
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector '[] Bool
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector '[Bool] ()
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector '[] Bool
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector '[Bool] ()
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @scrollable@
scrollableSelector :: Selector '[] Bool
scrollableSelector = mkSelector "scrollable"

-- | @Selector@ for @setScrollable:@
setScrollableSelector :: Selector '[Bool] ()
setScrollableSelector = mkSelector "setScrollable:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector '[] Bool
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector '[Bool] ()
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSTextAlignment] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @wraps@
wrapsSelector :: Selector '[] Bool
wrapsSelector = mkSelector "wraps"

-- | @Selector@ for @setWraps:@
setWrapsSelector :: Selector '[Bool] ()
setWrapsSelector = mkSelector "setWraps:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @keyEquivalent@
keyEquivalentSelector :: Selector '[] (Id NSString)
keyEquivalentSelector = mkSelector "keyEquivalent"

-- | @Selector@ for @formatter@
formatterSelector :: Selector '[] (Id NSFormatter)
formatterSelector = mkSelector "formatter"

-- | @Selector@ for @setFormatter:@
setFormatterSelector :: Selector '[Id NSFormatter] ()
setFormatterSelector = mkSelector "setFormatter:"

-- | @Selector@ for @objectValue@
objectValueSelector :: Selector '[] RawId
objectValueSelector = mkSelector "objectValue"

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector '[RawId] ()
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @hasValidObjectValue@
hasValidObjectValueSelector :: Selector '[] Bool
hasValidObjectValueSelector = mkSelector "hasValidObjectValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @intValue@
intValueSelector :: Selector '[] CInt
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @setIntValue:@
setIntValueSelector :: Selector '[CInt] ()
setIntValueSelector = mkSelector "setIntValue:"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector '[] CFloat
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector '[CFloat] ()
setFloatValueSelector = mkSelector "setFloatValue:"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @setDoubleValue:@
setDoubleValueSelector :: Selector '[CDouble] ()
setDoubleValueSelector = mkSelector "setDoubleValue:"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector '[] CLong
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @setIntegerValue:@
setIntegerValueSelector :: Selector '[CLong] ()
setIntegerValueSelector = mkSelector "setIntegerValue:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector '[] NSControlSize
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector '[NSControlSize] ()
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector '[] RawId
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector '[RawId] ()
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @cellSize@
cellSizeSelector :: Selector '[] NSSize
cellSizeSelector = mkSelector "cellSize"

-- | @Selector@ for @mouseDownFlags@
mouseDownFlagsSelector :: Selector '[] CLong
mouseDownFlagsSelector = mkSelector "mouseDownFlags"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @defaultMenu@
defaultMenuSelector :: Selector '[] (Id NSMenu)
defaultMenuSelector = mkSelector "defaultMenu"

-- | @Selector@ for @sendsActionOnEndEditing@
sendsActionOnEndEditingSelector :: Selector '[] Bool
sendsActionOnEndEditingSelector = mkSelector "sendsActionOnEndEditing"

-- | @Selector@ for @setSendsActionOnEndEditing:@
setSendsActionOnEndEditingSelector :: Selector '[Bool] ()
setSendsActionOnEndEditingSelector = mkSelector "setSendsActionOnEndEditing:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector '[] NSWritingDirection
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector '[] NSLineBreakMode
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector '[NSLineBreakMode] ()
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @allowsUndo@
allowsUndoSelector :: Selector '[] Bool
allowsUndoSelector = mkSelector "allowsUndo"

-- | @Selector@ for @setAllowsUndo:@
setAllowsUndoSelector :: Selector '[Bool] ()
setAllowsUndoSelector = mkSelector "setAllowsUndo:"

-- | @Selector@ for @truncatesLastVisibleLine@
truncatesLastVisibleLineSelector :: Selector '[] Bool
truncatesLastVisibleLineSelector = mkSelector "truncatesLastVisibleLine"

-- | @Selector@ for @setTruncatesLastVisibleLine:@
setTruncatesLastVisibleLineSelector :: Selector '[Bool] ()
setTruncatesLastVisibleLineSelector = mkSelector "setTruncatesLastVisibleLine:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector '[NSUserInterfaceLayoutDirection] ()
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @usesSingleLineMode@
usesSingleLineModeSelector :: Selector '[] Bool
usesSingleLineModeSelector = mkSelector "usesSingleLineMode"

-- | @Selector@ for @setUsesSingleLineMode:@
setUsesSingleLineModeSelector :: Selector '[Bool] ()
setUsesSingleLineModeSelector = mkSelector "setUsesSingleLineMode:"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector '[] NSControlTint
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector '[NSControlTint] ()
setControlTintSelector = mkSelector "setControlTint:"

-- | @Selector@ for @backgroundStyle@
backgroundStyleSelector :: Selector '[] NSBackgroundStyle
backgroundStyleSelector = mkSelector "backgroundStyle"

-- | @Selector@ for @setBackgroundStyle:@
setBackgroundStyleSelector :: Selector '[NSBackgroundStyle] ()
setBackgroundStyleSelector = mkSelector "setBackgroundStyle:"

-- | @Selector@ for @interiorBackgroundStyle@
interiorBackgroundStyleSelector :: Selector '[] NSBackgroundStyle
interiorBackgroundStyleSelector = mkSelector "interiorBackgroundStyle"

-- | @Selector@ for @allowsMixedState@
allowsMixedStateSelector :: Selector '[] Bool
allowsMixedStateSelector = mkSelector "allowsMixedState"

-- | @Selector@ for @setAllowsMixedState:@
setAllowsMixedStateSelector :: Selector '[Bool] ()
setAllowsMixedStateSelector = mkSelector "setAllowsMixedState:"

-- | @Selector@ for @nextState@
nextStateSelector :: Selector '[] CLong
nextStateSelector = mkSelector "nextState"

-- | @Selector@ for @attributedStringValue@
attributedStringValueSelector :: Selector '[] (Id NSAttributedString)
attributedStringValueSelector = mkSelector "attributedStringValue"

-- | @Selector@ for @setAttributedStringValue:@
setAttributedStringValueSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringValueSelector = mkSelector "setAttributedStringValue:"

-- | @Selector@ for @allowsEditingTextAttributes@
allowsEditingTextAttributesSelector :: Selector '[] Bool
allowsEditingTextAttributesSelector = mkSelector "allowsEditingTextAttributes"

-- | @Selector@ for @setAllowsEditingTextAttributes:@
setAllowsEditingTextAttributesSelector :: Selector '[Bool] ()
setAllowsEditingTextAttributesSelector = mkSelector "setAllowsEditingTextAttributes:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector '[] Bool
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector '[Bool] ()
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @refusesFirstResponder@
refusesFirstResponderSelector :: Selector '[] Bool
refusesFirstResponderSelector = mkSelector "refusesFirstResponder"

-- | @Selector@ for @setRefusesFirstResponder:@
setRefusesFirstResponderSelector :: Selector '[Bool] ()
setRefusesFirstResponderSelector = mkSelector "setRefusesFirstResponder:"

-- | @Selector@ for @acceptsFirstResponder@
acceptsFirstResponderSelector :: Selector '[] Bool
acceptsFirstResponderSelector = mkSelector "acceptsFirstResponder"

-- | @Selector@ for @showsFirstResponder@
showsFirstResponderSelector :: Selector '[] Bool
showsFirstResponderSelector = mkSelector "showsFirstResponder"

-- | @Selector@ for @setShowsFirstResponder:@
setShowsFirstResponderSelector :: Selector '[Bool] ()
setShowsFirstResponderSelector = mkSelector "setShowsFirstResponder:"

-- | @Selector@ for @focusRingType@
focusRingTypeSelector :: Selector '[] NSFocusRingType
focusRingTypeSelector = mkSelector "focusRingType"

-- | @Selector@ for @setFocusRingType:@
setFocusRingTypeSelector :: Selector '[NSFocusRingType] ()
setFocusRingTypeSelector = mkSelector "setFocusRingType:"

-- | @Selector@ for @defaultFocusRingType@
defaultFocusRingTypeSelector :: Selector '[] NSFocusRingType
defaultFocusRingTypeSelector = mkSelector "defaultFocusRingType"

-- | @Selector@ for @wantsNotificationForMarkedText@
wantsNotificationForMarkedTextSelector :: Selector '[] Bool
wantsNotificationForMarkedTextSelector = mkSelector "wantsNotificationForMarkedText"

