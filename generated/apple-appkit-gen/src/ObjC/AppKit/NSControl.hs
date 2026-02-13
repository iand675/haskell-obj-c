{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSControl@.
module ObjC.AppKit.NSControl
  ( NSControl
  , IsNSControl(..)
  , initWithFrame
  , initWithCoder
  , sizeThatFits
  , sizeToFit
  , sendActionOn
  , sendAction_to
  , takeIntValueFrom
  , takeFloatValueFrom
  , takeDoubleValueFrom
  , takeStringValueFrom
  , takeObjectValueFrom
  , takeIntegerValueFrom
  , performClick
  , expansionFrameWithFrame
  , drawWithExpansionFrame_inView
  , invalidateIntrinsicContentSizeForCell
  , setFloatingPointFormat_left_right
  , selectedCell
  , selectedTag
  , setNeedsDisplay
  , calcSize
  , updateCell
  , updateCellInside
  , drawCellInside
  , drawCell
  , selectCell
  , currentEditor
  , abortEditing
  , validateEditing
  , editWithFrame_editor_delegate_event
  , selectWithFrame_editor_delegate_start_length
  , endEditing
  , target
  , setTarget
  , action
  , setAction
  , tag
  , setTag
  , ignoresMultiClick
  , setIgnoresMultiClick
  , continuous
  , setContinuous
  , enabled
  , setEnabled
  , refusesFirstResponder
  , setRefusesFirstResponder
  , highlighted
  , setHighlighted
  , controlSize
  , setControlSize
  , formatter
  , setFormatter
  , objectValue
  , setObjectValue
  , stringValue
  , setStringValue
  , attributedStringValue
  , setAttributedStringValue
  , intValue
  , setIntValue
  , integerValue
  , setIntegerValue
  , floatValue
  , setFloatValue
  , doubleValue
  , setDoubleValue
  , font
  , setFont
  , usesSingleLineMode
  , setUsesSingleLineMode
  , lineBreakMode
  , setLineBreakMode
  , alignment
  , setAlignment
  , baseWritingDirection
  , setBaseWritingDirection
  , allowsExpansionToolTips
  , setAllowsExpansionToolTips
  , cellClass
  , setCellClass
  , cell
  , setCell
  , abortEditingSelector
  , actionSelector
  , alignmentSelector
  , allowsExpansionToolTipsSelector
  , attributedStringValueSelector
  , baseWritingDirectionSelector
  , calcSizeSelector
  , cellClassSelector
  , cellSelector
  , continuousSelector
  , controlSizeSelector
  , currentEditorSelector
  , doubleValueSelector
  , drawCellInsideSelector
  , drawCellSelector
  , drawWithExpansionFrame_inViewSelector
  , editWithFrame_editor_delegate_eventSelector
  , enabledSelector
  , endEditingSelector
  , expansionFrameWithFrameSelector
  , floatValueSelector
  , fontSelector
  , formatterSelector
  , highlightedSelector
  , ignoresMultiClickSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , intValueSelector
  , integerValueSelector
  , invalidateIntrinsicContentSizeForCellSelector
  , lineBreakModeSelector
  , objectValueSelector
  , performClickSelector
  , refusesFirstResponderSelector
  , selectCellSelector
  , selectWithFrame_editor_delegate_start_lengthSelector
  , selectedCellSelector
  , selectedTagSelector
  , sendActionOnSelector
  , sendAction_toSelector
  , setActionSelector
  , setAlignmentSelector
  , setAllowsExpansionToolTipsSelector
  , setAttributedStringValueSelector
  , setBaseWritingDirectionSelector
  , setCellClassSelector
  , setCellSelector
  , setContinuousSelector
  , setControlSizeSelector
  , setDoubleValueSelector
  , setEnabledSelector
  , setFloatValueSelector
  , setFloatingPointFormat_left_rightSelector
  , setFontSelector
  , setFormatterSelector
  , setHighlightedSelector
  , setIgnoresMultiClickSelector
  , setIntValueSelector
  , setIntegerValueSelector
  , setLineBreakModeSelector
  , setNeedsDisplaySelector
  , setObjectValueSelector
  , setRefusesFirstResponderSelector
  , setStringValueSelector
  , setTagSelector
  , setTargetSelector
  , setUsesSingleLineModeSelector
  , sizeThatFitsSelector
  , sizeToFitSelector
  , stringValueSelector
  , tagSelector
  , takeDoubleValueFromSelector
  , takeFloatValueFromSelector
  , takeIntValueFromSelector
  , takeIntegerValueFromSelector
  , takeObjectValueFromSelector
  , takeStringValueFromSelector
  , targetSelector
  , updateCellInsideSelector
  , updateCellSelector
  , usesSingleLineModeSelector
  , validateEditingSelector

  -- * Enum types
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge
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
import ObjC.Foundation.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSControl nsControl => nsControl -> NSRect -> IO (Id NSControl)
initWithFrame nsControl frameRect =
  sendOwnedMessage nsControl initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSControl nsControl, IsNSCoder coder) => nsControl -> coder -> IO (Id NSControl)
initWithCoder nsControl coder =
  sendOwnedMessage nsControl initWithCoderSelector (toNSCoder coder)

-- | @- sizeThatFits:@
sizeThatFits :: IsNSControl nsControl => nsControl -> NSSize -> IO NSSize
sizeThatFits nsControl size =
  sendMessage nsControl sizeThatFitsSelector size

-- | @- sizeToFit@
sizeToFit :: IsNSControl nsControl => nsControl -> IO ()
sizeToFit nsControl =
  sendMessage nsControl sizeToFitSelector

-- | @- sendActionOn:@
sendActionOn :: IsNSControl nsControl => nsControl -> NSEventMask -> IO CLong
sendActionOn nsControl mask =
  sendMessage nsControl sendActionOnSelector mask

-- | @- sendAction:to:@
sendAction_to :: IsNSControl nsControl => nsControl -> Sel -> RawId -> IO Bool
sendAction_to nsControl action target =
  sendMessage nsControl sendAction_toSelector action target

-- | @- takeIntValueFrom:@
takeIntValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeIntValueFrom nsControl sender =
  sendMessage nsControl takeIntValueFromSelector sender

-- | @- takeFloatValueFrom:@
takeFloatValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeFloatValueFrom nsControl sender =
  sendMessage nsControl takeFloatValueFromSelector sender

-- | @- takeDoubleValueFrom:@
takeDoubleValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeDoubleValueFrom nsControl sender =
  sendMessage nsControl takeDoubleValueFromSelector sender

-- | @- takeStringValueFrom:@
takeStringValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeStringValueFrom nsControl sender =
  sendMessage nsControl takeStringValueFromSelector sender

-- | @- takeObjectValueFrom:@
takeObjectValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeObjectValueFrom nsControl sender =
  sendMessage nsControl takeObjectValueFromSelector sender

-- | @- takeIntegerValueFrom:@
takeIntegerValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeIntegerValueFrom nsControl sender =
  sendMessage nsControl takeIntegerValueFromSelector sender

-- | @- performClick:@
performClick :: IsNSControl nsControl => nsControl -> RawId -> IO ()
performClick nsControl sender =
  sendMessage nsControl performClickSelector sender

-- | @- expansionFrameWithFrame:@
expansionFrameWithFrame :: IsNSControl nsControl => nsControl -> NSRect -> IO NSRect
expansionFrameWithFrame nsControl contentFrame =
  sendMessage nsControl expansionFrameWithFrameSelector contentFrame

-- | @- drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inView :: (IsNSControl nsControl, IsNSView view) => nsControl -> NSRect -> view -> IO ()
drawWithExpansionFrame_inView nsControl contentFrame view =
  sendMessage nsControl drawWithExpansionFrame_inViewSelector contentFrame (toNSView view)

-- | @- invalidateIntrinsicContentSizeForCell:@
invalidateIntrinsicContentSizeForCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
invalidateIntrinsicContentSizeForCell nsControl cell =
  sendMessage nsControl invalidateIntrinsicContentSizeForCellSelector (toNSCell cell)

-- | @- setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_right :: IsNSControl nsControl => nsControl -> Bool -> CULong -> CULong -> IO ()
setFloatingPointFormat_left_right nsControl autoRange leftDigits rightDigits =
  sendMessage nsControl setFloatingPointFormat_left_rightSelector autoRange leftDigits rightDigits

-- | @- selectedCell@
selectedCell :: IsNSControl nsControl => nsControl -> IO (Id NSCell)
selectedCell nsControl =
  sendMessage nsControl selectedCellSelector

-- | @- selectedTag@
selectedTag :: IsNSControl nsControl => nsControl -> IO CLong
selectedTag nsControl =
  sendMessage nsControl selectedTagSelector

-- | @- setNeedsDisplay@
setNeedsDisplay :: IsNSControl nsControl => nsControl -> IO ()
setNeedsDisplay nsControl =
  sendMessage nsControl setNeedsDisplaySelector

-- | @- calcSize@
calcSize :: IsNSControl nsControl => nsControl -> IO ()
calcSize nsControl =
  sendMessage nsControl calcSizeSelector

-- | @- updateCell:@
updateCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
updateCell nsControl cell =
  sendMessage nsControl updateCellSelector (toNSCell cell)

-- | @- updateCellInside:@
updateCellInside :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
updateCellInside nsControl cell =
  sendMessage nsControl updateCellInsideSelector (toNSCell cell)

-- | @- drawCellInside:@
drawCellInside :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
drawCellInside nsControl cell =
  sendMessage nsControl drawCellInsideSelector (toNSCell cell)

-- | @- drawCell:@
drawCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
drawCell nsControl cell =
  sendMessage nsControl drawCellSelector (toNSCell cell)

-- | @- selectCell:@
selectCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
selectCell nsControl cell =
  sendMessage nsControl selectCellSelector (toNSCell cell)

-- | @- currentEditor@
currentEditor :: IsNSControl nsControl => nsControl -> IO (Id NSText)
currentEditor nsControl =
  sendMessage nsControl currentEditorSelector

-- | @- abortEditing@
abortEditing :: IsNSControl nsControl => nsControl -> IO Bool
abortEditing nsControl =
  sendMessage nsControl abortEditingSelector

-- | @- validateEditing@
validateEditing :: IsNSControl nsControl => nsControl -> IO ()
validateEditing nsControl =
  sendMessage nsControl validateEditingSelector

-- | @- editWithFrame:editor:delegate:event:@
editWithFrame_editor_delegate_event :: (IsNSControl nsControl, IsNSText textObj, IsNSEvent event) => nsControl -> NSRect -> textObj -> RawId -> event -> IO ()
editWithFrame_editor_delegate_event nsControl rect textObj delegate event =
  sendMessage nsControl editWithFrame_editor_delegate_eventSelector rect (toNSText textObj) delegate (toNSEvent event)

-- | @- selectWithFrame:editor:delegate:start:length:@
selectWithFrame_editor_delegate_start_length :: (IsNSControl nsControl, IsNSText textObj) => nsControl -> NSRect -> textObj -> RawId -> CLong -> CLong -> IO ()
selectWithFrame_editor_delegate_start_length nsControl rect textObj delegate selStart selLength =
  sendMessage nsControl selectWithFrame_editor_delegate_start_lengthSelector rect (toNSText textObj) delegate selStart selLength

-- | @- endEditing:@
endEditing :: (IsNSControl nsControl, IsNSText textObj) => nsControl -> textObj -> IO ()
endEditing nsControl textObj =
  sendMessage nsControl endEditingSelector (toNSText textObj)

-- | @- target@
target :: IsNSControl nsControl => nsControl -> IO RawId
target nsControl =
  sendMessage nsControl targetSelector

-- | @- setTarget:@
setTarget :: IsNSControl nsControl => nsControl -> RawId -> IO ()
setTarget nsControl value =
  sendMessage nsControl setTargetSelector value

-- | @- action@
action :: IsNSControl nsControl => nsControl -> IO Sel
action nsControl =
  sendMessage nsControl actionSelector

-- | @- setAction:@
setAction :: IsNSControl nsControl => nsControl -> Sel -> IO ()
setAction nsControl value =
  sendMessage nsControl setActionSelector value

-- | @- tag@
tag :: IsNSControl nsControl => nsControl -> IO CLong
tag nsControl =
  sendMessage nsControl tagSelector

-- | @- setTag:@
setTag :: IsNSControl nsControl => nsControl -> CLong -> IO ()
setTag nsControl value =
  sendMessage nsControl setTagSelector value

-- | @- ignoresMultiClick@
ignoresMultiClick :: IsNSControl nsControl => nsControl -> IO Bool
ignoresMultiClick nsControl =
  sendMessage nsControl ignoresMultiClickSelector

-- | @- setIgnoresMultiClick:@
setIgnoresMultiClick :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setIgnoresMultiClick nsControl value =
  sendMessage nsControl setIgnoresMultiClickSelector value

-- | @- continuous@
continuous :: IsNSControl nsControl => nsControl -> IO Bool
continuous nsControl =
  sendMessage nsControl continuousSelector

-- | @- setContinuous:@
setContinuous :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setContinuous nsControl value =
  sendMessage nsControl setContinuousSelector value

-- | @- enabled@
enabled :: IsNSControl nsControl => nsControl -> IO Bool
enabled nsControl =
  sendMessage nsControl enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setEnabled nsControl value =
  sendMessage nsControl setEnabledSelector value

-- | @- refusesFirstResponder@
refusesFirstResponder :: IsNSControl nsControl => nsControl -> IO Bool
refusesFirstResponder nsControl =
  sendMessage nsControl refusesFirstResponderSelector

-- | @- setRefusesFirstResponder:@
setRefusesFirstResponder :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setRefusesFirstResponder nsControl value =
  sendMessage nsControl setRefusesFirstResponderSelector value

-- | @- highlighted@
highlighted :: IsNSControl nsControl => nsControl -> IO Bool
highlighted nsControl =
  sendMessage nsControl highlightedSelector

-- | @- setHighlighted:@
setHighlighted :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setHighlighted nsControl value =
  sendMessage nsControl setHighlightedSelector value

-- | @- controlSize@
controlSize :: IsNSControl nsControl => nsControl -> IO NSControlSize
controlSize nsControl =
  sendMessage nsControl controlSizeSelector

-- | @- setControlSize:@
setControlSize :: IsNSControl nsControl => nsControl -> NSControlSize -> IO ()
setControlSize nsControl value =
  sendMessage nsControl setControlSizeSelector value

-- | @- formatter@
formatter :: IsNSControl nsControl => nsControl -> IO (Id NSFormatter)
formatter nsControl =
  sendMessage nsControl formatterSelector

-- | @- setFormatter:@
setFormatter :: (IsNSControl nsControl, IsNSFormatter value) => nsControl -> value -> IO ()
setFormatter nsControl value =
  sendMessage nsControl setFormatterSelector (toNSFormatter value)

-- | @- objectValue@
objectValue :: IsNSControl nsControl => nsControl -> IO RawId
objectValue nsControl =
  sendMessage nsControl objectValueSelector

-- | @- setObjectValue:@
setObjectValue :: IsNSControl nsControl => nsControl -> RawId -> IO ()
setObjectValue nsControl value =
  sendMessage nsControl setObjectValueSelector value

-- | @- stringValue@
stringValue :: IsNSControl nsControl => nsControl -> IO (Id NSString)
stringValue nsControl =
  sendMessage nsControl stringValueSelector

-- | @- setStringValue:@
setStringValue :: (IsNSControl nsControl, IsNSString value) => nsControl -> value -> IO ()
setStringValue nsControl value =
  sendMessage nsControl setStringValueSelector (toNSString value)

-- | @- attributedStringValue@
attributedStringValue :: IsNSControl nsControl => nsControl -> IO (Id NSAttributedString)
attributedStringValue nsControl =
  sendMessage nsControl attributedStringValueSelector

-- | @- setAttributedStringValue:@
setAttributedStringValue :: (IsNSControl nsControl, IsNSAttributedString value) => nsControl -> value -> IO ()
setAttributedStringValue nsControl value =
  sendMessage nsControl setAttributedStringValueSelector (toNSAttributedString value)

-- | @- intValue@
intValue :: IsNSControl nsControl => nsControl -> IO CInt
intValue nsControl =
  sendMessage nsControl intValueSelector

-- | @- setIntValue:@
setIntValue :: IsNSControl nsControl => nsControl -> CInt -> IO ()
setIntValue nsControl value =
  sendMessage nsControl setIntValueSelector value

-- | @- integerValue@
integerValue :: IsNSControl nsControl => nsControl -> IO CLong
integerValue nsControl =
  sendMessage nsControl integerValueSelector

-- | @- setIntegerValue:@
setIntegerValue :: IsNSControl nsControl => nsControl -> CLong -> IO ()
setIntegerValue nsControl value =
  sendMessage nsControl setIntegerValueSelector value

-- | @- floatValue@
floatValue :: IsNSControl nsControl => nsControl -> IO CFloat
floatValue nsControl =
  sendMessage nsControl floatValueSelector

-- | @- setFloatValue:@
setFloatValue :: IsNSControl nsControl => nsControl -> CFloat -> IO ()
setFloatValue nsControl value =
  sendMessage nsControl setFloatValueSelector value

-- | @- doubleValue@
doubleValue :: IsNSControl nsControl => nsControl -> IO CDouble
doubleValue nsControl =
  sendMessage nsControl doubleValueSelector

-- | @- setDoubleValue:@
setDoubleValue :: IsNSControl nsControl => nsControl -> CDouble -> IO ()
setDoubleValue nsControl value =
  sendMessage nsControl setDoubleValueSelector value

-- | @- font@
font :: IsNSControl nsControl => nsControl -> IO (Id NSFont)
font nsControl =
  sendMessage nsControl fontSelector

-- | @- setFont:@
setFont :: (IsNSControl nsControl, IsNSFont value) => nsControl -> value -> IO ()
setFont nsControl value =
  sendMessage nsControl setFontSelector (toNSFont value)

-- | @- usesSingleLineMode@
usesSingleLineMode :: IsNSControl nsControl => nsControl -> IO Bool
usesSingleLineMode nsControl =
  sendMessage nsControl usesSingleLineModeSelector

-- | @- setUsesSingleLineMode:@
setUsesSingleLineMode :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setUsesSingleLineMode nsControl value =
  sendMessage nsControl setUsesSingleLineModeSelector value

-- | @- lineBreakMode@
lineBreakMode :: IsNSControl nsControl => nsControl -> IO NSLineBreakMode
lineBreakMode nsControl =
  sendMessage nsControl lineBreakModeSelector

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSControl nsControl => nsControl -> NSLineBreakMode -> IO ()
setLineBreakMode nsControl value =
  sendMessage nsControl setLineBreakModeSelector value

-- | @- alignment@
alignment :: IsNSControl nsControl => nsControl -> IO NSTextAlignment
alignment nsControl =
  sendMessage nsControl alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsNSControl nsControl => nsControl -> NSTextAlignment -> IO ()
setAlignment nsControl value =
  sendMessage nsControl setAlignmentSelector value

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSControl nsControl => nsControl -> IO NSWritingDirection
baseWritingDirection nsControl =
  sendMessage nsControl baseWritingDirectionSelector

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSControl nsControl => nsControl -> NSWritingDirection -> IO ()
setBaseWritingDirection nsControl value =
  sendMessage nsControl setBaseWritingDirectionSelector value

-- | @- allowsExpansionToolTips@
allowsExpansionToolTips :: IsNSControl nsControl => nsControl -> IO Bool
allowsExpansionToolTips nsControl =
  sendMessage nsControl allowsExpansionToolTipsSelector

-- | @- setAllowsExpansionToolTips:@
setAllowsExpansionToolTips :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setAllowsExpansionToolTips nsControl value =
  sendMessage nsControl setAllowsExpansionToolTipsSelector value

-- | @+ cellClass@
cellClass :: IO Class
cellClass  =
  do
    cls' <- getRequiredClass "NSControl"
    sendClassMessage cls' cellClassSelector

-- | @+ setCellClass:@
setCellClass :: Class -> IO ()
setCellClass value =
  do
    cls' <- getRequiredClass "NSControl"
    sendClassMessage cls' setCellClassSelector value

-- | @- cell@
cell :: IsNSControl nsControl => nsControl -> IO (Id NSCell)
cell nsControl =
  sendMessage nsControl cellSelector

-- | @- setCell:@
setCell :: (IsNSControl nsControl, IsNSCell value) => nsControl -> value -> IO ()
setCell nsControl value =
  sendMessage nsControl setCellSelector (toNSCell value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSControl)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSControl)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sizeThatFits:@
sizeThatFitsSelector :: Selector '[NSSize] NSSize
sizeThatFitsSelector = mkSelector "sizeThatFits:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @sendActionOn:@
sendActionOnSelector :: Selector '[NSEventMask] CLong
sendActionOnSelector = mkSelector "sendActionOn:"

-- | @Selector@ for @sendAction:to:@
sendAction_toSelector :: Selector '[Sel, RawId] Bool
sendAction_toSelector = mkSelector "sendAction:to:"

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

-- | @Selector@ for @performClick:@
performClickSelector :: Selector '[RawId] ()
performClickSelector = mkSelector "performClick:"

-- | @Selector@ for @expansionFrameWithFrame:@
expansionFrameWithFrameSelector :: Selector '[NSRect] NSRect
expansionFrameWithFrameSelector = mkSelector "expansionFrameWithFrame:"

-- | @Selector@ for @drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawWithExpansionFrame_inViewSelector = mkSelector "drawWithExpansionFrame:inView:"

-- | @Selector@ for @invalidateIntrinsicContentSizeForCell:@
invalidateIntrinsicContentSizeForCellSelector :: Selector '[Id NSCell] ()
invalidateIntrinsicContentSizeForCellSelector = mkSelector "invalidateIntrinsicContentSizeForCell:"

-- | @Selector@ for @setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_rightSelector :: Selector '[Bool, CULong, CULong] ()
setFloatingPointFormat_left_rightSelector = mkSelector "setFloatingPointFormat:left:right:"

-- | @Selector@ for @selectedCell@
selectedCellSelector :: Selector '[] (Id NSCell)
selectedCellSelector = mkSelector "selectedCell"

-- | @Selector@ for @selectedTag@
selectedTagSelector :: Selector '[] CLong
selectedTagSelector = mkSelector "selectedTag"

-- | @Selector@ for @setNeedsDisplay@
setNeedsDisplaySelector :: Selector '[] ()
setNeedsDisplaySelector = mkSelector "setNeedsDisplay"

-- | @Selector@ for @calcSize@
calcSizeSelector :: Selector '[] ()
calcSizeSelector = mkSelector "calcSize"

-- | @Selector@ for @updateCell:@
updateCellSelector :: Selector '[Id NSCell] ()
updateCellSelector = mkSelector "updateCell:"

-- | @Selector@ for @updateCellInside:@
updateCellInsideSelector :: Selector '[Id NSCell] ()
updateCellInsideSelector = mkSelector "updateCellInside:"

-- | @Selector@ for @drawCellInside:@
drawCellInsideSelector :: Selector '[Id NSCell] ()
drawCellInsideSelector = mkSelector "drawCellInside:"

-- | @Selector@ for @drawCell:@
drawCellSelector :: Selector '[Id NSCell] ()
drawCellSelector = mkSelector "drawCell:"

-- | @Selector@ for @selectCell:@
selectCellSelector :: Selector '[Id NSCell] ()
selectCellSelector = mkSelector "selectCell:"

-- | @Selector@ for @currentEditor@
currentEditorSelector :: Selector '[] (Id NSText)
currentEditorSelector = mkSelector "currentEditor"

-- | @Selector@ for @abortEditing@
abortEditingSelector :: Selector '[] Bool
abortEditingSelector = mkSelector "abortEditing"

-- | @Selector@ for @validateEditing@
validateEditingSelector :: Selector '[] ()
validateEditingSelector = mkSelector "validateEditing"

-- | @Selector@ for @editWithFrame:editor:delegate:event:@
editWithFrame_editor_delegate_eventSelector :: Selector '[NSRect, Id NSText, RawId, Id NSEvent] ()
editWithFrame_editor_delegate_eventSelector = mkSelector "editWithFrame:editor:delegate:event:"

-- | @Selector@ for @selectWithFrame:editor:delegate:start:length:@
selectWithFrame_editor_delegate_start_lengthSelector :: Selector '[NSRect, Id NSText, RawId, CLong, CLong] ()
selectWithFrame_editor_delegate_start_lengthSelector = mkSelector "selectWithFrame:editor:delegate:start:length:"

-- | @Selector@ for @endEditing:@
endEditingSelector :: Selector '[Id NSText] ()
endEditingSelector = mkSelector "endEditing:"

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

-- | @Selector@ for @ignoresMultiClick@
ignoresMultiClickSelector :: Selector '[] Bool
ignoresMultiClickSelector = mkSelector "ignoresMultiClick"

-- | @Selector@ for @setIgnoresMultiClick:@
setIgnoresMultiClickSelector :: Selector '[Bool] ()
setIgnoresMultiClickSelector = mkSelector "setIgnoresMultiClick:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector '[] Bool
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector '[Bool] ()
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @refusesFirstResponder@
refusesFirstResponderSelector :: Selector '[] Bool
refusesFirstResponderSelector = mkSelector "refusesFirstResponder"

-- | @Selector@ for @setRefusesFirstResponder:@
setRefusesFirstResponderSelector :: Selector '[Bool] ()
setRefusesFirstResponderSelector = mkSelector "setRefusesFirstResponder:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector '[] Bool
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector '[Bool] ()
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector '[] NSControlSize
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector '[NSControlSize] ()
setControlSizeSelector = mkSelector "setControlSize:"

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

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @attributedStringValue@
attributedStringValueSelector :: Selector '[] (Id NSAttributedString)
attributedStringValueSelector = mkSelector "attributedStringValue"

-- | @Selector@ for @setAttributedStringValue:@
setAttributedStringValueSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringValueSelector = mkSelector "setAttributedStringValue:"

-- | @Selector@ for @intValue@
intValueSelector :: Selector '[] CInt
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @setIntValue:@
setIntValueSelector :: Selector '[CInt] ()
setIntValueSelector = mkSelector "setIntValue:"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector '[] CLong
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @setIntegerValue:@
setIntegerValueSelector :: Selector '[CLong] ()
setIntegerValueSelector = mkSelector "setIntegerValue:"

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

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @usesSingleLineMode@
usesSingleLineModeSelector :: Selector '[] Bool
usesSingleLineModeSelector = mkSelector "usesSingleLineMode"

-- | @Selector@ for @setUsesSingleLineMode:@
setUsesSingleLineModeSelector :: Selector '[Bool] ()
setUsesSingleLineModeSelector = mkSelector "setUsesSingleLineMode:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector '[] NSLineBreakMode
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector '[NSLineBreakMode] ()
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSTextAlignment] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector '[] NSWritingDirection
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @allowsExpansionToolTips@
allowsExpansionToolTipsSelector :: Selector '[] Bool
allowsExpansionToolTipsSelector = mkSelector "allowsExpansionToolTips"

-- | @Selector@ for @setAllowsExpansionToolTips:@
setAllowsExpansionToolTipsSelector :: Selector '[Bool] ()
setAllowsExpansionToolTipsSelector = mkSelector "setAllowsExpansionToolTips:"

-- | @Selector@ for @cellClass@
cellClassSelector :: Selector '[] Class
cellClassSelector = mkSelector "cellClass"

-- | @Selector@ for @setCellClass:@
setCellClassSelector :: Selector '[Class] ()
setCellClassSelector = mkSelector "setCellClass:"

-- | @Selector@ for @cell@
cellSelector :: Selector '[] (Id NSCell)
cellSelector = mkSelector "cell"

-- | @Selector@ for @setCell:@
setCellSelector :: Selector '[Id NSCell] ()
setCellSelector = mkSelector "setCell:"

