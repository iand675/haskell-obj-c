{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameSelector
  , initWithCoderSelector
  , sizeThatFitsSelector
  , sizeToFitSelector
  , sendActionOnSelector
  , sendAction_toSelector
  , takeIntValueFromSelector
  , takeFloatValueFromSelector
  , takeDoubleValueFromSelector
  , takeStringValueFromSelector
  , takeObjectValueFromSelector
  , takeIntegerValueFromSelector
  , performClickSelector
  , expansionFrameWithFrameSelector
  , drawWithExpansionFrame_inViewSelector
  , invalidateIntrinsicContentSizeForCellSelector
  , setFloatingPointFormat_left_rightSelector
  , selectedCellSelector
  , selectedTagSelector
  , setNeedsDisplaySelector
  , calcSizeSelector
  , updateCellSelector
  , updateCellInsideSelector
  , drawCellInsideSelector
  , drawCellSelector
  , selectCellSelector
  , currentEditorSelector
  , abortEditingSelector
  , validateEditingSelector
  , editWithFrame_editor_delegate_eventSelector
  , selectWithFrame_editor_delegate_start_lengthSelector
  , endEditingSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , tagSelector
  , setTagSelector
  , ignoresMultiClickSelector
  , setIgnoresMultiClickSelector
  , continuousSelector
  , setContinuousSelector
  , enabledSelector
  , setEnabledSelector
  , refusesFirstResponderSelector
  , setRefusesFirstResponderSelector
  , highlightedSelector
  , setHighlightedSelector
  , controlSizeSelector
  , setControlSizeSelector
  , formatterSelector
  , setFormatterSelector
  , objectValueSelector
  , setObjectValueSelector
  , stringValueSelector
  , setStringValueSelector
  , attributedStringValueSelector
  , setAttributedStringValueSelector
  , intValueSelector
  , setIntValueSelector
  , integerValueSelector
  , setIntegerValueSelector
  , floatValueSelector
  , setFloatValueSelector
  , doubleValueSelector
  , setDoubleValueSelector
  , fontSelector
  , setFontSelector
  , usesSingleLineModeSelector
  , setUsesSingleLineModeSelector
  , lineBreakModeSelector
  , setLineBreakModeSelector
  , alignmentSelector
  , setAlignmentSelector
  , baseWritingDirectionSelector
  , setBaseWritingDirectionSelector
  , allowsExpansionToolTipsSelector
  , setAllowsExpansionToolTipsSelector
  , cellClassSelector
  , setCellClassSelector
  , cellSelector
  , setCellSelector

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

-- | @- initWithFrame:@
initWithFrame :: IsNSControl nsControl => nsControl -> NSRect -> IO (Id NSControl)
initWithFrame nsControl  frameRect =
  sendMsg nsControl (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSControl nsControl, IsNSCoder coder) => nsControl -> coder -> IO (Id NSControl)
initWithCoder nsControl  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsControl (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- sizeThatFits:@
sizeThatFits :: IsNSControl nsControl => nsControl -> NSSize -> IO NSSize
sizeThatFits nsControl  size =
  sendMsgStret nsControl (mkSelector "sizeThatFits:") retNSSize [argNSSize size]

-- | @- sizeToFit@
sizeToFit :: IsNSControl nsControl => nsControl -> IO ()
sizeToFit nsControl  =
  sendMsg nsControl (mkSelector "sizeToFit") retVoid []

-- | @- sendActionOn:@
sendActionOn :: IsNSControl nsControl => nsControl -> NSEventMask -> IO CLong
sendActionOn nsControl  mask =
  sendMsg nsControl (mkSelector "sendActionOn:") retCLong [argCULong (coerce mask)]

-- | @- sendAction:to:@
sendAction_to :: IsNSControl nsControl => nsControl -> Selector -> RawId -> IO Bool
sendAction_to nsControl  action target =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "sendAction:to:") retCULong [argPtr (unSelector action), argPtr (castPtr (unRawId target) :: Ptr ())]

-- | @- takeIntValueFrom:@
takeIntValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeIntValueFrom nsControl  sender =
  sendMsg nsControl (mkSelector "takeIntValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeFloatValueFrom:@
takeFloatValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeFloatValueFrom nsControl  sender =
  sendMsg nsControl (mkSelector "takeFloatValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeDoubleValueFrom:@
takeDoubleValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeDoubleValueFrom nsControl  sender =
  sendMsg nsControl (mkSelector "takeDoubleValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeStringValueFrom:@
takeStringValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeStringValueFrom nsControl  sender =
  sendMsg nsControl (mkSelector "takeStringValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeObjectValueFrom:@
takeObjectValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeObjectValueFrom nsControl  sender =
  sendMsg nsControl (mkSelector "takeObjectValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeIntegerValueFrom:@
takeIntegerValueFrom :: IsNSControl nsControl => nsControl -> RawId -> IO ()
takeIntegerValueFrom nsControl  sender =
  sendMsg nsControl (mkSelector "takeIntegerValueFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- performClick:@
performClick :: IsNSControl nsControl => nsControl -> RawId -> IO ()
performClick nsControl  sender =
  sendMsg nsControl (mkSelector "performClick:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- expansionFrameWithFrame:@
expansionFrameWithFrame :: IsNSControl nsControl => nsControl -> NSRect -> IO NSRect
expansionFrameWithFrame nsControl  contentFrame =
  sendMsgStret nsControl (mkSelector "expansionFrameWithFrame:") retNSRect [argNSRect contentFrame]

-- | @- drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inView :: (IsNSControl nsControl, IsNSView view) => nsControl -> NSRect -> view -> IO ()
drawWithExpansionFrame_inView nsControl  contentFrame view =
withObjCPtr view $ \raw_view ->
    sendMsg nsControl (mkSelector "drawWithExpansionFrame:inView:") retVoid [argNSRect contentFrame, argPtr (castPtr raw_view :: Ptr ())]

-- | @- invalidateIntrinsicContentSizeForCell:@
invalidateIntrinsicContentSizeForCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
invalidateIntrinsicContentSizeForCell nsControl  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsControl (mkSelector "invalidateIntrinsicContentSizeForCell:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_right :: IsNSControl nsControl => nsControl -> Bool -> CULong -> CULong -> IO ()
setFloatingPointFormat_left_right nsControl  autoRange leftDigits rightDigits =
  sendMsg nsControl (mkSelector "setFloatingPointFormat:left:right:") retVoid [argCULong (if autoRange then 1 else 0), argCULong (fromIntegral leftDigits), argCULong (fromIntegral rightDigits)]

-- | @- selectedCell@
selectedCell :: IsNSControl nsControl => nsControl -> IO (Id NSCell)
selectedCell nsControl  =
  sendMsg nsControl (mkSelector "selectedCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedTag@
selectedTag :: IsNSControl nsControl => nsControl -> IO CLong
selectedTag nsControl  =
  sendMsg nsControl (mkSelector "selectedTag") retCLong []

-- | @- setNeedsDisplay@
setNeedsDisplay :: IsNSControl nsControl => nsControl -> IO ()
setNeedsDisplay nsControl  =
  sendMsg nsControl (mkSelector "setNeedsDisplay") retVoid []

-- | @- calcSize@
calcSize :: IsNSControl nsControl => nsControl -> IO ()
calcSize nsControl  =
  sendMsg nsControl (mkSelector "calcSize") retVoid []

-- | @- updateCell:@
updateCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
updateCell nsControl  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsControl (mkSelector "updateCell:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- updateCellInside:@
updateCellInside :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
updateCellInside nsControl  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsControl (mkSelector "updateCellInside:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- drawCellInside:@
drawCellInside :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
drawCellInside nsControl  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsControl (mkSelector "drawCellInside:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- drawCell:@
drawCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
drawCell nsControl  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsControl (mkSelector "drawCell:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- selectCell:@
selectCell :: (IsNSControl nsControl, IsNSCell cell) => nsControl -> cell -> IO ()
selectCell nsControl  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsControl (mkSelector "selectCell:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- currentEditor@
currentEditor :: IsNSControl nsControl => nsControl -> IO (Id NSText)
currentEditor nsControl  =
  sendMsg nsControl (mkSelector "currentEditor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- abortEditing@
abortEditing :: IsNSControl nsControl => nsControl -> IO Bool
abortEditing nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "abortEditing") retCULong []

-- | @- validateEditing@
validateEditing :: IsNSControl nsControl => nsControl -> IO ()
validateEditing nsControl  =
  sendMsg nsControl (mkSelector "validateEditing") retVoid []

-- | @- editWithFrame:editor:delegate:event:@
editWithFrame_editor_delegate_event :: (IsNSControl nsControl, IsNSText textObj, IsNSEvent event) => nsControl -> NSRect -> textObj -> RawId -> event -> IO ()
editWithFrame_editor_delegate_event nsControl  rect textObj delegate event =
withObjCPtr textObj $ \raw_textObj ->
  withObjCPtr event $ \raw_event ->
      sendMsg nsControl (mkSelector "editWithFrame:editor:delegate:event:") retVoid [argNSRect rect, argPtr (castPtr raw_textObj :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | @- selectWithFrame:editor:delegate:start:length:@
selectWithFrame_editor_delegate_start_length :: (IsNSControl nsControl, IsNSText textObj) => nsControl -> NSRect -> textObj -> RawId -> CLong -> CLong -> IO ()
selectWithFrame_editor_delegate_start_length nsControl  rect textObj delegate selStart selLength =
withObjCPtr textObj $ \raw_textObj ->
    sendMsg nsControl (mkSelector "selectWithFrame:editor:delegate:start:length:") retVoid [argNSRect rect, argPtr (castPtr raw_textObj :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argCLong (fromIntegral selStart), argCLong (fromIntegral selLength)]

-- | @- endEditing:@
endEditing :: (IsNSControl nsControl, IsNSText textObj) => nsControl -> textObj -> IO ()
endEditing nsControl  textObj =
withObjCPtr textObj $ \raw_textObj ->
    sendMsg nsControl (mkSelector "endEditing:") retVoid [argPtr (castPtr raw_textObj :: Ptr ())]

-- | @- target@
target :: IsNSControl nsControl => nsControl -> IO RawId
target nsControl  =
  fmap (RawId . castPtr) $ sendMsg nsControl (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSControl nsControl => nsControl -> RawId -> IO ()
setTarget nsControl  value =
  sendMsg nsControl (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSControl nsControl => nsControl -> IO Selector
action nsControl  =
  fmap (Selector . castPtr) $ sendMsg nsControl (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSControl nsControl => nsControl -> Selector -> IO ()
setAction nsControl  value =
  sendMsg nsControl (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- tag@
tag :: IsNSControl nsControl => nsControl -> IO CLong
tag nsControl  =
  sendMsg nsControl (mkSelector "tag") retCLong []

-- | @- setTag:@
setTag :: IsNSControl nsControl => nsControl -> CLong -> IO ()
setTag nsControl  value =
  sendMsg nsControl (mkSelector "setTag:") retVoid [argCLong (fromIntegral value)]

-- | @- ignoresMultiClick@
ignoresMultiClick :: IsNSControl nsControl => nsControl -> IO Bool
ignoresMultiClick nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "ignoresMultiClick") retCULong []

-- | @- setIgnoresMultiClick:@
setIgnoresMultiClick :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setIgnoresMultiClick nsControl  value =
  sendMsg nsControl (mkSelector "setIgnoresMultiClick:") retVoid [argCULong (if value then 1 else 0)]

-- | @- continuous@
continuous :: IsNSControl nsControl => nsControl -> IO Bool
continuous nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "continuous") retCULong []

-- | @- setContinuous:@
setContinuous :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setContinuous nsControl  value =
  sendMsg nsControl (mkSelector "setContinuous:") retVoid [argCULong (if value then 1 else 0)]

-- | @- enabled@
enabled :: IsNSControl nsControl => nsControl -> IO Bool
enabled nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setEnabled nsControl  value =
  sendMsg nsControl (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- refusesFirstResponder@
refusesFirstResponder :: IsNSControl nsControl => nsControl -> IO Bool
refusesFirstResponder nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "refusesFirstResponder") retCULong []

-- | @- setRefusesFirstResponder:@
setRefusesFirstResponder :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setRefusesFirstResponder nsControl  value =
  sendMsg nsControl (mkSelector "setRefusesFirstResponder:") retVoid [argCULong (if value then 1 else 0)]

-- | @- highlighted@
highlighted :: IsNSControl nsControl => nsControl -> IO Bool
highlighted nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "highlighted") retCULong []

-- | @- setHighlighted:@
setHighlighted :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setHighlighted nsControl  value =
  sendMsg nsControl (mkSelector "setHighlighted:") retVoid [argCULong (if value then 1 else 0)]

-- | @- controlSize@
controlSize :: IsNSControl nsControl => nsControl -> IO NSControlSize
controlSize nsControl  =
  fmap (coerce :: CULong -> NSControlSize) $ sendMsg nsControl (mkSelector "controlSize") retCULong []

-- | @- setControlSize:@
setControlSize :: IsNSControl nsControl => nsControl -> NSControlSize -> IO ()
setControlSize nsControl  value =
  sendMsg nsControl (mkSelector "setControlSize:") retVoid [argCULong (coerce value)]

-- | @- formatter@
formatter :: IsNSControl nsControl => nsControl -> IO (Id NSFormatter)
formatter nsControl  =
  sendMsg nsControl (mkSelector "formatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFormatter:@
setFormatter :: (IsNSControl nsControl, IsNSFormatter value) => nsControl -> value -> IO ()
setFormatter nsControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsControl (mkSelector "setFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- objectValue@
objectValue :: IsNSControl nsControl => nsControl -> IO RawId
objectValue nsControl  =
  fmap (RawId . castPtr) $ sendMsg nsControl (mkSelector "objectValue") (retPtr retVoid) []

-- | @- setObjectValue:@
setObjectValue :: IsNSControl nsControl => nsControl -> RawId -> IO ()
setObjectValue nsControl  value =
  sendMsg nsControl (mkSelector "setObjectValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- stringValue@
stringValue :: IsNSControl nsControl => nsControl -> IO (Id NSString)
stringValue nsControl  =
  sendMsg nsControl (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStringValue:@
setStringValue :: (IsNSControl nsControl, IsNSString value) => nsControl -> value -> IO ()
setStringValue nsControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsControl (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedStringValue@
attributedStringValue :: IsNSControl nsControl => nsControl -> IO (Id NSAttributedString)
attributedStringValue nsControl  =
  sendMsg nsControl (mkSelector "attributedStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedStringValue:@
setAttributedStringValue :: (IsNSControl nsControl, IsNSAttributedString value) => nsControl -> value -> IO ()
setAttributedStringValue nsControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsControl (mkSelector "setAttributedStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- intValue@
intValue :: IsNSControl nsControl => nsControl -> IO CInt
intValue nsControl  =
  sendMsg nsControl (mkSelector "intValue") retCInt []

-- | @- setIntValue:@
setIntValue :: IsNSControl nsControl => nsControl -> CInt -> IO ()
setIntValue nsControl  value =
  sendMsg nsControl (mkSelector "setIntValue:") retVoid [argCInt (fromIntegral value)]

-- | @- integerValue@
integerValue :: IsNSControl nsControl => nsControl -> IO CLong
integerValue nsControl  =
  sendMsg nsControl (mkSelector "integerValue") retCLong []

-- | @- setIntegerValue:@
setIntegerValue :: IsNSControl nsControl => nsControl -> CLong -> IO ()
setIntegerValue nsControl  value =
  sendMsg nsControl (mkSelector "setIntegerValue:") retVoid [argCLong (fromIntegral value)]

-- | @- floatValue@
floatValue :: IsNSControl nsControl => nsControl -> IO CFloat
floatValue nsControl  =
  sendMsg nsControl (mkSelector "floatValue") retCFloat []

-- | @- setFloatValue:@
setFloatValue :: IsNSControl nsControl => nsControl -> CFloat -> IO ()
setFloatValue nsControl  value =
  sendMsg nsControl (mkSelector "setFloatValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- doubleValue@
doubleValue :: IsNSControl nsControl => nsControl -> IO CDouble
doubleValue nsControl  =
  sendMsg nsControl (mkSelector "doubleValue") retCDouble []

-- | @- setDoubleValue:@
setDoubleValue :: IsNSControl nsControl => nsControl -> CDouble -> IO ()
setDoubleValue nsControl  value =
  sendMsg nsControl (mkSelector "setDoubleValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- font@
font :: IsNSControl nsControl => nsControl -> IO (Id NSFont)
font nsControl  =
  sendMsg nsControl (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsNSControl nsControl, IsNSFont value) => nsControl -> value -> IO ()
setFont nsControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsControl (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesSingleLineMode@
usesSingleLineMode :: IsNSControl nsControl => nsControl -> IO Bool
usesSingleLineMode nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "usesSingleLineMode") retCULong []

-- | @- setUsesSingleLineMode:@
setUsesSingleLineMode :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setUsesSingleLineMode nsControl  value =
  sendMsg nsControl (mkSelector "setUsesSingleLineMode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- lineBreakMode@
lineBreakMode :: IsNSControl nsControl => nsControl -> IO NSLineBreakMode
lineBreakMode nsControl  =
  fmap (coerce :: CULong -> NSLineBreakMode) $ sendMsg nsControl (mkSelector "lineBreakMode") retCULong []

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSControl nsControl => nsControl -> NSLineBreakMode -> IO ()
setLineBreakMode nsControl  value =
  sendMsg nsControl (mkSelector "setLineBreakMode:") retVoid [argCULong (coerce value)]

-- | @- alignment@
alignment :: IsNSControl nsControl => nsControl -> IO NSTextAlignment
alignment nsControl  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsControl (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsNSControl nsControl => nsControl -> NSTextAlignment -> IO ()
setAlignment nsControl  value =
  sendMsg nsControl (mkSelector "setAlignment:") retVoid [argCLong (coerce value)]

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSControl nsControl => nsControl -> IO NSWritingDirection
baseWritingDirection nsControl  =
  fmap (coerce :: CLong -> NSWritingDirection) $ sendMsg nsControl (mkSelector "baseWritingDirection") retCLong []

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSControl nsControl => nsControl -> NSWritingDirection -> IO ()
setBaseWritingDirection nsControl  value =
  sendMsg nsControl (mkSelector "setBaseWritingDirection:") retVoid [argCLong (coerce value)]

-- | @- allowsExpansionToolTips@
allowsExpansionToolTips :: IsNSControl nsControl => nsControl -> IO Bool
allowsExpansionToolTips nsControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsControl (mkSelector "allowsExpansionToolTips") retCULong []

-- | @- setAllowsExpansionToolTips:@
setAllowsExpansionToolTips :: IsNSControl nsControl => nsControl -> Bool -> IO ()
setAllowsExpansionToolTips nsControl  value =
  sendMsg nsControl (mkSelector "setAllowsExpansionToolTips:") retVoid [argCULong (if value then 1 else 0)]

-- | @+ cellClass@
cellClass :: IO Class
cellClass  =
  do
    cls' <- getRequiredClass "NSControl"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "cellClass") (retPtr retVoid) []

-- | @+ setCellClass:@
setCellClass :: Class -> IO ()
setCellClass value =
  do
    cls' <- getRequiredClass "NSControl"
    sendClassMsg cls' (mkSelector "setCellClass:") retVoid [argPtr (unClass value)]

-- | @- cell@
cell :: IsNSControl nsControl => nsControl -> IO (Id NSCell)
cell nsControl  =
  sendMsg nsControl (mkSelector "cell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCell:@
setCell :: (IsNSControl nsControl, IsNSCell value) => nsControl -> value -> IO ()
setCell nsControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsControl (mkSelector "setCell:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sizeThatFits:@
sizeThatFitsSelector :: Selector
sizeThatFitsSelector = mkSelector "sizeThatFits:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @sendActionOn:@
sendActionOnSelector :: Selector
sendActionOnSelector = mkSelector "sendActionOn:"

-- | @Selector@ for @sendAction:to:@
sendAction_toSelector :: Selector
sendAction_toSelector = mkSelector "sendAction:to:"

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

-- | @Selector@ for @performClick:@
performClickSelector :: Selector
performClickSelector = mkSelector "performClick:"

-- | @Selector@ for @expansionFrameWithFrame:@
expansionFrameWithFrameSelector :: Selector
expansionFrameWithFrameSelector = mkSelector "expansionFrameWithFrame:"

-- | @Selector@ for @drawWithExpansionFrame:inView:@
drawWithExpansionFrame_inViewSelector :: Selector
drawWithExpansionFrame_inViewSelector = mkSelector "drawWithExpansionFrame:inView:"

-- | @Selector@ for @invalidateIntrinsicContentSizeForCell:@
invalidateIntrinsicContentSizeForCellSelector :: Selector
invalidateIntrinsicContentSizeForCellSelector = mkSelector "invalidateIntrinsicContentSizeForCell:"

-- | @Selector@ for @setFloatingPointFormat:left:right:@
setFloatingPointFormat_left_rightSelector :: Selector
setFloatingPointFormat_left_rightSelector = mkSelector "setFloatingPointFormat:left:right:"

-- | @Selector@ for @selectedCell@
selectedCellSelector :: Selector
selectedCellSelector = mkSelector "selectedCell"

-- | @Selector@ for @selectedTag@
selectedTagSelector :: Selector
selectedTagSelector = mkSelector "selectedTag"

-- | @Selector@ for @setNeedsDisplay@
setNeedsDisplaySelector :: Selector
setNeedsDisplaySelector = mkSelector "setNeedsDisplay"

-- | @Selector@ for @calcSize@
calcSizeSelector :: Selector
calcSizeSelector = mkSelector "calcSize"

-- | @Selector@ for @updateCell:@
updateCellSelector :: Selector
updateCellSelector = mkSelector "updateCell:"

-- | @Selector@ for @updateCellInside:@
updateCellInsideSelector :: Selector
updateCellInsideSelector = mkSelector "updateCellInside:"

-- | @Selector@ for @drawCellInside:@
drawCellInsideSelector :: Selector
drawCellInsideSelector = mkSelector "drawCellInside:"

-- | @Selector@ for @drawCell:@
drawCellSelector :: Selector
drawCellSelector = mkSelector "drawCell:"

-- | @Selector@ for @selectCell:@
selectCellSelector :: Selector
selectCellSelector = mkSelector "selectCell:"

-- | @Selector@ for @currentEditor@
currentEditorSelector :: Selector
currentEditorSelector = mkSelector "currentEditor"

-- | @Selector@ for @abortEditing@
abortEditingSelector :: Selector
abortEditingSelector = mkSelector "abortEditing"

-- | @Selector@ for @validateEditing@
validateEditingSelector :: Selector
validateEditingSelector = mkSelector "validateEditing"

-- | @Selector@ for @editWithFrame:editor:delegate:event:@
editWithFrame_editor_delegate_eventSelector :: Selector
editWithFrame_editor_delegate_eventSelector = mkSelector "editWithFrame:editor:delegate:event:"

-- | @Selector@ for @selectWithFrame:editor:delegate:start:length:@
selectWithFrame_editor_delegate_start_lengthSelector :: Selector
selectWithFrame_editor_delegate_start_lengthSelector = mkSelector "selectWithFrame:editor:delegate:start:length:"

-- | @Selector@ for @endEditing:@
endEditingSelector :: Selector
endEditingSelector = mkSelector "endEditing:"

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

-- | @Selector@ for @ignoresMultiClick@
ignoresMultiClickSelector :: Selector
ignoresMultiClickSelector = mkSelector "ignoresMultiClick"

-- | @Selector@ for @setIgnoresMultiClick:@
setIgnoresMultiClickSelector :: Selector
setIgnoresMultiClickSelector = mkSelector "setIgnoresMultiClick:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @refusesFirstResponder@
refusesFirstResponderSelector :: Selector
refusesFirstResponderSelector = mkSelector "refusesFirstResponder"

-- | @Selector@ for @setRefusesFirstResponder:@
setRefusesFirstResponderSelector :: Selector
setRefusesFirstResponderSelector = mkSelector "setRefusesFirstResponder:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector
setControlSizeSelector = mkSelector "setControlSize:"

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

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @attributedStringValue@
attributedStringValueSelector :: Selector
attributedStringValueSelector = mkSelector "attributedStringValue"

-- | @Selector@ for @setAttributedStringValue:@
setAttributedStringValueSelector :: Selector
setAttributedStringValueSelector = mkSelector "setAttributedStringValue:"

-- | @Selector@ for @intValue@
intValueSelector :: Selector
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @setIntValue:@
setIntValueSelector :: Selector
setIntValueSelector = mkSelector "setIntValue:"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @setIntegerValue:@
setIntegerValueSelector :: Selector
setIntegerValueSelector = mkSelector "setIntegerValue:"

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

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @usesSingleLineMode@
usesSingleLineModeSelector :: Selector
usesSingleLineModeSelector = mkSelector "usesSingleLineMode"

-- | @Selector@ for @setUsesSingleLineMode:@
setUsesSingleLineModeSelector :: Selector
setUsesSingleLineModeSelector = mkSelector "setUsesSingleLineMode:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @allowsExpansionToolTips@
allowsExpansionToolTipsSelector :: Selector
allowsExpansionToolTipsSelector = mkSelector "allowsExpansionToolTips"

-- | @Selector@ for @setAllowsExpansionToolTips:@
setAllowsExpansionToolTipsSelector :: Selector
setAllowsExpansionToolTipsSelector = mkSelector "setAllowsExpansionToolTips:"

-- | @Selector@ for @cellClass@
cellClassSelector :: Selector
cellClassSelector = mkSelector "cellClass"

-- | @Selector@ for @setCellClass:@
setCellClassSelector :: Selector
setCellClassSelector = mkSelector "setCellClass:"

-- | @Selector@ for @cell@
cellSelector :: Selector
cellSelector = mkSelector "cell"

-- | @Selector@ for @setCell:@
setCellSelector :: Selector
setCellSelector = mkSelector "setCell:"

