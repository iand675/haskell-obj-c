{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScroller@.
module ObjC.AppKit.NSScroller
  ( NSScroller
  , IsNSScroller(..)
  , scrollerWidthForControlSize_scrollerStyle
  , rectForPart
  , checkSpaceForParts
  , drawKnob
  , drawKnobSlotInRect_highlight
  , testPart
  , trackKnob
  , setKnobProportion
  , scrollerWidthForControlSize
  , scrollerWidth
  , setFloatValue_knobProportion
  , highlight
  , trackScrollButtons
  , drawParts
  , drawArrow_highlight
  , compatibleWithOverlayScrollers
  , preferredScrollerStyle
  , scrollerStyle
  , setScrollerStyle
  , knobStyle
  , setKnobStyle
  , usableParts
  , controlSize
  , setControlSize
  , hitPart
  , knobProportion
  , arrowsPosition
  , setArrowsPosition
  , controlTint
  , setControlTint
  , scrollerWidthForControlSize_scrollerStyleSelector
  , rectForPartSelector
  , checkSpaceForPartsSelector
  , drawKnobSelector
  , drawKnobSlotInRect_highlightSelector
  , testPartSelector
  , trackKnobSelector
  , setKnobProportionSelector
  , scrollerWidthForControlSizeSelector
  , scrollerWidthSelector
  , setFloatValue_knobProportionSelector
  , highlightSelector
  , trackScrollButtonsSelector
  , drawPartsSelector
  , drawArrow_highlightSelector
  , compatibleWithOverlayScrollersSelector
  , preferredScrollerStyleSelector
  , scrollerStyleSelector
  , setScrollerStyleSelector
  , knobStyleSelector
  , setKnobStyleSelector
  , usablePartsSelector
  , controlSizeSelector
  , setControlSizeSelector
  , hitPartSelector
  , knobProportionSelector
  , arrowsPositionSelector
  , setArrowsPositionSelector
  , controlTintSelector
  , setControlTintSelector

  -- * Enum types
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
  , NSScrollArrowPosition(NSScrollArrowPosition)
  , pattern NSScrollerArrowsMaxEnd
  , pattern NSScrollerArrowsMinEnd
  , pattern NSScrollerArrowsDefaultSetting
  , pattern NSScrollerArrowsNone
  , NSScrollerArrow(NSScrollerArrow)
  , pattern NSScrollerIncrementArrow
  , pattern NSScrollerDecrementArrow
  , NSScrollerKnobStyle(NSScrollerKnobStyle)
  , pattern NSScrollerKnobStyleDefault
  , pattern NSScrollerKnobStyleDark
  , pattern NSScrollerKnobStyleLight
  , NSScrollerPart(NSScrollerPart)
  , pattern NSScrollerNoPart
  , pattern NSScrollerDecrementPage
  , pattern NSScrollerKnob
  , pattern NSScrollerIncrementPage
  , pattern NSScrollerDecrementLine
  , pattern NSScrollerIncrementLine
  , pattern NSScrollerKnobSlot
  , NSScrollerStyle(NSScrollerStyle)
  , pattern NSScrollerStyleLegacy
  , pattern NSScrollerStyleOverlay
  , NSUsableScrollerParts(NSUsableScrollerParts)
  , pattern NSNoScrollerParts
  , pattern NSOnlyScrollerArrows
  , pattern NSAllScrollerParts

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

-- | @+ scrollerWidthForControlSize:scrollerStyle:@
scrollerWidthForControlSize_scrollerStyle :: NSControlSize -> NSScrollerStyle -> IO CDouble
scrollerWidthForControlSize_scrollerStyle controlSize scrollerStyle =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMsg cls' (mkSelector "scrollerWidthForControlSize:scrollerStyle:") retCDouble [argCULong (coerce controlSize), argCLong (coerce scrollerStyle)]

-- | @- rectForPart:@
rectForPart :: IsNSScroller nsScroller => nsScroller -> NSScrollerPart -> IO NSRect
rectForPart nsScroller  partCode =
  sendMsgStret nsScroller (mkSelector "rectForPart:") retNSRect [argCULong (coerce partCode)]

-- | @- checkSpaceForParts@
checkSpaceForParts :: IsNSScroller nsScroller => nsScroller -> IO ()
checkSpaceForParts nsScroller  =
  sendMsg nsScroller (mkSelector "checkSpaceForParts") retVoid []

-- | @- drawKnob@
drawKnob :: IsNSScroller nsScroller => nsScroller -> IO ()
drawKnob nsScroller  =
  sendMsg nsScroller (mkSelector "drawKnob") retVoid []

-- | @- drawKnobSlotInRect:highlight:@
drawKnobSlotInRect_highlight :: IsNSScroller nsScroller => nsScroller -> NSRect -> Bool -> IO ()
drawKnobSlotInRect_highlight nsScroller  slotRect flag =
  sendMsg nsScroller (mkSelector "drawKnobSlotInRect:highlight:") retVoid [argNSRect slotRect, argCULong (if flag then 1 else 0)]

-- | @- testPart:@
testPart :: IsNSScroller nsScroller => nsScroller -> NSPoint -> IO NSScrollerPart
testPart nsScroller  point =
  fmap (coerce :: CULong -> NSScrollerPart) $ sendMsg nsScroller (mkSelector "testPart:") retCULong [argNSPoint point]

-- | @- trackKnob:@
trackKnob :: (IsNSScroller nsScroller, IsNSEvent event) => nsScroller -> event -> IO ()
trackKnob nsScroller  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsScroller (mkSelector "trackKnob:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- setKnobProportion:@
setKnobProportion :: IsNSScroller nsScroller => nsScroller -> CDouble -> IO ()
setKnobProportion nsScroller  proportion =
  sendMsg nsScroller (mkSelector "setKnobProportion:") retVoid [argCDouble (fromIntegral proportion)]

-- | @+ scrollerWidthForControlSize:@
scrollerWidthForControlSize :: NSControlSize -> IO CDouble
scrollerWidthForControlSize controlSize =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMsg cls' (mkSelector "scrollerWidthForControlSize:") retCDouble [argCULong (coerce controlSize)]

-- | @+ scrollerWidth@
scrollerWidth :: IO CDouble
scrollerWidth  =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMsg cls' (mkSelector "scrollerWidth") retCDouble []

-- | @- setFloatValue:knobProportion:@
setFloatValue_knobProportion :: IsNSScroller nsScroller => nsScroller -> CFloat -> CDouble -> IO ()
setFloatValue_knobProportion nsScroller  value proportion =
  sendMsg nsScroller (mkSelector "setFloatValue:knobProportion:") retVoid [argCFloat (fromIntegral value), argCDouble (fromIntegral proportion)]

-- | @- highlight:@
highlight :: IsNSScroller nsScroller => nsScroller -> Bool -> IO ()
highlight nsScroller  flag =
  sendMsg nsScroller (mkSelector "highlight:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- trackScrollButtons:@
trackScrollButtons :: (IsNSScroller nsScroller, IsNSEvent event) => nsScroller -> event -> IO ()
trackScrollButtons nsScroller  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsScroller (mkSelector "trackScrollButtons:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- drawParts@
drawParts :: IsNSScroller nsScroller => nsScroller -> IO ()
drawParts nsScroller  =
  sendMsg nsScroller (mkSelector "drawParts") retVoid []

-- | @- drawArrow:highlight:@
drawArrow_highlight :: IsNSScroller nsScroller => nsScroller -> NSScrollerArrow -> Bool -> IO ()
drawArrow_highlight nsScroller  whichArrow flag =
  sendMsg nsScroller (mkSelector "drawArrow:highlight:") retVoid [argCULong (coerce whichArrow), argCULong (if flag then 1 else 0)]

-- | @+ compatibleWithOverlayScrollers@
compatibleWithOverlayScrollers :: IO Bool
compatibleWithOverlayScrollers  =
  do
    cls' <- getRequiredClass "NSScroller"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "compatibleWithOverlayScrollers") retCULong []

-- | @+ preferredScrollerStyle@
preferredScrollerStyle :: IO NSScrollerStyle
preferredScrollerStyle  =
  do
    cls' <- getRequiredClass "NSScroller"
    fmap (coerce :: CLong -> NSScrollerStyle) $ sendClassMsg cls' (mkSelector "preferredScrollerStyle") retCLong []

-- | @- scrollerStyle@
scrollerStyle :: IsNSScroller nsScroller => nsScroller -> IO NSScrollerStyle
scrollerStyle nsScroller  =
  fmap (coerce :: CLong -> NSScrollerStyle) $ sendMsg nsScroller (mkSelector "scrollerStyle") retCLong []

-- | @- setScrollerStyle:@
setScrollerStyle :: IsNSScroller nsScroller => nsScroller -> NSScrollerStyle -> IO ()
setScrollerStyle nsScroller  value =
  sendMsg nsScroller (mkSelector "setScrollerStyle:") retVoid [argCLong (coerce value)]

-- | @- knobStyle@
knobStyle :: IsNSScroller nsScroller => nsScroller -> IO NSScrollerKnobStyle
knobStyle nsScroller  =
  fmap (coerce :: CLong -> NSScrollerKnobStyle) $ sendMsg nsScroller (mkSelector "knobStyle") retCLong []

-- | @- setKnobStyle:@
setKnobStyle :: IsNSScroller nsScroller => nsScroller -> NSScrollerKnobStyle -> IO ()
setKnobStyle nsScroller  value =
  sendMsg nsScroller (mkSelector "setKnobStyle:") retVoid [argCLong (coerce value)]

-- | @- usableParts@
usableParts :: IsNSScroller nsScroller => nsScroller -> IO NSUsableScrollerParts
usableParts nsScroller  =
  fmap (coerce :: CULong -> NSUsableScrollerParts) $ sendMsg nsScroller (mkSelector "usableParts") retCULong []

-- | @- controlSize@
controlSize :: IsNSScroller nsScroller => nsScroller -> IO NSControlSize
controlSize nsScroller  =
  fmap (coerce :: CULong -> NSControlSize) $ sendMsg nsScroller (mkSelector "controlSize") retCULong []

-- | @- setControlSize:@
setControlSize :: IsNSScroller nsScroller => nsScroller -> NSControlSize -> IO ()
setControlSize nsScroller  value =
  sendMsg nsScroller (mkSelector "setControlSize:") retVoid [argCULong (coerce value)]

-- | @- hitPart@
hitPart :: IsNSScroller nsScroller => nsScroller -> IO NSScrollerPart
hitPart nsScroller  =
  fmap (coerce :: CULong -> NSScrollerPart) $ sendMsg nsScroller (mkSelector "hitPart") retCULong []

-- | @- knobProportion@
knobProportion :: IsNSScroller nsScroller => nsScroller -> IO CDouble
knobProportion nsScroller  =
  sendMsg nsScroller (mkSelector "knobProportion") retCDouble []

-- | @- arrowsPosition@
arrowsPosition :: IsNSScroller nsScroller => nsScroller -> IO NSScrollArrowPosition
arrowsPosition nsScroller  =
  fmap (coerce :: CULong -> NSScrollArrowPosition) $ sendMsg nsScroller (mkSelector "arrowsPosition") retCULong []

-- | @- setArrowsPosition:@
setArrowsPosition :: IsNSScroller nsScroller => nsScroller -> NSScrollArrowPosition -> IO ()
setArrowsPosition nsScroller  value =
  sendMsg nsScroller (mkSelector "setArrowsPosition:") retVoid [argCULong (coerce value)]

-- | @- controlTint@
controlTint :: IsNSScroller nsScroller => nsScroller -> IO NSControlTint
controlTint nsScroller  =
  fmap (coerce :: CULong -> NSControlTint) $ sendMsg nsScroller (mkSelector "controlTint") retCULong []

-- | @- setControlTint:@
setControlTint :: IsNSScroller nsScroller => nsScroller -> NSControlTint -> IO ()
setControlTint nsScroller  value =
  sendMsg nsScroller (mkSelector "setControlTint:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollerWidthForControlSize:scrollerStyle:@
scrollerWidthForControlSize_scrollerStyleSelector :: Selector
scrollerWidthForControlSize_scrollerStyleSelector = mkSelector "scrollerWidthForControlSize:scrollerStyle:"

-- | @Selector@ for @rectForPart:@
rectForPartSelector :: Selector
rectForPartSelector = mkSelector "rectForPart:"

-- | @Selector@ for @checkSpaceForParts@
checkSpaceForPartsSelector :: Selector
checkSpaceForPartsSelector = mkSelector "checkSpaceForParts"

-- | @Selector@ for @drawKnob@
drawKnobSelector :: Selector
drawKnobSelector = mkSelector "drawKnob"

-- | @Selector@ for @drawKnobSlotInRect:highlight:@
drawKnobSlotInRect_highlightSelector :: Selector
drawKnobSlotInRect_highlightSelector = mkSelector "drawKnobSlotInRect:highlight:"

-- | @Selector@ for @testPart:@
testPartSelector :: Selector
testPartSelector = mkSelector "testPart:"

-- | @Selector@ for @trackKnob:@
trackKnobSelector :: Selector
trackKnobSelector = mkSelector "trackKnob:"

-- | @Selector@ for @setKnobProportion:@
setKnobProportionSelector :: Selector
setKnobProportionSelector = mkSelector "setKnobProportion:"

-- | @Selector@ for @scrollerWidthForControlSize:@
scrollerWidthForControlSizeSelector :: Selector
scrollerWidthForControlSizeSelector = mkSelector "scrollerWidthForControlSize:"

-- | @Selector@ for @scrollerWidth@
scrollerWidthSelector :: Selector
scrollerWidthSelector = mkSelector "scrollerWidth"

-- | @Selector@ for @setFloatValue:knobProportion:@
setFloatValue_knobProportionSelector :: Selector
setFloatValue_knobProportionSelector = mkSelector "setFloatValue:knobProportion:"

-- | @Selector@ for @highlight:@
highlightSelector :: Selector
highlightSelector = mkSelector "highlight:"

-- | @Selector@ for @trackScrollButtons:@
trackScrollButtonsSelector :: Selector
trackScrollButtonsSelector = mkSelector "trackScrollButtons:"

-- | @Selector@ for @drawParts@
drawPartsSelector :: Selector
drawPartsSelector = mkSelector "drawParts"

-- | @Selector@ for @drawArrow:highlight:@
drawArrow_highlightSelector :: Selector
drawArrow_highlightSelector = mkSelector "drawArrow:highlight:"

-- | @Selector@ for @compatibleWithOverlayScrollers@
compatibleWithOverlayScrollersSelector :: Selector
compatibleWithOverlayScrollersSelector = mkSelector "compatibleWithOverlayScrollers"

-- | @Selector@ for @preferredScrollerStyle@
preferredScrollerStyleSelector :: Selector
preferredScrollerStyleSelector = mkSelector "preferredScrollerStyle"

-- | @Selector@ for @scrollerStyle@
scrollerStyleSelector :: Selector
scrollerStyleSelector = mkSelector "scrollerStyle"

-- | @Selector@ for @setScrollerStyle:@
setScrollerStyleSelector :: Selector
setScrollerStyleSelector = mkSelector "setScrollerStyle:"

-- | @Selector@ for @knobStyle@
knobStyleSelector :: Selector
knobStyleSelector = mkSelector "knobStyle"

-- | @Selector@ for @setKnobStyle:@
setKnobStyleSelector :: Selector
setKnobStyleSelector = mkSelector "setKnobStyle:"

-- | @Selector@ for @usableParts@
usablePartsSelector :: Selector
usablePartsSelector = mkSelector "usableParts"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @hitPart@
hitPartSelector :: Selector
hitPartSelector = mkSelector "hitPart"

-- | @Selector@ for @knobProportion@
knobProportionSelector :: Selector
knobProportionSelector = mkSelector "knobProportion"

-- | @Selector@ for @arrowsPosition@
arrowsPositionSelector :: Selector
arrowsPositionSelector = mkSelector "arrowsPosition"

-- | @Selector@ for @setArrowsPosition:@
setArrowsPositionSelector :: Selector
setArrowsPositionSelector = mkSelector "setArrowsPosition:"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector
setControlTintSelector = mkSelector "setControlTint:"

