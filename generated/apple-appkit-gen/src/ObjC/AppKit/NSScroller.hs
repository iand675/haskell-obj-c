{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , arrowsPositionSelector
  , checkSpaceForPartsSelector
  , compatibleWithOverlayScrollersSelector
  , controlSizeSelector
  , controlTintSelector
  , drawArrow_highlightSelector
  , drawKnobSelector
  , drawKnobSlotInRect_highlightSelector
  , drawPartsSelector
  , highlightSelector
  , hitPartSelector
  , knobProportionSelector
  , knobStyleSelector
  , preferredScrollerStyleSelector
  , rectForPartSelector
  , scrollerStyleSelector
  , scrollerWidthForControlSizeSelector
  , scrollerWidthForControlSize_scrollerStyleSelector
  , scrollerWidthSelector
  , setArrowsPositionSelector
  , setControlSizeSelector
  , setControlTintSelector
  , setFloatValue_knobProportionSelector
  , setKnobProportionSelector
  , setKnobStyleSelector
  , setScrollerStyleSelector
  , testPartSelector
  , trackKnobSelector
  , trackScrollButtonsSelector
  , usablePartsSelector

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

-- | @+ scrollerWidthForControlSize:scrollerStyle:@
scrollerWidthForControlSize_scrollerStyle :: NSControlSize -> NSScrollerStyle -> IO CDouble
scrollerWidthForControlSize_scrollerStyle controlSize scrollerStyle =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMessage cls' scrollerWidthForControlSize_scrollerStyleSelector controlSize scrollerStyle

-- | @- rectForPart:@
rectForPart :: IsNSScroller nsScroller => nsScroller -> NSScrollerPart -> IO NSRect
rectForPart nsScroller partCode =
  sendMessage nsScroller rectForPartSelector partCode

-- | @- checkSpaceForParts@
checkSpaceForParts :: IsNSScroller nsScroller => nsScroller -> IO ()
checkSpaceForParts nsScroller =
  sendMessage nsScroller checkSpaceForPartsSelector

-- | @- drawKnob@
drawKnob :: IsNSScroller nsScroller => nsScroller -> IO ()
drawKnob nsScroller =
  sendMessage nsScroller drawKnobSelector

-- | @- drawKnobSlotInRect:highlight:@
drawKnobSlotInRect_highlight :: IsNSScroller nsScroller => nsScroller -> NSRect -> Bool -> IO ()
drawKnobSlotInRect_highlight nsScroller slotRect flag =
  sendMessage nsScroller drawKnobSlotInRect_highlightSelector slotRect flag

-- | @- testPart:@
testPart :: IsNSScroller nsScroller => nsScroller -> NSPoint -> IO NSScrollerPart
testPart nsScroller point =
  sendMessage nsScroller testPartSelector point

-- | @- trackKnob:@
trackKnob :: (IsNSScroller nsScroller, IsNSEvent event) => nsScroller -> event -> IO ()
trackKnob nsScroller event =
  sendMessage nsScroller trackKnobSelector (toNSEvent event)

-- | @- setKnobProportion:@
setKnobProportion :: IsNSScroller nsScroller => nsScroller -> CDouble -> IO ()
setKnobProportion nsScroller proportion =
  sendMessage nsScroller setKnobProportionSelector proportion

-- | @+ scrollerWidthForControlSize:@
scrollerWidthForControlSize :: NSControlSize -> IO CDouble
scrollerWidthForControlSize controlSize =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMessage cls' scrollerWidthForControlSizeSelector controlSize

-- | @+ scrollerWidth@
scrollerWidth :: IO CDouble
scrollerWidth  =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMessage cls' scrollerWidthSelector

-- | @- setFloatValue:knobProportion:@
setFloatValue_knobProportion :: IsNSScroller nsScroller => nsScroller -> CFloat -> CDouble -> IO ()
setFloatValue_knobProportion nsScroller value proportion =
  sendMessage nsScroller setFloatValue_knobProportionSelector value proportion

-- | @- highlight:@
highlight :: IsNSScroller nsScroller => nsScroller -> Bool -> IO ()
highlight nsScroller flag =
  sendMessage nsScroller highlightSelector flag

-- | @- trackScrollButtons:@
trackScrollButtons :: (IsNSScroller nsScroller, IsNSEvent event) => nsScroller -> event -> IO ()
trackScrollButtons nsScroller event =
  sendMessage nsScroller trackScrollButtonsSelector (toNSEvent event)

-- | @- drawParts@
drawParts :: IsNSScroller nsScroller => nsScroller -> IO ()
drawParts nsScroller =
  sendMessage nsScroller drawPartsSelector

-- | @- drawArrow:highlight:@
drawArrow_highlight :: IsNSScroller nsScroller => nsScroller -> NSScrollerArrow -> Bool -> IO ()
drawArrow_highlight nsScroller whichArrow flag =
  sendMessage nsScroller drawArrow_highlightSelector whichArrow flag

-- | @+ compatibleWithOverlayScrollers@
compatibleWithOverlayScrollers :: IO Bool
compatibleWithOverlayScrollers  =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMessage cls' compatibleWithOverlayScrollersSelector

-- | @+ preferredScrollerStyle@
preferredScrollerStyle :: IO NSScrollerStyle
preferredScrollerStyle  =
  do
    cls' <- getRequiredClass "NSScroller"
    sendClassMessage cls' preferredScrollerStyleSelector

-- | @- scrollerStyle@
scrollerStyle :: IsNSScroller nsScroller => nsScroller -> IO NSScrollerStyle
scrollerStyle nsScroller =
  sendMessage nsScroller scrollerStyleSelector

-- | @- setScrollerStyle:@
setScrollerStyle :: IsNSScroller nsScroller => nsScroller -> NSScrollerStyle -> IO ()
setScrollerStyle nsScroller value =
  sendMessage nsScroller setScrollerStyleSelector value

-- | @- knobStyle@
knobStyle :: IsNSScroller nsScroller => nsScroller -> IO NSScrollerKnobStyle
knobStyle nsScroller =
  sendMessage nsScroller knobStyleSelector

-- | @- setKnobStyle:@
setKnobStyle :: IsNSScroller nsScroller => nsScroller -> NSScrollerKnobStyle -> IO ()
setKnobStyle nsScroller value =
  sendMessage nsScroller setKnobStyleSelector value

-- | @- usableParts@
usableParts :: IsNSScroller nsScroller => nsScroller -> IO NSUsableScrollerParts
usableParts nsScroller =
  sendMessage nsScroller usablePartsSelector

-- | @- controlSize@
controlSize :: IsNSScroller nsScroller => nsScroller -> IO NSControlSize
controlSize nsScroller =
  sendMessage nsScroller controlSizeSelector

-- | @- setControlSize:@
setControlSize :: IsNSScroller nsScroller => nsScroller -> NSControlSize -> IO ()
setControlSize nsScroller value =
  sendMessage nsScroller setControlSizeSelector value

-- | @- hitPart@
hitPart :: IsNSScroller nsScroller => nsScroller -> IO NSScrollerPart
hitPart nsScroller =
  sendMessage nsScroller hitPartSelector

-- | @- knobProportion@
knobProportion :: IsNSScroller nsScroller => nsScroller -> IO CDouble
knobProportion nsScroller =
  sendMessage nsScroller knobProportionSelector

-- | @- arrowsPosition@
arrowsPosition :: IsNSScroller nsScroller => nsScroller -> IO NSScrollArrowPosition
arrowsPosition nsScroller =
  sendMessage nsScroller arrowsPositionSelector

-- | @- setArrowsPosition:@
setArrowsPosition :: IsNSScroller nsScroller => nsScroller -> NSScrollArrowPosition -> IO ()
setArrowsPosition nsScroller value =
  sendMessage nsScroller setArrowsPositionSelector value

-- | @- controlTint@
controlTint :: IsNSScroller nsScroller => nsScroller -> IO NSControlTint
controlTint nsScroller =
  sendMessage nsScroller controlTintSelector

-- | @- setControlTint:@
setControlTint :: IsNSScroller nsScroller => nsScroller -> NSControlTint -> IO ()
setControlTint nsScroller value =
  sendMessage nsScroller setControlTintSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollerWidthForControlSize:scrollerStyle:@
scrollerWidthForControlSize_scrollerStyleSelector :: Selector '[NSControlSize, NSScrollerStyle] CDouble
scrollerWidthForControlSize_scrollerStyleSelector = mkSelector "scrollerWidthForControlSize:scrollerStyle:"

-- | @Selector@ for @rectForPart:@
rectForPartSelector :: Selector '[NSScrollerPart] NSRect
rectForPartSelector = mkSelector "rectForPart:"

-- | @Selector@ for @checkSpaceForParts@
checkSpaceForPartsSelector :: Selector '[] ()
checkSpaceForPartsSelector = mkSelector "checkSpaceForParts"

-- | @Selector@ for @drawKnob@
drawKnobSelector :: Selector '[] ()
drawKnobSelector = mkSelector "drawKnob"

-- | @Selector@ for @drawKnobSlotInRect:highlight:@
drawKnobSlotInRect_highlightSelector :: Selector '[NSRect, Bool] ()
drawKnobSlotInRect_highlightSelector = mkSelector "drawKnobSlotInRect:highlight:"

-- | @Selector@ for @testPart:@
testPartSelector :: Selector '[NSPoint] NSScrollerPart
testPartSelector = mkSelector "testPart:"

-- | @Selector@ for @trackKnob:@
trackKnobSelector :: Selector '[Id NSEvent] ()
trackKnobSelector = mkSelector "trackKnob:"

-- | @Selector@ for @setKnobProportion:@
setKnobProportionSelector :: Selector '[CDouble] ()
setKnobProportionSelector = mkSelector "setKnobProportion:"

-- | @Selector@ for @scrollerWidthForControlSize:@
scrollerWidthForControlSizeSelector :: Selector '[NSControlSize] CDouble
scrollerWidthForControlSizeSelector = mkSelector "scrollerWidthForControlSize:"

-- | @Selector@ for @scrollerWidth@
scrollerWidthSelector :: Selector '[] CDouble
scrollerWidthSelector = mkSelector "scrollerWidth"

-- | @Selector@ for @setFloatValue:knobProportion:@
setFloatValue_knobProportionSelector :: Selector '[CFloat, CDouble] ()
setFloatValue_knobProportionSelector = mkSelector "setFloatValue:knobProportion:"

-- | @Selector@ for @highlight:@
highlightSelector :: Selector '[Bool] ()
highlightSelector = mkSelector "highlight:"

-- | @Selector@ for @trackScrollButtons:@
trackScrollButtonsSelector :: Selector '[Id NSEvent] ()
trackScrollButtonsSelector = mkSelector "trackScrollButtons:"

-- | @Selector@ for @drawParts@
drawPartsSelector :: Selector '[] ()
drawPartsSelector = mkSelector "drawParts"

-- | @Selector@ for @drawArrow:highlight:@
drawArrow_highlightSelector :: Selector '[NSScrollerArrow, Bool] ()
drawArrow_highlightSelector = mkSelector "drawArrow:highlight:"

-- | @Selector@ for @compatibleWithOverlayScrollers@
compatibleWithOverlayScrollersSelector :: Selector '[] Bool
compatibleWithOverlayScrollersSelector = mkSelector "compatibleWithOverlayScrollers"

-- | @Selector@ for @preferredScrollerStyle@
preferredScrollerStyleSelector :: Selector '[] NSScrollerStyle
preferredScrollerStyleSelector = mkSelector "preferredScrollerStyle"

-- | @Selector@ for @scrollerStyle@
scrollerStyleSelector :: Selector '[] NSScrollerStyle
scrollerStyleSelector = mkSelector "scrollerStyle"

-- | @Selector@ for @setScrollerStyle:@
setScrollerStyleSelector :: Selector '[NSScrollerStyle] ()
setScrollerStyleSelector = mkSelector "setScrollerStyle:"

-- | @Selector@ for @knobStyle@
knobStyleSelector :: Selector '[] NSScrollerKnobStyle
knobStyleSelector = mkSelector "knobStyle"

-- | @Selector@ for @setKnobStyle:@
setKnobStyleSelector :: Selector '[NSScrollerKnobStyle] ()
setKnobStyleSelector = mkSelector "setKnobStyle:"

-- | @Selector@ for @usableParts@
usablePartsSelector :: Selector '[] NSUsableScrollerParts
usablePartsSelector = mkSelector "usableParts"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector '[] NSControlSize
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector '[NSControlSize] ()
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @hitPart@
hitPartSelector :: Selector '[] NSScrollerPart
hitPartSelector = mkSelector "hitPart"

-- | @Selector@ for @knobProportion@
knobProportionSelector :: Selector '[] CDouble
knobProportionSelector = mkSelector "knobProportion"

-- | @Selector@ for @arrowsPosition@
arrowsPositionSelector :: Selector '[] NSScrollArrowPosition
arrowsPositionSelector = mkSelector "arrowsPosition"

-- | @Selector@ for @setArrowsPosition:@
setArrowsPositionSelector :: Selector '[NSScrollArrowPosition] ()
setArrowsPositionSelector = mkSelector "setArrowsPosition:"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector '[] NSControlTint
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector '[NSControlTint] ()
setControlTintSelector = mkSelector "setControlTint:"

