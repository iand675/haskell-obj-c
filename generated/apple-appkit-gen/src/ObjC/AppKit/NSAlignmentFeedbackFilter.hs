{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAlignmentFeedbackFilter@.
module ObjC.AppKit.NSAlignmentFeedbackFilter
  ( NSAlignmentFeedbackFilter
  , IsNSAlignmentFeedbackFilter(..)
  , updateWithEvent
  , updateWithPanRecognizer
  , alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPoint
  , alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultX
  , alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultY
  , performFeedback_performanceTime
  , inputEventMask
  , alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector
  , alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector
  , alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector
  , inputEventMaskSelector
  , performFeedback_performanceTimeSelector
  , updateWithEventSelector
  , updateWithPanRecognizerSelector

  -- * Enum types
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
  , NSHapticFeedbackPerformanceTime(NSHapticFeedbackPerformanceTime)
  , pattern NSHapticFeedbackPerformanceTimeDefault
  , pattern NSHapticFeedbackPerformanceTimeNow
  , pattern NSHapticFeedbackPerformanceTimeDrawCompleted

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

-- | @- updateWithEvent:@
updateWithEvent :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSEvent event) => nsAlignmentFeedbackFilter -> event -> IO ()
updateWithEvent nsAlignmentFeedbackFilter event =
  sendMessage nsAlignmentFeedbackFilter updateWithEventSelector (toNSEvent event)

-- | @- updateWithPanRecognizer:@
updateWithPanRecognizer :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSPanGestureRecognizer panRecognizer) => nsAlignmentFeedbackFilter -> panRecognizer -> IO ()
updateWithPanRecognizer nsAlignmentFeedbackFilter panRecognizer =
  sendMessage nsAlignmentFeedbackFilter updateWithPanRecognizerSelector (toNSPanGestureRecognizer panRecognizer)

-- | @- alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:@
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPoint :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSView view) => nsAlignmentFeedbackFilter -> view -> NSPoint -> NSPoint -> NSPoint -> IO RawId
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPoint nsAlignmentFeedbackFilter view previousPoint alignedPoint defaultPoint =
  sendMessage nsAlignmentFeedbackFilter alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector (toNSView view) previousPoint alignedPoint defaultPoint

-- | @- alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:@
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultX :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSView view) => nsAlignmentFeedbackFilter -> view -> CDouble -> CDouble -> CDouble -> IO RawId
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultX nsAlignmentFeedbackFilter view previousX alignedX defaultX =
  sendMessage nsAlignmentFeedbackFilter alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector (toNSView view) previousX alignedX defaultX

-- | @- alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:@
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultY :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSView view) => nsAlignmentFeedbackFilter -> view -> CDouble -> CDouble -> CDouble -> IO RawId
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultY nsAlignmentFeedbackFilter view previousY alignedY defaultY =
  sendMessage nsAlignmentFeedbackFilter alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector (toNSView view) previousY alignedY defaultY

-- | @- performFeedback:performanceTime:@
performFeedback_performanceTime :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSArray alignmentFeedbackTokens) => nsAlignmentFeedbackFilter -> alignmentFeedbackTokens -> NSHapticFeedbackPerformanceTime -> IO ()
performFeedback_performanceTime nsAlignmentFeedbackFilter alignmentFeedbackTokens performanceTime =
  sendMessage nsAlignmentFeedbackFilter performFeedback_performanceTimeSelector (toNSArray alignmentFeedbackTokens) performanceTime

-- | @+ inputEventMask@
inputEventMask :: IO NSEventMask
inputEventMask  =
  do
    cls' <- getRequiredClass "NSAlignmentFeedbackFilter"
    sendClassMessage cls' inputEventMaskSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithEvent:@
updateWithEventSelector :: Selector '[Id NSEvent] ()
updateWithEventSelector = mkSelector "updateWithEvent:"

-- | @Selector@ for @updateWithPanRecognizer:@
updateWithPanRecognizerSelector :: Selector '[Id NSPanGestureRecognizer] ()
updateWithPanRecognizerSelector = mkSelector "updateWithPanRecognizer:"

-- | @Selector@ for @alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:@
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector :: Selector '[Id NSView, NSPoint, NSPoint, NSPoint] RawId
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector = mkSelector "alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:"

-- | @Selector@ for @alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:@
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector :: Selector '[Id NSView, CDouble, CDouble, CDouble] RawId
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector = mkSelector "alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:"

-- | @Selector@ for @alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:@
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector :: Selector '[Id NSView, CDouble, CDouble, CDouble] RawId
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector = mkSelector "alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:"

-- | @Selector@ for @performFeedback:performanceTime:@
performFeedback_performanceTimeSelector :: Selector '[Id NSArray, NSHapticFeedbackPerformanceTime] ()
performFeedback_performanceTimeSelector = mkSelector "performFeedback:performanceTime:"

-- | @Selector@ for @inputEventMask@
inputEventMaskSelector :: Selector '[] NSEventMask
inputEventMaskSelector = mkSelector "inputEventMask"

