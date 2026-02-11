{-# LANGUAGE PatternSynonyms #-}
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
  , updateWithEventSelector
  , updateWithPanRecognizerSelector
  , alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector
  , alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector
  , alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector
  , performFeedback_performanceTimeSelector
  , inputEventMaskSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- updateWithEvent:@
updateWithEvent :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSEvent event) => nsAlignmentFeedbackFilter -> event -> IO ()
updateWithEvent nsAlignmentFeedbackFilter  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsAlignmentFeedbackFilter (mkSelector "updateWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- updateWithPanRecognizer:@
updateWithPanRecognizer :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSPanGestureRecognizer panRecognizer) => nsAlignmentFeedbackFilter -> panRecognizer -> IO ()
updateWithPanRecognizer nsAlignmentFeedbackFilter  panRecognizer =
withObjCPtr panRecognizer $ \raw_panRecognizer ->
    sendMsg nsAlignmentFeedbackFilter (mkSelector "updateWithPanRecognizer:") retVoid [argPtr (castPtr raw_panRecognizer :: Ptr ())]

-- | @- alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:@
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPoint :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSView view) => nsAlignmentFeedbackFilter -> view -> NSPoint -> NSPoint -> NSPoint -> IO RawId
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPoint nsAlignmentFeedbackFilter  view previousPoint alignedPoint defaultPoint =
withObjCPtr view $ \raw_view ->
    fmap (RawId . castPtr) $ sendMsg nsAlignmentFeedbackFilter (mkSelector "alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSPoint previousPoint, argNSPoint alignedPoint, argNSPoint defaultPoint]

-- | @- alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:@
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultX :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSView view) => nsAlignmentFeedbackFilter -> view -> CDouble -> CDouble -> CDouble -> IO RawId
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultX nsAlignmentFeedbackFilter  view previousX alignedX defaultX =
withObjCPtr view $ \raw_view ->
    fmap (RawId . castPtr) $ sendMsg nsAlignmentFeedbackFilter (mkSelector "alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argCDouble (fromIntegral previousX), argCDouble (fromIntegral alignedX), argCDouble (fromIntegral defaultX)]

-- | @- alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:@
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultY :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSView view) => nsAlignmentFeedbackFilter -> view -> CDouble -> CDouble -> CDouble -> IO RawId
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultY nsAlignmentFeedbackFilter  view previousY alignedY defaultY =
withObjCPtr view $ \raw_view ->
    fmap (RawId . castPtr) $ sendMsg nsAlignmentFeedbackFilter (mkSelector "alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argCDouble (fromIntegral previousY), argCDouble (fromIntegral alignedY), argCDouble (fromIntegral defaultY)]

-- | @- performFeedback:performanceTime:@
performFeedback_performanceTime :: (IsNSAlignmentFeedbackFilter nsAlignmentFeedbackFilter, IsNSArray alignmentFeedbackTokens) => nsAlignmentFeedbackFilter -> alignmentFeedbackTokens -> NSHapticFeedbackPerformanceTime -> IO ()
performFeedback_performanceTime nsAlignmentFeedbackFilter  alignmentFeedbackTokens performanceTime =
withObjCPtr alignmentFeedbackTokens $ \raw_alignmentFeedbackTokens ->
    sendMsg nsAlignmentFeedbackFilter (mkSelector "performFeedback:performanceTime:") retVoid [argPtr (castPtr raw_alignmentFeedbackTokens :: Ptr ()), argCULong (coerce performanceTime)]

-- | @+ inputEventMask@
inputEventMask :: IO NSEventMask
inputEventMask  =
  do
    cls' <- getRequiredClass "NSAlignmentFeedbackFilter"
    fmap (coerce :: CULong -> NSEventMask) $ sendClassMsg cls' (mkSelector "inputEventMask") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithEvent:@
updateWithEventSelector :: Selector
updateWithEventSelector = mkSelector "updateWithEvent:"

-- | @Selector@ for @updateWithPanRecognizer:@
updateWithPanRecognizerSelector :: Selector
updateWithPanRecognizerSelector = mkSelector "updateWithPanRecognizer:"

-- | @Selector@ for @alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:@
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector :: Selector
alignmentFeedbackTokenForMovementInView_previousPoint_alignedPoint_defaultPointSelector = mkSelector "alignmentFeedbackTokenForMovementInView:previousPoint:alignedPoint:defaultPoint:"

-- | @Selector@ for @alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:@
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector :: Selector
alignmentFeedbackTokenForHorizontalMovementInView_previousX_alignedX_defaultXSelector = mkSelector "alignmentFeedbackTokenForHorizontalMovementInView:previousX:alignedX:defaultX:"

-- | @Selector@ for @alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:@
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector :: Selector
alignmentFeedbackTokenForVerticalMovementInView_previousY_alignedY_defaultYSelector = mkSelector "alignmentFeedbackTokenForVerticalMovementInView:previousY:alignedY:defaultY:"

-- | @Selector@ for @performFeedback:performanceTime:@
performFeedback_performanceTimeSelector :: Selector
performFeedback_performanceTimeSelector = mkSelector "performFeedback:performanceTime:"

-- | @Selector@ for @inputEventMask@
inputEventMaskSelector :: Selector
inputEventMaskSelector = mkSelector "inputEventMask"

