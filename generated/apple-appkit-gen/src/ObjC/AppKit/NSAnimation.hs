{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAnimation@.
module ObjC.AppKit.NSAnimation
  ( NSAnimation
  , IsNSAnimation(..)
  , initWithDuration_animationCurve
  , initWithCoder
  , startAnimation
  , stopAnimation
  , addProgressMark
  , removeProgressMark
  , startWhenAnimation_reachesProgress
  , stopWhenAnimation_reachesProgress
  , clearStartAnimation
  , clearStopAnimation
  , animating
  , currentProgress
  , setCurrentProgress
  , duration
  , setDuration
  , animationBlockingMode
  , setAnimationBlockingMode
  , frameRate
  , setFrameRate
  , animationCurve
  , setAnimationCurve
  , currentValue
  , delegate
  , setDelegate
  , progressMarks
  , setProgressMarks
  , runLoopModesForAnimating
  , initWithDuration_animationCurveSelector
  , initWithCoderSelector
  , startAnimationSelector
  , stopAnimationSelector
  , addProgressMarkSelector
  , removeProgressMarkSelector
  , startWhenAnimation_reachesProgressSelector
  , stopWhenAnimation_reachesProgressSelector
  , clearStartAnimationSelector
  , clearStopAnimationSelector
  , animatingSelector
  , currentProgressSelector
  , setCurrentProgressSelector
  , durationSelector
  , setDurationSelector
  , animationBlockingModeSelector
  , setAnimationBlockingModeSelector
  , frameRateSelector
  , setFrameRateSelector
  , animationCurveSelector
  , setAnimationCurveSelector
  , currentValueSelector
  , delegateSelector
  , setDelegateSelector
  , progressMarksSelector
  , setProgressMarksSelector
  , runLoopModesForAnimatingSelector

  -- * Enum types
  , NSAnimationBlockingMode(NSAnimationBlockingMode)
  , pattern NSAnimationBlocking
  , pattern NSAnimationNonblocking
  , pattern NSAnimationNonblockingThreaded
  , NSAnimationCurve(NSAnimationCurve)
  , pattern NSAnimationEaseInOut
  , pattern NSAnimationEaseIn
  , pattern NSAnimationEaseOut
  , pattern NSAnimationLinear

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDuration:animationCurve:@
initWithDuration_animationCurve :: IsNSAnimation nsAnimation => nsAnimation -> CDouble -> NSAnimationCurve -> IO (Id NSAnimation)
initWithDuration_animationCurve nsAnimation  duration animationCurve =
    sendMsg nsAnimation (mkSelector "initWithDuration:animationCurve:") (retPtr retVoid) [argCDouble duration, argCULong (coerce animationCurve)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSAnimation nsAnimation, IsNSCoder coder) => nsAnimation -> coder -> IO (Id NSAnimation)
initWithCoder nsAnimation  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsAnimation (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- startAnimation@
startAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
startAnimation nsAnimation  =
    sendMsg nsAnimation (mkSelector "startAnimation") retVoid []

-- | @- stopAnimation@
stopAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
stopAnimation nsAnimation  =
    sendMsg nsAnimation (mkSelector "stopAnimation") retVoid []

-- | @- addProgressMark:@
addProgressMark :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
addProgressMark nsAnimation  progressMark =
    sendMsg nsAnimation (mkSelector "addProgressMark:") retVoid [argCFloat progressMark]

-- | @- removeProgressMark:@
removeProgressMark :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
removeProgressMark nsAnimation  progressMark =
    sendMsg nsAnimation (mkSelector "removeProgressMark:") retVoid [argCFloat progressMark]

-- | @- startWhenAnimation:reachesProgress:@
startWhenAnimation_reachesProgress :: (IsNSAnimation nsAnimation, IsNSAnimation animation) => nsAnimation -> animation -> CFloat -> IO ()
startWhenAnimation_reachesProgress nsAnimation  animation startProgress =
  withObjCPtr animation $ \raw_animation ->
      sendMsg nsAnimation (mkSelector "startWhenAnimation:reachesProgress:") retVoid [argPtr (castPtr raw_animation :: Ptr ()), argCFloat startProgress]

-- | @- stopWhenAnimation:reachesProgress:@
stopWhenAnimation_reachesProgress :: (IsNSAnimation nsAnimation, IsNSAnimation animation) => nsAnimation -> animation -> CFloat -> IO ()
stopWhenAnimation_reachesProgress nsAnimation  animation stopProgress =
  withObjCPtr animation $ \raw_animation ->
      sendMsg nsAnimation (mkSelector "stopWhenAnimation:reachesProgress:") retVoid [argPtr (castPtr raw_animation :: Ptr ()), argCFloat stopProgress]

-- | @- clearStartAnimation@
clearStartAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
clearStartAnimation nsAnimation  =
    sendMsg nsAnimation (mkSelector "clearStartAnimation") retVoid []

-- | @- clearStopAnimation@
clearStopAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
clearStopAnimation nsAnimation  =
    sendMsg nsAnimation (mkSelector "clearStopAnimation") retVoid []

-- | @- animating@
animating :: IsNSAnimation nsAnimation => nsAnimation -> IO Bool
animating nsAnimation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAnimation (mkSelector "animating") retCULong []

-- | @- currentProgress@
currentProgress :: IsNSAnimation nsAnimation => nsAnimation -> IO CFloat
currentProgress nsAnimation  =
    sendMsg nsAnimation (mkSelector "currentProgress") retCFloat []

-- | @- setCurrentProgress:@
setCurrentProgress :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
setCurrentProgress nsAnimation  value =
    sendMsg nsAnimation (mkSelector "setCurrentProgress:") retVoid [argCFloat value]

-- | @- duration@
duration :: IsNSAnimation nsAnimation => nsAnimation -> IO CDouble
duration nsAnimation  =
    sendMsg nsAnimation (mkSelector "duration") retCDouble []

-- | @- setDuration:@
setDuration :: IsNSAnimation nsAnimation => nsAnimation -> CDouble -> IO ()
setDuration nsAnimation  value =
    sendMsg nsAnimation (mkSelector "setDuration:") retVoid [argCDouble value]

-- | @- animationBlockingMode@
animationBlockingMode :: IsNSAnimation nsAnimation => nsAnimation -> IO NSAnimationBlockingMode
animationBlockingMode nsAnimation  =
    fmap (coerce :: CULong -> NSAnimationBlockingMode) $ sendMsg nsAnimation (mkSelector "animationBlockingMode") retCULong []

-- | @- setAnimationBlockingMode:@
setAnimationBlockingMode :: IsNSAnimation nsAnimation => nsAnimation -> NSAnimationBlockingMode -> IO ()
setAnimationBlockingMode nsAnimation  value =
    sendMsg nsAnimation (mkSelector "setAnimationBlockingMode:") retVoid [argCULong (coerce value)]

-- | @- frameRate@
frameRate :: IsNSAnimation nsAnimation => nsAnimation -> IO CFloat
frameRate nsAnimation  =
    sendMsg nsAnimation (mkSelector "frameRate") retCFloat []

-- | @- setFrameRate:@
setFrameRate :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
setFrameRate nsAnimation  value =
    sendMsg nsAnimation (mkSelector "setFrameRate:") retVoid [argCFloat value]

-- | @- animationCurve@
animationCurve :: IsNSAnimation nsAnimation => nsAnimation -> IO NSAnimationCurve
animationCurve nsAnimation  =
    fmap (coerce :: CULong -> NSAnimationCurve) $ sendMsg nsAnimation (mkSelector "animationCurve") retCULong []

-- | @- setAnimationCurve:@
setAnimationCurve :: IsNSAnimation nsAnimation => nsAnimation -> NSAnimationCurve -> IO ()
setAnimationCurve nsAnimation  value =
    sendMsg nsAnimation (mkSelector "setAnimationCurve:") retVoid [argCULong (coerce value)]

-- | @- currentValue@
currentValue :: IsNSAnimation nsAnimation => nsAnimation -> IO CFloat
currentValue nsAnimation  =
    sendMsg nsAnimation (mkSelector "currentValue") retCFloat []

-- | @- delegate@
delegate :: IsNSAnimation nsAnimation => nsAnimation -> IO RawId
delegate nsAnimation  =
    fmap (RawId . castPtr) $ sendMsg nsAnimation (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSAnimation nsAnimation => nsAnimation -> RawId -> IO ()
setDelegate nsAnimation  value =
    sendMsg nsAnimation (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- progressMarks@
progressMarks :: IsNSAnimation nsAnimation => nsAnimation -> IO (Id NSArray)
progressMarks nsAnimation  =
    sendMsg nsAnimation (mkSelector "progressMarks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProgressMarks:@
setProgressMarks :: (IsNSAnimation nsAnimation, IsNSArray value) => nsAnimation -> value -> IO ()
setProgressMarks nsAnimation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsAnimation (mkSelector "setProgressMarks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- runLoopModesForAnimating@
runLoopModesForAnimating :: IsNSAnimation nsAnimation => nsAnimation -> IO (Id NSArray)
runLoopModesForAnimating nsAnimation  =
    sendMsg nsAnimation (mkSelector "runLoopModesForAnimating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDuration:animationCurve:@
initWithDuration_animationCurveSelector :: Selector
initWithDuration_animationCurveSelector = mkSelector "initWithDuration:animationCurve:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @startAnimation@
startAnimationSelector :: Selector
startAnimationSelector = mkSelector "startAnimation"

-- | @Selector@ for @stopAnimation@
stopAnimationSelector :: Selector
stopAnimationSelector = mkSelector "stopAnimation"

-- | @Selector@ for @addProgressMark:@
addProgressMarkSelector :: Selector
addProgressMarkSelector = mkSelector "addProgressMark:"

-- | @Selector@ for @removeProgressMark:@
removeProgressMarkSelector :: Selector
removeProgressMarkSelector = mkSelector "removeProgressMark:"

-- | @Selector@ for @startWhenAnimation:reachesProgress:@
startWhenAnimation_reachesProgressSelector :: Selector
startWhenAnimation_reachesProgressSelector = mkSelector "startWhenAnimation:reachesProgress:"

-- | @Selector@ for @stopWhenAnimation:reachesProgress:@
stopWhenAnimation_reachesProgressSelector :: Selector
stopWhenAnimation_reachesProgressSelector = mkSelector "stopWhenAnimation:reachesProgress:"

-- | @Selector@ for @clearStartAnimation@
clearStartAnimationSelector :: Selector
clearStartAnimationSelector = mkSelector "clearStartAnimation"

-- | @Selector@ for @clearStopAnimation@
clearStopAnimationSelector :: Selector
clearStopAnimationSelector = mkSelector "clearStopAnimation"

-- | @Selector@ for @animating@
animatingSelector :: Selector
animatingSelector = mkSelector "animating"

-- | @Selector@ for @currentProgress@
currentProgressSelector :: Selector
currentProgressSelector = mkSelector "currentProgress"

-- | @Selector@ for @setCurrentProgress:@
setCurrentProgressSelector :: Selector
setCurrentProgressSelector = mkSelector "setCurrentProgress:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @animationBlockingMode@
animationBlockingModeSelector :: Selector
animationBlockingModeSelector = mkSelector "animationBlockingMode"

-- | @Selector@ for @setAnimationBlockingMode:@
setAnimationBlockingModeSelector :: Selector
setAnimationBlockingModeSelector = mkSelector "setAnimationBlockingMode:"

-- | @Selector@ for @frameRate@
frameRateSelector :: Selector
frameRateSelector = mkSelector "frameRate"

-- | @Selector@ for @setFrameRate:@
setFrameRateSelector :: Selector
setFrameRateSelector = mkSelector "setFrameRate:"

-- | @Selector@ for @animationCurve@
animationCurveSelector :: Selector
animationCurveSelector = mkSelector "animationCurve"

-- | @Selector@ for @setAnimationCurve:@
setAnimationCurveSelector :: Selector
setAnimationCurveSelector = mkSelector "setAnimationCurve:"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @progressMarks@
progressMarksSelector :: Selector
progressMarksSelector = mkSelector "progressMarks"

-- | @Selector@ for @setProgressMarks:@
setProgressMarksSelector :: Selector
setProgressMarksSelector = mkSelector "setProgressMarks:"

-- | @Selector@ for @runLoopModesForAnimating@
runLoopModesForAnimatingSelector :: Selector
runLoopModesForAnimatingSelector = mkSelector "runLoopModesForAnimating"

