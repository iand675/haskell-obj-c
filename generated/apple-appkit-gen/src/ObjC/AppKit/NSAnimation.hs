{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addProgressMarkSelector
  , animatingSelector
  , animationBlockingModeSelector
  , animationCurveSelector
  , clearStartAnimationSelector
  , clearStopAnimationSelector
  , currentProgressSelector
  , currentValueSelector
  , delegateSelector
  , durationSelector
  , frameRateSelector
  , initWithCoderSelector
  , initWithDuration_animationCurveSelector
  , progressMarksSelector
  , removeProgressMarkSelector
  , runLoopModesForAnimatingSelector
  , setAnimationBlockingModeSelector
  , setAnimationCurveSelector
  , setCurrentProgressSelector
  , setDelegateSelector
  , setDurationSelector
  , setFrameRateSelector
  , setProgressMarksSelector
  , startAnimationSelector
  , startWhenAnimation_reachesProgressSelector
  , stopAnimationSelector
  , stopWhenAnimation_reachesProgressSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDuration:animationCurve:@
initWithDuration_animationCurve :: IsNSAnimation nsAnimation => nsAnimation -> CDouble -> NSAnimationCurve -> IO (Id NSAnimation)
initWithDuration_animationCurve nsAnimation duration animationCurve =
  sendOwnedMessage nsAnimation initWithDuration_animationCurveSelector duration animationCurve

-- | @- initWithCoder:@
initWithCoder :: (IsNSAnimation nsAnimation, IsNSCoder coder) => nsAnimation -> coder -> IO (Id NSAnimation)
initWithCoder nsAnimation coder =
  sendOwnedMessage nsAnimation initWithCoderSelector (toNSCoder coder)

-- | @- startAnimation@
startAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
startAnimation nsAnimation =
  sendMessage nsAnimation startAnimationSelector

-- | @- stopAnimation@
stopAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
stopAnimation nsAnimation =
  sendMessage nsAnimation stopAnimationSelector

-- | @- addProgressMark:@
addProgressMark :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
addProgressMark nsAnimation progressMark =
  sendMessage nsAnimation addProgressMarkSelector progressMark

-- | @- removeProgressMark:@
removeProgressMark :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
removeProgressMark nsAnimation progressMark =
  sendMessage nsAnimation removeProgressMarkSelector progressMark

-- | @- startWhenAnimation:reachesProgress:@
startWhenAnimation_reachesProgress :: (IsNSAnimation nsAnimation, IsNSAnimation animation) => nsAnimation -> animation -> CFloat -> IO ()
startWhenAnimation_reachesProgress nsAnimation animation startProgress =
  sendMessage nsAnimation startWhenAnimation_reachesProgressSelector (toNSAnimation animation) startProgress

-- | @- stopWhenAnimation:reachesProgress:@
stopWhenAnimation_reachesProgress :: (IsNSAnimation nsAnimation, IsNSAnimation animation) => nsAnimation -> animation -> CFloat -> IO ()
stopWhenAnimation_reachesProgress nsAnimation animation stopProgress =
  sendMessage nsAnimation stopWhenAnimation_reachesProgressSelector (toNSAnimation animation) stopProgress

-- | @- clearStartAnimation@
clearStartAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
clearStartAnimation nsAnimation =
  sendMessage nsAnimation clearStartAnimationSelector

-- | @- clearStopAnimation@
clearStopAnimation :: IsNSAnimation nsAnimation => nsAnimation -> IO ()
clearStopAnimation nsAnimation =
  sendMessage nsAnimation clearStopAnimationSelector

-- | @- animating@
animating :: IsNSAnimation nsAnimation => nsAnimation -> IO Bool
animating nsAnimation =
  sendMessage nsAnimation animatingSelector

-- | @- currentProgress@
currentProgress :: IsNSAnimation nsAnimation => nsAnimation -> IO CFloat
currentProgress nsAnimation =
  sendMessage nsAnimation currentProgressSelector

-- | @- setCurrentProgress:@
setCurrentProgress :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
setCurrentProgress nsAnimation value =
  sendMessage nsAnimation setCurrentProgressSelector value

-- | @- duration@
duration :: IsNSAnimation nsAnimation => nsAnimation -> IO CDouble
duration nsAnimation =
  sendMessage nsAnimation durationSelector

-- | @- setDuration:@
setDuration :: IsNSAnimation nsAnimation => nsAnimation -> CDouble -> IO ()
setDuration nsAnimation value =
  sendMessage nsAnimation setDurationSelector value

-- | @- animationBlockingMode@
animationBlockingMode :: IsNSAnimation nsAnimation => nsAnimation -> IO NSAnimationBlockingMode
animationBlockingMode nsAnimation =
  sendMessage nsAnimation animationBlockingModeSelector

-- | @- setAnimationBlockingMode:@
setAnimationBlockingMode :: IsNSAnimation nsAnimation => nsAnimation -> NSAnimationBlockingMode -> IO ()
setAnimationBlockingMode nsAnimation value =
  sendMessage nsAnimation setAnimationBlockingModeSelector value

-- | @- frameRate@
frameRate :: IsNSAnimation nsAnimation => nsAnimation -> IO CFloat
frameRate nsAnimation =
  sendMessage nsAnimation frameRateSelector

-- | @- setFrameRate:@
setFrameRate :: IsNSAnimation nsAnimation => nsAnimation -> CFloat -> IO ()
setFrameRate nsAnimation value =
  sendMessage nsAnimation setFrameRateSelector value

-- | @- animationCurve@
animationCurve :: IsNSAnimation nsAnimation => nsAnimation -> IO NSAnimationCurve
animationCurve nsAnimation =
  sendMessage nsAnimation animationCurveSelector

-- | @- setAnimationCurve:@
setAnimationCurve :: IsNSAnimation nsAnimation => nsAnimation -> NSAnimationCurve -> IO ()
setAnimationCurve nsAnimation value =
  sendMessage nsAnimation setAnimationCurveSelector value

-- | @- currentValue@
currentValue :: IsNSAnimation nsAnimation => nsAnimation -> IO CFloat
currentValue nsAnimation =
  sendMessage nsAnimation currentValueSelector

-- | @- delegate@
delegate :: IsNSAnimation nsAnimation => nsAnimation -> IO RawId
delegate nsAnimation =
  sendMessage nsAnimation delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSAnimation nsAnimation => nsAnimation -> RawId -> IO ()
setDelegate nsAnimation value =
  sendMessage nsAnimation setDelegateSelector value

-- | @- progressMarks@
progressMarks :: IsNSAnimation nsAnimation => nsAnimation -> IO (Id NSArray)
progressMarks nsAnimation =
  sendMessage nsAnimation progressMarksSelector

-- | @- setProgressMarks:@
setProgressMarks :: (IsNSAnimation nsAnimation, IsNSArray value) => nsAnimation -> value -> IO ()
setProgressMarks nsAnimation value =
  sendMessage nsAnimation setProgressMarksSelector (toNSArray value)

-- | @- runLoopModesForAnimating@
runLoopModesForAnimating :: IsNSAnimation nsAnimation => nsAnimation -> IO (Id NSArray)
runLoopModesForAnimating nsAnimation =
  sendMessage nsAnimation runLoopModesForAnimatingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDuration:animationCurve:@
initWithDuration_animationCurveSelector :: Selector '[CDouble, NSAnimationCurve] (Id NSAnimation)
initWithDuration_animationCurveSelector = mkSelector "initWithDuration:animationCurve:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSAnimation)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @startAnimation@
startAnimationSelector :: Selector '[] ()
startAnimationSelector = mkSelector "startAnimation"

-- | @Selector@ for @stopAnimation@
stopAnimationSelector :: Selector '[] ()
stopAnimationSelector = mkSelector "stopAnimation"

-- | @Selector@ for @addProgressMark:@
addProgressMarkSelector :: Selector '[CFloat] ()
addProgressMarkSelector = mkSelector "addProgressMark:"

-- | @Selector@ for @removeProgressMark:@
removeProgressMarkSelector :: Selector '[CFloat] ()
removeProgressMarkSelector = mkSelector "removeProgressMark:"

-- | @Selector@ for @startWhenAnimation:reachesProgress:@
startWhenAnimation_reachesProgressSelector :: Selector '[Id NSAnimation, CFloat] ()
startWhenAnimation_reachesProgressSelector = mkSelector "startWhenAnimation:reachesProgress:"

-- | @Selector@ for @stopWhenAnimation:reachesProgress:@
stopWhenAnimation_reachesProgressSelector :: Selector '[Id NSAnimation, CFloat] ()
stopWhenAnimation_reachesProgressSelector = mkSelector "stopWhenAnimation:reachesProgress:"

-- | @Selector@ for @clearStartAnimation@
clearStartAnimationSelector :: Selector '[] ()
clearStartAnimationSelector = mkSelector "clearStartAnimation"

-- | @Selector@ for @clearStopAnimation@
clearStopAnimationSelector :: Selector '[] ()
clearStopAnimationSelector = mkSelector "clearStopAnimation"

-- | @Selector@ for @animating@
animatingSelector :: Selector '[] Bool
animatingSelector = mkSelector "animating"

-- | @Selector@ for @currentProgress@
currentProgressSelector :: Selector '[] CFloat
currentProgressSelector = mkSelector "currentProgress"

-- | @Selector@ for @setCurrentProgress:@
setCurrentProgressSelector :: Selector '[CFloat] ()
setCurrentProgressSelector = mkSelector "setCurrentProgress:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @animationBlockingMode@
animationBlockingModeSelector :: Selector '[] NSAnimationBlockingMode
animationBlockingModeSelector = mkSelector "animationBlockingMode"

-- | @Selector@ for @setAnimationBlockingMode:@
setAnimationBlockingModeSelector :: Selector '[NSAnimationBlockingMode] ()
setAnimationBlockingModeSelector = mkSelector "setAnimationBlockingMode:"

-- | @Selector@ for @frameRate@
frameRateSelector :: Selector '[] CFloat
frameRateSelector = mkSelector "frameRate"

-- | @Selector@ for @setFrameRate:@
setFrameRateSelector :: Selector '[CFloat] ()
setFrameRateSelector = mkSelector "setFrameRate:"

-- | @Selector@ for @animationCurve@
animationCurveSelector :: Selector '[] NSAnimationCurve
animationCurveSelector = mkSelector "animationCurve"

-- | @Selector@ for @setAnimationCurve:@
setAnimationCurveSelector :: Selector '[NSAnimationCurve] ()
setAnimationCurveSelector = mkSelector "setAnimationCurve:"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] CFloat
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @progressMarks@
progressMarksSelector :: Selector '[] (Id NSArray)
progressMarksSelector = mkSelector "progressMarks"

-- | @Selector@ for @setProgressMarks:@
setProgressMarksSelector :: Selector '[Id NSArray] ()
setProgressMarksSelector = mkSelector "setProgressMarks:"

-- | @Selector@ for @runLoopModesForAnimating@
runLoopModesForAnimatingSelector :: Selector '[] (Id NSArray)
runLoopModesForAnimatingSelector = mkSelector "runLoopModesForAnimating"

