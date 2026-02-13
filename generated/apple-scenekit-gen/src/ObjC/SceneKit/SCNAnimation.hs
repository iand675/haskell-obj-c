{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNAnimation represents an animation that targets a specific key path.
--
-- Generated bindings for @SCNAnimation@.
module ObjC.SceneKit.SCNAnimation
  ( SCNAnimation
  , IsSCNAnimation(..)
  , animationWithContentsOfURL
  , animationNamed
  , animationWithCAAnimation
  , duration
  , setDuration
  , keyPath
  , setKeyPath
  , timingFunction
  , setTimingFunction
  , blendInDuration
  , setBlendInDuration
  , blendOutDuration
  , setBlendOutDuration
  , removedOnCompletion
  , setRemovedOnCompletion
  , appliedOnCompletion
  , setAppliedOnCompletion
  , repeatCount
  , setRepeatCount
  , autoreverses
  , setAutoreverses
  , startDelay
  , setStartDelay
  , timeOffset
  , setTimeOffset
  , fillsForward
  , setFillsForward
  , fillsBackward
  , setFillsBackward
  , usesSceneTimeBase
  , setUsesSceneTimeBase
  , animationDidStart
  , setAnimationDidStart
  , animationDidStop
  , setAnimationDidStop
  , animationEvents
  , setAnimationEvents
  , additive
  , setAdditive
  , cumulative
  , setCumulative
  , additiveSelector
  , animationDidStartSelector
  , animationDidStopSelector
  , animationEventsSelector
  , animationNamedSelector
  , animationWithCAAnimationSelector
  , animationWithContentsOfURLSelector
  , appliedOnCompletionSelector
  , autoreversesSelector
  , blendInDurationSelector
  , blendOutDurationSelector
  , cumulativeSelector
  , durationSelector
  , fillsBackwardSelector
  , fillsForwardSelector
  , keyPathSelector
  , removedOnCompletionSelector
  , repeatCountSelector
  , setAdditiveSelector
  , setAnimationDidStartSelector
  , setAnimationDidStopSelector
  , setAnimationEventsSelector
  , setAppliedOnCompletionSelector
  , setAutoreversesSelector
  , setBlendInDurationSelector
  , setBlendOutDurationSelector
  , setCumulativeSelector
  , setDurationSelector
  , setFillsBackwardSelector
  , setFillsForwardSelector
  , setKeyPathSelector
  , setRemovedOnCompletionSelector
  , setRepeatCountSelector
  , setStartDelaySelector
  , setTimeOffsetSelector
  , setTimingFunctionSelector
  , setUsesSceneTimeBaseSelector
  , startDelaySelector
  , timeOffsetSelector
  , timingFunctionSelector
  , usesSceneTimeBaseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | Loads and returns an animation loaded from the specified URL.
--
-- @animationUrl@ — The url to load.
--
-- ObjC selector: @+ animationWithContentsOfURL:@
animationWithContentsOfURL :: IsNSURL animationUrl => animationUrl -> IO (Id SCNAnimation)
animationWithContentsOfURL animationUrl =
  do
    cls' <- getRequiredClass "SCNAnimation"
    sendClassMessage cls' animationWithContentsOfURLSelector (toNSURL animationUrl)

-- | Loads and returns the animation with the specified name in the current application bundle.
--
-- @animationName@ — The name of the animation to load.
--
-- ObjC selector: @+ animationNamed:@
animationNamed :: IsNSString animationName => animationName -> IO (Id SCNAnimation)
animationNamed animationName =
  do
    cls' <- getRequiredClass "SCNAnimation"
    sendClassMessage cls' animationNamedSelector (toNSString animationName)

-- | Returns a SCNAnimation initialized from a CAAnimation.
--
-- @caAnimation@ — The CAAnimation to initialize from.
--
-- Only CABasicAnimation, CAKeyframeAnimation and CAAnimationGroup are currently supported.
--
-- ObjC selector: @+ animationWithCAAnimation:@
animationWithCAAnimation :: IsCAAnimation caAnimation => caAnimation -> IO (Id SCNAnimation)
animationWithCAAnimation caAnimation =
  do
    cls' <- getRequiredClass "SCNAnimation"
    sendClassMessage cls' animationWithCAAnimationSelector (toCAAnimation caAnimation)

-- | The duration of the animation in seconds. Defaults to 0.
--
-- ObjC selector: @- duration@
duration :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
duration scnAnimation =
  sendMessage scnAnimation durationSelector

-- | The duration of the animation in seconds. Defaults to 0.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setDuration scnAnimation value =
  sendMessage scnAnimation setDurationSelector value

-- | The key-path describing the property to be animated for single-property animations, nil for animations targetting multiple nodes. defaults to nil. The key-path uses the KVC syntax. It's also possible to target a specific sub-node with the following syntax:    /<node-name>.property1.property2.field    (field is optional, <node-name> is the name of the targeted node).
--
-- ObjC selector: @- keyPath@
keyPath :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Id NSString)
keyPath scnAnimation =
  sendMessage scnAnimation keyPathSelector

-- | The key-path describing the property to be animated for single-property animations, nil for animations targetting multiple nodes. defaults to nil. The key-path uses the KVC syntax. It's also possible to target a specific sub-node with the following syntax:    /<node-name>.property1.property2.field    (field is optional, <node-name> is the name of the targeted node).
--
-- ObjC selector: @- setKeyPath:@
setKeyPath :: (IsSCNAnimation scnAnimation, IsNSString value) => scnAnimation -> value -> IO ()
setKeyPath scnAnimation value =
  sendMessage scnAnimation setKeyPathSelector (toNSString value)

-- | A timing function defining the pacing of the animation. Defaults to nil indicating linear pacing.
--
-- ObjC selector: @- timingFunction@
timingFunction :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Id SCNTimingFunction)
timingFunction scnAnimation =
  sendMessage scnAnimation timingFunctionSelector

-- | A timing function defining the pacing of the animation. Defaults to nil indicating linear pacing.
--
-- ObjC selector: @- setTimingFunction:@
setTimingFunction :: (IsSCNAnimation scnAnimation, IsSCNTimingFunction value) => scnAnimation -> value -> IO ()
setTimingFunction scnAnimation value =
  sendMessage scnAnimation setTimingFunctionSelector (toSCNTimingFunction value)

-- | Determines the receiver's blend-in duration.
--
-- When the blendInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- blendInDuration@
blendInDuration :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
blendInDuration scnAnimation =
  sendMessage scnAnimation blendInDurationSelector

-- | Determines the receiver's blend-in duration.
--
-- When the blendInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- setBlendInDuration:@
setBlendInDuration :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setBlendInDuration scnAnimation value =
  sendMessage scnAnimation setBlendInDurationSelector value

-- | Determines the receiver's blend-out duration.
--
-- When the blendOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- blendOutDuration@
blendOutDuration :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
blendOutDuration scnAnimation =
  sendMessage scnAnimation blendOutDurationSelector

-- | Determines the receiver's blend-out duration.
--
-- When the blendOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- setBlendOutDuration:@
setBlendOutDuration :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setBlendOutDuration scnAnimation value =
  sendMessage scnAnimation setBlendOutDurationSelector value

-- | When true, the animation is removed from the render tree once its active duration has passed. Defaults to YES.
--
-- ObjC selector: @- removedOnCompletion@
removedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
removedOnCompletion scnAnimation =
  sendMessage scnAnimation removedOnCompletionSelector

-- | When true, the animation is removed from the render tree once its active duration has passed. Defaults to YES.
--
-- ObjC selector: @- setRemovedOnCompletion:@
setRemovedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setRemovedOnCompletion scnAnimation value =
  sendMessage scnAnimation setRemovedOnCompletionSelector value

-- | When true, the animation is applied to the model tree once its active duration has passed. Defaults to NO.
--
-- ObjC selector: @- appliedOnCompletion@
appliedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
appliedOnCompletion scnAnimation =
  sendMessage scnAnimation appliedOnCompletionSelector

-- | When true, the animation is applied to the model tree once its active duration has passed. Defaults to NO.
--
-- ObjC selector: @- setAppliedOnCompletion:@
setAppliedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setAppliedOnCompletion scnAnimation value =
  sendMessage scnAnimation setAppliedOnCompletionSelector value

-- | The repeat count of the object. May be fractional. Defaults to 0.
--
-- ObjC selector: @- repeatCount@
repeatCount :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
repeatCount scnAnimation =
  sendMessage scnAnimation repeatCountSelector

-- | The repeat count of the object. May be fractional. Defaults to 0.
--
-- ObjC selector: @- setRepeatCount:@
setRepeatCount :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setRepeatCount scnAnimation value =
  sendMessage scnAnimation setRepeatCountSelector value

-- | When true, the object plays backwards after playing forwards. Defaults to NO.
--
-- ObjC selector: @- autoreverses@
autoreverses :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
autoreverses scnAnimation =
  sendMessage scnAnimation autoreversesSelector

-- | When true, the object plays backwards after playing forwards. Defaults to NO.
--
-- ObjC selector: @- setAutoreverses:@
setAutoreverses :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setAutoreverses scnAnimation value =
  sendMessage scnAnimation setAutoreversesSelector value

-- | The relative delay to start the animation, in relation to its parent animation if applicable. Defaults to 0.
--
-- This property is bridged with CoreAnimations's beginTime. However, for top level animations, startDelay is relative to the current time (unlike CAAnimation's beginTime that is absolute). So if a CAAnimation has a non-zero beginTime, startDelay is initialized as caAnimation.beginTime - CACurrentMediaTime().
--
-- ObjC selector: @- startDelay@
startDelay :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
startDelay scnAnimation =
  sendMessage scnAnimation startDelaySelector

-- | The relative delay to start the animation, in relation to its parent animation if applicable. Defaults to 0.
--
-- This property is bridged with CoreAnimations's beginTime. However, for top level animations, startDelay is relative to the current time (unlike CAAnimation's beginTime that is absolute). So if a CAAnimation has a non-zero beginTime, startDelay is initialized as caAnimation.beginTime - CACurrentMediaTime().
--
-- ObjC selector: @- setStartDelay:@
setStartDelay :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setStartDelay scnAnimation value =
  sendMessage scnAnimation setStartDelaySelector value

-- | Additional offset in active local time. i.e. to convert from parent time tp to active local time t: t = (tp - begin) * speed + offset. Defaults to 0.
--
-- ObjC selector: @- timeOffset@
timeOffset :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
timeOffset scnAnimation =
  sendMessage scnAnimation timeOffsetSelector

-- | Additional offset in active local time. i.e. to convert from parent time tp to active local time t: t = (tp - begin) * speed + offset. Defaults to 0.
--
-- ObjC selector: @- setTimeOffset:@
setTimeOffset :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setTimeOffset scnAnimation value =
  sendMessage scnAnimation setTimeOffsetSelector value

-- | When true, the animation remains active after its active duration and evaluates to its end value. Defaults to NO.
--
-- ObjC selector: @- fillsForward@
fillsForward :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
fillsForward scnAnimation =
  sendMessage scnAnimation fillsForwardSelector

-- | When true, the animation remains active after its active duration and evaluates to its end value. Defaults to NO.
--
-- ObjC selector: @- setFillsForward:@
setFillsForward :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setFillsForward scnAnimation value =
  sendMessage scnAnimation setFillsForwardSelector value

-- | When true, the animation is active before its active duration and evaluates to its start value. Defaults to NO.
--
-- ObjC selector: @- fillsBackward@
fillsBackward :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
fillsBackward scnAnimation =
  sendMessage scnAnimation fillsBackwardSelector

-- | When true, the animation is active before its active duration and evaluates to its start value. Defaults to NO.
--
-- ObjC selector: @- setFillsBackward:@
setFillsBackward :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setFillsBackward scnAnimation value =
  sendMessage scnAnimation setFillsBackwardSelector value

-- | Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene. The "sceneTime" base is typically used by players or editors that need to preview, edit and being able to change the evaluation time.
--
-- See: SCNSceneSourceAnimationImportPolicyKey
--
-- ObjC selector: @- usesSceneTimeBase@
usesSceneTimeBase :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
usesSceneTimeBase scnAnimation =
  sendMessage scnAnimation usesSceneTimeBaseSelector

-- | Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene. The "sceneTime" base is typically used by players or editors that need to preview, edit and being able to change the evaluation time.
--
-- See: SCNSceneSourceAnimationImportPolicyKey
--
-- ObjC selector: @- setUsesSceneTimeBase:@
setUsesSceneTimeBase :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setUsesSceneTimeBase scnAnimation value =
  sendMessage scnAnimation setUsesSceneTimeBaseSelector value

-- | Called when the animation starts.
--
-- ObjC selector: @- animationDidStart@
animationDidStart :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Ptr ())
animationDidStart scnAnimation =
  sendMessage scnAnimation animationDidStartSelector

-- | Called when the animation starts.
--
-- ObjC selector: @- setAnimationDidStart:@
setAnimationDidStart :: IsSCNAnimation scnAnimation => scnAnimation -> Ptr () -> IO ()
setAnimationDidStart scnAnimation value =
  sendMessage scnAnimation setAnimationDidStartSelector value

-- | Called when the animation either completes its active duration or is removed from the object it is attached to (i.e. the layer). The 'completed' argument of SCNAnimationDidStopBlock is true if the animation reached the end of its active duration without being removed.
--
-- ObjC selector: @- animationDidStop@
animationDidStop :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Ptr ())
animationDidStop scnAnimation =
  sendMessage scnAnimation animationDidStopSelector

-- | Called when the animation either completes its active duration or is removed from the object it is attached to (i.e. the layer). The 'completed' argument of SCNAnimationDidStopBlock is true if the animation reached the end of its active duration without being removed.
--
-- ObjC selector: @- setAnimationDidStop:@
setAnimationDidStop :: IsSCNAnimation scnAnimation => scnAnimation -> Ptr () -> IO ()
setAnimationDidStop scnAnimation value =
  sendMessage scnAnimation setAnimationDidStopSelector value

-- | Specifies the animation events attached to the receiver.
--
-- See: SCNAnimationEvent
--
-- ObjC selector: @- animationEvents@
animationEvents :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Id NSArray)
animationEvents scnAnimation =
  sendMessage scnAnimation animationEventsSelector

-- | Specifies the animation events attached to the receiver.
--
-- See: SCNAnimationEvent
--
-- ObjC selector: @- setAnimationEvents:@
setAnimationEvents :: (IsSCNAnimation scnAnimation, IsNSArray value) => scnAnimation -> value -> IO ()
setAnimationEvents scnAnimation value =
  sendMessage scnAnimation setAnimationEventsSelector (toNSArray value)

-- | When true the value specified by the animation will be "added" to the current presentation value of the property to produce the new presentation value. The addition function is type-dependent, e.g. for affine transforms the two matrices are concatenated. Defaults to NO.
--
-- ObjC selector: @- additive@
additive :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
additive scnAnimation =
  sendMessage scnAnimation additiveSelector

-- | When true the value specified by the animation will be "added" to the current presentation value of the property to produce the new presentation value. The addition function is type-dependent, e.g. for affine transforms the two matrices are concatenated. Defaults to NO.
--
-- ObjC selector: @- setAdditive:@
setAdditive :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setAdditive scnAnimation value =
  sendMessage scnAnimation setAdditiveSelector value

-- | The `cumulative' property affects how repeating animations produce their result. If true then the current value of the animation is the value at the end of the previous repeat cycle, plus the value of the current repeat cycle. If false, the value is simply the value calculated for the current repeat cycle. Defaults to NO.
--
-- ObjC selector: @- cumulative@
cumulative :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
cumulative scnAnimation =
  sendMessage scnAnimation cumulativeSelector

-- | The `cumulative' property affects how repeating animations produce their result. If true then the current value of the animation is the value at the end of the previous repeat cycle, plus the value of the current repeat cycle. If false, the value is simply the value calculated for the current repeat cycle. Defaults to NO.
--
-- ObjC selector: @- setCumulative:@
setCumulative :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setCumulative scnAnimation value =
  sendMessage scnAnimation setCumulativeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationWithContentsOfURL:@
animationWithContentsOfURLSelector :: Selector '[Id NSURL] (Id SCNAnimation)
animationWithContentsOfURLSelector = mkSelector "animationWithContentsOfURL:"

-- | @Selector@ for @animationNamed:@
animationNamedSelector :: Selector '[Id NSString] (Id SCNAnimation)
animationNamedSelector = mkSelector "animationNamed:"

-- | @Selector@ for @animationWithCAAnimation:@
animationWithCAAnimationSelector :: Selector '[Id CAAnimation] (Id SCNAnimation)
animationWithCAAnimationSelector = mkSelector "animationWithCAAnimation:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector '[] (Id NSString)
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @setKeyPath:@
setKeyPathSelector :: Selector '[Id NSString] ()
setKeyPathSelector = mkSelector "setKeyPath:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector '[] (Id SCNTimingFunction)
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector '[Id SCNTimingFunction] ()
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @blendInDuration@
blendInDurationSelector :: Selector '[] CDouble
blendInDurationSelector = mkSelector "blendInDuration"

-- | @Selector@ for @setBlendInDuration:@
setBlendInDurationSelector :: Selector '[CDouble] ()
setBlendInDurationSelector = mkSelector "setBlendInDuration:"

-- | @Selector@ for @blendOutDuration@
blendOutDurationSelector :: Selector '[] CDouble
blendOutDurationSelector = mkSelector "blendOutDuration"

-- | @Selector@ for @setBlendOutDuration:@
setBlendOutDurationSelector :: Selector '[CDouble] ()
setBlendOutDurationSelector = mkSelector "setBlendOutDuration:"

-- | @Selector@ for @removedOnCompletion@
removedOnCompletionSelector :: Selector '[] Bool
removedOnCompletionSelector = mkSelector "removedOnCompletion"

-- | @Selector@ for @setRemovedOnCompletion:@
setRemovedOnCompletionSelector :: Selector '[Bool] ()
setRemovedOnCompletionSelector = mkSelector "setRemovedOnCompletion:"

-- | @Selector@ for @appliedOnCompletion@
appliedOnCompletionSelector :: Selector '[] Bool
appliedOnCompletionSelector = mkSelector "appliedOnCompletion"

-- | @Selector@ for @setAppliedOnCompletion:@
setAppliedOnCompletionSelector :: Selector '[Bool] ()
setAppliedOnCompletionSelector = mkSelector "setAppliedOnCompletion:"

-- | @Selector@ for @repeatCount@
repeatCountSelector :: Selector '[] CDouble
repeatCountSelector = mkSelector "repeatCount"

-- | @Selector@ for @setRepeatCount:@
setRepeatCountSelector :: Selector '[CDouble] ()
setRepeatCountSelector = mkSelector "setRepeatCount:"

-- | @Selector@ for @autoreverses@
autoreversesSelector :: Selector '[] Bool
autoreversesSelector = mkSelector "autoreverses"

-- | @Selector@ for @setAutoreverses:@
setAutoreversesSelector :: Selector '[Bool] ()
setAutoreversesSelector = mkSelector "setAutoreverses:"

-- | @Selector@ for @startDelay@
startDelaySelector :: Selector '[] CDouble
startDelaySelector = mkSelector "startDelay"

-- | @Selector@ for @setStartDelay:@
setStartDelaySelector :: Selector '[CDouble] ()
setStartDelaySelector = mkSelector "setStartDelay:"

-- | @Selector@ for @timeOffset@
timeOffsetSelector :: Selector '[] CDouble
timeOffsetSelector = mkSelector "timeOffset"

-- | @Selector@ for @setTimeOffset:@
setTimeOffsetSelector :: Selector '[CDouble] ()
setTimeOffsetSelector = mkSelector "setTimeOffset:"

-- | @Selector@ for @fillsForward@
fillsForwardSelector :: Selector '[] Bool
fillsForwardSelector = mkSelector "fillsForward"

-- | @Selector@ for @setFillsForward:@
setFillsForwardSelector :: Selector '[Bool] ()
setFillsForwardSelector = mkSelector "setFillsForward:"

-- | @Selector@ for @fillsBackward@
fillsBackwardSelector :: Selector '[] Bool
fillsBackwardSelector = mkSelector "fillsBackward"

-- | @Selector@ for @setFillsBackward:@
setFillsBackwardSelector :: Selector '[Bool] ()
setFillsBackwardSelector = mkSelector "setFillsBackward:"

-- | @Selector@ for @usesSceneTimeBase@
usesSceneTimeBaseSelector :: Selector '[] Bool
usesSceneTimeBaseSelector = mkSelector "usesSceneTimeBase"

-- | @Selector@ for @setUsesSceneTimeBase:@
setUsesSceneTimeBaseSelector :: Selector '[Bool] ()
setUsesSceneTimeBaseSelector = mkSelector "setUsesSceneTimeBase:"

-- | @Selector@ for @animationDidStart@
animationDidStartSelector :: Selector '[] (Ptr ())
animationDidStartSelector = mkSelector "animationDidStart"

-- | @Selector@ for @setAnimationDidStart:@
setAnimationDidStartSelector :: Selector '[Ptr ()] ()
setAnimationDidStartSelector = mkSelector "setAnimationDidStart:"

-- | @Selector@ for @animationDidStop@
animationDidStopSelector :: Selector '[] (Ptr ())
animationDidStopSelector = mkSelector "animationDidStop"

-- | @Selector@ for @setAnimationDidStop:@
setAnimationDidStopSelector :: Selector '[Ptr ()] ()
setAnimationDidStopSelector = mkSelector "setAnimationDidStop:"

-- | @Selector@ for @animationEvents@
animationEventsSelector :: Selector '[] (Id NSArray)
animationEventsSelector = mkSelector "animationEvents"

-- | @Selector@ for @setAnimationEvents:@
setAnimationEventsSelector :: Selector '[Id NSArray] ()
setAnimationEventsSelector = mkSelector "setAnimationEvents:"

-- | @Selector@ for @additive@
additiveSelector :: Selector '[] Bool
additiveSelector = mkSelector "additive"

-- | @Selector@ for @setAdditive:@
setAdditiveSelector :: Selector '[Bool] ()
setAdditiveSelector = mkSelector "setAdditive:"

-- | @Selector@ for @cumulative@
cumulativeSelector :: Selector '[] Bool
cumulativeSelector = mkSelector "cumulative"

-- | @Selector@ for @setCumulative:@
setCumulativeSelector :: Selector '[Bool] ()
setCumulativeSelector = mkSelector "setCumulative:"

