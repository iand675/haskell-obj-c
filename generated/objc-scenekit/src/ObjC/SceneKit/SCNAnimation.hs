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
  , animationWithContentsOfURLSelector
  , animationNamedSelector
  , animationWithCAAnimationSelector
  , durationSelector
  , setDurationSelector
  , keyPathSelector
  , setKeyPathSelector
  , timingFunctionSelector
  , setTimingFunctionSelector
  , blendInDurationSelector
  , setBlendInDurationSelector
  , blendOutDurationSelector
  , setBlendOutDurationSelector
  , removedOnCompletionSelector
  , setRemovedOnCompletionSelector
  , appliedOnCompletionSelector
  , setAppliedOnCompletionSelector
  , repeatCountSelector
  , setRepeatCountSelector
  , autoreversesSelector
  , setAutoreversesSelector
  , startDelaySelector
  , setStartDelaySelector
  , timeOffsetSelector
  , setTimeOffsetSelector
  , fillsForwardSelector
  , setFillsForwardSelector
  , fillsBackwardSelector
  , setFillsBackwardSelector
  , usesSceneTimeBaseSelector
  , setUsesSceneTimeBaseSelector
  , animationDidStartSelector
  , setAnimationDidStartSelector
  , animationDidStopSelector
  , setAnimationDidStopSelector
  , animationEventsSelector
  , setAnimationEventsSelector
  , additiveSelector
  , setAdditiveSelector
  , cumulativeSelector
  , setCumulativeSelector


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
    withObjCPtr animationUrl $ \raw_animationUrl ->
      sendClassMsg cls' (mkSelector "animationWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_animationUrl :: Ptr ())] >>= retainedObject . castPtr

-- | Loads and returns the animation with the specified name in the current application bundle.
--
-- @animationName@ — The name of the animation to load.
--
-- ObjC selector: @+ animationNamed:@
animationNamed :: IsNSString animationName => animationName -> IO (Id SCNAnimation)
animationNamed animationName =
  do
    cls' <- getRequiredClass "SCNAnimation"
    withObjCPtr animationName $ \raw_animationName ->
      sendClassMsg cls' (mkSelector "animationNamed:") (retPtr retVoid) [argPtr (castPtr raw_animationName :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr caAnimation $ \raw_caAnimation ->
      sendClassMsg cls' (mkSelector "animationWithCAAnimation:") (retPtr retVoid) [argPtr (castPtr raw_caAnimation :: Ptr ())] >>= retainedObject . castPtr

-- | The duration of the animation in seconds. Defaults to 0.
--
-- ObjC selector: @- duration@
duration :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
duration scnAnimation  =
  sendMsg scnAnimation (mkSelector "duration") retCDouble []

-- | The duration of the animation in seconds. Defaults to 0.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setDuration scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setDuration:") retVoid [argCDouble (fromIntegral value)]

-- | The key-path describing the property to be animated for single-property animations, nil for animations targetting multiple nodes. defaults to nil. The key-path uses the KVC syntax. It's also possible to target a specific sub-node with the following syntax:    /<node-name>.property1.property2.field    (field is optional, <node-name> is the name of the targeted node).
--
-- ObjC selector: @- keyPath@
keyPath :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Id NSString)
keyPath scnAnimation  =
  sendMsg scnAnimation (mkSelector "keyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key-path describing the property to be animated for single-property animations, nil for animations targetting multiple nodes. defaults to nil. The key-path uses the KVC syntax. It's also possible to target a specific sub-node with the following syntax:    /<node-name>.property1.property2.field    (field is optional, <node-name> is the name of the targeted node).
--
-- ObjC selector: @- setKeyPath:@
setKeyPath :: (IsSCNAnimation scnAnimation, IsNSString value) => scnAnimation -> value -> IO ()
setKeyPath scnAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnAnimation (mkSelector "setKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A timing function defining the pacing of the animation. Defaults to nil indicating linear pacing.
--
-- ObjC selector: @- timingFunction@
timingFunction :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Id SCNTimingFunction)
timingFunction scnAnimation  =
  sendMsg scnAnimation (mkSelector "timingFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A timing function defining the pacing of the animation. Defaults to nil indicating linear pacing.
--
-- ObjC selector: @- setTimingFunction:@
setTimingFunction :: (IsSCNAnimation scnAnimation, IsSCNTimingFunction value) => scnAnimation -> value -> IO ()
setTimingFunction scnAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnAnimation (mkSelector "setTimingFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Determines the receiver's blend-in duration.
--
-- When the blendInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- blendInDuration@
blendInDuration :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
blendInDuration scnAnimation  =
  sendMsg scnAnimation (mkSelector "blendInDuration") retCDouble []

-- | Determines the receiver's blend-in duration.
--
-- When the blendInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- setBlendInDuration:@
setBlendInDuration :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setBlendInDuration scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setBlendInDuration:") retVoid [argCDouble (fromIntegral value)]

-- | Determines the receiver's blend-out duration.
--
-- When the blendOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- blendOutDuration@
blendOutDuration :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
blendOutDuration scnAnimation  =
  sendMsg scnAnimation (mkSelector "blendOutDuration") retCDouble []

-- | Determines the receiver's blend-out duration.
--
-- When the blendOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- setBlendOutDuration:@
setBlendOutDuration :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setBlendOutDuration scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setBlendOutDuration:") retVoid [argCDouble (fromIntegral value)]

-- | When true, the animation is removed from the render tree once its active duration has passed. Defaults to YES.
--
-- ObjC selector: @- removedOnCompletion@
removedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
removedOnCompletion scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "removedOnCompletion") retCULong []

-- | When true, the animation is removed from the render tree once its active duration has passed. Defaults to YES.
--
-- ObjC selector: @- setRemovedOnCompletion:@
setRemovedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setRemovedOnCompletion scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setRemovedOnCompletion:") retVoid [argCULong (if value then 1 else 0)]

-- | When true, the animation is applied to the model tree once its active duration has passed. Defaults to NO.
--
-- ObjC selector: @- appliedOnCompletion@
appliedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
appliedOnCompletion scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "appliedOnCompletion") retCULong []

-- | When true, the animation is applied to the model tree once its active duration has passed. Defaults to NO.
--
-- ObjC selector: @- setAppliedOnCompletion:@
setAppliedOnCompletion :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setAppliedOnCompletion scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setAppliedOnCompletion:") retVoid [argCULong (if value then 1 else 0)]

-- | The repeat count of the object. May be fractional. Defaults to 0.
--
-- ObjC selector: @- repeatCount@
repeatCount :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
repeatCount scnAnimation  =
  sendMsg scnAnimation (mkSelector "repeatCount") retCDouble []

-- | The repeat count of the object. May be fractional. Defaults to 0.
--
-- ObjC selector: @- setRepeatCount:@
setRepeatCount :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setRepeatCount scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setRepeatCount:") retVoid [argCDouble (fromIntegral value)]

-- | When true, the object plays backwards after playing forwards. Defaults to NO.
--
-- ObjC selector: @- autoreverses@
autoreverses :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
autoreverses scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "autoreverses") retCULong []

-- | When true, the object plays backwards after playing forwards. Defaults to NO.
--
-- ObjC selector: @- setAutoreverses:@
setAutoreverses :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setAutoreverses scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setAutoreverses:") retVoid [argCULong (if value then 1 else 0)]

-- | The relative delay to start the animation, in relation to its parent animation if applicable. Defaults to 0.
--
-- This property is bridged with CoreAnimations's beginTime. However, for top level animations, startDelay is relative to the current time (unlike CAAnimation's beginTime that is absolute). So if a CAAnimation has a non-zero beginTime, startDelay is initialized as caAnimation.beginTime - CACurrentMediaTime().
--
-- ObjC selector: @- startDelay@
startDelay :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
startDelay scnAnimation  =
  sendMsg scnAnimation (mkSelector "startDelay") retCDouble []

-- | The relative delay to start the animation, in relation to its parent animation if applicable. Defaults to 0.
--
-- This property is bridged with CoreAnimations's beginTime. However, for top level animations, startDelay is relative to the current time (unlike CAAnimation's beginTime that is absolute). So if a CAAnimation has a non-zero beginTime, startDelay is initialized as caAnimation.beginTime - CACurrentMediaTime().
--
-- ObjC selector: @- setStartDelay:@
setStartDelay :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setStartDelay scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setStartDelay:") retVoid [argCDouble (fromIntegral value)]

-- | Additional offset in active local time. i.e. to convert from parent time tp to active local time t: t = (tp - begin) * speed + offset. Defaults to 0.
--
-- ObjC selector: @- timeOffset@
timeOffset :: IsSCNAnimation scnAnimation => scnAnimation -> IO CDouble
timeOffset scnAnimation  =
  sendMsg scnAnimation (mkSelector "timeOffset") retCDouble []

-- | Additional offset in active local time. i.e. to convert from parent time tp to active local time t: t = (tp - begin) * speed + offset. Defaults to 0.
--
-- ObjC selector: @- setTimeOffset:@
setTimeOffset :: IsSCNAnimation scnAnimation => scnAnimation -> CDouble -> IO ()
setTimeOffset scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setTimeOffset:") retVoid [argCDouble (fromIntegral value)]

-- | When true, the animation remains active after its active duration and evaluates to its end value. Defaults to NO.
--
-- ObjC selector: @- fillsForward@
fillsForward :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
fillsForward scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "fillsForward") retCULong []

-- | When true, the animation remains active after its active duration and evaluates to its end value. Defaults to NO.
--
-- ObjC selector: @- setFillsForward:@
setFillsForward :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setFillsForward scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setFillsForward:") retVoid [argCULong (if value then 1 else 0)]

-- | When true, the animation is active before its active duration and evaluates to its start value. Defaults to NO.
--
-- ObjC selector: @- fillsBackward@
fillsBackward :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
fillsBackward scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "fillsBackward") retCULong []

-- | When true, the animation is active before its active duration and evaluates to its start value. Defaults to NO.
--
-- ObjC selector: @- setFillsBackward:@
setFillsBackward :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setFillsBackward scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setFillsBackward:") retVoid [argCULong (if value then 1 else 0)]

-- | Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene. The "sceneTime" base is typically used by players or editors that need to preview, edit and being able to change the evaluation time.
--
-- See: SCNSceneSourceAnimationImportPolicyKey
--
-- ObjC selector: @- usesSceneTimeBase@
usesSceneTimeBase :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
usesSceneTimeBase scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "usesSceneTimeBase") retCULong []

-- | Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene. The "sceneTime" base is typically used by players or editors that need to preview, edit and being able to change the evaluation time.
--
-- See: SCNSceneSourceAnimationImportPolicyKey
--
-- ObjC selector: @- setUsesSceneTimeBase:@
setUsesSceneTimeBase :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setUsesSceneTimeBase scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setUsesSceneTimeBase:") retVoid [argCULong (if value then 1 else 0)]

-- | Called when the animation starts.
--
-- ObjC selector: @- animationDidStart@
animationDidStart :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Ptr ())
animationDidStart scnAnimation  =
  fmap castPtr $ sendMsg scnAnimation (mkSelector "animationDidStart") (retPtr retVoid) []

-- | Called when the animation starts.
--
-- ObjC selector: @- setAnimationDidStart:@
setAnimationDidStart :: IsSCNAnimation scnAnimation => scnAnimation -> Ptr () -> IO ()
setAnimationDidStart scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setAnimationDidStart:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called when the animation either completes its active duration or is removed from the object it is attached to (i.e. the layer). The 'completed' argument of SCNAnimationDidStopBlock is true if the animation reached the end of its active duration without being removed.
--
-- ObjC selector: @- animationDidStop@
animationDidStop :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Ptr ())
animationDidStop scnAnimation  =
  fmap castPtr $ sendMsg scnAnimation (mkSelector "animationDidStop") (retPtr retVoid) []

-- | Called when the animation either completes its active duration or is removed from the object it is attached to (i.e. the layer). The 'completed' argument of SCNAnimationDidStopBlock is true if the animation reached the end of its active duration without being removed.
--
-- ObjC selector: @- setAnimationDidStop:@
setAnimationDidStop :: IsSCNAnimation scnAnimation => scnAnimation -> Ptr () -> IO ()
setAnimationDidStop scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setAnimationDidStop:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Specifies the animation events attached to the receiver.
--
-- See: SCNAnimationEvent
--
-- ObjC selector: @- animationEvents@
animationEvents :: IsSCNAnimation scnAnimation => scnAnimation -> IO (Id NSArray)
animationEvents scnAnimation  =
  sendMsg scnAnimation (mkSelector "animationEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies the animation events attached to the receiver.
--
-- See: SCNAnimationEvent
--
-- ObjC selector: @- setAnimationEvents:@
setAnimationEvents :: (IsSCNAnimation scnAnimation, IsNSArray value) => scnAnimation -> value -> IO ()
setAnimationEvents scnAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnAnimation (mkSelector "setAnimationEvents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | When true the value specified by the animation will be "added" to the current presentation value of the property to produce the new presentation value. The addition function is type-dependent, e.g. for affine transforms the two matrices are concatenated. Defaults to NO.
--
-- ObjC selector: @- additive@
additive :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
additive scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "additive") retCULong []

-- | When true the value specified by the animation will be "added" to the current presentation value of the property to produce the new presentation value. The addition function is type-dependent, e.g. for affine transforms the two matrices are concatenated. Defaults to NO.
--
-- ObjC selector: @- setAdditive:@
setAdditive :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setAdditive scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setAdditive:") retVoid [argCULong (if value then 1 else 0)]

-- | The `cumulative' property affects how repeating animations produce their result. If true then the current value of the animation is the value at the end of the previous repeat cycle, plus the value of the current repeat cycle. If false, the value is simply the value calculated for the current repeat cycle. Defaults to NO.
--
-- ObjC selector: @- cumulative@
cumulative :: IsSCNAnimation scnAnimation => scnAnimation -> IO Bool
cumulative scnAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimation (mkSelector "cumulative") retCULong []

-- | The `cumulative' property affects how repeating animations produce their result. If true then the current value of the animation is the value at the end of the previous repeat cycle, plus the value of the current repeat cycle. If false, the value is simply the value calculated for the current repeat cycle. Defaults to NO.
--
-- ObjC selector: @- setCumulative:@
setCumulative :: IsSCNAnimation scnAnimation => scnAnimation -> Bool -> IO ()
setCumulative scnAnimation  value =
  sendMsg scnAnimation (mkSelector "setCumulative:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationWithContentsOfURL:@
animationWithContentsOfURLSelector :: Selector
animationWithContentsOfURLSelector = mkSelector "animationWithContentsOfURL:"

-- | @Selector@ for @animationNamed:@
animationNamedSelector :: Selector
animationNamedSelector = mkSelector "animationNamed:"

-- | @Selector@ for @animationWithCAAnimation:@
animationWithCAAnimationSelector :: Selector
animationWithCAAnimationSelector = mkSelector "animationWithCAAnimation:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @setKeyPath:@
setKeyPathSelector :: Selector
setKeyPathSelector = mkSelector "setKeyPath:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @blendInDuration@
blendInDurationSelector :: Selector
blendInDurationSelector = mkSelector "blendInDuration"

-- | @Selector@ for @setBlendInDuration:@
setBlendInDurationSelector :: Selector
setBlendInDurationSelector = mkSelector "setBlendInDuration:"

-- | @Selector@ for @blendOutDuration@
blendOutDurationSelector :: Selector
blendOutDurationSelector = mkSelector "blendOutDuration"

-- | @Selector@ for @setBlendOutDuration:@
setBlendOutDurationSelector :: Selector
setBlendOutDurationSelector = mkSelector "setBlendOutDuration:"

-- | @Selector@ for @removedOnCompletion@
removedOnCompletionSelector :: Selector
removedOnCompletionSelector = mkSelector "removedOnCompletion"

-- | @Selector@ for @setRemovedOnCompletion:@
setRemovedOnCompletionSelector :: Selector
setRemovedOnCompletionSelector = mkSelector "setRemovedOnCompletion:"

-- | @Selector@ for @appliedOnCompletion@
appliedOnCompletionSelector :: Selector
appliedOnCompletionSelector = mkSelector "appliedOnCompletion"

-- | @Selector@ for @setAppliedOnCompletion:@
setAppliedOnCompletionSelector :: Selector
setAppliedOnCompletionSelector = mkSelector "setAppliedOnCompletion:"

-- | @Selector@ for @repeatCount@
repeatCountSelector :: Selector
repeatCountSelector = mkSelector "repeatCount"

-- | @Selector@ for @setRepeatCount:@
setRepeatCountSelector :: Selector
setRepeatCountSelector = mkSelector "setRepeatCount:"

-- | @Selector@ for @autoreverses@
autoreversesSelector :: Selector
autoreversesSelector = mkSelector "autoreverses"

-- | @Selector@ for @setAutoreverses:@
setAutoreversesSelector :: Selector
setAutoreversesSelector = mkSelector "setAutoreverses:"

-- | @Selector@ for @startDelay@
startDelaySelector :: Selector
startDelaySelector = mkSelector "startDelay"

-- | @Selector@ for @setStartDelay:@
setStartDelaySelector :: Selector
setStartDelaySelector = mkSelector "setStartDelay:"

-- | @Selector@ for @timeOffset@
timeOffsetSelector :: Selector
timeOffsetSelector = mkSelector "timeOffset"

-- | @Selector@ for @setTimeOffset:@
setTimeOffsetSelector :: Selector
setTimeOffsetSelector = mkSelector "setTimeOffset:"

-- | @Selector@ for @fillsForward@
fillsForwardSelector :: Selector
fillsForwardSelector = mkSelector "fillsForward"

-- | @Selector@ for @setFillsForward:@
setFillsForwardSelector :: Selector
setFillsForwardSelector = mkSelector "setFillsForward:"

-- | @Selector@ for @fillsBackward@
fillsBackwardSelector :: Selector
fillsBackwardSelector = mkSelector "fillsBackward"

-- | @Selector@ for @setFillsBackward:@
setFillsBackwardSelector :: Selector
setFillsBackwardSelector = mkSelector "setFillsBackward:"

-- | @Selector@ for @usesSceneTimeBase@
usesSceneTimeBaseSelector :: Selector
usesSceneTimeBaseSelector = mkSelector "usesSceneTimeBase"

-- | @Selector@ for @setUsesSceneTimeBase:@
setUsesSceneTimeBaseSelector :: Selector
setUsesSceneTimeBaseSelector = mkSelector "setUsesSceneTimeBase:"

-- | @Selector@ for @animationDidStart@
animationDidStartSelector :: Selector
animationDidStartSelector = mkSelector "animationDidStart"

-- | @Selector@ for @setAnimationDidStart:@
setAnimationDidStartSelector :: Selector
setAnimationDidStartSelector = mkSelector "setAnimationDidStart:"

-- | @Selector@ for @animationDidStop@
animationDidStopSelector :: Selector
animationDidStopSelector = mkSelector "animationDidStop"

-- | @Selector@ for @setAnimationDidStop:@
setAnimationDidStopSelector :: Selector
setAnimationDidStopSelector = mkSelector "setAnimationDidStop:"

-- | @Selector@ for @animationEvents@
animationEventsSelector :: Selector
animationEventsSelector = mkSelector "animationEvents"

-- | @Selector@ for @setAnimationEvents:@
setAnimationEventsSelector :: Selector
setAnimationEventsSelector = mkSelector "setAnimationEvents:"

-- | @Selector@ for @additive@
additiveSelector :: Selector
additiveSelector = mkSelector "additive"

-- | @Selector@ for @setAdditive:@
setAdditiveSelector :: Selector
setAdditiveSelector = mkSelector "setAdditive:"

-- | @Selector@ for @cumulative@
cumulativeSelector :: Selector
cumulativeSelector = mkSelector "cumulative"

-- | @Selector@ for @setCumulative:@
setCumulativeSelector :: Selector
setCumulativeSelector = mkSelector "setCumulative:"

