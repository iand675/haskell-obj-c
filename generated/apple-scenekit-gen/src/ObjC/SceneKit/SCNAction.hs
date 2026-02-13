{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNAction@.
module ObjC.SceneKit.SCNAction
  ( SCNAction
  , IsSCNAction(..)
  , reversedAction
  , moveByX_y_z_duration
  , moveBy_duration
  , moveTo_duration
  , rotateByX_y_z_duration
  , rotateToX_y_z_duration
  , rotateToX_y_z_duration_shortestUnitArc
  , rotateByAngle_aroundAxis_duration
  , rotateToAxisAngle_duration
  , scaleBy_duration
  , scaleTo_duration
  , sequence_
  , group
  , repeatAction_count
  , repeatActionForever
  , fadeInWithDuration
  , fadeOutWithDuration
  , fadeOpacityBy_duration
  , fadeOpacityTo_duration
  , hide
  , unhide
  , waitForDuration
  , waitForDuration_withRange
  , removeFromParentNode
  , runBlock
  , runBlock_queue
  , javaScriptActionWithScript_duration
  , customActionWithDuration_actionBlock
  , playAudioSource_waitForCompletion
  , duration
  , setDuration
  , timingMode
  , setTimingMode
  , timingFunction
  , setTimingFunction
  , speed
  , setSpeed
  , customActionWithDuration_actionBlockSelector
  , durationSelector
  , fadeInWithDurationSelector
  , fadeOpacityBy_durationSelector
  , fadeOpacityTo_durationSelector
  , fadeOutWithDurationSelector
  , groupSelector
  , hideSelector
  , javaScriptActionWithScript_durationSelector
  , moveByX_y_z_durationSelector
  , moveBy_durationSelector
  , moveTo_durationSelector
  , playAudioSource_waitForCompletionSelector
  , removeFromParentNodeSelector
  , repeatActionForeverSelector
  , repeatAction_countSelector
  , reversedActionSelector
  , rotateByAngle_aroundAxis_durationSelector
  , rotateByX_y_z_durationSelector
  , rotateToAxisAngle_durationSelector
  , rotateToX_y_z_durationSelector
  , rotateToX_y_z_duration_shortestUnitArcSelector
  , runBlockSelector
  , runBlock_queueSelector
  , scaleBy_durationSelector
  , scaleTo_durationSelector
  , sequenceSelector
  , setDurationSelector
  , setSpeedSelector
  , setTimingFunctionSelector
  , setTimingModeSelector
  , speedSelector
  , timingFunctionSelector
  , timingModeSelector
  , unhideSelector
  , waitForDurationSelector
  , waitForDuration_withRangeSelector

  -- * Enum types
  , SCNActionTimingMode(SCNActionTimingMode)
  , pattern SCNActionTimingModeLinear
  , pattern SCNActionTimingModeEaseIn
  , pattern SCNActionTimingModeEaseOut
  , pattern SCNActionTimingModeEaseInEaseOut

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | reversedAction
--
-- Creates an action that reverses the behavior of another action.
--
-- ObjC selector: @- reversedAction@
reversedAction :: IsSCNAction scnAction => scnAction -> IO (Id SCNAction)
reversedAction scnAction =
  sendMessage scnAction reversedActionSelector

-- | @+ moveByX:y:z:duration:@
moveByX_y_z_duration :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNAction)
moveByX_y_z_duration deltaX deltaY deltaZ duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' moveByX_y_z_durationSelector deltaX deltaY deltaZ duration

-- | @+ moveBy:duration:@
moveBy_duration :: SCNVector3 -> CDouble -> IO (Id SCNAction)
moveBy_duration delta duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' moveBy_durationSelector delta duration

-- | @+ moveTo:duration:@
moveTo_duration :: SCNVector3 -> CDouble -> IO (Id SCNAction)
moveTo_duration location duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' moveTo_durationSelector location duration

-- | @+ rotateByX:y:z:duration:@
rotateByX_y_z_duration :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNAction)
rotateByX_y_z_duration xAngle yAngle zAngle duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' rotateByX_y_z_durationSelector xAngle yAngle zAngle duration

-- | @+ rotateToX:y:z:duration:@
rotateToX_y_z_duration :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNAction)
rotateToX_y_z_duration xAngle yAngle zAngle duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' rotateToX_y_z_durationSelector xAngle yAngle zAngle duration

-- | @+ rotateToX:y:z:duration:shortestUnitArc:@
rotateToX_y_z_duration_shortestUnitArc :: CDouble -> CDouble -> CDouble -> CDouble -> Bool -> IO (Id SCNAction)
rotateToX_y_z_duration_shortestUnitArc xAngle yAngle zAngle duration shortestUnitArc =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' rotateToX_y_z_duration_shortestUnitArcSelector xAngle yAngle zAngle duration shortestUnitArc

-- | @+ rotateByAngle:aroundAxis:duration:@
rotateByAngle_aroundAxis_duration :: CDouble -> SCNVector3 -> CDouble -> IO (Id SCNAction)
rotateByAngle_aroundAxis_duration angle axis duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' rotateByAngle_aroundAxis_durationSelector angle axis duration

-- | @+ rotateToAxisAngle:duration:@
rotateToAxisAngle_duration :: SCNVector4 -> CDouble -> IO (Id SCNAction)
rotateToAxisAngle_duration axisAngle duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' rotateToAxisAngle_durationSelector axisAngle duration

-- | @+ scaleBy:duration:@
scaleBy_duration :: CDouble -> CDouble -> IO (Id SCNAction)
scaleBy_duration scale sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' scaleBy_durationSelector scale sec

-- | @+ scaleTo:duration:@
scaleTo_duration :: CDouble -> CDouble -> IO (Id SCNAction)
scaleTo_duration scale sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' scaleTo_durationSelector scale sec

-- | @+ sequence:@
sequence_ :: IsNSArray actions => actions -> IO (Id SCNAction)
sequence_ actions =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' sequenceSelector (toNSArray actions)

-- | @+ group:@
group :: IsNSArray actions => actions -> IO (Id SCNAction)
group actions =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' groupSelector (toNSArray actions)

-- | @+ repeatAction:count:@
repeatAction_count :: IsSCNAction action => action -> CULong -> IO (Id SCNAction)
repeatAction_count action count =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' repeatAction_countSelector (toSCNAction action) count

-- | @+ repeatActionForever:@
repeatActionForever :: IsSCNAction action => action -> IO (Id SCNAction)
repeatActionForever action =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' repeatActionForeverSelector (toSCNAction action)

-- | @+ fadeInWithDuration:@
fadeInWithDuration :: CDouble -> IO (Id SCNAction)
fadeInWithDuration sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' fadeInWithDurationSelector sec

-- | @+ fadeOutWithDuration:@
fadeOutWithDuration :: CDouble -> IO (Id SCNAction)
fadeOutWithDuration sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' fadeOutWithDurationSelector sec

-- | @+ fadeOpacityBy:duration:@
fadeOpacityBy_duration :: CDouble -> CDouble -> IO (Id SCNAction)
fadeOpacityBy_duration factor sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' fadeOpacityBy_durationSelector factor sec

-- | @+ fadeOpacityTo:duration:@
fadeOpacityTo_duration :: CDouble -> CDouble -> IO (Id SCNAction)
fadeOpacityTo_duration opacity sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' fadeOpacityTo_durationSelector opacity sec

-- | @+ hide@
hide :: IO (Id SCNAction)
hide  =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' hideSelector

-- | @+ unhide@
unhide :: IO (Id SCNAction)
unhide  =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' unhideSelector

-- | @+ waitForDuration:@
waitForDuration :: CDouble -> IO (Id SCNAction)
waitForDuration sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' waitForDurationSelector sec

-- | @+ waitForDuration:withRange:@
waitForDuration_withRange :: CDouble -> CDouble -> IO (Id SCNAction)
waitForDuration_withRange sec durationRange =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' waitForDuration_withRangeSelector sec durationRange

-- | @+ removeFromParentNode@
removeFromParentNode :: IO (Id SCNAction)
removeFromParentNode  =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' removeFromParentNodeSelector

-- | @+ runBlock:@
runBlock :: Ptr () -> IO (Id SCNAction)
runBlock block =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' runBlockSelector block

-- | @+ runBlock:queue:@
runBlock_queue :: IsNSObject queue => Ptr () -> queue -> IO (Id SCNAction)
runBlock_queue block queue =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' runBlock_queueSelector block (toNSObject queue)

-- | @+ javaScriptActionWithScript:duration:@
javaScriptActionWithScript_duration :: IsNSString script => script -> CDouble -> IO (Id SCNAction)
javaScriptActionWithScript_duration script seconds =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' javaScriptActionWithScript_durationSelector (toNSString script) seconds

-- | @+ customActionWithDuration:actionBlock:@
customActionWithDuration_actionBlock :: CDouble -> Ptr () -> IO (Id SCNAction)
customActionWithDuration_actionBlock seconds block =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' customActionWithDuration_actionBlockSelector seconds block

-- | Creates an action that plays a sound
--
-- @source@ — The audio source to play (see SCNAudioSource.h)
--
-- @wait@ — If YES, then the duration of this action is the same as the length of the audio playback. If NO, the action is considered to have completed immediately.
--
-- ObjC selector: @+ playAudioSource:waitForCompletion:@
playAudioSource_waitForCompletion :: IsSCNAudioSource source => source -> Bool -> IO (Id SCNAction)
playAudioSource_waitForCompletion source wait =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMessage cls' playAudioSource_waitForCompletionSelector (toSCNAudioSource source) wait

-- | duration
--
-- This is the expected duration of an action’s animation. The actual time an action takes to complete is modified by the speed property of the action.
--
-- ObjC selector: @- duration@
duration :: IsSCNAction scnAction => scnAction -> IO CDouble
duration scnAction =
  sendMessage scnAction durationSelector

-- | duration
--
-- This is the expected duration of an action’s animation. The actual time an action takes to complete is modified by the speed property of the action.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsSCNAction scnAction => scnAction -> CDouble -> IO ()
setDuration scnAction value =
  sendMessage scnAction setDurationSelector value

-- | timingMode
--
-- The timing mode used to execute an action.
--
-- ObjC selector: @- timingMode@
timingMode :: IsSCNAction scnAction => scnAction -> IO SCNActionTimingMode
timingMode scnAction =
  sendMessage scnAction timingModeSelector

-- | timingMode
--
-- The timing mode used to execute an action.
--
-- ObjC selector: @- setTimingMode:@
setTimingMode :: IsSCNAction scnAction => scnAction -> SCNActionTimingMode -> IO ()
setTimingMode scnAction value =
  sendMessage scnAction setTimingModeSelector value

-- | When set, prodives a custom timing via a block. Applies after the 'timingMode' property is taken into account, defaults to nil
--
-- See: SCNActionTimingFunction
--
-- ObjC selector: @- timingFunction@
timingFunction :: IsSCNAction scnAction => scnAction -> IO (Ptr ())
timingFunction scnAction =
  sendMessage scnAction timingFunctionSelector

-- | When set, prodives a custom timing via a block. Applies after the 'timingMode' property is taken into account, defaults to nil
--
-- See: SCNActionTimingFunction
--
-- ObjC selector: @- setTimingFunction:@
setTimingFunction :: IsSCNAction scnAction => scnAction -> Ptr () -> IO ()
setTimingFunction scnAction value =
  sendMessage scnAction setTimingFunctionSelector value

-- | speed
--
-- A speed factor that modifies how fast an action runs. Defaults to 1.
--
-- ObjC selector: @- speed@
speed :: IsSCNAction scnAction => scnAction -> IO CDouble
speed scnAction =
  sendMessage scnAction speedSelector

-- | speed
--
-- A speed factor that modifies how fast an action runs. Defaults to 1.
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSCNAction scnAction => scnAction -> CDouble -> IO ()
setSpeed scnAction value =
  sendMessage scnAction setSpeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reversedAction@
reversedActionSelector :: Selector '[] (Id SCNAction)
reversedActionSelector = mkSelector "reversedAction"

-- | @Selector@ for @moveByX:y:z:duration:@
moveByX_y_z_durationSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id SCNAction)
moveByX_y_z_durationSelector = mkSelector "moveByX:y:z:duration:"

-- | @Selector@ for @moveBy:duration:@
moveBy_durationSelector :: Selector '[SCNVector3, CDouble] (Id SCNAction)
moveBy_durationSelector = mkSelector "moveBy:duration:"

-- | @Selector@ for @moveTo:duration:@
moveTo_durationSelector :: Selector '[SCNVector3, CDouble] (Id SCNAction)
moveTo_durationSelector = mkSelector "moveTo:duration:"

-- | @Selector@ for @rotateByX:y:z:duration:@
rotateByX_y_z_durationSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id SCNAction)
rotateByX_y_z_durationSelector = mkSelector "rotateByX:y:z:duration:"

-- | @Selector@ for @rotateToX:y:z:duration:@
rotateToX_y_z_durationSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id SCNAction)
rotateToX_y_z_durationSelector = mkSelector "rotateToX:y:z:duration:"

-- | @Selector@ for @rotateToX:y:z:duration:shortestUnitArc:@
rotateToX_y_z_duration_shortestUnitArcSelector :: Selector '[CDouble, CDouble, CDouble, CDouble, Bool] (Id SCNAction)
rotateToX_y_z_duration_shortestUnitArcSelector = mkSelector "rotateToX:y:z:duration:shortestUnitArc:"

-- | @Selector@ for @rotateByAngle:aroundAxis:duration:@
rotateByAngle_aroundAxis_durationSelector :: Selector '[CDouble, SCNVector3, CDouble] (Id SCNAction)
rotateByAngle_aroundAxis_durationSelector = mkSelector "rotateByAngle:aroundAxis:duration:"

-- | @Selector@ for @rotateToAxisAngle:duration:@
rotateToAxisAngle_durationSelector :: Selector '[SCNVector4, CDouble] (Id SCNAction)
rotateToAxisAngle_durationSelector = mkSelector "rotateToAxisAngle:duration:"

-- | @Selector@ for @scaleBy:duration:@
scaleBy_durationSelector :: Selector '[CDouble, CDouble] (Id SCNAction)
scaleBy_durationSelector = mkSelector "scaleBy:duration:"

-- | @Selector@ for @scaleTo:duration:@
scaleTo_durationSelector :: Selector '[CDouble, CDouble] (Id SCNAction)
scaleTo_durationSelector = mkSelector "scaleTo:duration:"

-- | @Selector@ for @sequence:@
sequenceSelector :: Selector '[Id NSArray] (Id SCNAction)
sequenceSelector = mkSelector "sequence:"

-- | @Selector@ for @group:@
groupSelector :: Selector '[Id NSArray] (Id SCNAction)
groupSelector = mkSelector "group:"

-- | @Selector@ for @repeatAction:count:@
repeatAction_countSelector :: Selector '[Id SCNAction, CULong] (Id SCNAction)
repeatAction_countSelector = mkSelector "repeatAction:count:"

-- | @Selector@ for @repeatActionForever:@
repeatActionForeverSelector :: Selector '[Id SCNAction] (Id SCNAction)
repeatActionForeverSelector = mkSelector "repeatActionForever:"

-- | @Selector@ for @fadeInWithDuration:@
fadeInWithDurationSelector :: Selector '[CDouble] (Id SCNAction)
fadeInWithDurationSelector = mkSelector "fadeInWithDuration:"

-- | @Selector@ for @fadeOutWithDuration:@
fadeOutWithDurationSelector :: Selector '[CDouble] (Id SCNAction)
fadeOutWithDurationSelector = mkSelector "fadeOutWithDuration:"

-- | @Selector@ for @fadeOpacityBy:duration:@
fadeOpacityBy_durationSelector :: Selector '[CDouble, CDouble] (Id SCNAction)
fadeOpacityBy_durationSelector = mkSelector "fadeOpacityBy:duration:"

-- | @Selector@ for @fadeOpacityTo:duration:@
fadeOpacityTo_durationSelector :: Selector '[CDouble, CDouble] (Id SCNAction)
fadeOpacityTo_durationSelector = mkSelector "fadeOpacityTo:duration:"

-- | @Selector@ for @hide@
hideSelector :: Selector '[] (Id SCNAction)
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector '[] (Id SCNAction)
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @waitForDuration:@
waitForDurationSelector :: Selector '[CDouble] (Id SCNAction)
waitForDurationSelector = mkSelector "waitForDuration:"

-- | @Selector@ for @waitForDuration:withRange:@
waitForDuration_withRangeSelector :: Selector '[CDouble, CDouble] (Id SCNAction)
waitForDuration_withRangeSelector = mkSelector "waitForDuration:withRange:"

-- | @Selector@ for @removeFromParentNode@
removeFromParentNodeSelector :: Selector '[] (Id SCNAction)
removeFromParentNodeSelector = mkSelector "removeFromParentNode"

-- | @Selector@ for @runBlock:@
runBlockSelector :: Selector '[Ptr ()] (Id SCNAction)
runBlockSelector = mkSelector "runBlock:"

-- | @Selector@ for @runBlock:queue:@
runBlock_queueSelector :: Selector '[Ptr (), Id NSObject] (Id SCNAction)
runBlock_queueSelector = mkSelector "runBlock:queue:"

-- | @Selector@ for @javaScriptActionWithScript:duration:@
javaScriptActionWithScript_durationSelector :: Selector '[Id NSString, CDouble] (Id SCNAction)
javaScriptActionWithScript_durationSelector = mkSelector "javaScriptActionWithScript:duration:"

-- | @Selector@ for @customActionWithDuration:actionBlock:@
customActionWithDuration_actionBlockSelector :: Selector '[CDouble, Ptr ()] (Id SCNAction)
customActionWithDuration_actionBlockSelector = mkSelector "customActionWithDuration:actionBlock:"

-- | @Selector@ for @playAudioSource:waitForCompletion:@
playAudioSource_waitForCompletionSelector :: Selector '[Id SCNAudioSource, Bool] (Id SCNAction)
playAudioSource_waitForCompletionSelector = mkSelector "playAudioSource:waitForCompletion:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @timingMode@
timingModeSelector :: Selector '[] SCNActionTimingMode
timingModeSelector = mkSelector "timingMode"

-- | @Selector@ for @setTimingMode:@
setTimingModeSelector :: Selector '[SCNActionTimingMode] ()
setTimingModeSelector = mkSelector "setTimingMode:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector '[] (Ptr ())
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector '[Ptr ()] ()
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CDouble
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[CDouble] ()
setSpeedSelector = mkSelector "setSpeed:"

