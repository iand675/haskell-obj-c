{-# LANGUAGE PatternSynonyms #-}
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
  , reversedActionSelector
  , moveByX_y_z_durationSelector
  , moveBy_durationSelector
  , moveTo_durationSelector
  , rotateByX_y_z_durationSelector
  , rotateToX_y_z_durationSelector
  , rotateToX_y_z_duration_shortestUnitArcSelector
  , rotateByAngle_aroundAxis_durationSelector
  , rotateToAxisAngle_durationSelector
  , scaleBy_durationSelector
  , scaleTo_durationSelector
  , sequenceSelector
  , groupSelector
  , repeatAction_countSelector
  , repeatActionForeverSelector
  , fadeInWithDurationSelector
  , fadeOutWithDurationSelector
  , fadeOpacityBy_durationSelector
  , fadeOpacityTo_durationSelector
  , hideSelector
  , unhideSelector
  , waitForDurationSelector
  , waitForDuration_withRangeSelector
  , removeFromParentNodeSelector
  , runBlockSelector
  , runBlock_queueSelector
  , javaScriptActionWithScript_durationSelector
  , customActionWithDuration_actionBlockSelector
  , playAudioSource_waitForCompletionSelector
  , durationSelector
  , setDurationSelector
  , timingModeSelector
  , setTimingModeSelector
  , timingFunctionSelector
  , setTimingFunctionSelector
  , speedSelector
  , setSpeedSelector

  -- * Enum types
  , SCNActionTimingMode(SCNActionTimingMode)
  , pattern SCNActionTimingModeLinear
  , pattern SCNActionTimingModeEaseIn
  , pattern SCNActionTimingModeEaseOut
  , pattern SCNActionTimingModeEaseInEaseOut

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
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | reversedAction
--
-- Creates an action that reverses the behavior of another action.
--
-- ObjC selector: @- reversedAction@
reversedAction :: IsSCNAction scnAction => scnAction -> IO (Id SCNAction)
reversedAction scnAction  =
  sendMsg scnAction (mkSelector "reversedAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ moveByX:y:z:duration:@
moveByX_y_z_duration :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNAction)
moveByX_y_z_duration deltaX deltaY deltaZ duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "moveByX:y:z:duration:") (retPtr retVoid) [argCDouble (fromIntegral deltaX), argCDouble (fromIntegral deltaY), argCDouble (fromIntegral deltaZ), argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ moveBy:duration:@
moveBy_duration :: SCNVector3 -> CDouble -> IO (Id SCNAction)
moveBy_duration delta duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "moveBy:duration:") (retPtr retVoid) [argSCNVector3 delta, argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ moveTo:duration:@
moveTo_duration :: SCNVector3 -> CDouble -> IO (Id SCNAction)
moveTo_duration location duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "moveTo:duration:") (retPtr retVoid) [argSCNVector3 location, argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ rotateByX:y:z:duration:@
rotateByX_y_z_duration :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNAction)
rotateByX_y_z_duration xAngle yAngle zAngle duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "rotateByX:y:z:duration:") (retPtr retVoid) [argCDouble (fromIntegral xAngle), argCDouble (fromIntegral yAngle), argCDouble (fromIntegral zAngle), argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ rotateToX:y:z:duration:@
rotateToX_y_z_duration :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNAction)
rotateToX_y_z_duration xAngle yAngle zAngle duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "rotateToX:y:z:duration:") (retPtr retVoid) [argCDouble (fromIntegral xAngle), argCDouble (fromIntegral yAngle), argCDouble (fromIntegral zAngle), argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ rotateToX:y:z:duration:shortestUnitArc:@
rotateToX_y_z_duration_shortestUnitArc :: CDouble -> CDouble -> CDouble -> CDouble -> Bool -> IO (Id SCNAction)
rotateToX_y_z_duration_shortestUnitArc xAngle yAngle zAngle duration shortestUnitArc =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "rotateToX:y:z:duration:shortestUnitArc:") (retPtr retVoid) [argCDouble (fromIntegral xAngle), argCDouble (fromIntegral yAngle), argCDouble (fromIntegral zAngle), argCDouble (fromIntegral duration), argCULong (if shortestUnitArc then 1 else 0)] >>= retainedObject . castPtr

-- | @+ rotateByAngle:aroundAxis:duration:@
rotateByAngle_aroundAxis_duration :: CDouble -> SCNVector3 -> CDouble -> IO (Id SCNAction)
rotateByAngle_aroundAxis_duration angle axis duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "rotateByAngle:aroundAxis:duration:") (retPtr retVoid) [argCDouble (fromIntegral angle), argSCNVector3 axis, argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ rotateToAxisAngle:duration:@
rotateToAxisAngle_duration :: SCNVector4 -> CDouble -> IO (Id SCNAction)
rotateToAxisAngle_duration axisAngle duration =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "rotateToAxisAngle:duration:") (retPtr retVoid) [argSCNVector4 axisAngle, argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ scaleBy:duration:@
scaleBy_duration :: CDouble -> CDouble -> IO (Id SCNAction)
scaleBy_duration scale sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "scaleBy:duration:") (retPtr retVoid) [argCDouble (fromIntegral scale), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ scaleTo:duration:@
scaleTo_duration :: CDouble -> CDouble -> IO (Id SCNAction)
scaleTo_duration scale sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "scaleTo:duration:") (retPtr retVoid) [argCDouble (fromIntegral scale), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ sequence:@
sequence_ :: IsNSArray actions => actions -> IO (Id SCNAction)
sequence_ actions =
  do
    cls' <- getRequiredClass "SCNAction"
    withObjCPtr actions $ \raw_actions ->
      sendClassMsg cls' (mkSelector "sequence:") (retPtr retVoid) [argPtr (castPtr raw_actions :: Ptr ())] >>= retainedObject . castPtr

-- | @+ group:@
group :: IsNSArray actions => actions -> IO (Id SCNAction)
group actions =
  do
    cls' <- getRequiredClass "SCNAction"
    withObjCPtr actions $ \raw_actions ->
      sendClassMsg cls' (mkSelector "group:") (retPtr retVoid) [argPtr (castPtr raw_actions :: Ptr ())] >>= retainedObject . castPtr

-- | @+ repeatAction:count:@
repeatAction_count :: IsSCNAction action => action -> CULong -> IO (Id SCNAction)
repeatAction_count action count =
  do
    cls' <- getRequiredClass "SCNAction"
    withObjCPtr action $ \raw_action ->
      sendClassMsg cls' (mkSelector "repeatAction:count:") (retPtr retVoid) [argPtr (castPtr raw_action :: Ptr ()), argCULong (fromIntegral count)] >>= retainedObject . castPtr

-- | @+ repeatActionForever:@
repeatActionForever :: IsSCNAction action => action -> IO (Id SCNAction)
repeatActionForever action =
  do
    cls' <- getRequiredClass "SCNAction"
    withObjCPtr action $ \raw_action ->
      sendClassMsg cls' (mkSelector "repeatActionForever:") (retPtr retVoid) [argPtr (castPtr raw_action :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fadeInWithDuration:@
fadeInWithDuration :: CDouble -> IO (Id SCNAction)
fadeInWithDuration sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "fadeInWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ fadeOutWithDuration:@
fadeOutWithDuration :: CDouble -> IO (Id SCNAction)
fadeOutWithDuration sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "fadeOutWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ fadeOpacityBy:duration:@
fadeOpacityBy_duration :: CDouble -> CDouble -> IO (Id SCNAction)
fadeOpacityBy_duration factor sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "fadeOpacityBy:duration:") (retPtr retVoid) [argCDouble (fromIntegral factor), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ fadeOpacityTo:duration:@
fadeOpacityTo_duration :: CDouble -> CDouble -> IO (Id SCNAction)
fadeOpacityTo_duration opacity sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "fadeOpacityTo:duration:") (retPtr retVoid) [argCDouble (fromIntegral opacity), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ hide@
hide :: IO (Id SCNAction)
hide  =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "hide") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ unhide@
unhide :: IO (Id SCNAction)
unhide  =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "unhide") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ waitForDuration:@
waitForDuration :: CDouble -> IO (Id SCNAction)
waitForDuration sec =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "waitForDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ waitForDuration:withRange:@
waitForDuration_withRange :: CDouble -> CDouble -> IO (Id SCNAction)
waitForDuration_withRange sec durationRange =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "waitForDuration:withRange:") (retPtr retVoid) [argCDouble (fromIntegral sec), argCDouble (fromIntegral durationRange)] >>= retainedObject . castPtr

-- | @+ removeFromParentNode@
removeFromParentNode :: IO (Id SCNAction)
removeFromParentNode  =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "removeFromParentNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ runBlock:@
runBlock :: Ptr () -> IO (Id SCNAction)
runBlock block =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "runBlock:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | @+ runBlock:queue:@
runBlock_queue :: IsNSObject queue => Ptr () -> queue -> IO (Id SCNAction)
runBlock_queue block queue =
  do
    cls' <- getRequiredClass "SCNAction"
    withObjCPtr queue $ \raw_queue ->
      sendClassMsg cls' (mkSelector "runBlock:queue:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= retainedObject . castPtr

-- | @+ javaScriptActionWithScript:duration:@
javaScriptActionWithScript_duration :: IsNSString script => script -> CDouble -> IO (Id SCNAction)
javaScriptActionWithScript_duration script seconds =
  do
    cls' <- getRequiredClass "SCNAction"
    withObjCPtr script $ \raw_script ->
      sendClassMsg cls' (mkSelector "javaScriptActionWithScript:duration:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ()), argCDouble (fromIntegral seconds)] >>= retainedObject . castPtr

-- | @+ customActionWithDuration:actionBlock:@
customActionWithDuration_actionBlock :: CDouble -> Ptr () -> IO (Id SCNAction)
customActionWithDuration_actionBlock seconds block =
  do
    cls' <- getRequiredClass "SCNAction"
    sendClassMsg cls' (mkSelector "customActionWithDuration:actionBlock:") (retPtr retVoid) [argCDouble (fromIntegral seconds), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "playAudioSource:waitForCompletion:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCULong (if wait then 1 else 0)] >>= retainedObject . castPtr

-- | duration
--
-- This is the expected duration of an action’s animation. The actual time an action takes to complete is modified by the speed property of the action.
--
-- ObjC selector: @- duration@
duration :: IsSCNAction scnAction => scnAction -> IO CDouble
duration scnAction  =
  sendMsg scnAction (mkSelector "duration") retCDouble []

-- | duration
--
-- This is the expected duration of an action’s animation. The actual time an action takes to complete is modified by the speed property of the action.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsSCNAction scnAction => scnAction -> CDouble -> IO ()
setDuration scnAction  value =
  sendMsg scnAction (mkSelector "setDuration:") retVoid [argCDouble (fromIntegral value)]

-- | timingMode
--
-- The timing mode used to execute an action.
--
-- ObjC selector: @- timingMode@
timingMode :: IsSCNAction scnAction => scnAction -> IO SCNActionTimingMode
timingMode scnAction  =
  fmap (coerce :: CLong -> SCNActionTimingMode) $ sendMsg scnAction (mkSelector "timingMode") retCLong []

-- | timingMode
--
-- The timing mode used to execute an action.
--
-- ObjC selector: @- setTimingMode:@
setTimingMode :: IsSCNAction scnAction => scnAction -> SCNActionTimingMode -> IO ()
setTimingMode scnAction  value =
  sendMsg scnAction (mkSelector "setTimingMode:") retVoid [argCLong (coerce value)]

-- | When set, prodives a custom timing via a block. Applies after the 'timingMode' property is taken into account, defaults to nil
--
-- See: SCNActionTimingFunction
--
-- ObjC selector: @- timingFunction@
timingFunction :: IsSCNAction scnAction => scnAction -> IO (Ptr ())
timingFunction scnAction  =
  fmap castPtr $ sendMsg scnAction (mkSelector "timingFunction") (retPtr retVoid) []

-- | When set, prodives a custom timing via a block. Applies after the 'timingMode' property is taken into account, defaults to nil
--
-- See: SCNActionTimingFunction
--
-- ObjC selector: @- setTimingFunction:@
setTimingFunction :: IsSCNAction scnAction => scnAction -> Ptr () -> IO ()
setTimingFunction scnAction  value =
  sendMsg scnAction (mkSelector "setTimingFunction:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | speed
--
-- A speed factor that modifies how fast an action runs. Defaults to 1.
--
-- ObjC selector: @- speed@
speed :: IsSCNAction scnAction => scnAction -> IO CDouble
speed scnAction  =
  sendMsg scnAction (mkSelector "speed") retCDouble []

-- | speed
--
-- A speed factor that modifies how fast an action runs. Defaults to 1.
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSCNAction scnAction => scnAction -> CDouble -> IO ()
setSpeed scnAction  value =
  sendMsg scnAction (mkSelector "setSpeed:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reversedAction@
reversedActionSelector :: Selector
reversedActionSelector = mkSelector "reversedAction"

-- | @Selector@ for @moveByX:y:z:duration:@
moveByX_y_z_durationSelector :: Selector
moveByX_y_z_durationSelector = mkSelector "moveByX:y:z:duration:"

-- | @Selector@ for @moveBy:duration:@
moveBy_durationSelector :: Selector
moveBy_durationSelector = mkSelector "moveBy:duration:"

-- | @Selector@ for @moveTo:duration:@
moveTo_durationSelector :: Selector
moveTo_durationSelector = mkSelector "moveTo:duration:"

-- | @Selector@ for @rotateByX:y:z:duration:@
rotateByX_y_z_durationSelector :: Selector
rotateByX_y_z_durationSelector = mkSelector "rotateByX:y:z:duration:"

-- | @Selector@ for @rotateToX:y:z:duration:@
rotateToX_y_z_durationSelector :: Selector
rotateToX_y_z_durationSelector = mkSelector "rotateToX:y:z:duration:"

-- | @Selector@ for @rotateToX:y:z:duration:shortestUnitArc:@
rotateToX_y_z_duration_shortestUnitArcSelector :: Selector
rotateToX_y_z_duration_shortestUnitArcSelector = mkSelector "rotateToX:y:z:duration:shortestUnitArc:"

-- | @Selector@ for @rotateByAngle:aroundAxis:duration:@
rotateByAngle_aroundAxis_durationSelector :: Selector
rotateByAngle_aroundAxis_durationSelector = mkSelector "rotateByAngle:aroundAxis:duration:"

-- | @Selector@ for @rotateToAxisAngle:duration:@
rotateToAxisAngle_durationSelector :: Selector
rotateToAxisAngle_durationSelector = mkSelector "rotateToAxisAngle:duration:"

-- | @Selector@ for @scaleBy:duration:@
scaleBy_durationSelector :: Selector
scaleBy_durationSelector = mkSelector "scaleBy:duration:"

-- | @Selector@ for @scaleTo:duration:@
scaleTo_durationSelector :: Selector
scaleTo_durationSelector = mkSelector "scaleTo:duration:"

-- | @Selector@ for @sequence:@
sequenceSelector :: Selector
sequenceSelector = mkSelector "sequence:"

-- | @Selector@ for @group:@
groupSelector :: Selector
groupSelector = mkSelector "group:"

-- | @Selector@ for @repeatAction:count:@
repeatAction_countSelector :: Selector
repeatAction_countSelector = mkSelector "repeatAction:count:"

-- | @Selector@ for @repeatActionForever:@
repeatActionForeverSelector :: Selector
repeatActionForeverSelector = mkSelector "repeatActionForever:"

-- | @Selector@ for @fadeInWithDuration:@
fadeInWithDurationSelector :: Selector
fadeInWithDurationSelector = mkSelector "fadeInWithDuration:"

-- | @Selector@ for @fadeOutWithDuration:@
fadeOutWithDurationSelector :: Selector
fadeOutWithDurationSelector = mkSelector "fadeOutWithDuration:"

-- | @Selector@ for @fadeOpacityBy:duration:@
fadeOpacityBy_durationSelector :: Selector
fadeOpacityBy_durationSelector = mkSelector "fadeOpacityBy:duration:"

-- | @Selector@ for @fadeOpacityTo:duration:@
fadeOpacityTo_durationSelector :: Selector
fadeOpacityTo_durationSelector = mkSelector "fadeOpacityTo:duration:"

-- | @Selector@ for @hide@
hideSelector :: Selector
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @waitForDuration:@
waitForDurationSelector :: Selector
waitForDurationSelector = mkSelector "waitForDuration:"

-- | @Selector@ for @waitForDuration:withRange:@
waitForDuration_withRangeSelector :: Selector
waitForDuration_withRangeSelector = mkSelector "waitForDuration:withRange:"

-- | @Selector@ for @removeFromParentNode@
removeFromParentNodeSelector :: Selector
removeFromParentNodeSelector = mkSelector "removeFromParentNode"

-- | @Selector@ for @runBlock:@
runBlockSelector :: Selector
runBlockSelector = mkSelector "runBlock:"

-- | @Selector@ for @runBlock:queue:@
runBlock_queueSelector :: Selector
runBlock_queueSelector = mkSelector "runBlock:queue:"

-- | @Selector@ for @javaScriptActionWithScript:duration:@
javaScriptActionWithScript_durationSelector :: Selector
javaScriptActionWithScript_durationSelector = mkSelector "javaScriptActionWithScript:duration:"

-- | @Selector@ for @customActionWithDuration:actionBlock:@
customActionWithDuration_actionBlockSelector :: Selector
customActionWithDuration_actionBlockSelector = mkSelector "customActionWithDuration:actionBlock:"

-- | @Selector@ for @playAudioSource:waitForCompletion:@
playAudioSource_waitForCompletionSelector :: Selector
playAudioSource_waitForCompletionSelector = mkSelector "playAudioSource:waitForCompletion:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @timingMode@
timingModeSelector :: Selector
timingModeSelector = mkSelector "timingMode"

-- | @Selector@ for @setTimingMode:@
setTimingModeSelector :: Selector
setTimingModeSelector = mkSelector "setTimingMode:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

