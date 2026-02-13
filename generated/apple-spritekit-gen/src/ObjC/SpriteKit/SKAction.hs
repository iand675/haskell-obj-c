{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An SKAction object is an action that is executed by a node in the scene. Actions are most often used to change the structure and content of the node to which they are attached, but can also make other changes to the scene. When the scene processes its nodes, actions associated with those nodes are evaluated.
--
-- Generated bindings for @SKAction@.
module ObjC.SpriteKit.SKAction
  ( SKAction
  , IsSKAction(..)
  , reversedAction
  , stereoPanTo_duration
  , stereoPanBy_duration
  , changeReverbTo_duration
  , changeReverbBy_duration
  , changeObstructionTo_duration
  , changeObstructionBy_duration
  , changeOcclusionTo_duration
  , changeOcclusionBy_duration
  , warpTo_duration
  , animateWithWarps_times
  , animateWithWarps_times_restore
  , changeVolumeTo_duration
  , changeVolumeBy_duration
  , play
  , pause
  , stop
  , changePlaybackRateTo_duration
  , changePlaybackRateBy_duration
  , changeChargeTo_duration
  , changeChargeBy_duration
  , changeMassTo_duration
  , changeMassBy_duration
  , applyTorque_duration
  , applyAngularImpulse_duration
  , moveByX_y_duration
  , moveToX_duration
  , moveToY_duration
  , rotateByAngle_duration
  , rotateToAngle_duration
  , rotateToAngle_duration_shortestUnitArc
  , resizeByWidth_height_duration
  , resizeToWidth_height_duration
  , resizeToWidth_duration
  , resizeToHeight_duration
  , scaleBy_duration
  , scaleXBy_y_duration
  , scaleTo_duration
  , scaleXTo_y_duration
  , scaleXTo_duration
  , scaleYTo_duration
  , sequence_
  , group
  , repeatAction_count
  , repeatActionForever
  , fadeInWithDuration
  , fadeOutWithDuration
  , fadeAlphaBy_duration
  , fadeAlphaTo_duration
  , hide
  , unhide
  , setTexture
  , setNormalTexture
  , setTexture_resize
  , setNormalTexture_resize
  , animateWithTextures_timePerFrame
  , animateWithNormalTextures_timePerFrame
  , animateWithTextures_timePerFrame_resize_restore
  , animateWithNormalTextures_timePerFrame_resize_restore
  , playSoundFileNamed_waitForCompletion
  , colorizeWithColor_colorBlendFactor_duration
  , colorizeWithColorBlendFactor_duration
  , falloffTo_duration
  , falloffBy_duration
  , followPath_duration
  , followPath_asOffset_orientToPath_duration
  , followPath_speed
  , followPath_asOffset_orientToPath_speed
  , speedBy_duration
  , speedTo_duration
  , reachToNode_rootNode_duration
  , reachToNode_rootNode_velocity
  , strengthTo_duration
  , strengthBy_duration
  , waitForDuration
  , waitForDuration_withRange
  , removeFromParent
  , performSelector_onTarget
  , runBlock
  , runBlock_queue
  , runAction_onChildWithName
  , customActionWithDuration_actionBlock
  , actionNamed
  , actionNamed_duration
  , actionNamed_fromURL
  , actionNamed_fromURL_duration
  , duration
  , setDuration
  , timingMode
  , setTimingMode
  , timingFunction
  , setTimingFunction
  , speed
  , setSpeed
  , actionNamedSelector
  , actionNamed_durationSelector
  , actionNamed_fromURLSelector
  , actionNamed_fromURL_durationSelector
  , animateWithNormalTextures_timePerFrameSelector
  , animateWithNormalTextures_timePerFrame_resize_restoreSelector
  , animateWithTextures_timePerFrameSelector
  , animateWithTextures_timePerFrame_resize_restoreSelector
  , animateWithWarps_timesSelector
  , animateWithWarps_times_restoreSelector
  , applyAngularImpulse_durationSelector
  , applyTorque_durationSelector
  , changeChargeBy_durationSelector
  , changeChargeTo_durationSelector
  , changeMassBy_durationSelector
  , changeMassTo_durationSelector
  , changeObstructionBy_durationSelector
  , changeObstructionTo_durationSelector
  , changeOcclusionBy_durationSelector
  , changeOcclusionTo_durationSelector
  , changePlaybackRateBy_durationSelector
  , changePlaybackRateTo_durationSelector
  , changeReverbBy_durationSelector
  , changeReverbTo_durationSelector
  , changeVolumeBy_durationSelector
  , changeVolumeTo_durationSelector
  , colorizeWithColorBlendFactor_durationSelector
  , colorizeWithColor_colorBlendFactor_durationSelector
  , customActionWithDuration_actionBlockSelector
  , durationSelector
  , fadeAlphaBy_durationSelector
  , fadeAlphaTo_durationSelector
  , fadeInWithDurationSelector
  , fadeOutWithDurationSelector
  , falloffBy_durationSelector
  , falloffTo_durationSelector
  , followPath_asOffset_orientToPath_durationSelector
  , followPath_asOffset_orientToPath_speedSelector
  , followPath_durationSelector
  , followPath_speedSelector
  , groupSelector
  , hideSelector
  , moveByX_y_durationSelector
  , moveToX_durationSelector
  , moveToY_durationSelector
  , pauseSelector
  , performSelector_onTargetSelector
  , playSelector
  , playSoundFileNamed_waitForCompletionSelector
  , reachToNode_rootNode_durationSelector
  , reachToNode_rootNode_velocitySelector
  , removeFromParentSelector
  , repeatActionForeverSelector
  , repeatAction_countSelector
  , resizeByWidth_height_durationSelector
  , resizeToHeight_durationSelector
  , resizeToWidth_durationSelector
  , resizeToWidth_height_durationSelector
  , reversedActionSelector
  , rotateByAngle_durationSelector
  , rotateToAngle_durationSelector
  , rotateToAngle_duration_shortestUnitArcSelector
  , runAction_onChildWithNameSelector
  , runBlockSelector
  , runBlock_queueSelector
  , scaleBy_durationSelector
  , scaleTo_durationSelector
  , scaleXBy_y_durationSelector
  , scaleXTo_durationSelector
  , scaleXTo_y_durationSelector
  , scaleYTo_durationSelector
  , sequenceSelector
  , setDurationSelector
  , setNormalTextureSelector
  , setNormalTexture_resizeSelector
  , setSpeedSelector
  , setTextureSelector
  , setTexture_resizeSelector
  , setTimingFunctionSelector
  , setTimingModeSelector
  , speedBy_durationSelector
  , speedSelector
  , speedTo_durationSelector
  , stereoPanBy_durationSelector
  , stereoPanTo_durationSelector
  , stopSelector
  , strengthBy_durationSelector
  , strengthTo_durationSelector
  , timingFunctionSelector
  , timingModeSelector
  , unhideSelector
  , waitForDurationSelector
  , waitForDuration_withRangeSelector
  , warpTo_durationSelector

  -- * Enum types
  , SKActionTimingMode(SKActionTimingMode)
  , pattern SKActionTimingLinear
  , pattern SKActionTimingEaseIn
  , pattern SKActionTimingEaseOut
  , pattern SKActionTimingEaseInEaseOut

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an action that reverses the behavior of another action
--
-- Returns: This method always returns an action object; however, not all actions are reversible
--
-- ObjC selector: @- reversedAction@
reversedAction :: IsSKAction skAction => skAction -> IO (Id SKAction)
reversedAction skAction =
  sendMessage skAction reversedActionSelector

-- | @+ stereoPanTo:duration:@
stereoPanTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
stereoPanTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' stereoPanTo_durationSelector v duration

-- | @+ stereoPanBy:duration:@
stereoPanBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
stereoPanBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' stereoPanBy_durationSelector v duration

-- | @+ changeReverbTo:duration:@
changeReverbTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeReverbTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeReverbTo_durationSelector v duration

-- | @+ changeReverbBy:duration:@
changeReverbBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeReverbBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeReverbBy_durationSelector v duration

-- | @+ changeObstructionTo:duration:@
changeObstructionTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeObstructionTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeObstructionTo_durationSelector v duration

-- | @+ changeObstructionBy:duration:@
changeObstructionBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeObstructionBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeObstructionBy_durationSelector v duration

-- | @+ changeOcclusionTo:duration:@
changeOcclusionTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeOcclusionTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeOcclusionTo_durationSelector v duration

-- | @+ changeOcclusionBy:duration:@
changeOcclusionBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeOcclusionBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeOcclusionBy_durationSelector v duration

-- | @+ warpTo:duration:@
warpTo_duration :: IsSKWarpGeometry warp => warp -> CDouble -> IO (Id SKAction)
warpTo_duration warp duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' warpTo_durationSelector (toSKWarpGeometry warp) duration

-- | @+ animateWithWarps:times:@
animateWithWarps_times :: (IsNSArray warps, IsNSArray times) => warps -> times -> IO (Id SKAction)
animateWithWarps_times warps times =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' animateWithWarps_timesSelector (toNSArray warps) (toNSArray times)

-- | @+ animateWithWarps:times:restore:@
animateWithWarps_times_restore :: (IsNSArray warps, IsNSArray times) => warps -> times -> Bool -> IO (Id SKAction)
animateWithWarps_times_restore warps times restore =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' animateWithWarps_times_restoreSelector (toNSArray warps) (toNSArray times) restore

-- | @+ changeVolumeTo:duration:@
changeVolumeTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeVolumeTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeVolumeTo_durationSelector v duration

-- | @+ changeVolumeBy:duration:@
changeVolumeBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeVolumeBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeVolumeBy_durationSelector v duration

-- | @+ play@
play :: IO (Id SKAction)
play  =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' playSelector

-- | @+ pause@
pause :: IO (Id SKAction)
pause  =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' pauseSelector

-- | @+ stop@
stop :: IO (Id SKAction)
stop  =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' stopSelector

-- | @+ changePlaybackRateTo:duration:@
changePlaybackRateTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changePlaybackRateTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changePlaybackRateTo_durationSelector v duration

-- | @+ changePlaybackRateBy:duration:@
changePlaybackRateBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changePlaybackRateBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changePlaybackRateBy_durationSelector v duration

-- | @+ changeChargeTo:duration:@
changeChargeTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeChargeTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeChargeTo_durationSelector v duration

-- | @+ changeChargeBy:duration:@
changeChargeBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeChargeBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeChargeBy_durationSelector v duration

-- | @+ changeMassTo:duration:@
changeMassTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeMassTo_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeMassTo_durationSelector v duration

-- | @+ changeMassBy:duration:@
changeMassBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
changeMassBy_duration v duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' changeMassBy_durationSelector v duration

-- | @+ applyTorque:duration:@
applyTorque_duration :: CDouble -> CDouble -> IO (Id SKAction)
applyTorque_duration torque duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' applyTorque_durationSelector torque duration

-- | @+ applyAngularImpulse:duration:@
applyAngularImpulse_duration :: CDouble -> CDouble -> IO (Id SKAction)
applyAngularImpulse_duration impulse duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' applyAngularImpulse_durationSelector impulse duration

-- | @+ moveByX:y:duration:@
moveByX_y_duration :: CDouble -> CDouble -> CDouble -> IO (Id SKAction)
moveByX_y_duration deltaX deltaY duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' moveByX_y_durationSelector deltaX deltaY duration

-- | @+ moveToX:duration:@
moveToX_duration :: CDouble -> CDouble -> IO (Id SKAction)
moveToX_duration x duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' moveToX_durationSelector x duration

-- | @+ moveToY:duration:@
moveToY_duration :: CDouble -> CDouble -> IO (Id SKAction)
moveToY_duration y duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' moveToY_durationSelector y duration

-- | Creates an action that rotates the node by a relative value
--
-- @radians@ — The amount to rotate the node, in radians
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ rotateByAngle:duration:@
rotateByAngle_duration :: CDouble -> CDouble -> IO (Id SKAction)
rotateByAngle_duration radians duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' rotateByAngle_durationSelector radians duration

-- | Creates an action that rotates the node counterclockwise to an absolute angle
--
-- @radians@ — The angle to rotate the node to, in radians
--
-- @duration@ — The duration of the animation
--
-- ObjC selector: @+ rotateToAngle:duration:@
rotateToAngle_duration :: CDouble -> CDouble -> IO (Id SKAction)
rotateToAngle_duration radians duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' rotateToAngle_durationSelector radians duration

-- | Creates an action that rotates the node to an absolute value
--
-- @radians@ — The angle to rotate the node to, in radians
--
-- @duration@ — The duration of the animation, in seconds
--
-- @shortestUnitArc@ — If YES, then the rotation is performed in whichever direction results in the smallest rotation. If NO, then the rotation is interpolated
--
-- ObjC selector: @+ rotateToAngle:duration:shortestUnitArc:@
rotateToAngle_duration_shortestUnitArc :: CDouble -> CDouble -> Bool -> IO (Id SKAction)
rotateToAngle_duration_shortestUnitArc radians duration shortestUnitArc =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' rotateToAngle_duration_shortestUnitArcSelector radians duration shortestUnitArc

-- | Creates an action that adjusts the size of a sprite
--
-- @width@ — The amount to add to the sprite’s width
--
-- @height@ — The amount to add to the sprite’s height
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ resizeByWidth:height:duration:@
resizeByWidth_height_duration :: CDouble -> CDouble -> CDouble -> IO (Id SKAction)
resizeByWidth_height_duration width height duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' resizeByWidth_height_durationSelector width height duration

-- | Creates an action that changes the width and height of a sprite to a new absolute value
--
-- @width@ — The new width of the sprite
--
-- @height@ — The new height of the sprite
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ resizeToWidth:height:duration:@
resizeToWidth_height_duration :: CDouble -> CDouble -> CDouble -> IO (Id SKAction)
resizeToWidth_height_duration width height duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' resizeToWidth_height_durationSelector width height duration

-- | @+ resizeToWidth:duration:@
resizeToWidth_duration :: CDouble -> CDouble -> IO (Id SKAction)
resizeToWidth_duration width duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' resizeToWidth_durationSelector width duration

-- | @+ resizeToHeight:duration:@
resizeToHeight_duration :: CDouble -> CDouble -> IO (Id SKAction)
resizeToHeight_duration height duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' resizeToHeight_durationSelector height duration

-- | Creates an action that changes the x and y scale values of a node by a relative value
--
-- @scale@ — The amount to modify to the node’s x and y scale values
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ scaleBy:duration:@
scaleBy_duration :: CDouble -> CDouble -> IO (Id SKAction)
scaleBy_duration scale duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' scaleBy_durationSelector scale duration

-- | @+ scaleXBy:y:duration:@
scaleXBy_y_duration :: CDouble -> CDouble -> CDouble -> IO (Id SKAction)
scaleXBy_y_duration xScale yScale duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' scaleXBy_y_durationSelector xScale yScale duration

-- | Creates an action that changes the x and y scale values of a node by a relative value
--
-- @scale@ — The new value for the node’s x and y scale values
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ scaleTo:duration:@
scaleTo_duration :: CDouble -> CDouble -> IO (Id SKAction)
scaleTo_duration scale duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' scaleTo_durationSelector scale duration

-- | @+ scaleXTo:y:duration:@
scaleXTo_y_duration :: CDouble -> CDouble -> CDouble -> IO (Id SKAction)
scaleXTo_y_duration xScale yScale duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' scaleXTo_y_durationSelector xScale yScale duration

-- | @+ scaleXTo:duration:@
scaleXTo_duration :: CDouble -> CDouble -> IO (Id SKAction)
scaleXTo_duration scale duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' scaleXTo_durationSelector scale duration

-- | @+ scaleYTo:duration:@
scaleYTo_duration :: CDouble -> CDouble -> IO (Id SKAction)
scaleYTo_duration scale duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' scaleYTo_durationSelector scale duration

-- | Creates an action that runs a collection of actions sequentially
--
-- @actions@ — An array of SKAction objects
--
-- When the action executes, the first action in the sequence starts and runs to completion. Subsequent actions in the sequence run in a similar fashion until all of the actions in the sequence have executed. The duration of the sequence action is the sum of the durations of the actions in the sequence.
--
-- This action is reversible; it creates a new sequence action that reverses the order of the actions. Each action in the reversed sequence is itself reversed. For example, if an action sequence is {1,2,3}, the reversed sequence would be {3R,2R,1R}.
--
-- ObjC selector: @+ sequence:@
sequence_ :: IsNSArray actions => actions -> IO (Id SKAction)
sequence_ actions =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' sequenceSelector (toNSArray actions)

-- | Creates an action that runs a collection of actions concurrently
--
-- @actions@ — An array of SKAction objects
--
-- When the action executes, the actions that comprise the group all start immediately and run in parallel. The duration of the group action is the longest duration among the collection of actions. If an action in the group has a duration less than the group’s duration, the action completes, then idles until the group completes the remaining actions. This matters most when creating a repeating action that repeats a group.
--
-- ObjC selector: @+ group:@
group :: IsNSArray actions => actions -> IO (Id SKAction)
group actions =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' groupSelector (toNSArray actions)

-- | Creates an action that repeats another action a specified number of times
--
-- @action@ — The action to execute
--
-- @count@ — The number of times to execute the action
--
-- ObjC selector: @+ repeatAction:count:@
repeatAction_count :: IsSKAction action => action -> CULong -> IO (Id SKAction)
repeatAction_count action count =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' repeatAction_countSelector (toSKAction action) count

-- | Creates an action that repeats forever
--
-- @action@ — The action to execute
--
-- ObjC selector: @+ repeatActionForever:@
repeatActionForever :: IsSKAction action => action -> IO (Id SKAction)
repeatActionForever action =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' repeatActionForeverSelector (toSKAction action)

-- | Creates an action that changes the alpha value of the node to 1.0
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ fadeInWithDuration:@
fadeInWithDuration :: CDouble -> IO (Id SKAction)
fadeInWithDuration duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' fadeInWithDurationSelector duration

-- | Creates an action that changes the alpha value of the node to 0.0
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ fadeOutWithDuration:@
fadeOutWithDuration :: CDouble -> IO (Id SKAction)
fadeOutWithDuration duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' fadeOutWithDurationSelector duration

-- | Creates an action that adjusts the alpha value of a node by a relative value
--
-- @factor@ — The amount to modify the node’s alpha value
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ fadeAlphaBy:duration:@
fadeAlphaBy_duration :: CDouble -> CDouble -> IO (Id SKAction)
fadeAlphaBy_duration factor duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' fadeAlphaBy_durationSelector factor duration

-- | Creates an action that adjusts the alpha value of a node to a new value
--
-- @alpha@ — The new value of the node’s alpha
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ fadeAlphaTo:duration:@
fadeAlphaTo_duration :: CDouble -> CDouble -> IO (Id SKAction)
fadeAlphaTo_duration alpha duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' fadeAlphaTo_durationSelector alpha duration

-- | Creates an action that hides a node
--
-- ObjC selector: @+ hide@
hide :: IO (Id SKAction)
hide  =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' hideSelector

-- | Creates an action that unhides a node
--
-- ObjC selector: @+ unhide@
unhide :: IO (Id SKAction)
unhide  =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' unhideSelector

-- | Creates an action that changes a sprite’s texture
--
-- @texture@ — The new texture to use on the sprite
--
-- ObjC selector: @+ setTexture:@
setTexture :: IsSKTexture texture => texture -> IO (Id SKAction)
setTexture texture =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' setTextureSelector (toSKTexture texture)

-- | @+ setNormalTexture:@
setNormalTexture :: IsSKTexture texture => texture -> IO (Id SKAction)
setNormalTexture texture =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' setNormalTextureSelector (toSKTexture texture)

-- | Creates an action that changes a sprite’s texture, possibly resizing the sprite
--
-- @texture@ — The new texture to use on the sprite
--
-- @resize@ — If YES, the sprite is resized to match the new texture. If NO, the size of the sprite is unchanged.
--
-- ObjC selector: @+ setTexture:resize:@
setTexture_resize :: IsSKTexture texture => texture -> Bool -> IO (Id SKAction)
setTexture_resize texture resize =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' setTexture_resizeSelector (toSKTexture texture) resize

-- | @+ setNormalTexture:resize:@
setNormalTexture_resize :: IsSKTexture texture => texture -> Bool -> IO (Id SKAction)
setNormalTexture_resize texture resize =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' setNormalTexture_resizeSelector (toSKTexture texture) resize

-- | Creates an action that animates changes to a sprite’s texture
--
-- @textures@ — An array of textures to use when animating a sprite
--
-- @sec@ — The amount of time that each texture is displayed
--
-- ObjC selector: @+ animateWithTextures:timePerFrame:@
animateWithTextures_timePerFrame :: IsNSArray textures => textures -> CDouble -> IO (Id SKAction)
animateWithTextures_timePerFrame textures sec =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' animateWithTextures_timePerFrameSelector (toNSArray textures) sec

-- | @+ animateWithNormalTextures:timePerFrame:@
animateWithNormalTextures_timePerFrame :: IsNSArray textures => textures -> CDouble -> IO (Id SKAction)
animateWithNormalTextures_timePerFrame textures sec =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' animateWithNormalTextures_timePerFrameSelector (toNSArray textures) sec

-- | Creates an action that animates changes to a sprite’s texture
--
-- @textures@ — An array of textures to use when animating a sprite
--
-- @sec@ — The amount of time that each texture is displayed
--
-- @resize@ — If YES, the sprite is resized to match each new texture. If NO, the size of the sprite remains at a constant size.
--
-- @restore@ — If YES, When the action completes, the sprite’s texture is restored to the texture it had before the action completed. (If the resize parameter is YES, the sprite is resized to match the size of the original texture. If NO, when the action completes the sprite’s texture remains set to the final texture in the array.
--
-- ObjC selector: @+ animateWithTextures:timePerFrame:resize:restore:@
animateWithTextures_timePerFrame_resize_restore :: IsNSArray textures => textures -> CDouble -> Bool -> Bool -> IO (Id SKAction)
animateWithTextures_timePerFrame_resize_restore textures sec resize restore =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' animateWithTextures_timePerFrame_resize_restoreSelector (toNSArray textures) sec resize restore

-- | @+ animateWithNormalTextures:timePerFrame:resize:restore:@
animateWithNormalTextures_timePerFrame_resize_restore :: IsNSArray textures => textures -> CDouble -> Bool -> Bool -> IO (Id SKAction)
animateWithNormalTextures_timePerFrame_resize_restore textures sec resize restore =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' animateWithNormalTextures_timePerFrame_resize_restoreSelector (toNSArray textures) sec resize restore

-- | Creates an action that plays a sound
--
-- @soundFile@ — The name of a sound file in the app’s bundle
--
-- @wait@ — If YES, then the duration of this action is the same as the length of the audio playback. If NO, the action is considered to have completed immediately.
--
-- The file name must be the name or path of a file of a platform supported audio file format. Use a LinearPCM format audio file with 8 or 16 bits per channel for best performance
--
-- ObjC selector: @+ playSoundFileNamed:waitForCompletion:@
playSoundFileNamed_waitForCompletion :: IsNSString soundFile => soundFile -> Bool -> IO (Id SKAction)
playSoundFileNamed_waitForCompletion soundFile wait =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' playSoundFileNamed_waitForCompletionSelector (toNSString soundFile) wait

-- | Creates an animation that animates a sprite’s color and blend factor
--
-- @color@ — The new color for the sprite
--
-- @colorBlendFactor@ — The new blend factor for the sprite
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ colorizeWithColor:colorBlendFactor:duration:@
colorizeWithColor_colorBlendFactor_duration :: IsNSColor color => color -> CDouble -> CDouble -> IO (Id SKAction)
colorizeWithColor_colorBlendFactor_duration color colorBlendFactor duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' colorizeWithColor_colorBlendFactor_durationSelector (toNSColor color) colorBlendFactor duration

-- | @+ colorizeWithColorBlendFactor:duration:@
colorizeWithColorBlendFactor_duration :: CDouble -> CDouble -> IO (Id SKAction)
colorizeWithColorBlendFactor_duration colorBlendFactor sec =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' colorizeWithColorBlendFactor_durationSelector colorBlendFactor sec

-- | Creates an action that sets the falloff of a field
--
-- @falloff@ — The new value for falloff
--
-- @duration@ — The duration of the animation, in seconds
--
-- See: SKFieldNode
--
-- ObjC selector: @+ falloffTo:duration:@
falloffTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
falloffTo_duration falloff duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' falloffTo_durationSelector falloff duration

-- | Creates an action that sets the falloff of a field
--
-- @falloff@ — The value to modify falloff by
--
-- @duration@ — The duration of the animation, in seconds
--
-- See: SKFieldNode
--
-- ObjC selector: @+ falloffBy:duration:@
falloffBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
falloffBy_duration falloff duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' falloffBy_durationSelector falloff duration

-- | Creates an action that moves the node along a relative path, orienting the node to the path
--
-- @path@ — A Core Graphics path whose coordinates are relative to the node’s current position
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ followPath:duration:@
followPath_duration :: RawId -> CDouble -> IO (Id SKAction)
followPath_duration path duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' followPath_durationSelector path duration

-- | Creates an action that moves the node along a path
--
-- @path@ — A Core Graphics path whose coordinates are relative to the node’s current position
--
-- @offset@ — If YES, the points in the path are relative offsets to the node’s starting position. If NO, the points in the node are absolute coordinate values.
--
-- @orient@ — If YES, the node’s zRotation property animates so that the node turns to follow the path. If NO, the zRotation property of the node is unchanged.
--
-- @duration@ — The duration of the animation
--
-- ObjC selector: @+ followPath:asOffset:orientToPath:duration:@
followPath_asOffset_orientToPath_duration :: RawId -> Bool -> Bool -> CDouble -> IO (Id SKAction)
followPath_asOffset_orientToPath_duration path offset orient duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' followPath_asOffset_orientToPath_durationSelector path offset orient duration

-- | Creates an action that moves the node along a relative path, orienting the node to the path
--
-- @path@ — A Core Graphics path whose coordinates are relative to the node’s current position
--
-- @speed@ — The speed in pixels per second to move along the path
--
-- ObjC selector: @+ followPath:speed:@
followPath_speed :: RawId -> CDouble -> IO (Id SKAction)
followPath_speed path speed =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' followPath_speedSelector path speed

-- | @+ followPath:asOffset:orientToPath:speed:@
followPath_asOffset_orientToPath_speed :: RawId -> Bool -> Bool -> CDouble -> IO (Id SKAction)
followPath_asOffset_orientToPath_speed path offset orient speed =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' followPath_asOffset_orientToPath_speedSelector path offset orient speed

-- | Creates an action that changes how fast the node executes actions by a relative value
--
-- @speed@ — amount to modify the speed by
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ speedBy:duration:@
speedBy_duration :: CDouble -> CDouble -> IO (Id SKAction)
speedBy_duration speed duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' speedBy_durationSelector speed duration

-- | Creates an action that changes how fast the node executes actions
--
-- @speed@ — The new value for the node’s speed
--
-- @duration@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ speedTo:duration:@
speedTo_duration :: CDouble -> CDouble -> IO (Id SKAction)
speedTo_duration speed duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' speedTo_durationSelector speed duration

-- | Creates an action that performs an inverse kinematic reach. This action must be run on a descendent of the rootNode for animation to occur. Running this action on the rootNode itself will not cause any animation to occur.
--
-- @node@ — The node to reach for
--
-- @root@ — Where to start the inverse kinematic operation from
--
-- @sec@ — The duration of the animation, in seconds
--
-- ObjC selector: @+ reachToNode:rootNode:duration:@
reachToNode_rootNode_duration :: (IsSKNode node, IsSKNode root) => node -> root -> CDouble -> IO (Id SKAction)
reachToNode_rootNode_duration node root sec =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' reachToNode_rootNode_durationSelector (toSKNode node) (toSKNode root) sec

-- | Creates an action that performs an inverse kinematic reach. This action must be run on a descendent of the rootNode for animation to occur. Running this action on the rootNode itself will not cause any animation to occur.
--
-- @node@ — The node to reach for
--
-- @root@ — Where to start the inverse kinematic operation from
--
-- @velocity@ — The speed in points per second of the end node in the chain
--
-- ObjC selector: @+ reachToNode:rootNode:velocity:@
reachToNode_rootNode_velocity :: (IsSKNode node, IsSKNode root) => node -> root -> CDouble -> IO (Id SKAction)
reachToNode_rootNode_velocity node root velocity =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' reachToNode_rootNode_velocitySelector (toSKNode node) (toSKNode root) velocity

-- | Creates an action that sets the strength of a field
--
-- @strength@ — The new value for strength
--
-- @duration@ — The duration of the animation, in seconds
--
-- See: SKFieldNode
--
-- ObjC selector: @+ strengthTo:duration:@
strengthTo_duration :: CFloat -> CDouble -> IO (Id SKAction)
strengthTo_duration strength duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' strengthTo_durationSelector strength duration

-- | Creates an action that sets the strength of a field
--
-- @strength@ — The value to modify strength by
--
-- @duration@ — The duration of the animation, in seconds
--
-- See: SKFieldNode
--
-- ObjC selector: @+ strengthBy:duration:@
strengthBy_duration :: CFloat -> CDouble -> IO (Id SKAction)
strengthBy_duration strength duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' strengthBy_durationSelector strength duration

-- | Creates an action that idles for a specified period of time
--
-- @duration@ — The duration of the idle, in seconds
--
-- ObjC selector: @+ waitForDuration:@
waitForDuration :: CDouble -> IO (Id SKAction)
waitForDuration duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' waitForDurationSelector duration

-- | Creates an action that idles for a randomized period of time
--
-- @duration@ — The duration of the idle, in seconds
--
-- @durationRange@ — The range of possible values for the duration
--
-- ObjC selector: @+ waitForDuration:withRange:@
waitForDuration_withRange :: CDouble -> CDouble -> IO (Id SKAction)
waitForDuration_withRange duration durationRange =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' waitForDuration_withRangeSelector duration durationRange

-- | Creates an action that removes the node from its parent
--
-- ObjC selector: @+ removeFromParent@
removeFromParent :: IO (Id SKAction)
removeFromParent  =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' removeFromParentSelector

-- | Creates an action that calls a method on an object
--
-- @selector@ — The selector of the method to call
--
-- @target@ — The target object
--
-- ObjC selector: @+ performSelector:onTarget:@
performSelector_onTarget :: Sel -> RawId -> IO (Id SKAction)
performSelector_onTarget selector target =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' performSelector_onTargetSelector selector target

-- | Creates an action that executes a block
--
-- @block@ — The block to run
--
-- ObjC selector: @+ runBlock:@
runBlock :: Ptr () -> IO (Id SKAction)
runBlock block =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' runBlockSelector block

-- | Creates an action that executes a block
--
-- @block@ — The block to run
--
-- @queue@ — The queue to perform the action on
--
-- ObjC selector: @+ runBlock:queue:@
runBlock_queue :: IsNSObject queue => Ptr () -> queue -> IO (Id SKAction)
runBlock_queue block queue =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' runBlock_queueSelector block (toNSObject queue)

-- | Creates an action that runs an action on a named child object
--
-- @action@ — the action to run
--
-- @name@ — the name of a child object
--
-- See: SKNode.name
--
-- ObjC selector: @+ runAction:onChildWithName:@
runAction_onChildWithName :: (IsSKAction action, IsNSString name) => action -> name -> IO (Id SKAction)
runAction_onChildWithName action name =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' runAction_onChildWithNameSelector (toSKAction action) (toNSString name)

-- | Creates an action that executes a block over a duration
--
-- @duration@ — The duration of the animation, in seconds
--
-- @block@ — The block to run. The block takes the following parameters: node The node on which the action is running. elapsedTime The amount of time that has passed in the animation.
--
-- ObjC selector: @+ customActionWithDuration:actionBlock:@
customActionWithDuration_actionBlock :: CDouble -> Ptr () -> IO (Id SKAction)
customActionWithDuration_actionBlock duration block =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' customActionWithDuration_actionBlockSelector duration block

-- | Creates an action of the given name from an action file.
--
-- @name@ — The name of the action
--
-- ObjC selector: @+ actionNamed:@
actionNamed :: IsNSString name => name -> IO (Id SKAction)
actionNamed name =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' actionNamedSelector (toNSString name)

-- | Creates an action of the given name from an action file with a new duration.
--
-- @name@ — The name of the action
--
-- @duration@ — The duration of the action
--
-- ObjC selector: @+ actionNamed:duration:@
actionNamed_duration :: IsNSString name => name -> CDouble -> IO (Id SKAction)
actionNamed_duration name duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' actionNamed_durationSelector (toNSString name) duration

-- | Creates an action of the given name from an action file.
--
-- @name@ — The name of the action
--
-- @url@ — The url of the file containing the action
--
-- ObjC selector: @+ actionNamed:fromURL:@
actionNamed_fromURL :: (IsNSString name, IsNSURL url) => name -> url -> IO (Id SKAction)
actionNamed_fromURL name url =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' actionNamed_fromURLSelector (toNSString name) (toNSURL url)

-- | Creates an action of the given name from an action file with a new duration.
--
-- @name@ — The name of the action
--
-- @url@ — The url of the file containing the action
--
-- @duration@ — The duration of the action
--
-- ObjC selector: @+ actionNamed:fromURL:duration:@
actionNamed_fromURL_duration :: (IsNSString name, IsNSURL url) => name -> url -> CDouble -> IO (Id SKAction)
actionNamed_fromURL_duration name url duration =
  do
    cls' <- getRequiredClass "SKAction"
    sendClassMessage cls' actionNamed_fromURL_durationSelector (toNSString name) (toNSURL url) duration

-- | The duration required to complete an action, in seconds.
--
-- ObjC selector: @- duration@
duration :: IsSKAction skAction => skAction -> IO CDouble
duration skAction =
  sendMessage skAction durationSelector

-- | The duration required to complete an action, in seconds.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsSKAction skAction => skAction -> CDouble -> IO ()
setDuration skAction value =
  sendMessage skAction setDurationSelector value

-- | The timing mode used to execute an action
--
-- See: SKActionTimingMode
--
-- ObjC selector: @- timingMode@
timingMode :: IsSKAction skAction => skAction -> IO SKActionTimingMode
timingMode skAction =
  sendMessage skAction timingModeSelector

-- | The timing mode used to execute an action
--
-- See: SKActionTimingMode
--
-- ObjC selector: @- setTimingMode:@
setTimingMode :: IsSKAction skAction => skAction -> SKActionTimingMode -> IO ()
setTimingMode skAction value =
  sendMessage skAction setTimingModeSelector value

-- | When set, prodives a custom timing via a block. Applies after the 'timingMode' property is taken into account, defaults to nil
--
-- See: SKActionTimingFunction
--
-- ObjC selector: @- timingFunction@
timingFunction :: IsSKAction skAction => skAction -> IO (Ptr ())
timingFunction skAction =
  sendMessage skAction timingFunctionSelector

-- | When set, prodives a custom timing via a block. Applies after the 'timingMode' property is taken into account, defaults to nil
--
-- See: SKActionTimingFunction
--
-- ObjC selector: @- setTimingFunction:@
setTimingFunction :: IsSKAction skAction => skAction -> Ptr () -> IO ()
setTimingFunction skAction value =
  sendMessage skAction setTimingFunctionSelector value

-- | A speed factor that modifies how fast an action runs. Default value is 1.0
--
-- ObjC selector: @- speed@
speed :: IsSKAction skAction => skAction -> IO CDouble
speed skAction =
  sendMessage skAction speedSelector

-- | A speed factor that modifies how fast an action runs. Default value is 1.0
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSKAction skAction => skAction -> CDouble -> IO ()
setSpeed skAction value =
  sendMessage skAction setSpeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reversedAction@
reversedActionSelector :: Selector '[] (Id SKAction)
reversedActionSelector = mkSelector "reversedAction"

-- | @Selector@ for @stereoPanTo:duration:@
stereoPanTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
stereoPanTo_durationSelector = mkSelector "stereoPanTo:duration:"

-- | @Selector@ for @stereoPanBy:duration:@
stereoPanBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
stereoPanBy_durationSelector = mkSelector "stereoPanBy:duration:"

-- | @Selector@ for @changeReverbTo:duration:@
changeReverbTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeReverbTo_durationSelector = mkSelector "changeReverbTo:duration:"

-- | @Selector@ for @changeReverbBy:duration:@
changeReverbBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeReverbBy_durationSelector = mkSelector "changeReverbBy:duration:"

-- | @Selector@ for @changeObstructionTo:duration:@
changeObstructionTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeObstructionTo_durationSelector = mkSelector "changeObstructionTo:duration:"

-- | @Selector@ for @changeObstructionBy:duration:@
changeObstructionBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeObstructionBy_durationSelector = mkSelector "changeObstructionBy:duration:"

-- | @Selector@ for @changeOcclusionTo:duration:@
changeOcclusionTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeOcclusionTo_durationSelector = mkSelector "changeOcclusionTo:duration:"

-- | @Selector@ for @changeOcclusionBy:duration:@
changeOcclusionBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeOcclusionBy_durationSelector = mkSelector "changeOcclusionBy:duration:"

-- | @Selector@ for @warpTo:duration:@
warpTo_durationSelector :: Selector '[Id SKWarpGeometry, CDouble] (Id SKAction)
warpTo_durationSelector = mkSelector "warpTo:duration:"

-- | @Selector@ for @animateWithWarps:times:@
animateWithWarps_timesSelector :: Selector '[Id NSArray, Id NSArray] (Id SKAction)
animateWithWarps_timesSelector = mkSelector "animateWithWarps:times:"

-- | @Selector@ for @animateWithWarps:times:restore:@
animateWithWarps_times_restoreSelector :: Selector '[Id NSArray, Id NSArray, Bool] (Id SKAction)
animateWithWarps_times_restoreSelector = mkSelector "animateWithWarps:times:restore:"

-- | @Selector@ for @changeVolumeTo:duration:@
changeVolumeTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeVolumeTo_durationSelector = mkSelector "changeVolumeTo:duration:"

-- | @Selector@ for @changeVolumeBy:duration:@
changeVolumeBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeVolumeBy_durationSelector = mkSelector "changeVolumeBy:duration:"

-- | @Selector@ for @play@
playSelector :: Selector '[] (Id SKAction)
playSelector = mkSelector "play"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] (Id SKAction)
pauseSelector = mkSelector "pause"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] (Id SKAction)
stopSelector = mkSelector "stop"

-- | @Selector@ for @changePlaybackRateTo:duration:@
changePlaybackRateTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changePlaybackRateTo_durationSelector = mkSelector "changePlaybackRateTo:duration:"

-- | @Selector@ for @changePlaybackRateBy:duration:@
changePlaybackRateBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changePlaybackRateBy_durationSelector = mkSelector "changePlaybackRateBy:duration:"

-- | @Selector@ for @changeChargeTo:duration:@
changeChargeTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeChargeTo_durationSelector = mkSelector "changeChargeTo:duration:"

-- | @Selector@ for @changeChargeBy:duration:@
changeChargeBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeChargeBy_durationSelector = mkSelector "changeChargeBy:duration:"

-- | @Selector@ for @changeMassTo:duration:@
changeMassTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeMassTo_durationSelector = mkSelector "changeMassTo:duration:"

-- | @Selector@ for @changeMassBy:duration:@
changeMassBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
changeMassBy_durationSelector = mkSelector "changeMassBy:duration:"

-- | @Selector@ for @applyTorque:duration:@
applyTorque_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
applyTorque_durationSelector = mkSelector "applyTorque:duration:"

-- | @Selector@ for @applyAngularImpulse:duration:@
applyAngularImpulse_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
applyAngularImpulse_durationSelector = mkSelector "applyAngularImpulse:duration:"

-- | @Selector@ for @moveByX:y:duration:@
moveByX_y_durationSelector :: Selector '[CDouble, CDouble, CDouble] (Id SKAction)
moveByX_y_durationSelector = mkSelector "moveByX:y:duration:"

-- | @Selector@ for @moveToX:duration:@
moveToX_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
moveToX_durationSelector = mkSelector "moveToX:duration:"

-- | @Selector@ for @moveToY:duration:@
moveToY_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
moveToY_durationSelector = mkSelector "moveToY:duration:"

-- | @Selector@ for @rotateByAngle:duration:@
rotateByAngle_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
rotateByAngle_durationSelector = mkSelector "rotateByAngle:duration:"

-- | @Selector@ for @rotateToAngle:duration:@
rotateToAngle_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
rotateToAngle_durationSelector = mkSelector "rotateToAngle:duration:"

-- | @Selector@ for @rotateToAngle:duration:shortestUnitArc:@
rotateToAngle_duration_shortestUnitArcSelector :: Selector '[CDouble, CDouble, Bool] (Id SKAction)
rotateToAngle_duration_shortestUnitArcSelector = mkSelector "rotateToAngle:duration:shortestUnitArc:"

-- | @Selector@ for @resizeByWidth:height:duration:@
resizeByWidth_height_durationSelector :: Selector '[CDouble, CDouble, CDouble] (Id SKAction)
resizeByWidth_height_durationSelector = mkSelector "resizeByWidth:height:duration:"

-- | @Selector@ for @resizeToWidth:height:duration:@
resizeToWidth_height_durationSelector :: Selector '[CDouble, CDouble, CDouble] (Id SKAction)
resizeToWidth_height_durationSelector = mkSelector "resizeToWidth:height:duration:"

-- | @Selector@ for @resizeToWidth:duration:@
resizeToWidth_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
resizeToWidth_durationSelector = mkSelector "resizeToWidth:duration:"

-- | @Selector@ for @resizeToHeight:duration:@
resizeToHeight_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
resizeToHeight_durationSelector = mkSelector "resizeToHeight:duration:"

-- | @Selector@ for @scaleBy:duration:@
scaleBy_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
scaleBy_durationSelector = mkSelector "scaleBy:duration:"

-- | @Selector@ for @scaleXBy:y:duration:@
scaleXBy_y_durationSelector :: Selector '[CDouble, CDouble, CDouble] (Id SKAction)
scaleXBy_y_durationSelector = mkSelector "scaleXBy:y:duration:"

-- | @Selector@ for @scaleTo:duration:@
scaleTo_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
scaleTo_durationSelector = mkSelector "scaleTo:duration:"

-- | @Selector@ for @scaleXTo:y:duration:@
scaleXTo_y_durationSelector :: Selector '[CDouble, CDouble, CDouble] (Id SKAction)
scaleXTo_y_durationSelector = mkSelector "scaleXTo:y:duration:"

-- | @Selector@ for @scaleXTo:duration:@
scaleXTo_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
scaleXTo_durationSelector = mkSelector "scaleXTo:duration:"

-- | @Selector@ for @scaleYTo:duration:@
scaleYTo_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
scaleYTo_durationSelector = mkSelector "scaleYTo:duration:"

-- | @Selector@ for @sequence:@
sequenceSelector :: Selector '[Id NSArray] (Id SKAction)
sequenceSelector = mkSelector "sequence:"

-- | @Selector@ for @group:@
groupSelector :: Selector '[Id NSArray] (Id SKAction)
groupSelector = mkSelector "group:"

-- | @Selector@ for @repeatAction:count:@
repeatAction_countSelector :: Selector '[Id SKAction, CULong] (Id SKAction)
repeatAction_countSelector = mkSelector "repeatAction:count:"

-- | @Selector@ for @repeatActionForever:@
repeatActionForeverSelector :: Selector '[Id SKAction] (Id SKAction)
repeatActionForeverSelector = mkSelector "repeatActionForever:"

-- | @Selector@ for @fadeInWithDuration:@
fadeInWithDurationSelector :: Selector '[CDouble] (Id SKAction)
fadeInWithDurationSelector = mkSelector "fadeInWithDuration:"

-- | @Selector@ for @fadeOutWithDuration:@
fadeOutWithDurationSelector :: Selector '[CDouble] (Id SKAction)
fadeOutWithDurationSelector = mkSelector "fadeOutWithDuration:"

-- | @Selector@ for @fadeAlphaBy:duration:@
fadeAlphaBy_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
fadeAlphaBy_durationSelector = mkSelector "fadeAlphaBy:duration:"

-- | @Selector@ for @fadeAlphaTo:duration:@
fadeAlphaTo_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
fadeAlphaTo_durationSelector = mkSelector "fadeAlphaTo:duration:"

-- | @Selector@ for @hide@
hideSelector :: Selector '[] (Id SKAction)
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector '[] (Id SKAction)
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector '[Id SKTexture] (Id SKAction)
setTextureSelector = mkSelector "setTexture:"

-- | @Selector@ for @setNormalTexture:@
setNormalTextureSelector :: Selector '[Id SKTexture] (Id SKAction)
setNormalTextureSelector = mkSelector "setNormalTexture:"

-- | @Selector@ for @setTexture:resize:@
setTexture_resizeSelector :: Selector '[Id SKTexture, Bool] (Id SKAction)
setTexture_resizeSelector = mkSelector "setTexture:resize:"

-- | @Selector@ for @setNormalTexture:resize:@
setNormalTexture_resizeSelector :: Selector '[Id SKTexture, Bool] (Id SKAction)
setNormalTexture_resizeSelector = mkSelector "setNormalTexture:resize:"

-- | @Selector@ for @animateWithTextures:timePerFrame:@
animateWithTextures_timePerFrameSelector :: Selector '[Id NSArray, CDouble] (Id SKAction)
animateWithTextures_timePerFrameSelector = mkSelector "animateWithTextures:timePerFrame:"

-- | @Selector@ for @animateWithNormalTextures:timePerFrame:@
animateWithNormalTextures_timePerFrameSelector :: Selector '[Id NSArray, CDouble] (Id SKAction)
animateWithNormalTextures_timePerFrameSelector = mkSelector "animateWithNormalTextures:timePerFrame:"

-- | @Selector@ for @animateWithTextures:timePerFrame:resize:restore:@
animateWithTextures_timePerFrame_resize_restoreSelector :: Selector '[Id NSArray, CDouble, Bool, Bool] (Id SKAction)
animateWithTextures_timePerFrame_resize_restoreSelector = mkSelector "animateWithTextures:timePerFrame:resize:restore:"

-- | @Selector@ for @animateWithNormalTextures:timePerFrame:resize:restore:@
animateWithNormalTextures_timePerFrame_resize_restoreSelector :: Selector '[Id NSArray, CDouble, Bool, Bool] (Id SKAction)
animateWithNormalTextures_timePerFrame_resize_restoreSelector = mkSelector "animateWithNormalTextures:timePerFrame:resize:restore:"

-- | @Selector@ for @playSoundFileNamed:waitForCompletion:@
playSoundFileNamed_waitForCompletionSelector :: Selector '[Id NSString, Bool] (Id SKAction)
playSoundFileNamed_waitForCompletionSelector = mkSelector "playSoundFileNamed:waitForCompletion:"

-- | @Selector@ for @colorizeWithColor:colorBlendFactor:duration:@
colorizeWithColor_colorBlendFactor_durationSelector :: Selector '[Id NSColor, CDouble, CDouble] (Id SKAction)
colorizeWithColor_colorBlendFactor_durationSelector = mkSelector "colorizeWithColor:colorBlendFactor:duration:"

-- | @Selector@ for @colorizeWithColorBlendFactor:duration:@
colorizeWithColorBlendFactor_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
colorizeWithColorBlendFactor_durationSelector = mkSelector "colorizeWithColorBlendFactor:duration:"

-- | @Selector@ for @falloffTo:duration:@
falloffTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
falloffTo_durationSelector = mkSelector "falloffTo:duration:"

-- | @Selector@ for @falloffBy:duration:@
falloffBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
falloffBy_durationSelector = mkSelector "falloffBy:duration:"

-- | @Selector@ for @followPath:duration:@
followPath_durationSelector :: Selector '[RawId, CDouble] (Id SKAction)
followPath_durationSelector = mkSelector "followPath:duration:"

-- | @Selector@ for @followPath:asOffset:orientToPath:duration:@
followPath_asOffset_orientToPath_durationSelector :: Selector '[RawId, Bool, Bool, CDouble] (Id SKAction)
followPath_asOffset_orientToPath_durationSelector = mkSelector "followPath:asOffset:orientToPath:duration:"

-- | @Selector@ for @followPath:speed:@
followPath_speedSelector :: Selector '[RawId, CDouble] (Id SKAction)
followPath_speedSelector = mkSelector "followPath:speed:"

-- | @Selector@ for @followPath:asOffset:orientToPath:speed:@
followPath_asOffset_orientToPath_speedSelector :: Selector '[RawId, Bool, Bool, CDouble] (Id SKAction)
followPath_asOffset_orientToPath_speedSelector = mkSelector "followPath:asOffset:orientToPath:speed:"

-- | @Selector@ for @speedBy:duration:@
speedBy_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
speedBy_durationSelector = mkSelector "speedBy:duration:"

-- | @Selector@ for @speedTo:duration:@
speedTo_durationSelector :: Selector '[CDouble, CDouble] (Id SKAction)
speedTo_durationSelector = mkSelector "speedTo:duration:"

-- | @Selector@ for @reachToNode:rootNode:duration:@
reachToNode_rootNode_durationSelector :: Selector '[Id SKNode, Id SKNode, CDouble] (Id SKAction)
reachToNode_rootNode_durationSelector = mkSelector "reachToNode:rootNode:duration:"

-- | @Selector@ for @reachToNode:rootNode:velocity:@
reachToNode_rootNode_velocitySelector :: Selector '[Id SKNode, Id SKNode, CDouble] (Id SKAction)
reachToNode_rootNode_velocitySelector = mkSelector "reachToNode:rootNode:velocity:"

-- | @Selector@ for @strengthTo:duration:@
strengthTo_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
strengthTo_durationSelector = mkSelector "strengthTo:duration:"

-- | @Selector@ for @strengthBy:duration:@
strengthBy_durationSelector :: Selector '[CFloat, CDouble] (Id SKAction)
strengthBy_durationSelector = mkSelector "strengthBy:duration:"

-- | @Selector@ for @waitForDuration:@
waitForDurationSelector :: Selector '[CDouble] (Id SKAction)
waitForDurationSelector = mkSelector "waitForDuration:"

-- | @Selector@ for @waitForDuration:withRange:@
waitForDuration_withRangeSelector :: Selector '[CDouble, CDouble] (Id SKAction)
waitForDuration_withRangeSelector = mkSelector "waitForDuration:withRange:"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector '[] (Id SKAction)
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @performSelector:onTarget:@
performSelector_onTargetSelector :: Selector '[Sel, RawId] (Id SKAction)
performSelector_onTargetSelector = mkSelector "performSelector:onTarget:"

-- | @Selector@ for @runBlock:@
runBlockSelector :: Selector '[Ptr ()] (Id SKAction)
runBlockSelector = mkSelector "runBlock:"

-- | @Selector@ for @runBlock:queue:@
runBlock_queueSelector :: Selector '[Ptr (), Id NSObject] (Id SKAction)
runBlock_queueSelector = mkSelector "runBlock:queue:"

-- | @Selector@ for @runAction:onChildWithName:@
runAction_onChildWithNameSelector :: Selector '[Id SKAction, Id NSString] (Id SKAction)
runAction_onChildWithNameSelector = mkSelector "runAction:onChildWithName:"

-- | @Selector@ for @customActionWithDuration:actionBlock:@
customActionWithDuration_actionBlockSelector :: Selector '[CDouble, Ptr ()] (Id SKAction)
customActionWithDuration_actionBlockSelector = mkSelector "customActionWithDuration:actionBlock:"

-- | @Selector@ for @actionNamed:@
actionNamedSelector :: Selector '[Id NSString] (Id SKAction)
actionNamedSelector = mkSelector "actionNamed:"

-- | @Selector@ for @actionNamed:duration:@
actionNamed_durationSelector :: Selector '[Id NSString, CDouble] (Id SKAction)
actionNamed_durationSelector = mkSelector "actionNamed:duration:"

-- | @Selector@ for @actionNamed:fromURL:@
actionNamed_fromURLSelector :: Selector '[Id NSString, Id NSURL] (Id SKAction)
actionNamed_fromURLSelector = mkSelector "actionNamed:fromURL:"

-- | @Selector@ for @actionNamed:fromURL:duration:@
actionNamed_fromURL_durationSelector :: Selector '[Id NSString, Id NSURL, CDouble] (Id SKAction)
actionNamed_fromURL_durationSelector = mkSelector "actionNamed:fromURL:duration:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @timingMode@
timingModeSelector :: Selector '[] SKActionTimingMode
timingModeSelector = mkSelector "timingMode"

-- | @Selector@ for @setTimingMode:@
setTimingModeSelector :: Selector '[SKActionTimingMode] ()
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

