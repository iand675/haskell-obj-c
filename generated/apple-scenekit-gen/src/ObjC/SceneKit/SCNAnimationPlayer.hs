{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNAnimationPlayer let you control when and how to play and blend an animation
--
-- Generated bindings for @SCNAnimationPlayer@.
module ObjC.SceneKit.SCNAnimationPlayer
  ( SCNAnimationPlayer
  , IsSCNAnimationPlayer(..)
  , animationPlayerWithAnimation
  , play
  , stop
  , stopWithBlendOutDuration
  , animation
  , speed
  , setSpeed
  , blendFactor
  , setBlendFactor
  , paused
  , setPaused
  , animationPlayerWithAnimationSelector
  , animationSelector
  , blendFactorSelector
  , pausedSelector
  , playSelector
  , setBlendFactorSelector
  , setPausedSelector
  , setSpeedSelector
  , speedSelector
  , stopSelector
  , stopWithBlendOutDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an animation player with an animation
--
-- @animation@ — The animation to play
--
-- ObjC selector: @+ animationPlayerWithAnimation:@
animationPlayerWithAnimation :: IsSCNAnimation animation => animation -> IO (Id SCNAnimationPlayer)
animationPlayerWithAnimation animation =
  do
    cls' <- getRequiredClass "SCNAnimationPlayer"
    sendClassMessage cls' animationPlayerWithAnimationSelector (toSCNAnimation animation)

-- | Set paused to NO and restart playing from the beginning of the animation.
--
-- ObjC selector: @- play@
play :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO ()
play scnAnimationPlayer =
  sendMessage scnAnimationPlayer playSelector

-- | Stop the animation.
--
-- ObjC selector: @- stop@
stop :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO ()
stop scnAnimationPlayer =
  sendMessage scnAnimationPlayer stopSelector

-- | Stop the animation and smoothly blend out the animation over the specified duration.
--
-- ObjC selector: @- stopWithBlendOutDuration:@
stopWithBlendOutDuration :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> CDouble -> IO ()
stopWithBlendOutDuration scnAnimationPlayer duration =
  sendMessage scnAnimationPlayer stopWithBlendOutDurationSelector duration

-- | The played animation
--
-- ObjC selector: @- animation@
animation :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO (Id SCNAnimation)
animation scnAnimationPlayer =
  sendMessage scnAnimationPlayer animationSelector

-- | The speed to play the animation at. Defaults to 1.0. Animatable
--
-- ObjC selector: @- speed@
speed :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO CDouble
speed scnAnimationPlayer =
  sendMessage scnAnimationPlayer speedSelector

-- | The speed to play the animation at. Defaults to 1.0. Animatable
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> CDouble -> IO ()
setSpeed scnAnimationPlayer value =
  sendMessage scnAnimationPlayer setSpeedSelector value

-- | Controls the influence of the played animation. When set to 1 the animation is applied without any blending. When set to less than 1, the animation value is blent with the current presentation value of the animated property. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- blendFactor@
blendFactor :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO CDouble
blendFactor scnAnimationPlayer =
  sendMessage scnAnimationPlayer blendFactorSelector

-- | Controls the influence of the played animation. When set to 1 the animation is applied without any blending. When set to less than 1, the animation value is blent with the current presentation value of the animated property. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- setBlendFactor:@
setBlendFactor :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> CDouble -> IO ()
setBlendFactor scnAnimationPlayer value =
  sendMessage scnAnimationPlayer setBlendFactorSelector value

-- | Specifies if the animation is paused. Defaults to NO.
--
-- ObjC selector: @- paused@
paused :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO Bool
paused scnAnimationPlayer =
  sendMessage scnAnimationPlayer pausedSelector

-- | Specifies if the animation is paused. Defaults to NO.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> Bool -> IO ()
setPaused scnAnimationPlayer value =
  sendMessage scnAnimationPlayer setPausedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationPlayerWithAnimation:@
animationPlayerWithAnimationSelector :: Selector '[Id SCNAnimation] (Id SCNAnimationPlayer)
animationPlayerWithAnimationSelector = mkSelector "animationPlayerWithAnimation:"

-- | @Selector@ for @play@
playSelector :: Selector '[] ()
playSelector = mkSelector "play"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @stopWithBlendOutDuration:@
stopWithBlendOutDurationSelector :: Selector '[CDouble] ()
stopWithBlendOutDurationSelector = mkSelector "stopWithBlendOutDuration:"

-- | @Selector@ for @animation@
animationSelector :: Selector '[] (Id SCNAnimation)
animationSelector = mkSelector "animation"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CDouble
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[CDouble] ()
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @blendFactor@
blendFactorSelector :: Selector '[] CDouble
blendFactorSelector = mkSelector "blendFactor"

-- | @Selector@ for @setBlendFactor:@
setBlendFactorSelector :: Selector '[CDouble] ()
setBlendFactorSelector = mkSelector "setBlendFactor:"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

