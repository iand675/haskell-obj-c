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
  , playSelector
  , stopSelector
  , stopWithBlendOutDurationSelector
  , animationSelector
  , speedSelector
  , setSpeedSelector
  , blendFactorSelector
  , setBlendFactorSelector
  , pausedSelector
  , setPausedSelector


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

-- | Initialize an animation player with an animation
--
-- @animation@ — The animation to play
--
-- ObjC selector: @+ animationPlayerWithAnimation:@
animationPlayerWithAnimation :: IsSCNAnimation animation => animation -> IO (Id SCNAnimationPlayer)
animationPlayerWithAnimation animation =
  do
    cls' <- getRequiredClass "SCNAnimationPlayer"
    withObjCPtr animation $ \raw_animation ->
      sendClassMsg cls' (mkSelector "animationPlayerWithAnimation:") (retPtr retVoid) [argPtr (castPtr raw_animation :: Ptr ())] >>= retainedObject . castPtr

-- | Set paused to NO and restart playing from the beginning of the animation.
--
-- ObjC selector: @- play@
play :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO ()
play scnAnimationPlayer  =
  sendMsg scnAnimationPlayer (mkSelector "play") retVoid []

-- | Stop the animation.
--
-- ObjC selector: @- stop@
stop :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO ()
stop scnAnimationPlayer  =
  sendMsg scnAnimationPlayer (mkSelector "stop") retVoid []

-- | Stop the animation and smoothly blend out the animation over the specified duration.
--
-- ObjC selector: @- stopWithBlendOutDuration:@
stopWithBlendOutDuration :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> CDouble -> IO ()
stopWithBlendOutDuration scnAnimationPlayer  duration =
  sendMsg scnAnimationPlayer (mkSelector "stopWithBlendOutDuration:") retVoid [argCDouble (fromIntegral duration)]

-- | The played animation
--
-- ObjC selector: @- animation@
animation :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO (Id SCNAnimation)
animation scnAnimationPlayer  =
  sendMsg scnAnimationPlayer (mkSelector "animation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The speed to play the animation at. Defaults to 1.0. Animatable
--
-- ObjC selector: @- speed@
speed :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO CDouble
speed scnAnimationPlayer  =
  sendMsg scnAnimationPlayer (mkSelector "speed") retCDouble []

-- | The speed to play the animation at. Defaults to 1.0. Animatable
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> CDouble -> IO ()
setSpeed scnAnimationPlayer  value =
  sendMsg scnAnimationPlayer (mkSelector "setSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | Controls the influence of the played animation. When set to 1 the animation is applied without any blending. When set to less than 1, the animation value is blent with the current presentation value of the animated property. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- blendFactor@
blendFactor :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO CDouble
blendFactor scnAnimationPlayer  =
  sendMsg scnAnimationPlayer (mkSelector "blendFactor") retCDouble []

-- | Controls the influence of the played animation. When set to 1 the animation is applied without any blending. When set to less than 1, the animation value is blent with the current presentation value of the animated property. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- setBlendFactor:@
setBlendFactor :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> CDouble -> IO ()
setBlendFactor scnAnimationPlayer  value =
  sendMsg scnAnimationPlayer (mkSelector "setBlendFactor:") retVoid [argCDouble (fromIntegral value)]

-- | Specifies if the animation is paused. Defaults to NO.
--
-- ObjC selector: @- paused@
paused :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> IO Bool
paused scnAnimationPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAnimationPlayer (mkSelector "paused") retCULong []

-- | Specifies if the animation is paused. Defaults to NO.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSCNAnimationPlayer scnAnimationPlayer => scnAnimationPlayer -> Bool -> IO ()
setPaused scnAnimationPlayer  value =
  sendMsg scnAnimationPlayer (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationPlayerWithAnimation:@
animationPlayerWithAnimationSelector :: Selector
animationPlayerWithAnimationSelector = mkSelector "animationPlayerWithAnimation:"

-- | @Selector@ for @play@
playSelector :: Selector
playSelector = mkSelector "play"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @stopWithBlendOutDuration:@
stopWithBlendOutDurationSelector :: Selector
stopWithBlendOutDurationSelector = mkSelector "stopWithBlendOutDuration:"

-- | @Selector@ for @animation@
animationSelector :: Selector
animationSelector = mkSelector "animation"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @blendFactor@
blendFactorSelector :: Selector
blendFactorSelector = mkSelector "blendFactor"

-- | @Selector@ for @setBlendFactor:@
setBlendFactorSelector :: Selector
setBlendFactorSelector = mkSelector "setBlendFactor:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

