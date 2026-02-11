{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The base animation class. *
--
-- Generated bindings for @CAAnimation@.
module ObjC.SceneKit.CAAnimation
  ( CAAnimation
  , IsCAAnimation(..)
  , animationWithSCNAnimation
  , usesSceneTimeBase
  , setUsesSceneTimeBase
  , fadeInDuration
  , setFadeInDuration
  , fadeOutDuration
  , setFadeOutDuration
  , animationWithSCNAnimationSelector
  , usesSceneTimeBaseSelector
  , setUsesSceneTimeBaseSelector
  , fadeInDurationSelector
  , setFadeInDurationSelector
  , fadeOutDurationSelector
  , setFadeOutDurationSelector


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

-- | Bridge with SCNAnimation
--
-- Initializes a CoreAnimation animation from a SCNAnimation
--
-- ObjC selector: @+ animationWithSCNAnimation:@
animationWithSCNAnimation :: IsSCNAnimation animation => animation -> IO (Id CAAnimation)
animationWithSCNAnimation animation =
  do
    cls' <- getRequiredClass "CAAnimation"
    withObjCPtr animation $ \raw_animation ->
      sendClassMsg cls' (mkSelector "animationWithSCNAnimation:") (retPtr retVoid) [argPtr (castPtr raw_animation :: Ptr ())] >>= retainedObject . castPtr

-- | usesSceneTimeBase
--
-- Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene.
--
-- ObjC selector: @- usesSceneTimeBase@
usesSceneTimeBase :: IsCAAnimation caAnimation => caAnimation -> IO Bool
usesSceneTimeBase caAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caAnimation (mkSelector "usesSceneTimeBase") retCULong []

-- | usesSceneTimeBase
--
-- Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene.
--
-- ObjC selector: @- setUsesSceneTimeBase:@
setUsesSceneTimeBase :: IsCAAnimation caAnimation => caAnimation -> Bool -> IO ()
setUsesSceneTimeBase caAnimation  value =
  sendMsg caAnimation (mkSelector "setUsesSceneTimeBase:") retVoid [argCULong (if value then 1 else 0)]

-- | fadeInDuration
--
-- Determines the receiver's fade-in duration.
--
-- When the fadeInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- fadeInDuration@
fadeInDuration :: IsCAAnimation caAnimation => caAnimation -> IO CDouble
fadeInDuration caAnimation  =
  sendMsg caAnimation (mkSelector "fadeInDuration") retCDouble []

-- | fadeInDuration
--
-- Determines the receiver's fade-in duration.
--
-- When the fadeInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- setFadeInDuration:@
setFadeInDuration :: IsCAAnimation caAnimation => caAnimation -> CDouble -> IO ()
setFadeInDuration caAnimation  value =
  sendMsg caAnimation (mkSelector "setFadeInDuration:") retVoid [argCDouble (fromIntegral value)]

-- | fadeOutDuration
--
-- Determines the receiver's fade-out duration.
--
-- When the fadeOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- fadeOutDuration@
fadeOutDuration :: IsCAAnimation caAnimation => caAnimation -> IO CDouble
fadeOutDuration caAnimation  =
  sendMsg caAnimation (mkSelector "fadeOutDuration") retCDouble []

-- | fadeOutDuration
--
-- Determines the receiver's fade-out duration.
--
-- When the fadeOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- setFadeOutDuration:@
setFadeOutDuration :: IsCAAnimation caAnimation => caAnimation -> CDouble -> IO ()
setFadeOutDuration caAnimation  value =
  sendMsg caAnimation (mkSelector "setFadeOutDuration:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationWithSCNAnimation:@
animationWithSCNAnimationSelector :: Selector
animationWithSCNAnimationSelector = mkSelector "animationWithSCNAnimation:"

-- | @Selector@ for @usesSceneTimeBase@
usesSceneTimeBaseSelector :: Selector
usesSceneTimeBaseSelector = mkSelector "usesSceneTimeBase"

-- | @Selector@ for @setUsesSceneTimeBase:@
setUsesSceneTimeBaseSelector :: Selector
setUsesSceneTimeBaseSelector = mkSelector "setUsesSceneTimeBase:"

-- | @Selector@ for @fadeInDuration@
fadeInDurationSelector :: Selector
fadeInDurationSelector = mkSelector "fadeInDuration"

-- | @Selector@ for @setFadeInDuration:@
setFadeInDurationSelector :: Selector
setFadeInDurationSelector = mkSelector "setFadeInDuration:"

-- | @Selector@ for @fadeOutDuration@
fadeOutDurationSelector :: Selector
fadeOutDurationSelector = mkSelector "fadeOutDuration"

-- | @Selector@ for @setFadeOutDuration:@
setFadeOutDurationSelector :: Selector
setFadeOutDurationSelector = mkSelector "setFadeOutDuration:"

