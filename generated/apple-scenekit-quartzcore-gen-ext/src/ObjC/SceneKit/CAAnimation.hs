{-# LANGUAGE DataKinds #-}
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
  , fadeInDurationSelector
  , fadeOutDurationSelector
  , setFadeInDurationSelector
  , setFadeOutDurationSelector
  , setUsesSceneTimeBaseSelector
  , usesSceneTimeBaseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
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
    sendClassMessage cls' animationWithSCNAnimationSelector (toSCNAnimation animation)

-- | usesSceneTimeBase
--
-- Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene.
--
-- ObjC selector: @- usesSceneTimeBase@
usesSceneTimeBase :: IsCAAnimation caAnimation => caAnimation -> IO Bool
usesSceneTimeBase caAnimation =
  sendMessage caAnimation usesSceneTimeBaseSelector

-- | usesSceneTimeBase
--
-- Determines whether the receiver is evaluated using the scene time or the system time. Defaults to NO.
--
-- A scene-time based animation is evaluated using the "sceneTime" value of the renderer that renders the scene.
--
-- ObjC selector: @- setUsesSceneTimeBase:@
setUsesSceneTimeBase :: IsCAAnimation caAnimation => caAnimation -> Bool -> IO ()
setUsesSceneTimeBase caAnimation value =
  sendMessage caAnimation setUsesSceneTimeBaseSelector value

-- | fadeInDuration
--
-- Determines the receiver's fade-in duration.
--
-- When the fadeInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- fadeInDuration@
fadeInDuration :: IsCAAnimation caAnimation => caAnimation -> IO CDouble
fadeInDuration caAnimation =
  sendMessage caAnimation fadeInDurationSelector

-- | fadeInDuration
--
-- Determines the receiver's fade-in duration.
--
-- When the fadeInDuration is greater than zero, the effect of the animation progressively increase from 0% to 100% during the specified duration.
--
-- ObjC selector: @- setFadeInDuration:@
setFadeInDuration :: IsCAAnimation caAnimation => caAnimation -> CDouble -> IO ()
setFadeInDuration caAnimation value =
  sendMessage caAnimation setFadeInDurationSelector value

-- | fadeOutDuration
--
-- Determines the receiver's fade-out duration.
--
-- When the fadeOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- fadeOutDuration@
fadeOutDuration :: IsCAAnimation caAnimation => caAnimation -> IO CDouble
fadeOutDuration caAnimation =
  sendMessage caAnimation fadeOutDurationSelector

-- | fadeOutDuration
--
-- Determines the receiver's fade-out duration.
--
-- When the fadeOutDuration is greater than zero, the effect of the animation progressively decrease from 100% to 0% at the end of the animation duration.
--
-- ObjC selector: @- setFadeOutDuration:@
setFadeOutDuration :: IsCAAnimation caAnimation => caAnimation -> CDouble -> IO ()
setFadeOutDuration caAnimation value =
  sendMessage caAnimation setFadeOutDurationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationWithSCNAnimation:@
animationWithSCNAnimationSelector :: Selector '[Id SCNAnimation] (Id CAAnimation)
animationWithSCNAnimationSelector = mkSelector "animationWithSCNAnimation:"

-- | @Selector@ for @usesSceneTimeBase@
usesSceneTimeBaseSelector :: Selector '[] Bool
usesSceneTimeBaseSelector = mkSelector "usesSceneTimeBase"

-- | @Selector@ for @setUsesSceneTimeBase:@
setUsesSceneTimeBaseSelector :: Selector '[Bool] ()
setUsesSceneTimeBaseSelector = mkSelector "setUsesSceneTimeBase:"

-- | @Selector@ for @fadeInDuration@
fadeInDurationSelector :: Selector '[] CDouble
fadeInDurationSelector = mkSelector "fadeInDuration"

-- | @Selector@ for @setFadeInDuration:@
setFadeInDurationSelector :: Selector '[CDouble] ()
setFadeInDurationSelector = mkSelector "setFadeInDuration:"

-- | @Selector@ for @fadeOutDuration@
fadeOutDurationSelector :: Selector '[] CDouble
fadeOutDurationSelector = mkSelector "fadeOutDuration"

-- | @Selector@ for @setFadeOutDuration:@
setFadeOutDurationSelector :: Selector '[CDouble] ()
setFadeOutDurationSelector = mkSelector "setFadeOutDuration:"

