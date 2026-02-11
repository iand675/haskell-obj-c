{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A transition style from one scene to another.
--
-- Generated bindings for @SKTransition@.
module ObjC.SpriteKit.SKTransition
  ( SKTransition
  , IsSKTransition(..)
  , crossFadeWithDuration
  , fadeWithDuration
  , fadeWithColor_duration
  , flipHorizontalWithDuration
  , flipVerticalWithDuration
  , revealWithDirection_duration
  , moveInWithDirection_duration
  , pushWithDirection_duration
  , doorsOpenHorizontalWithDuration
  , doorsOpenVerticalWithDuration
  , doorsCloseHorizontalWithDuration
  , doorsCloseVerticalWithDuration
  , doorwayWithDuration
  , transitionWithCIFilter_duration
  , pausesIncomingScene
  , setPausesIncomingScene
  , pausesOutgoingScene
  , setPausesOutgoingScene
  , crossFadeWithDurationSelector
  , fadeWithDurationSelector
  , fadeWithColor_durationSelector
  , flipHorizontalWithDurationSelector
  , flipVerticalWithDurationSelector
  , revealWithDirection_durationSelector
  , moveInWithDirection_durationSelector
  , pushWithDirection_durationSelector
  , doorsOpenHorizontalWithDurationSelector
  , doorsOpenVerticalWithDurationSelector
  , doorsCloseHorizontalWithDurationSelector
  , doorsCloseVerticalWithDurationSelector
  , doorwayWithDurationSelector
  , transitionWithCIFilter_durationSelector
  , pausesIncomingSceneSelector
  , setPausesIncomingSceneSelector
  , pausesOutgoingSceneSelector
  , setPausesOutgoingSceneSelector

  -- * Enum types
  , SKTransitionDirection(SKTransitionDirection)
  , pattern SKTransitionDirectionUp
  , pattern SKTransitionDirectionDown
  , pattern SKTransitionDirectionRight
  , pattern SKTransitionDirectionLeft

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

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ crossFadeWithDuration:@
crossFadeWithDuration :: CDouble -> IO (Id SKTransition)
crossFadeWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "crossFadeWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ fadeWithDuration:@
fadeWithDuration :: CDouble -> IO (Id SKTransition)
fadeWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "fadeWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ fadeWithColor:duration:@
fadeWithColor_duration :: IsNSColor color => color -> CDouble -> IO (Id SKTransition)
fadeWithColor_duration color sec =
  do
    cls' <- getRequiredClass "SKTransition"
    withObjCPtr color $ \raw_color ->
      sendClassMsg cls' (mkSelector "fadeWithColor:duration:") (retPtr retVoid) [argPtr (castPtr raw_color :: Ptr ()), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ flipHorizontalWithDuration:@
flipHorizontalWithDuration :: CDouble -> IO (Id SKTransition)
flipHorizontalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "flipHorizontalWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ flipVerticalWithDuration:@
flipVerticalWithDuration :: CDouble -> IO (Id SKTransition)
flipVerticalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "flipVerticalWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ revealWithDirection:duration:@
revealWithDirection_duration :: SKTransitionDirection -> CDouble -> IO (Id SKTransition)
revealWithDirection_duration direction sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "revealWithDirection:duration:") (retPtr retVoid) [argCLong (coerce direction), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ moveInWithDirection:duration:@
moveInWithDirection_duration :: SKTransitionDirection -> CDouble -> IO (Id SKTransition)
moveInWithDirection_duration direction sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "moveInWithDirection:duration:") (retPtr retVoid) [argCLong (coerce direction), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ pushWithDirection:duration:@
pushWithDirection_duration :: SKTransitionDirection -> CDouble -> IO (Id SKTransition)
pushWithDirection_duration direction sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "pushWithDirection:duration:") (retPtr retVoid) [argCLong (coerce direction), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ doorsOpenHorizontalWithDuration:@
doorsOpenHorizontalWithDuration :: CDouble -> IO (Id SKTransition)
doorsOpenHorizontalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "doorsOpenHorizontalWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ doorsOpenVerticalWithDuration:@
doorsOpenVerticalWithDuration :: CDouble -> IO (Id SKTransition)
doorsOpenVerticalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "doorsOpenVerticalWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ doorsCloseHorizontalWithDuration:@
doorsCloseHorizontalWithDuration :: CDouble -> IO (Id SKTransition)
doorsCloseHorizontalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "doorsCloseHorizontalWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ doorsCloseVerticalWithDuration:@
doorsCloseVerticalWithDuration :: CDouble -> IO (Id SKTransition)
doorsCloseVerticalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "doorsCloseVerticalWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ doorwayWithDuration:@
doorwayWithDuration :: CDouble -> IO (Id SKTransition)
doorwayWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMsg cls' (mkSelector "doorwayWithDuration:") (retPtr retVoid) [argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | @+ transitionWithCIFilter:duration:@
transitionWithCIFilter_duration :: IsCIFilter filter_ => filter_ -> CDouble -> IO (Id SKTransition)
transitionWithCIFilter_duration filter_ sec =
  do
    cls' <- getRequiredClass "SKTransition"
    withObjCPtr filter_ $ \raw_filter_ ->
      sendClassMsg cls' (mkSelector "transitionWithCIFilter:duration:") (retPtr retVoid) [argPtr (castPtr raw_filter_ :: Ptr ()), argCDouble (fromIntegral sec)] >>= retainedObject . castPtr

-- | Pause the incoming Scene during the transition, defaults to YES.
--
-- ObjC selector: @- pausesIncomingScene@
pausesIncomingScene :: IsSKTransition skTransition => skTransition -> IO Bool
pausesIncomingScene skTransition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skTransition (mkSelector "pausesIncomingScene") retCULong []

-- | Pause the incoming Scene during the transition, defaults to YES.
--
-- ObjC selector: @- setPausesIncomingScene:@
setPausesIncomingScene :: IsSKTransition skTransition => skTransition -> Bool -> IO ()
setPausesIncomingScene skTransition  value =
  sendMsg skTransition (mkSelector "setPausesIncomingScene:") retVoid [argCULong (if value then 1 else 0)]

-- | Pause the outgoing Scene during the transition, defaults to YES.
--
-- ObjC selector: @- pausesOutgoingScene@
pausesOutgoingScene :: IsSKTransition skTransition => skTransition -> IO Bool
pausesOutgoingScene skTransition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skTransition (mkSelector "pausesOutgoingScene") retCULong []

-- | Pause the outgoing Scene during the transition, defaults to YES.
--
-- ObjC selector: @- setPausesOutgoingScene:@
setPausesOutgoingScene :: IsSKTransition skTransition => skTransition -> Bool -> IO ()
setPausesOutgoingScene skTransition  value =
  sendMsg skTransition (mkSelector "setPausesOutgoingScene:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @crossFadeWithDuration:@
crossFadeWithDurationSelector :: Selector
crossFadeWithDurationSelector = mkSelector "crossFadeWithDuration:"

-- | @Selector@ for @fadeWithDuration:@
fadeWithDurationSelector :: Selector
fadeWithDurationSelector = mkSelector "fadeWithDuration:"

-- | @Selector@ for @fadeWithColor:duration:@
fadeWithColor_durationSelector :: Selector
fadeWithColor_durationSelector = mkSelector "fadeWithColor:duration:"

-- | @Selector@ for @flipHorizontalWithDuration:@
flipHorizontalWithDurationSelector :: Selector
flipHorizontalWithDurationSelector = mkSelector "flipHorizontalWithDuration:"

-- | @Selector@ for @flipVerticalWithDuration:@
flipVerticalWithDurationSelector :: Selector
flipVerticalWithDurationSelector = mkSelector "flipVerticalWithDuration:"

-- | @Selector@ for @revealWithDirection:duration:@
revealWithDirection_durationSelector :: Selector
revealWithDirection_durationSelector = mkSelector "revealWithDirection:duration:"

-- | @Selector@ for @moveInWithDirection:duration:@
moveInWithDirection_durationSelector :: Selector
moveInWithDirection_durationSelector = mkSelector "moveInWithDirection:duration:"

-- | @Selector@ for @pushWithDirection:duration:@
pushWithDirection_durationSelector :: Selector
pushWithDirection_durationSelector = mkSelector "pushWithDirection:duration:"

-- | @Selector@ for @doorsOpenHorizontalWithDuration:@
doorsOpenHorizontalWithDurationSelector :: Selector
doorsOpenHorizontalWithDurationSelector = mkSelector "doorsOpenHorizontalWithDuration:"

-- | @Selector@ for @doorsOpenVerticalWithDuration:@
doorsOpenVerticalWithDurationSelector :: Selector
doorsOpenVerticalWithDurationSelector = mkSelector "doorsOpenVerticalWithDuration:"

-- | @Selector@ for @doorsCloseHorizontalWithDuration:@
doorsCloseHorizontalWithDurationSelector :: Selector
doorsCloseHorizontalWithDurationSelector = mkSelector "doorsCloseHorizontalWithDuration:"

-- | @Selector@ for @doorsCloseVerticalWithDuration:@
doorsCloseVerticalWithDurationSelector :: Selector
doorsCloseVerticalWithDurationSelector = mkSelector "doorsCloseVerticalWithDuration:"

-- | @Selector@ for @doorwayWithDuration:@
doorwayWithDurationSelector :: Selector
doorwayWithDurationSelector = mkSelector "doorwayWithDuration:"

-- | @Selector@ for @transitionWithCIFilter:duration:@
transitionWithCIFilter_durationSelector :: Selector
transitionWithCIFilter_durationSelector = mkSelector "transitionWithCIFilter:duration:"

-- | @Selector@ for @pausesIncomingScene@
pausesIncomingSceneSelector :: Selector
pausesIncomingSceneSelector = mkSelector "pausesIncomingScene"

-- | @Selector@ for @setPausesIncomingScene:@
setPausesIncomingSceneSelector :: Selector
setPausesIncomingSceneSelector = mkSelector "setPausesIncomingScene:"

-- | @Selector@ for @pausesOutgoingScene@
pausesOutgoingSceneSelector :: Selector
pausesOutgoingSceneSelector = mkSelector "pausesOutgoingScene"

-- | @Selector@ for @setPausesOutgoingScene:@
setPausesOutgoingSceneSelector :: Selector
setPausesOutgoingSceneSelector = mkSelector "setPausesOutgoingScene:"

