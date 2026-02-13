{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , doorsCloseHorizontalWithDurationSelector
  , doorsCloseVerticalWithDurationSelector
  , doorsOpenHorizontalWithDurationSelector
  , doorsOpenVerticalWithDurationSelector
  , doorwayWithDurationSelector
  , fadeWithColor_durationSelector
  , fadeWithDurationSelector
  , flipHorizontalWithDurationSelector
  , flipVerticalWithDurationSelector
  , moveInWithDirection_durationSelector
  , pausesIncomingSceneSelector
  , pausesOutgoingSceneSelector
  , pushWithDirection_durationSelector
  , revealWithDirection_durationSelector
  , setPausesIncomingSceneSelector
  , setPausesOutgoingSceneSelector
  , transitionWithCIFilter_durationSelector

  -- * Enum types
  , SKTransitionDirection(SKTransitionDirection)
  , pattern SKTransitionDirectionUp
  , pattern SKTransitionDirectionDown
  , pattern SKTransitionDirectionRight
  , pattern SKTransitionDirectionLeft

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
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ crossFadeWithDuration:@
crossFadeWithDuration :: CDouble -> IO (Id SKTransition)
crossFadeWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' crossFadeWithDurationSelector sec

-- | @+ fadeWithDuration:@
fadeWithDuration :: CDouble -> IO (Id SKTransition)
fadeWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' fadeWithDurationSelector sec

-- | @+ fadeWithColor:duration:@
fadeWithColor_duration :: IsNSColor color => color -> CDouble -> IO (Id SKTransition)
fadeWithColor_duration color sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' fadeWithColor_durationSelector (toNSColor color) sec

-- | @+ flipHorizontalWithDuration:@
flipHorizontalWithDuration :: CDouble -> IO (Id SKTransition)
flipHorizontalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' flipHorizontalWithDurationSelector sec

-- | @+ flipVerticalWithDuration:@
flipVerticalWithDuration :: CDouble -> IO (Id SKTransition)
flipVerticalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' flipVerticalWithDurationSelector sec

-- | @+ revealWithDirection:duration:@
revealWithDirection_duration :: SKTransitionDirection -> CDouble -> IO (Id SKTransition)
revealWithDirection_duration direction sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' revealWithDirection_durationSelector direction sec

-- | @+ moveInWithDirection:duration:@
moveInWithDirection_duration :: SKTransitionDirection -> CDouble -> IO (Id SKTransition)
moveInWithDirection_duration direction sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' moveInWithDirection_durationSelector direction sec

-- | @+ pushWithDirection:duration:@
pushWithDirection_duration :: SKTransitionDirection -> CDouble -> IO (Id SKTransition)
pushWithDirection_duration direction sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' pushWithDirection_durationSelector direction sec

-- | @+ doorsOpenHorizontalWithDuration:@
doorsOpenHorizontalWithDuration :: CDouble -> IO (Id SKTransition)
doorsOpenHorizontalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' doorsOpenHorizontalWithDurationSelector sec

-- | @+ doorsOpenVerticalWithDuration:@
doorsOpenVerticalWithDuration :: CDouble -> IO (Id SKTransition)
doorsOpenVerticalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' doorsOpenVerticalWithDurationSelector sec

-- | @+ doorsCloseHorizontalWithDuration:@
doorsCloseHorizontalWithDuration :: CDouble -> IO (Id SKTransition)
doorsCloseHorizontalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' doorsCloseHorizontalWithDurationSelector sec

-- | @+ doorsCloseVerticalWithDuration:@
doorsCloseVerticalWithDuration :: CDouble -> IO (Id SKTransition)
doorsCloseVerticalWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' doorsCloseVerticalWithDurationSelector sec

-- | @+ doorwayWithDuration:@
doorwayWithDuration :: CDouble -> IO (Id SKTransition)
doorwayWithDuration sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' doorwayWithDurationSelector sec

-- | @+ transitionWithCIFilter:duration:@
transitionWithCIFilter_duration :: IsCIFilter filter_ => filter_ -> CDouble -> IO (Id SKTransition)
transitionWithCIFilter_duration filter_ sec =
  do
    cls' <- getRequiredClass "SKTransition"
    sendClassMessage cls' transitionWithCIFilter_durationSelector (toCIFilter filter_) sec

-- | Pause the incoming Scene during the transition, defaults to YES.
--
-- ObjC selector: @- pausesIncomingScene@
pausesIncomingScene :: IsSKTransition skTransition => skTransition -> IO Bool
pausesIncomingScene skTransition =
  sendMessage skTransition pausesIncomingSceneSelector

-- | Pause the incoming Scene during the transition, defaults to YES.
--
-- ObjC selector: @- setPausesIncomingScene:@
setPausesIncomingScene :: IsSKTransition skTransition => skTransition -> Bool -> IO ()
setPausesIncomingScene skTransition value =
  sendMessage skTransition setPausesIncomingSceneSelector value

-- | Pause the outgoing Scene during the transition, defaults to YES.
--
-- ObjC selector: @- pausesOutgoingScene@
pausesOutgoingScene :: IsSKTransition skTransition => skTransition -> IO Bool
pausesOutgoingScene skTransition =
  sendMessage skTransition pausesOutgoingSceneSelector

-- | Pause the outgoing Scene during the transition, defaults to YES.
--
-- ObjC selector: @- setPausesOutgoingScene:@
setPausesOutgoingScene :: IsSKTransition skTransition => skTransition -> Bool -> IO ()
setPausesOutgoingScene skTransition value =
  sendMessage skTransition setPausesOutgoingSceneSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @crossFadeWithDuration:@
crossFadeWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
crossFadeWithDurationSelector = mkSelector "crossFadeWithDuration:"

-- | @Selector@ for @fadeWithDuration:@
fadeWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
fadeWithDurationSelector = mkSelector "fadeWithDuration:"

-- | @Selector@ for @fadeWithColor:duration:@
fadeWithColor_durationSelector :: Selector '[Id NSColor, CDouble] (Id SKTransition)
fadeWithColor_durationSelector = mkSelector "fadeWithColor:duration:"

-- | @Selector@ for @flipHorizontalWithDuration:@
flipHorizontalWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
flipHorizontalWithDurationSelector = mkSelector "flipHorizontalWithDuration:"

-- | @Selector@ for @flipVerticalWithDuration:@
flipVerticalWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
flipVerticalWithDurationSelector = mkSelector "flipVerticalWithDuration:"

-- | @Selector@ for @revealWithDirection:duration:@
revealWithDirection_durationSelector :: Selector '[SKTransitionDirection, CDouble] (Id SKTransition)
revealWithDirection_durationSelector = mkSelector "revealWithDirection:duration:"

-- | @Selector@ for @moveInWithDirection:duration:@
moveInWithDirection_durationSelector :: Selector '[SKTransitionDirection, CDouble] (Id SKTransition)
moveInWithDirection_durationSelector = mkSelector "moveInWithDirection:duration:"

-- | @Selector@ for @pushWithDirection:duration:@
pushWithDirection_durationSelector :: Selector '[SKTransitionDirection, CDouble] (Id SKTransition)
pushWithDirection_durationSelector = mkSelector "pushWithDirection:duration:"

-- | @Selector@ for @doorsOpenHorizontalWithDuration:@
doorsOpenHorizontalWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
doorsOpenHorizontalWithDurationSelector = mkSelector "doorsOpenHorizontalWithDuration:"

-- | @Selector@ for @doorsOpenVerticalWithDuration:@
doorsOpenVerticalWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
doorsOpenVerticalWithDurationSelector = mkSelector "doorsOpenVerticalWithDuration:"

-- | @Selector@ for @doorsCloseHorizontalWithDuration:@
doorsCloseHorizontalWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
doorsCloseHorizontalWithDurationSelector = mkSelector "doorsCloseHorizontalWithDuration:"

-- | @Selector@ for @doorsCloseVerticalWithDuration:@
doorsCloseVerticalWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
doorsCloseVerticalWithDurationSelector = mkSelector "doorsCloseVerticalWithDuration:"

-- | @Selector@ for @doorwayWithDuration:@
doorwayWithDurationSelector :: Selector '[CDouble] (Id SKTransition)
doorwayWithDurationSelector = mkSelector "doorwayWithDuration:"

-- | @Selector@ for @transitionWithCIFilter:duration:@
transitionWithCIFilter_durationSelector :: Selector '[Id CIFilter, CDouble] (Id SKTransition)
transitionWithCIFilter_durationSelector = mkSelector "transitionWithCIFilter:duration:"

-- | @Selector@ for @pausesIncomingScene@
pausesIncomingSceneSelector :: Selector '[] Bool
pausesIncomingSceneSelector = mkSelector "pausesIncomingScene"

-- | @Selector@ for @setPausesIncomingScene:@
setPausesIncomingSceneSelector :: Selector '[Bool] ()
setPausesIncomingSceneSelector = mkSelector "setPausesIncomingScene:"

-- | @Selector@ for @pausesOutgoingScene@
pausesOutgoingSceneSelector :: Selector '[] Bool
pausesOutgoingSceneSelector = mkSelector "pausesOutgoingScene"

-- | @Selector@ for @setPausesOutgoingScene:@
setPausesOutgoingSceneSelector :: Selector '[Bool] ()
setPausesOutgoingSceneSelector = mkSelector "setPausesOutgoingScene:"

