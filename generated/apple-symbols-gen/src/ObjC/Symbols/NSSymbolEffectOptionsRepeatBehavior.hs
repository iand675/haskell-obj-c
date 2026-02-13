{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The behavior of repetition to use when a symbol effect is animating.
--
-- Generated bindings for @NSSymbolEffectOptionsRepeatBehavior@.
module ObjC.Symbols.NSSymbolEffectOptionsRepeatBehavior
  ( NSSymbolEffectOptionsRepeatBehavior
  , IsNSSymbolEffectOptionsRepeatBehavior(..)
  , new
  , init_
  , behaviorPeriodic
  , behaviorPeriodicWithCount
  , behaviorPeriodicWithDelay
  , behaviorPeriodicWithCount_delay
  , behaviorContinuous
  , behaviorContinuousSelector
  , behaviorPeriodicSelector
  , behaviorPeriodicWithCountSelector
  , behaviorPeriodicWithCount_delaySelector
  , behaviorPeriodicWithDelaySelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSSymbolEffectOptionsRepeatBehavior)
new  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSSymbolEffectOptionsRepeatBehavior nsSymbolEffectOptionsRepeatBehavior => nsSymbolEffectOptionsRepeatBehavior -> IO (Id NSSymbolEffectOptionsRepeatBehavior)
init_ nsSymbolEffectOptionsRepeatBehavior =
  sendOwnedMessage nsSymbolEffectOptionsRepeatBehavior initSelector

-- | Creates and returns a repeat behavior that prefers to repeat indefinitely using periodic animations. Periodic animations play the effect at regular intervals starting and stopping each time.
--
-- - Returns: A new behavior that prefers to repeat indefinitely using periodic animations.
--
-- ObjC selector: @+ behaviorPeriodic@
behaviorPeriodic :: IO (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodic  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"
    sendClassMessage cls' behaviorPeriodicSelector

-- | Creates and returns a repeat behavior with a preferred play count using periodic animations. Periodic animations play the effect at regular intervals starting and stopping each time.
--
-- - Parameter count: The preferred number of times to play the   effect. Very   large or small values may be clamped.
--
-- - Returns: A new behavior with the preferred play count using periodic animations.
--
-- ObjC selector: @+ behaviorPeriodicWithCount:@
behaviorPeriodicWithCount :: CLong -> IO (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicWithCount count =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"
    sendClassMessage cls' behaviorPeriodicWithCountSelector count

-- | Creates and returns a repeat behavior with a preferred repeat delay using periodic animations. Periodic animations play the effect at regular intervals starting and stopping each time.
--
-- - Parameter delay: The preferred delay between repetitions,   in seconds.
--
-- - Returns: A new behavior that prefers to repeat indefinitely with a specified delay using periodic animations.
--
-- ObjC selector: @+ behaviorPeriodicWithDelay:@
behaviorPeriodicWithDelay :: CDouble -> IO (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicWithDelay delay =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"
    sendClassMessage cls' behaviorPeriodicWithDelaySelector delay

-- | Creates and returns a repeat behavior with a preferred play count and delay using periodic animations. Periodic animations play the effect at regular intervals starting and stopping each time.
--
-- - Parameter count: The preferred number of times to play the   effect. Very   large or small values may be clamped.
--
-- - Parameter delay: The preferred delay between repetitions,   in seconds.
--
-- - Returns: A new behavior with the preferred play count and delay using periodic animations.
--
-- ObjC selector: @+ behaviorPeriodicWithCount:delay:@
behaviorPeriodicWithCount_delay :: CLong -> CDouble -> IO (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicWithCount_delay count delay =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"
    sendClassMessage cls' behaviorPeriodicWithCount_delaySelector count delay

-- | Creates and returns a repeat behavior that prefers to repeat indefinitely, using continuous animations if available. Continuous animations have an intro, a body that runs as long as the effect is enabled, and an outro. If available these animations provide a smoother animation when an effect repeats indefinitely.
--
-- - Returns: A new behavior that prefers to repeat indefinitely with continuous animations.
--
-- ObjC selector: @+ behaviorContinuous@
behaviorContinuous :: IO (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorContinuous  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"
    sendClassMessage cls' behaviorContinuousSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSSymbolEffectOptionsRepeatBehavior)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSymbolEffectOptionsRepeatBehavior)
initSelector = mkSelector "init"

-- | @Selector@ for @behaviorPeriodic@
behaviorPeriodicSelector :: Selector '[] (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicSelector = mkSelector "behaviorPeriodic"

-- | @Selector@ for @behaviorPeriodicWithCount:@
behaviorPeriodicWithCountSelector :: Selector '[CLong] (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicWithCountSelector = mkSelector "behaviorPeriodicWithCount:"

-- | @Selector@ for @behaviorPeriodicWithDelay:@
behaviorPeriodicWithDelaySelector :: Selector '[CDouble] (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicWithDelaySelector = mkSelector "behaviorPeriodicWithDelay:"

-- | @Selector@ for @behaviorPeriodicWithCount:delay:@
behaviorPeriodicWithCount_delaySelector :: Selector '[CLong, CDouble] (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorPeriodicWithCount_delaySelector = mkSelector "behaviorPeriodicWithCount:delay:"

-- | @Selector@ for @behaviorContinuous@
behaviorContinuousSelector :: Selector '[] (Id NSSymbolEffectOptionsRepeatBehavior)
behaviorContinuousSelector = mkSelector "behaviorContinuous"

