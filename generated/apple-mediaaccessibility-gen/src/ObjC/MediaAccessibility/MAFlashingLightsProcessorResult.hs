{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MAFlashingLightsProcessorResult@.
module ObjC.MediaAccessibility.MAFlashingLightsProcessorResult
  ( MAFlashingLightsProcessorResult
  , IsMAFlashingLightsProcessorResult(..)
  , surfaceProcessed
  , mitigationLevel
  , intensityLevel
  , intensityLevelSelector
  , mitigationLevelSelector
  , surfaceProcessedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaAccessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- surfaceProcessed@
surfaceProcessed :: IsMAFlashingLightsProcessorResult maFlashingLightsProcessorResult => maFlashingLightsProcessorResult -> IO Bool
surfaceProcessed maFlashingLightsProcessorResult =
  sendMessage maFlashingLightsProcessorResult surfaceProcessedSelector

-- | @- mitigationLevel@
mitigationLevel :: IsMAFlashingLightsProcessorResult maFlashingLightsProcessorResult => maFlashingLightsProcessorResult -> IO CFloat
mitigationLevel maFlashingLightsProcessorResult =
  sendMessage maFlashingLightsProcessorResult mitigationLevelSelector

-- | @- intensityLevel@
intensityLevel :: IsMAFlashingLightsProcessorResult maFlashingLightsProcessorResult => maFlashingLightsProcessorResult -> IO CFloat
intensityLevel maFlashingLightsProcessorResult =
  sendMessage maFlashingLightsProcessorResult intensityLevelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @surfaceProcessed@
surfaceProcessedSelector :: Selector '[] Bool
surfaceProcessedSelector = mkSelector "surfaceProcessed"

-- | @Selector@ for @mitigationLevel@
mitigationLevelSelector :: Selector '[] CFloat
mitigationLevelSelector = mkSelector "mitigationLevel"

-- | @Selector@ for @intensityLevel@
intensityLevelSelector :: Selector '[] CFloat
intensityLevelSelector = mkSelector "intensityLevel"

