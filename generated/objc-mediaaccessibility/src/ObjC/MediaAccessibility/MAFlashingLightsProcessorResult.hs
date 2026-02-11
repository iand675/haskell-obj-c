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
  , surfaceProcessedSelector
  , mitigationLevelSelector
  , intensityLevelSelector


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

import ObjC.MediaAccessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- surfaceProcessed@
surfaceProcessed :: IsMAFlashingLightsProcessorResult maFlashingLightsProcessorResult => maFlashingLightsProcessorResult -> IO Bool
surfaceProcessed maFlashingLightsProcessorResult  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg maFlashingLightsProcessorResult (mkSelector "surfaceProcessed") retCULong []

-- | @- mitigationLevel@
mitigationLevel :: IsMAFlashingLightsProcessorResult maFlashingLightsProcessorResult => maFlashingLightsProcessorResult -> IO CFloat
mitigationLevel maFlashingLightsProcessorResult  =
  sendMsg maFlashingLightsProcessorResult (mkSelector "mitigationLevel") retCFloat []

-- | @- intensityLevel@
intensityLevel :: IsMAFlashingLightsProcessorResult maFlashingLightsProcessorResult => maFlashingLightsProcessorResult -> IO CFloat
intensityLevel maFlashingLightsProcessorResult  =
  sendMsg maFlashingLightsProcessorResult (mkSelector "intensityLevel") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @surfaceProcessed@
surfaceProcessedSelector :: Selector
surfaceProcessedSelector = mkSelector "surfaceProcessed"

-- | @Selector@ for @mitigationLevel@
mitigationLevelSelector :: Selector
mitigationLevelSelector = mkSelector "mitigationLevel"

-- | @Selector@ for @intensityLevel@
intensityLevelSelector :: Selector
intensityLevelSelector = mkSelector "intensityLevel"

