{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MAFlashingLightsProcessor@.
module ObjC.MediaAccessibility.MAFlashingLightsProcessor
  ( MAFlashingLightsProcessor
  , IsMAFlashingLightsProcessor(..)
  , canProcessSurface
  , processSurface_outSurface_timestamp_options
  , canProcessSurfaceSelector
  , processSurface_outSurface_timestamp_optionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaAccessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Determines whether the flashing lights processor is able to process the content in the surface for flashing lights. This might be false on unsupported hardware or unsupported color spaces.
--
-- Returns: A boolean result.
--
-- ObjC selector: @- canProcessSurface:@
canProcessSurface :: IsMAFlashingLightsProcessor maFlashingLightsProcessor => maFlashingLightsProcessor -> Ptr () -> IO Bool
canProcessSurface maFlashingLightsProcessor surface =
  sendMessage maFlashingLightsProcessor canProcessSurfaceSelector surface

-- | Processes an inSurface by analyzing pixels for sequences of flashing lights and then darkens content to reduce the risk of discomfort from some users. The outSurface will contain the mitigated content. The timestamp indicates the time at which the surface will be shown in the video playback. FPS will be determined based on the values of the timestamps. Options dictionary for additional parameters.
--
-- Returns: An object which indicates whether the surface was able to be processed, the amount of mitigation that was applied, and the intensitry level that was detected.
--
-- ObjC selector: @- processSurface:outSurface:timestamp:options:@
processSurface_outSurface_timestamp_options :: (IsMAFlashingLightsProcessor maFlashingLightsProcessor, IsNSDictionary options) => maFlashingLightsProcessor -> Ptr () -> Ptr () -> CDouble -> options -> IO (Id MAFlashingLightsProcessorResult)
processSurface_outSurface_timestamp_options maFlashingLightsProcessor inSurface outSurface timestamp options =
  sendMessage maFlashingLightsProcessor processSurface_outSurface_timestamp_optionsSelector inSurface outSurface timestamp (toNSDictionary options)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canProcessSurface:@
canProcessSurfaceSelector :: Selector '[Ptr ()] Bool
canProcessSurfaceSelector = mkSelector "canProcessSurface:"

-- | @Selector@ for @processSurface:outSurface:timestamp:options:@
processSurface_outSurface_timestamp_optionsSelector :: Selector '[Ptr (), Ptr (), CDouble, Id NSDictionary] (Id MAFlashingLightsProcessorResult)
processSurface_outSurface_timestamp_optionsSelector = mkSelector "processSurface:outSurface:timestamp:options:"

