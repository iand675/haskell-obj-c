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

-- | Determines whether the flashing lights processor is able to process the content in the surface for flashing lights. This might be false on unsupported hardware or unsupported color spaces.
--
-- Returns: A boolean result.
--
-- ObjC selector: @- canProcessSurface:@
canProcessSurface :: IsMAFlashingLightsProcessor maFlashingLightsProcessor => maFlashingLightsProcessor -> Ptr () -> IO Bool
canProcessSurface maFlashingLightsProcessor  surface =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg maFlashingLightsProcessor (mkSelector "canProcessSurface:") retCULong [argPtr surface]

-- | Processes an inSurface by analyzing pixels for sequences of flashing lights and then darkens content to reduce the risk of discomfort from some users. The outSurface will contain the mitigated content. The timestamp indicates the time at which the surface will be shown in the video playback. FPS will be determined based on the values of the timestamps. Options dictionary for additional parameters.
--
-- Returns: An object which indicates whether the surface was able to be processed, the amount of mitigation that was applied, and the intensitry level that was detected.
--
-- ObjC selector: @- processSurface:outSurface:timestamp:options:@
processSurface_outSurface_timestamp_options :: (IsMAFlashingLightsProcessor maFlashingLightsProcessor, IsNSDictionary options) => maFlashingLightsProcessor -> Ptr () -> Ptr () -> CDouble -> options -> IO (Id MAFlashingLightsProcessorResult)
processSurface_outSurface_timestamp_options maFlashingLightsProcessor  inSurface outSurface timestamp options =
withObjCPtr options $ \raw_options ->
    sendMsg maFlashingLightsProcessor (mkSelector "processSurface:outSurface:timestamp:options:") (retPtr retVoid) [argPtr inSurface, argPtr outSurface, argCDouble (fromIntegral timestamp), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canProcessSurface:@
canProcessSurfaceSelector :: Selector
canProcessSurfaceSelector = mkSelector "canProcessSurface:"

-- | @Selector@ for @processSurface:outSurface:timestamp:options:@
processSurface_outSurface_timestamp_optionsSelector :: Selector
processSurface_outSurface_timestamp_optionsSelector = mkSelector "processSurface:outSurface:timestamp:options:"

