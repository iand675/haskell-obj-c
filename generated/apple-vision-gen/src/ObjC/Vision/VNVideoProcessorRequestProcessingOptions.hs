{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Options applied to a request's processing of the video.
--
-- Generated bindings for @VNVideoProcessorRequestProcessingOptions@.
module ObjC.Vision.VNVideoProcessorRequestProcessingOptions
  ( VNVideoProcessorRequestProcessingOptions
  , IsVNVideoProcessorRequestProcessingOptions(..)
  , cadence
  , setCadence
  , cadenceSelector
  , setCadenceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The cadence at which the request should be performed.
--
-- If this property is not defined, then every frame will be processed.
--
-- ObjC selector: @- cadence@
cadence :: IsVNVideoProcessorRequestProcessingOptions vnVideoProcessorRequestProcessingOptions => vnVideoProcessorRequestProcessingOptions -> IO (Id VNVideoProcessorCadence)
cadence vnVideoProcessorRequestProcessingOptions =
  sendMessage vnVideoProcessorRequestProcessingOptions cadenceSelector

-- | The cadence at which the request should be performed.
--
-- If this property is not defined, then every frame will be processed.
--
-- ObjC selector: @- setCadence:@
setCadence :: (IsVNVideoProcessorRequestProcessingOptions vnVideoProcessorRequestProcessingOptions, IsVNVideoProcessorCadence value) => vnVideoProcessorRequestProcessingOptions -> value -> IO ()
setCadence vnVideoProcessorRequestProcessingOptions value =
  sendMessage vnVideoProcessorRequestProcessingOptions setCadenceSelector (toVNVideoProcessorCadence value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cadence@
cadenceSelector :: Selector '[] (Id VNVideoProcessorCadence)
cadenceSelector = mkSelector "cadence"

-- | @Selector@ for @setCadence:@
setCadenceSelector :: Selector '[Id VNVideoProcessorCadence] ()
setCadenceSelector = mkSelector "setCadence:"

