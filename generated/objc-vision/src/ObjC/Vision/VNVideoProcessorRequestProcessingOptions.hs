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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The cadence at which the request should be performed.
--
-- If this property is not defined, then every frame will be processed.
--
-- ObjC selector: @- cadence@
cadence :: IsVNVideoProcessorRequestProcessingOptions vnVideoProcessorRequestProcessingOptions => vnVideoProcessorRequestProcessingOptions -> IO (Id VNVideoProcessorCadence)
cadence vnVideoProcessorRequestProcessingOptions  =
  sendMsg vnVideoProcessorRequestProcessingOptions (mkSelector "cadence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The cadence at which the request should be performed.
--
-- If this property is not defined, then every frame will be processed.
--
-- ObjC selector: @- setCadence:@
setCadence :: (IsVNVideoProcessorRequestProcessingOptions vnVideoProcessorRequestProcessingOptions, IsVNVideoProcessorCadence value) => vnVideoProcessorRequestProcessingOptions -> value -> IO ()
setCadence vnVideoProcessorRequestProcessingOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg vnVideoProcessorRequestProcessingOptions (mkSelector "setCadence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cadence@
cadenceSelector :: Selector
cadenceSelector = mkSelector "cadence"

-- | @Selector@ for @setCadence:@
setCadenceSelector :: Selector
setCadenceSelector = mkSelector "setCadence:"

