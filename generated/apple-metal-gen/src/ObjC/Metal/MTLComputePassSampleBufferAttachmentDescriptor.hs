{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLComputePassSampleBufferAttachmentDescriptor@.
module ObjC.Metal.MTLComputePassSampleBufferAttachmentDescriptor
  ( MTLComputePassSampleBufferAttachmentDescriptor
  , IsMTLComputePassSampleBufferAttachmentDescriptor(..)
  , sampleBuffer
  , setSampleBuffer
  , startOfEncoderSampleIndex
  , setStartOfEncoderSampleIndex
  , endOfEncoderSampleIndex
  , setEndOfEncoderSampleIndex
  , sampleBufferSelector
  , setSampleBufferSelector
  , startOfEncoderSampleIndexSelector
  , setStartOfEncoderSampleIndexSelector
  , endOfEncoderSampleIndexSelector
  , setEndOfEncoderSampleIndexSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sampleBuffer
--
-- The sample buffer to store samples for the compute-pass defined samples.If sampleBuffer is non-nil, the sample indices will be used to store samples intothe sample buffer.  If no sample buffer is provided, no samples will be taken.If any of the sample indices are specified as MTLCounterDontSample, no samplewill be taken for that action.
--
-- ObjC selector: @- sampleBuffer@
sampleBuffer :: IsMTLComputePassSampleBufferAttachmentDescriptor mtlComputePassSampleBufferAttachmentDescriptor => mtlComputePassSampleBufferAttachmentDescriptor -> IO RawId
sampleBuffer mtlComputePassSampleBufferAttachmentDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlComputePassSampleBufferAttachmentDescriptor (mkSelector "sampleBuffer") (retPtr retVoid) []

-- | sampleBuffer
--
-- The sample buffer to store samples for the compute-pass defined samples.If sampleBuffer is non-nil, the sample indices will be used to store samples intothe sample buffer.  If no sample buffer is provided, no samples will be taken.If any of the sample indices are specified as MTLCounterDontSample, no samplewill be taken for that action.
--
-- ObjC selector: @- setSampleBuffer:@
setSampleBuffer :: IsMTLComputePassSampleBufferAttachmentDescriptor mtlComputePassSampleBufferAttachmentDescriptor => mtlComputePassSampleBufferAttachmentDescriptor -> RawId -> IO ()
setSampleBuffer mtlComputePassSampleBufferAttachmentDescriptor  value =
    sendMsg mtlComputePassSampleBufferAttachmentDescriptor (mkSelector "setSampleBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | startOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the start of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a compute pass will fail.
--
-- ObjC selector: @- startOfEncoderSampleIndex@
startOfEncoderSampleIndex :: IsMTLComputePassSampleBufferAttachmentDescriptor mtlComputePassSampleBufferAttachmentDescriptor => mtlComputePassSampleBufferAttachmentDescriptor -> IO CULong
startOfEncoderSampleIndex mtlComputePassSampleBufferAttachmentDescriptor  =
    sendMsg mtlComputePassSampleBufferAttachmentDescriptor (mkSelector "startOfEncoderSampleIndex") retCULong []

-- | startOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the start of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a compute pass will fail.
--
-- ObjC selector: @- setStartOfEncoderSampleIndex:@
setStartOfEncoderSampleIndex :: IsMTLComputePassSampleBufferAttachmentDescriptor mtlComputePassSampleBufferAttachmentDescriptor => mtlComputePassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfEncoderSampleIndex mtlComputePassSampleBufferAttachmentDescriptor  value =
    sendMsg mtlComputePassSampleBufferAttachmentDescriptor (mkSelector "setStartOfEncoderSampleIndex:") retVoid [argCULong value]

-- | endOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the end of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a compute pass will fail.
--
-- ObjC selector: @- endOfEncoderSampleIndex@
endOfEncoderSampleIndex :: IsMTLComputePassSampleBufferAttachmentDescriptor mtlComputePassSampleBufferAttachmentDescriptor => mtlComputePassSampleBufferAttachmentDescriptor -> IO CULong
endOfEncoderSampleIndex mtlComputePassSampleBufferAttachmentDescriptor  =
    sendMsg mtlComputePassSampleBufferAttachmentDescriptor (mkSelector "endOfEncoderSampleIndex") retCULong []

-- | endOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the end of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a compute pass will fail.
--
-- ObjC selector: @- setEndOfEncoderSampleIndex:@
setEndOfEncoderSampleIndex :: IsMTLComputePassSampleBufferAttachmentDescriptor mtlComputePassSampleBufferAttachmentDescriptor => mtlComputePassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfEncoderSampleIndex mtlComputePassSampleBufferAttachmentDescriptor  value =
    sendMsg mtlComputePassSampleBufferAttachmentDescriptor (mkSelector "setEndOfEncoderSampleIndex:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sampleBuffer@
sampleBufferSelector :: Selector
sampleBufferSelector = mkSelector "sampleBuffer"

-- | @Selector@ for @setSampleBuffer:@
setSampleBufferSelector :: Selector
setSampleBufferSelector = mkSelector "setSampleBuffer:"

-- | @Selector@ for @startOfEncoderSampleIndex@
startOfEncoderSampleIndexSelector :: Selector
startOfEncoderSampleIndexSelector = mkSelector "startOfEncoderSampleIndex"

-- | @Selector@ for @setStartOfEncoderSampleIndex:@
setStartOfEncoderSampleIndexSelector :: Selector
setStartOfEncoderSampleIndexSelector = mkSelector "setStartOfEncoderSampleIndex:"

-- | @Selector@ for @endOfEncoderSampleIndex@
endOfEncoderSampleIndexSelector :: Selector
endOfEncoderSampleIndexSelector = mkSelector "endOfEncoderSampleIndex"

-- | @Selector@ for @setEndOfEncoderSampleIndex:@
setEndOfEncoderSampleIndexSelector :: Selector
setEndOfEncoderSampleIndexSelector = mkSelector "setEndOfEncoderSampleIndex:"

