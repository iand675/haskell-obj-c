{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLAccelerationStructurePassSampleBufferAttachmentDescriptor@.
module ObjC.Metal.MTLAccelerationStructurePassSampleBufferAttachmentDescriptor
  ( MTLAccelerationStructurePassSampleBufferAttachmentDescriptor
  , IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor(..)
  , sampleBuffer
  , setSampleBuffer
  , startOfEncoderSampleIndex
  , setStartOfEncoderSampleIndex
  , endOfEncoderSampleIndex
  , setEndOfEncoderSampleIndex
  , endOfEncoderSampleIndexSelector
  , sampleBufferSelector
  , setEndOfEncoderSampleIndexSelector
  , setSampleBufferSelector
  , setStartOfEncoderSampleIndexSelector
  , startOfEncoderSampleIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sampleBuffer
--
-- The sample buffer to store samples for the acceleration structure pass defined samples.If sampleBuffer is non-nil, the sample indices will be used to store samples intothe sample buffer.  If no sample buffer is provided, no samples will be taken.If any of the sample indices are specified as MTLCounterDontSample, no samplewill be taken for that action.
--
-- ObjC selector: @- sampleBuffer@
sampleBuffer :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor mtlAccelerationStructurePassSampleBufferAttachmentDescriptor => mtlAccelerationStructurePassSampleBufferAttachmentDescriptor -> IO RawId
sampleBuffer mtlAccelerationStructurePassSampleBufferAttachmentDescriptor =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptor sampleBufferSelector

-- | sampleBuffer
--
-- The sample buffer to store samples for the acceleration structure pass defined samples.If sampleBuffer is non-nil, the sample indices will be used to store samples intothe sample buffer.  If no sample buffer is provided, no samples will be taken.If any of the sample indices are specified as MTLCounterDontSample, no samplewill be taken for that action.
--
-- ObjC selector: @- setSampleBuffer:@
setSampleBuffer :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor mtlAccelerationStructurePassSampleBufferAttachmentDescriptor => mtlAccelerationStructurePassSampleBufferAttachmentDescriptor -> RawId -> IO ()
setSampleBuffer mtlAccelerationStructurePassSampleBufferAttachmentDescriptor value =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptor setSampleBufferSelector value

-- | startOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the start of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of an acceleration structure pass will fail.
--
-- ObjC selector: @- startOfEncoderSampleIndex@
startOfEncoderSampleIndex :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor mtlAccelerationStructurePassSampleBufferAttachmentDescriptor => mtlAccelerationStructurePassSampleBufferAttachmentDescriptor -> IO CULong
startOfEncoderSampleIndex mtlAccelerationStructurePassSampleBufferAttachmentDescriptor =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptor startOfEncoderSampleIndexSelector

-- | startOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the start of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of an acceleration structure pass will fail.
--
-- ObjC selector: @- setStartOfEncoderSampleIndex:@
setStartOfEncoderSampleIndex :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor mtlAccelerationStructurePassSampleBufferAttachmentDescriptor => mtlAccelerationStructurePassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfEncoderSampleIndex mtlAccelerationStructurePassSampleBufferAttachmentDescriptor value =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptor setStartOfEncoderSampleIndexSelector value

-- | endOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the end of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of an acceleration structure pass will fail.
--
-- ObjC selector: @- endOfEncoderSampleIndex@
endOfEncoderSampleIndex :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor mtlAccelerationStructurePassSampleBufferAttachmentDescriptor => mtlAccelerationStructurePassSampleBufferAttachmentDescriptor -> IO CULong
endOfEncoderSampleIndex mtlAccelerationStructurePassSampleBufferAttachmentDescriptor =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptor endOfEncoderSampleIndexSelector

-- | endOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the end of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of an acceleration structure pass will fail.
--
-- ObjC selector: @- setEndOfEncoderSampleIndex:@
setEndOfEncoderSampleIndex :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor mtlAccelerationStructurePassSampleBufferAttachmentDescriptor => mtlAccelerationStructurePassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfEncoderSampleIndex mtlAccelerationStructurePassSampleBufferAttachmentDescriptor value =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptor setEndOfEncoderSampleIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sampleBuffer@
sampleBufferSelector :: Selector '[] RawId
sampleBufferSelector = mkSelector "sampleBuffer"

-- | @Selector@ for @setSampleBuffer:@
setSampleBufferSelector :: Selector '[RawId] ()
setSampleBufferSelector = mkSelector "setSampleBuffer:"

-- | @Selector@ for @startOfEncoderSampleIndex@
startOfEncoderSampleIndexSelector :: Selector '[] CULong
startOfEncoderSampleIndexSelector = mkSelector "startOfEncoderSampleIndex"

-- | @Selector@ for @setStartOfEncoderSampleIndex:@
setStartOfEncoderSampleIndexSelector :: Selector '[CULong] ()
setStartOfEncoderSampleIndexSelector = mkSelector "setStartOfEncoderSampleIndex:"

-- | @Selector@ for @endOfEncoderSampleIndex@
endOfEncoderSampleIndexSelector :: Selector '[] CULong
endOfEncoderSampleIndexSelector = mkSelector "endOfEncoderSampleIndex"

-- | @Selector@ for @setEndOfEncoderSampleIndex:@
setEndOfEncoderSampleIndexSelector :: Selector '[CULong] ()
setEndOfEncoderSampleIndexSelector = mkSelector "setEndOfEncoderSampleIndex:"

