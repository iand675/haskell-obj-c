{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassSampleBufferAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPassSampleBufferAttachmentDescriptor
  ( MTLRenderPassSampleBufferAttachmentDescriptor
  , IsMTLRenderPassSampleBufferAttachmentDescriptor(..)
  , sampleBuffer
  , setSampleBuffer
  , startOfVertexSampleIndex
  , setStartOfVertexSampleIndex
  , endOfVertexSampleIndex
  , setEndOfVertexSampleIndex
  , startOfFragmentSampleIndex
  , setStartOfFragmentSampleIndex
  , endOfFragmentSampleIndex
  , setEndOfFragmentSampleIndex
  , endOfFragmentSampleIndexSelector
  , endOfVertexSampleIndexSelector
  , sampleBufferSelector
  , setEndOfFragmentSampleIndexSelector
  , setEndOfVertexSampleIndexSelector
  , setSampleBufferSelector
  , setStartOfFragmentSampleIndexSelector
  , setStartOfVertexSampleIndexSelector
  , startOfFragmentSampleIndexSelector
  , startOfVertexSampleIndexSelector


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
-- The sample buffer to store samples for the render-pass defined samples.If sampleBuffer is non-nil, the sample indices will be used to store samples intothe sample buffer.  If no sample buffer is provided, no samples will be taken.If any of the sample indices are specified as MTLCounterDontSample, no samplewill be taken for that action.
--
-- ObjC selector: @- sampleBuffer@
sampleBuffer :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO RawId
sampleBuffer mtlRenderPassSampleBufferAttachmentDescriptor =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor sampleBufferSelector

-- | sampleBuffer
--
-- The sample buffer to store samples for the render-pass defined samples.If sampleBuffer is non-nil, the sample indices will be used to store samples intothe sample buffer.  If no sample buffer is provided, no samples will be taken.If any of the sample indices are specified as MTLCounterDontSample, no samplewill be taken for that action.
--
-- ObjC selector: @- setSampleBuffer:@
setSampleBuffer :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> RawId -> IO ()
setSampleBuffer mtlRenderPassSampleBufferAttachmentDescriptor value =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor setSampleBufferSelector value

-- | startOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the start of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- startOfVertexSampleIndex@
startOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
startOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor startOfVertexSampleIndexSelector

-- | startOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the start of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setStartOfVertexSampleIndex:@
setStartOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor value =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor setStartOfVertexSampleIndexSelector value

-- | endOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the end of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- endOfVertexSampleIndex@
endOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
endOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor endOfVertexSampleIndexSelector

-- | endOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the end of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setEndOfVertexSampleIndex:@
setEndOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor value =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor setEndOfVertexSampleIndexSelector value

-- | startOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the start of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- startOfFragmentSampleIndex@
startOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
startOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor startOfFragmentSampleIndexSelector

-- | startOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the start of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setStartOfFragmentSampleIndex:@
setStartOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor value =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor setStartOfFragmentSampleIndexSelector value

-- | endOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the end of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- endOfFragmentSampleIndex@
endOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
endOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor endOfFragmentSampleIndexSelector

-- | endOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the end of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setEndOfFragmentSampleIndex:@
setEndOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor value =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptor setEndOfFragmentSampleIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sampleBuffer@
sampleBufferSelector :: Selector '[] RawId
sampleBufferSelector = mkSelector "sampleBuffer"

-- | @Selector@ for @setSampleBuffer:@
setSampleBufferSelector :: Selector '[RawId] ()
setSampleBufferSelector = mkSelector "setSampleBuffer:"

-- | @Selector@ for @startOfVertexSampleIndex@
startOfVertexSampleIndexSelector :: Selector '[] CULong
startOfVertexSampleIndexSelector = mkSelector "startOfVertexSampleIndex"

-- | @Selector@ for @setStartOfVertexSampleIndex:@
setStartOfVertexSampleIndexSelector :: Selector '[CULong] ()
setStartOfVertexSampleIndexSelector = mkSelector "setStartOfVertexSampleIndex:"

-- | @Selector@ for @endOfVertexSampleIndex@
endOfVertexSampleIndexSelector :: Selector '[] CULong
endOfVertexSampleIndexSelector = mkSelector "endOfVertexSampleIndex"

-- | @Selector@ for @setEndOfVertexSampleIndex:@
setEndOfVertexSampleIndexSelector :: Selector '[CULong] ()
setEndOfVertexSampleIndexSelector = mkSelector "setEndOfVertexSampleIndex:"

-- | @Selector@ for @startOfFragmentSampleIndex@
startOfFragmentSampleIndexSelector :: Selector '[] CULong
startOfFragmentSampleIndexSelector = mkSelector "startOfFragmentSampleIndex"

-- | @Selector@ for @setStartOfFragmentSampleIndex:@
setStartOfFragmentSampleIndexSelector :: Selector '[CULong] ()
setStartOfFragmentSampleIndexSelector = mkSelector "setStartOfFragmentSampleIndex:"

-- | @Selector@ for @endOfFragmentSampleIndex@
endOfFragmentSampleIndexSelector :: Selector '[] CULong
endOfFragmentSampleIndexSelector = mkSelector "endOfFragmentSampleIndex"

-- | @Selector@ for @setEndOfFragmentSampleIndex:@
setEndOfFragmentSampleIndexSelector :: Selector '[CULong] ()
setEndOfFragmentSampleIndexSelector = mkSelector "setEndOfFragmentSampleIndex:"

