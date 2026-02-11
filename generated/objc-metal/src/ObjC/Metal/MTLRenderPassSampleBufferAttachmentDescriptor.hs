{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassSampleBufferAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPassSampleBufferAttachmentDescriptor
  ( MTLRenderPassSampleBufferAttachmentDescriptor
  , IsMTLRenderPassSampleBufferAttachmentDescriptor(..)
  , startOfVertexSampleIndex
  , setStartOfVertexSampleIndex
  , endOfVertexSampleIndex
  , setEndOfVertexSampleIndex
  , startOfFragmentSampleIndex
  , setStartOfFragmentSampleIndex
  , endOfFragmentSampleIndex
  , setEndOfFragmentSampleIndex
  , startOfVertexSampleIndexSelector
  , setStartOfVertexSampleIndexSelector
  , endOfVertexSampleIndexSelector
  , setEndOfVertexSampleIndexSelector
  , startOfFragmentSampleIndexSelector
  , setStartOfFragmentSampleIndexSelector
  , endOfFragmentSampleIndexSelector
  , setEndOfFragmentSampleIndexSelector


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

-- | startOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the start of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- startOfVertexSampleIndex@
startOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
startOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "startOfVertexSampleIndex") retCULong []

-- | startOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the start of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setStartOfVertexSampleIndex:@
setStartOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  value =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "setStartOfVertexSampleIndex:") retVoid [argCULong (fromIntegral value)]

-- | endOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the end of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- endOfVertexSampleIndex@
endOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
endOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "endOfVertexSampleIndex") retCULong []

-- | endOfVertexSampleIndex
--
-- The sample index to use to store the sample taken at the end of vertex processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setEndOfVertexSampleIndex:@
setEndOfVertexSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfVertexSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  value =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "setEndOfVertexSampleIndex:") retVoid [argCULong (fromIntegral value)]

-- | startOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the start of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- startOfFragmentSampleIndex@
startOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
startOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "startOfFragmentSampleIndex") retCULong []

-- | startOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the start of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setStartOfFragmentSampleIndex:@
setStartOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  value =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "setStartOfFragmentSampleIndex:") retVoid [argCULong (fromIntegral value)]

-- | endOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the end of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- endOfFragmentSampleIndex@
endOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> IO CULong
endOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "endOfFragmentSampleIndex") retCULong []

-- | endOfFragmentSampleIndex
--
-- The sample index to use to store the sample taken at the end of fragment processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a render pass will fail.
--
-- ObjC selector: @- setEndOfFragmentSampleIndex:@
setEndOfFragmentSampleIndex :: IsMTLRenderPassSampleBufferAttachmentDescriptor mtlRenderPassSampleBufferAttachmentDescriptor => mtlRenderPassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfFragmentSampleIndex mtlRenderPassSampleBufferAttachmentDescriptor  value =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptor (mkSelector "setEndOfFragmentSampleIndex:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startOfVertexSampleIndex@
startOfVertexSampleIndexSelector :: Selector
startOfVertexSampleIndexSelector = mkSelector "startOfVertexSampleIndex"

-- | @Selector@ for @setStartOfVertexSampleIndex:@
setStartOfVertexSampleIndexSelector :: Selector
setStartOfVertexSampleIndexSelector = mkSelector "setStartOfVertexSampleIndex:"

-- | @Selector@ for @endOfVertexSampleIndex@
endOfVertexSampleIndexSelector :: Selector
endOfVertexSampleIndexSelector = mkSelector "endOfVertexSampleIndex"

-- | @Selector@ for @setEndOfVertexSampleIndex:@
setEndOfVertexSampleIndexSelector :: Selector
setEndOfVertexSampleIndexSelector = mkSelector "setEndOfVertexSampleIndex:"

-- | @Selector@ for @startOfFragmentSampleIndex@
startOfFragmentSampleIndexSelector :: Selector
startOfFragmentSampleIndexSelector = mkSelector "startOfFragmentSampleIndex"

-- | @Selector@ for @setStartOfFragmentSampleIndex:@
setStartOfFragmentSampleIndexSelector :: Selector
setStartOfFragmentSampleIndexSelector = mkSelector "setStartOfFragmentSampleIndex:"

-- | @Selector@ for @endOfFragmentSampleIndex@
endOfFragmentSampleIndexSelector :: Selector
endOfFragmentSampleIndexSelector = mkSelector "endOfFragmentSampleIndex"

-- | @Selector@ for @setEndOfFragmentSampleIndex:@
setEndOfFragmentSampleIndexSelector :: Selector
setEndOfFragmentSampleIndexSelector = mkSelector "setEndOfFragmentSampleIndex:"

