{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLResourceStatePassSampleBufferAttachmentDescriptor@.
module ObjC.Metal.MTLResourceStatePassSampleBufferAttachmentDescriptor
  ( MTLResourceStatePassSampleBufferAttachmentDescriptor
  , IsMTLResourceStatePassSampleBufferAttachmentDescriptor(..)
  , startOfEncoderSampleIndex
  , setStartOfEncoderSampleIndex
  , endOfEncoderSampleIndex
  , setEndOfEncoderSampleIndex
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

-- | startOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the start of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a resourceState pass will fail.
--
-- ObjC selector: @- startOfEncoderSampleIndex@
startOfEncoderSampleIndex :: IsMTLResourceStatePassSampleBufferAttachmentDescriptor mtlResourceStatePassSampleBufferAttachmentDescriptor => mtlResourceStatePassSampleBufferAttachmentDescriptor -> IO CULong
startOfEncoderSampleIndex mtlResourceStatePassSampleBufferAttachmentDescriptor  =
  sendMsg mtlResourceStatePassSampleBufferAttachmentDescriptor (mkSelector "startOfEncoderSampleIndex") retCULong []

-- | startOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the start of command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a resourceState pass will fail.
--
-- ObjC selector: @- setStartOfEncoderSampleIndex:@
setStartOfEncoderSampleIndex :: IsMTLResourceStatePassSampleBufferAttachmentDescriptor mtlResourceStatePassSampleBufferAttachmentDescriptor => mtlResourceStatePassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setStartOfEncoderSampleIndex mtlResourceStatePassSampleBufferAttachmentDescriptor  value =
  sendMsg mtlResourceStatePassSampleBufferAttachmentDescriptor (mkSelector "setStartOfEncoderSampleIndex:") retVoid [argCULong (fromIntegral value)]

-- | endOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the end of Command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a resourceState pass will fail.
--
-- ObjC selector: @- endOfEncoderSampleIndex@
endOfEncoderSampleIndex :: IsMTLResourceStatePassSampleBufferAttachmentDescriptor mtlResourceStatePassSampleBufferAttachmentDescriptor => mtlResourceStatePassSampleBufferAttachmentDescriptor -> IO CULong
endOfEncoderSampleIndex mtlResourceStatePassSampleBufferAttachmentDescriptor  =
  sendMsg mtlResourceStatePassSampleBufferAttachmentDescriptor (mkSelector "endOfEncoderSampleIndex") retCULong []

-- | endOfEncoderSampleIndex
--
-- The sample index to use to store the sample taken at the end of Command encoder processing.  Setting the value to MTLCounterDontSample will cause this sample to be omitted.
--
-- On devices where MTLCounterSamplingPointAtStageBoundary is unsupported, this sample index is invalid and must be set to MTLCounterDontSample or creation of a resourceState pass will fail.
--
-- ObjC selector: @- setEndOfEncoderSampleIndex:@
setEndOfEncoderSampleIndex :: IsMTLResourceStatePassSampleBufferAttachmentDescriptor mtlResourceStatePassSampleBufferAttachmentDescriptor => mtlResourceStatePassSampleBufferAttachmentDescriptor -> CULong -> IO ()
setEndOfEncoderSampleIndex mtlResourceStatePassSampleBufferAttachmentDescriptor  value =
  sendMsg mtlResourceStatePassSampleBufferAttachmentDescriptor (mkSelector "setEndOfEncoderSampleIndex:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

