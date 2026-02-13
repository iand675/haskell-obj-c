{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLResourceStatePassDescriptor
--
-- MTLResourceStatePassDescriptor represents a collection of attachments to be used to create a concrete resourceState command encoder
--
-- Generated bindings for @MTLResourceStatePassDescriptor@.
module ObjC.Metal.MTLResourceStatePassDescriptor
  ( MTLResourceStatePassDescriptor
  , IsMTLResourceStatePassDescriptor(..)
  , resourceStatePassDescriptor
  , sampleBufferAttachments
  , resourceStatePassDescriptorSelector
  , sampleBufferAttachmentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | resourceStatePassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ resourceStatePassDescriptor@
resourceStatePassDescriptor :: IO (Id MTLResourceStatePassDescriptor)
resourceStatePassDescriptor  =
  do
    cls' <- getRequiredClass "MTLResourceStatePassDescriptor"
    sendClassMessage cls' resourceStatePassDescriptorSelector

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLResourceStatePassDescriptor mtlResourceStatePassDescriptor => mtlResourceStatePassDescriptor -> IO (Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlResourceStatePassDescriptor =
  sendMessage mtlResourceStatePassDescriptor sampleBufferAttachmentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resourceStatePassDescriptor@
resourceStatePassDescriptorSelector :: Selector '[] (Id MTLResourceStatePassDescriptor)
resourceStatePassDescriptorSelector = mkSelector "resourceStatePassDescriptor"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector '[] (Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

