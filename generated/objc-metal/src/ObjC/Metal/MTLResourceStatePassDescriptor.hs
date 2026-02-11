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

-- | resourceStatePassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ resourceStatePassDescriptor@
resourceStatePassDescriptor :: IO (Id MTLResourceStatePassDescriptor)
resourceStatePassDescriptor  =
  do
    cls' <- getRequiredClass "MTLResourceStatePassDescriptor"
    sendClassMsg cls' (mkSelector "resourceStatePassDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLResourceStatePassDescriptor mtlResourceStatePassDescriptor => mtlResourceStatePassDescriptor -> IO (Id MTLResourceStatePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlResourceStatePassDescriptor  =
  sendMsg mtlResourceStatePassDescriptor (mkSelector "sampleBufferAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resourceStatePassDescriptor@
resourceStatePassDescriptorSelector :: Selector
resourceStatePassDescriptorSelector = mkSelector "resourceStatePassDescriptor"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

