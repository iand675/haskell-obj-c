{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLBlitPassDescriptor
--
-- MTLBlitPassDescriptor represents a collection of attachments to be used to create a concrete blit command encoder
--
-- Generated bindings for @MTLBlitPassDescriptor@.
module ObjC.Metal.MTLBlitPassDescriptor
  ( MTLBlitPassDescriptor
  , IsMTLBlitPassDescriptor(..)
  , blitPassDescriptor
  , sampleBufferAttachments
  , blitPassDescriptorSelector
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

-- | blitPassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ blitPassDescriptor@
blitPassDescriptor :: IO (Id MTLBlitPassDescriptor)
blitPassDescriptor  =
  do
    cls' <- getRequiredClass "MTLBlitPassDescriptor"
    sendClassMessage cls' blitPassDescriptorSelector

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLBlitPassDescriptor mtlBlitPassDescriptor => mtlBlitPassDescriptor -> IO (Id MTLBlitPassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlBlitPassDescriptor =
  sendMessage mtlBlitPassDescriptor sampleBufferAttachmentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @blitPassDescriptor@
blitPassDescriptorSelector :: Selector '[] (Id MTLBlitPassDescriptor)
blitPassDescriptorSelector = mkSelector "blitPassDescriptor"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector '[] (Id MTLBlitPassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

