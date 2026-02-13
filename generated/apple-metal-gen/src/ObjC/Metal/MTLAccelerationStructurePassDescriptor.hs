{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLAccelerationStructurePassDescriptor
--
-- MTLAccelerationStructurePassDescriptor represents a collection of attachments to be used to create a concrete acceleration structure encoder.
--
-- Generated bindings for @MTLAccelerationStructurePassDescriptor@.
module ObjC.Metal.MTLAccelerationStructurePassDescriptor
  ( MTLAccelerationStructurePassDescriptor
  , IsMTLAccelerationStructurePassDescriptor(..)
  , accelerationStructurePassDescriptor
  , sampleBufferAttachments
  , accelerationStructurePassDescriptorSelector
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

-- | accelerationStructurePassDescriptor
--
-- Create an autoreleased default acceleration structure pass descriptor
--
-- ObjC selector: @+ accelerationStructurePassDescriptor@
accelerationStructurePassDescriptor :: IO (Id MTLAccelerationStructurePassDescriptor)
accelerationStructurePassDescriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructurePassDescriptor"
    sendClassMessage cls' accelerationStructurePassDescriptorSelector

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLAccelerationStructurePassDescriptor mtlAccelerationStructurePassDescriptor => mtlAccelerationStructurePassDescriptor -> IO (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlAccelerationStructurePassDescriptor =
  sendMessage mtlAccelerationStructurePassDescriptor sampleBufferAttachmentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accelerationStructurePassDescriptor@
accelerationStructurePassDescriptorSelector :: Selector '[] (Id MTLAccelerationStructurePassDescriptor)
accelerationStructurePassDescriptorSelector = mkSelector "accelerationStructurePassDescriptor"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector '[] (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

