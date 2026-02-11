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

-- | accelerationStructurePassDescriptor
--
-- Create an autoreleased default acceleration structure pass descriptor
--
-- ObjC selector: @+ accelerationStructurePassDescriptor@
accelerationStructurePassDescriptor :: IO (Id MTLAccelerationStructurePassDescriptor)
accelerationStructurePassDescriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructurePassDescriptor"
    sendClassMsg cls' (mkSelector "accelerationStructurePassDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLAccelerationStructurePassDescriptor mtlAccelerationStructurePassDescriptor => mtlAccelerationStructurePassDescriptor -> IO (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlAccelerationStructurePassDescriptor  =
  sendMsg mtlAccelerationStructurePassDescriptor (mkSelector "sampleBufferAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accelerationStructurePassDescriptor@
accelerationStructurePassDescriptorSelector :: Selector
accelerationStructurePassDescriptorSelector = mkSelector "accelerationStructurePassDescriptor"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

