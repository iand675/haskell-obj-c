{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLComputePassDescriptor
--
-- MTLComputePassDescriptor represents a collection of attachments to be used to create a concrete compute command encoder
--
-- Generated bindings for @MTLComputePassDescriptor@.
module ObjC.Metal.MTLComputePassDescriptor
  ( MTLComputePassDescriptor
  , IsMTLComputePassDescriptor(..)
  , computePassDescriptor
  , dispatchType
  , setDispatchType
  , sampleBufferAttachments
  , computePassDescriptorSelector
  , dispatchTypeSelector
  , sampleBufferAttachmentsSelector
  , setDispatchTypeSelector

  -- * Enum types
  , MTLDispatchType(MTLDispatchType)
  , pattern MTLDispatchTypeSerial
  , pattern MTLDispatchTypeConcurrent

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | computePassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ computePassDescriptor@
computePassDescriptor :: IO (Id MTLComputePassDescriptor)
computePassDescriptor  =
  do
    cls' <- getRequiredClass "MTLComputePassDescriptor"
    sendClassMessage cls' computePassDescriptorSelector

-- | dispatchType
--
-- The dispatch type of the compute command encoder.
--
-- ObjC selector: @- dispatchType@
dispatchType :: IsMTLComputePassDescriptor mtlComputePassDescriptor => mtlComputePassDescriptor -> IO MTLDispatchType
dispatchType mtlComputePassDescriptor =
  sendMessage mtlComputePassDescriptor dispatchTypeSelector

-- | dispatchType
--
-- The dispatch type of the compute command encoder.
--
-- ObjC selector: @- setDispatchType:@
setDispatchType :: IsMTLComputePassDescriptor mtlComputePassDescriptor => mtlComputePassDescriptor -> MTLDispatchType -> IO ()
setDispatchType mtlComputePassDescriptor value =
  sendMessage mtlComputePassDescriptor setDispatchTypeSelector value

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLComputePassDescriptor mtlComputePassDescriptor => mtlComputePassDescriptor -> IO (Id MTLComputePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlComputePassDescriptor =
  sendMessage mtlComputePassDescriptor sampleBufferAttachmentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @computePassDescriptor@
computePassDescriptorSelector :: Selector '[] (Id MTLComputePassDescriptor)
computePassDescriptorSelector = mkSelector "computePassDescriptor"

-- | @Selector@ for @dispatchType@
dispatchTypeSelector :: Selector '[] MTLDispatchType
dispatchTypeSelector = mkSelector "dispatchType"

-- | @Selector@ for @setDispatchType:@
setDispatchTypeSelector :: Selector '[MTLDispatchType] ()
setDispatchTypeSelector = mkSelector "setDispatchType:"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector '[] (Id MTLComputePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

