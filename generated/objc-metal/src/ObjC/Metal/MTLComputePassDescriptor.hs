{-# LANGUAGE PatternSynonyms #-}
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
  , setDispatchTypeSelector
  , sampleBufferAttachmentsSelector

  -- * Enum types
  , MTLDispatchType(MTLDispatchType)
  , pattern MTLDispatchTypeSerial
  , pattern MTLDispatchTypeConcurrent

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
    sendClassMsg cls' (mkSelector "computePassDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dispatchType
--
-- The dispatch type of the compute command encoder.
--
-- ObjC selector: @- dispatchType@
dispatchType :: IsMTLComputePassDescriptor mtlComputePassDescriptor => mtlComputePassDescriptor -> IO MTLDispatchType
dispatchType mtlComputePassDescriptor  =
  fmap (coerce :: CULong -> MTLDispatchType) $ sendMsg mtlComputePassDescriptor (mkSelector "dispatchType") retCULong []

-- | dispatchType
--
-- The dispatch type of the compute command encoder.
--
-- ObjC selector: @- setDispatchType:@
setDispatchType :: IsMTLComputePassDescriptor mtlComputePassDescriptor => mtlComputePassDescriptor -> MTLDispatchType -> IO ()
setDispatchType mtlComputePassDescriptor  value =
  sendMsg mtlComputePassDescriptor (mkSelector "setDispatchType:") retVoid [argCULong (coerce value)]

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLComputePassDescriptor mtlComputePassDescriptor => mtlComputePassDescriptor -> IO (Id MTLComputePassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlComputePassDescriptor  =
  sendMsg mtlComputePassDescriptor (mkSelector "sampleBufferAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @computePassDescriptor@
computePassDescriptorSelector :: Selector
computePassDescriptorSelector = mkSelector "computePassDescriptor"

-- | @Selector@ for @dispatchType@
dispatchTypeSelector :: Selector
dispatchTypeSelector = mkSelector "dispatchType"

-- | @Selector@ for @setDispatchType:@
setDispatchTypeSelector :: Selector
setDispatchTypeSelector = mkSelector "setDispatchType:"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

