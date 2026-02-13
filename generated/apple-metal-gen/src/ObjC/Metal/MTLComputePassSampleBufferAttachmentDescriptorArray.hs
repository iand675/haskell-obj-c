{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLComputePassSampleBufferAttachmentDescriptorArray@.
module ObjC.Metal.MTLComputePassSampleBufferAttachmentDescriptorArray
  ( MTLComputePassSampleBufferAttachmentDescriptorArray
  , IsMTLComputePassSampleBufferAttachmentDescriptorArray(..)
  , objectAtIndexedSubscript
  , setObject_atIndexedSubscript
  , objectAtIndexedSubscriptSelector
  , setObject_atIndexedSubscriptSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTLComputePassSampleBufferAttachmentDescriptorArray mtlComputePassSampleBufferAttachmentDescriptorArray => mtlComputePassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLComputePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlComputePassSampleBufferAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlComputePassSampleBufferAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLComputePassSampleBufferAttachmentDescriptorArray mtlComputePassSampleBufferAttachmentDescriptorArray, IsMTLComputePassSampleBufferAttachmentDescriptor attachment) => mtlComputePassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlComputePassSampleBufferAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlComputePassSampleBufferAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLComputePassSampleBufferAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLComputePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLComputePassSampleBufferAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

