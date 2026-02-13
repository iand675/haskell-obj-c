{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLResourceStatePassSampleBufferAttachmentDescriptorArray@.
module ObjC.Metal.MTLResourceStatePassSampleBufferAttachmentDescriptorArray
  ( MTLResourceStatePassSampleBufferAttachmentDescriptorArray
  , IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray mtlResourceStatePassSampleBufferAttachmentDescriptorArray => mtlResourceStatePassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLResourceStatePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlResourceStatePassSampleBufferAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlResourceStatePassSampleBufferAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray mtlResourceStatePassSampleBufferAttachmentDescriptorArray, IsMTLResourceStatePassSampleBufferAttachmentDescriptor attachment) => mtlResourceStatePassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlResourceStatePassSampleBufferAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlResourceStatePassSampleBufferAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLResourceStatePassSampleBufferAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLResourceStatePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLResourceStatePassSampleBufferAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

