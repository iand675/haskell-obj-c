{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassSampleBufferAttachmentDescriptorArray@.
module ObjC.Metal.MTLRenderPassSampleBufferAttachmentDescriptorArray
  ( MTLRenderPassSampleBufferAttachmentDescriptorArray
  , IsMTLRenderPassSampleBufferAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLRenderPassSampleBufferAttachmentDescriptorArray mtlRenderPassSampleBufferAttachmentDescriptorArray => mtlRenderPassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLRenderPassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlRenderPassSampleBufferAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRenderPassSampleBufferAttachmentDescriptorArray mtlRenderPassSampleBufferAttachmentDescriptorArray, IsMTLRenderPassSampleBufferAttachmentDescriptor attachment) => mtlRenderPassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlRenderPassSampleBufferAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlRenderPassSampleBufferAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLRenderPassSampleBufferAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLRenderPassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLRenderPassSampleBufferAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

