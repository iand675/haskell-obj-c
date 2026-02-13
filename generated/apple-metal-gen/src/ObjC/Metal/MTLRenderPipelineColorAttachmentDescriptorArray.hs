{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPipelineColorAttachmentDescriptorArray@.
module ObjC.Metal.MTLRenderPipelineColorAttachmentDescriptorArray
  ( MTLRenderPipelineColorAttachmentDescriptorArray
  , IsMTLRenderPipelineColorAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLRenderPipelineColorAttachmentDescriptorArray mtlRenderPipelineColorAttachmentDescriptorArray => mtlRenderPipelineColorAttachmentDescriptorArray -> CULong -> IO (Id MTLRenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscript mtlRenderPipelineColorAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlRenderPipelineColorAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRenderPipelineColorAttachmentDescriptorArray mtlRenderPipelineColorAttachmentDescriptorArray, IsMTLRenderPipelineColorAttachmentDescriptor attachment) => mtlRenderPipelineColorAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlRenderPipelineColorAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlRenderPipelineColorAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLRenderPipelineColorAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLRenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLRenderPipelineColorAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

