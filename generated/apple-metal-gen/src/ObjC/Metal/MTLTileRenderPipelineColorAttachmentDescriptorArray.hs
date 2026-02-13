{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLTileRenderPipelineColorAttachmentDescriptorArray@.
module ObjC.Metal.MTLTileRenderPipelineColorAttachmentDescriptorArray
  ( MTLTileRenderPipelineColorAttachmentDescriptorArray
  , IsMTLTileRenderPipelineColorAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLTileRenderPipelineColorAttachmentDescriptorArray mtlTileRenderPipelineColorAttachmentDescriptorArray => mtlTileRenderPipelineColorAttachmentDescriptorArray -> CULong -> IO (Id MTLTileRenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscript mtlTileRenderPipelineColorAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlTileRenderPipelineColorAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLTileRenderPipelineColorAttachmentDescriptorArray mtlTileRenderPipelineColorAttachmentDescriptorArray, IsMTLTileRenderPipelineColorAttachmentDescriptor attachment) => mtlTileRenderPipelineColorAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlTileRenderPipelineColorAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlTileRenderPipelineColorAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLTileRenderPipelineColorAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLTileRenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLTileRenderPipelineColorAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

