{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassColorAttachmentDescriptorArray@.
module ObjC.Metal.MTLRenderPassColorAttachmentDescriptorArray
  ( MTLRenderPassColorAttachmentDescriptorArray
  , IsMTLRenderPassColorAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLRenderPassColorAttachmentDescriptorArray mtlRenderPassColorAttachmentDescriptorArray => mtlRenderPassColorAttachmentDescriptorArray -> CULong -> IO (Id MTLRenderPassColorAttachmentDescriptor)
objectAtIndexedSubscript mtlRenderPassColorAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlRenderPassColorAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRenderPassColorAttachmentDescriptorArray mtlRenderPassColorAttachmentDescriptorArray, IsMTLRenderPassColorAttachmentDescriptor attachment) => mtlRenderPassColorAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlRenderPassColorAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlRenderPassColorAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLRenderPassColorAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLRenderPassColorAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLRenderPassColorAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

