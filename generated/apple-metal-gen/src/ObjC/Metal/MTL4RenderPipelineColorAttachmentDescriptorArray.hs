{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An array of color attachment descriptions for a render pipeline.
--
-- Generated bindings for @MTL4RenderPipelineColorAttachmentDescriptorArray@.
module ObjC.Metal.MTL4RenderPipelineColorAttachmentDescriptorArray
  ( MTL4RenderPipelineColorAttachmentDescriptorArray
  , IsMTL4RenderPipelineColorAttachmentDescriptorArray(..)
  , objectAtIndexedSubscript
  , setObject_atIndexedSubscript
  , reset
  , objectAtIndexedSubscriptSelector
  , resetSelector
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

-- | Accesses a color attachment at a specific index.
--
-- - Parameter attachmentIndex: Index of the attachment to access.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTL4RenderPipelineColorAttachmentDescriptorArray mtL4RenderPipelineColorAttachmentDescriptorArray => mtL4RenderPipelineColorAttachmentDescriptorArray -> CULong -> IO (Id MTL4RenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscript mtL4RenderPipelineColorAttachmentDescriptorArray attachmentIndex =
  sendMessage mtL4RenderPipelineColorAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | Sets an attachment at an index.
--
-- This function offers 'copy' semantics.
--
-- You can safely set the color attachment at any legal index to nil. This has the effect of resetting that attachment descriptor's state to its default values.
--
-- - Parameters:   - attachment: the descriptor of the attachment to set.   - attachmentIndex: the index of the attachment within the array.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTL4RenderPipelineColorAttachmentDescriptorArray mtL4RenderPipelineColorAttachmentDescriptorArray, IsMTL4RenderPipelineColorAttachmentDescriptor attachment) => mtL4RenderPipelineColorAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtL4RenderPipelineColorAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtL4RenderPipelineColorAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTL4RenderPipelineColorAttachmentDescriptor attachment) attachmentIndex

-- | Resets the elements of the descriptor array
--
-- ObjC selector: @- reset@
reset :: IsMTL4RenderPipelineColorAttachmentDescriptorArray mtL4RenderPipelineColorAttachmentDescriptorArray => mtL4RenderPipelineColorAttachmentDescriptorArray -> IO ()
reset mtL4RenderPipelineColorAttachmentDescriptorArray =
  sendMessage mtL4RenderPipelineColorAttachmentDescriptorArray resetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTL4RenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTL4RenderPipelineColorAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

