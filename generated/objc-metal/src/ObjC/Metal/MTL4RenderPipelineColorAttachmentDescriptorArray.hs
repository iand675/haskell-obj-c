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
  , setObject_atIndexedSubscriptSelector
  , resetSelector


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

-- | Accesses a color attachment at a specific index.
--
-- - Parameter attachmentIndex: Index of the attachment to access.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTL4RenderPipelineColorAttachmentDescriptorArray mtL4RenderPipelineColorAttachmentDescriptorArray => mtL4RenderPipelineColorAttachmentDescriptorArray -> CULong -> IO (Id MTL4RenderPipelineColorAttachmentDescriptor)
objectAtIndexedSubscript mtL4RenderPipelineColorAttachmentDescriptorArray  attachmentIndex =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral attachmentIndex)] >>= retainedObject . castPtr

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
setObject_atIndexedSubscript mtL4RenderPipelineColorAttachmentDescriptorArray  attachment attachmentIndex =
withObjCPtr attachment $ \raw_attachment ->
    sendMsg mtL4RenderPipelineColorAttachmentDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_attachment :: Ptr ()), argCULong (fromIntegral attachmentIndex)]

-- | Resets the elements of the descriptor array
--
-- ObjC selector: @- reset@
reset :: IsMTL4RenderPipelineColorAttachmentDescriptorArray mtL4RenderPipelineColorAttachmentDescriptorArray => mtL4RenderPipelineColorAttachmentDescriptorArray -> IO ()
reset mtL4RenderPipelineColorAttachmentDescriptorArray  =
  sendMsg mtL4RenderPipelineColorAttachmentDescriptorArray (mkSelector "reset") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

