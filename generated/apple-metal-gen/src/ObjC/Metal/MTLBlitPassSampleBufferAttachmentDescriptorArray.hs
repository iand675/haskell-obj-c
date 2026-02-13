{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLBlitPassSampleBufferAttachmentDescriptorArray@.
module ObjC.Metal.MTLBlitPassSampleBufferAttachmentDescriptorArray
  ( MTLBlitPassSampleBufferAttachmentDescriptorArray
  , IsMTLBlitPassSampleBufferAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLBlitPassSampleBufferAttachmentDescriptorArray mtlBlitPassSampleBufferAttachmentDescriptorArray => mtlBlitPassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLBlitPassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlBlitPassSampleBufferAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlBlitPassSampleBufferAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLBlitPassSampleBufferAttachmentDescriptorArray mtlBlitPassSampleBufferAttachmentDescriptorArray, IsMTLBlitPassSampleBufferAttachmentDescriptor attachment) => mtlBlitPassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlBlitPassSampleBufferAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlBlitPassSampleBufferAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLBlitPassSampleBufferAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLBlitPassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLBlitPassSampleBufferAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

