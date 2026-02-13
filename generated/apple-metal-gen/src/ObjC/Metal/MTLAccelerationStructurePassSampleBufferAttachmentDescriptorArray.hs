{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray@.
module ObjC.Metal.MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray
  ( MTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray
  , IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray => mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray attachmentIndex =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray objectAtIndexedSubscriptSelector attachmentIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptorArray mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray, IsMTLAccelerationStructurePassSampleBufferAttachmentDescriptor attachment) => mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray attachment attachmentIndex =
  sendMessage mtlAccelerationStructurePassSampleBufferAttachmentDescriptorArray setObject_atIndexedSubscriptSelector (toMTLAccelerationStructurePassSampleBufferAttachmentDescriptor attachment) attachmentIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLAccelerationStructurePassSampleBufferAttachmentDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

