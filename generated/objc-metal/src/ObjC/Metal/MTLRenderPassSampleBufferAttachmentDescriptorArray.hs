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

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTLRenderPassSampleBufferAttachmentDescriptorArray mtlRenderPassSampleBufferAttachmentDescriptorArray => mtlRenderPassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLRenderPassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlRenderPassSampleBufferAttachmentDescriptorArray  attachmentIndex =
  sendMsg mtlRenderPassSampleBufferAttachmentDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral attachmentIndex)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRenderPassSampleBufferAttachmentDescriptorArray mtlRenderPassSampleBufferAttachmentDescriptorArray, IsMTLRenderPassSampleBufferAttachmentDescriptor attachment) => mtlRenderPassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlRenderPassSampleBufferAttachmentDescriptorArray  attachment attachmentIndex =
withObjCPtr attachment $ \raw_attachment ->
    sendMsg mtlRenderPassSampleBufferAttachmentDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_attachment :: Ptr ()), argCULong (fromIntegral attachmentIndex)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

