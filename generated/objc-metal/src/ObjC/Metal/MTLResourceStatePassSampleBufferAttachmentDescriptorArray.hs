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
objectAtIndexedSubscript :: IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray mtlResourceStatePassSampleBufferAttachmentDescriptorArray => mtlResourceStatePassSampleBufferAttachmentDescriptorArray -> CULong -> IO (Id MTLResourceStatePassSampleBufferAttachmentDescriptor)
objectAtIndexedSubscript mtlResourceStatePassSampleBufferAttachmentDescriptorArray  attachmentIndex =
  sendMsg mtlResourceStatePassSampleBufferAttachmentDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral attachmentIndex)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLResourceStatePassSampleBufferAttachmentDescriptorArray mtlResourceStatePassSampleBufferAttachmentDescriptorArray, IsMTLResourceStatePassSampleBufferAttachmentDescriptor attachment) => mtlResourceStatePassSampleBufferAttachmentDescriptorArray -> attachment -> CULong -> IO ()
setObject_atIndexedSubscript mtlResourceStatePassSampleBufferAttachmentDescriptorArray  attachment attachmentIndex =
withObjCPtr attachment $ \raw_attachment ->
    sendMsg mtlResourceStatePassSampleBufferAttachmentDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_attachment :: Ptr ()), argCULong (fromIntegral attachmentIndex)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

