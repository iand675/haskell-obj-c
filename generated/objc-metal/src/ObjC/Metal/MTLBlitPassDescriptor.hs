{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLBlitPassDescriptor
--
-- MTLBlitPassDescriptor represents a collection of attachments to be used to create a concrete blit command encoder
--
-- Generated bindings for @MTLBlitPassDescriptor@.
module ObjC.Metal.MTLBlitPassDescriptor
  ( MTLBlitPassDescriptor
  , IsMTLBlitPassDescriptor(..)
  , blitPassDescriptor
  , sampleBufferAttachments
  , blitPassDescriptorSelector
  , sampleBufferAttachmentsSelector


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

-- | blitPassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ blitPassDescriptor@
blitPassDescriptor :: IO (Id MTLBlitPassDescriptor)
blitPassDescriptor  =
  do
    cls' <- getRequiredClass "MTLBlitPassDescriptor"
    sendClassMsg cls' (mkSelector "blitPassDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLBlitPassDescriptor mtlBlitPassDescriptor => mtlBlitPassDescriptor -> IO (Id MTLBlitPassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlBlitPassDescriptor  =
  sendMsg mtlBlitPassDescriptor (mkSelector "sampleBufferAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @blitPassDescriptor@
blitPassDescriptorSelector :: Selector
blitPassDescriptorSelector = mkSelector "blitPassDescriptor"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

