{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLPipelineBufferDescriptorArray@.
module ObjC.Metal.MTLPipelineBufferDescriptorArray
  ( MTLPipelineBufferDescriptorArray
  , IsMTLPipelineBufferDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLPipelineBufferDescriptorArray mtlPipelineBufferDescriptorArray => mtlPipelineBufferDescriptorArray -> CULong -> IO (Id MTLPipelineBufferDescriptor)
objectAtIndexedSubscript mtlPipelineBufferDescriptorArray  bufferIndex =
  sendMsg mtlPipelineBufferDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral bufferIndex)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLPipelineBufferDescriptorArray mtlPipelineBufferDescriptorArray, IsMTLPipelineBufferDescriptor buffer) => mtlPipelineBufferDescriptorArray -> buffer -> CULong -> IO ()
setObject_atIndexedSubscript mtlPipelineBufferDescriptorArray  buffer bufferIndex =
withObjCPtr buffer $ \raw_buffer ->
    sendMsg mtlPipelineBufferDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argCULong (fromIntegral bufferIndex)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

