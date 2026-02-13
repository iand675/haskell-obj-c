{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTLPipelineBufferDescriptorArray mtlPipelineBufferDescriptorArray => mtlPipelineBufferDescriptorArray -> CULong -> IO (Id MTLPipelineBufferDescriptor)
objectAtIndexedSubscript mtlPipelineBufferDescriptorArray bufferIndex =
  sendMessage mtlPipelineBufferDescriptorArray objectAtIndexedSubscriptSelector bufferIndex

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLPipelineBufferDescriptorArray mtlPipelineBufferDescriptorArray, IsMTLPipelineBufferDescriptor buffer) => mtlPipelineBufferDescriptorArray -> buffer -> CULong -> IO ()
setObject_atIndexedSubscript mtlPipelineBufferDescriptorArray buffer bufferIndex =
  sendMessage mtlPipelineBufferDescriptorArray setObject_atIndexedSubscriptSelector (toMTLPipelineBufferDescriptor buffer) bufferIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLPipelineBufferDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLPipelineBufferDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

