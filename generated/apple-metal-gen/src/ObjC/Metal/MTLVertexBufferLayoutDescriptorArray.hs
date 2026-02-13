{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLVertexBufferLayoutDescriptorArray@.
module ObjC.Metal.MTLVertexBufferLayoutDescriptorArray
  ( MTLVertexBufferLayoutDescriptorArray
  , IsMTLVertexBufferLayoutDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLVertexBufferLayoutDescriptorArray mtlVertexBufferLayoutDescriptorArray => mtlVertexBufferLayoutDescriptorArray -> CULong -> IO (Id MTLVertexBufferLayoutDescriptor)
objectAtIndexedSubscript mtlVertexBufferLayoutDescriptorArray index =
  sendMessage mtlVertexBufferLayoutDescriptorArray objectAtIndexedSubscriptSelector index

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLVertexBufferLayoutDescriptorArray mtlVertexBufferLayoutDescriptorArray, IsMTLVertexBufferLayoutDescriptor bufferDesc) => mtlVertexBufferLayoutDescriptorArray -> bufferDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlVertexBufferLayoutDescriptorArray bufferDesc index =
  sendMessage mtlVertexBufferLayoutDescriptorArray setObject_atIndexedSubscriptSelector (toMTLVertexBufferLayoutDescriptor bufferDesc) index

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLVertexBufferLayoutDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLVertexBufferLayoutDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

