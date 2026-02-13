{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLVertexAttributeDescriptorArray@.
module ObjC.Metal.MTLVertexAttributeDescriptorArray
  ( MTLVertexAttributeDescriptorArray
  , IsMTLVertexAttributeDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLVertexAttributeDescriptorArray mtlVertexAttributeDescriptorArray => mtlVertexAttributeDescriptorArray -> CULong -> IO (Id MTLVertexAttributeDescriptor)
objectAtIndexedSubscript mtlVertexAttributeDescriptorArray index =
  sendMessage mtlVertexAttributeDescriptorArray objectAtIndexedSubscriptSelector index

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLVertexAttributeDescriptorArray mtlVertexAttributeDescriptorArray, IsMTLVertexAttributeDescriptor attributeDesc) => mtlVertexAttributeDescriptorArray -> attributeDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlVertexAttributeDescriptorArray attributeDesc index =
  sendMessage mtlVertexAttributeDescriptorArray setObject_atIndexedSubscriptSelector (toMTLVertexAttributeDescriptor attributeDesc) index

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLVertexAttributeDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLVertexAttributeDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

