{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLAttributeDescriptorArray@.
module ObjC.Metal.MTLAttributeDescriptorArray
  ( MTLAttributeDescriptorArray
  , IsMTLAttributeDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLAttributeDescriptorArray mtlAttributeDescriptorArray => mtlAttributeDescriptorArray -> CULong -> IO (Id MTLAttributeDescriptor)
objectAtIndexedSubscript mtlAttributeDescriptorArray index =
  sendMessage mtlAttributeDescriptorArray objectAtIndexedSubscriptSelector index

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLAttributeDescriptorArray mtlAttributeDescriptorArray, IsMTLAttributeDescriptor attributeDesc) => mtlAttributeDescriptorArray -> attributeDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlAttributeDescriptorArray attributeDesc index =
  sendMessage mtlAttributeDescriptorArray setObject_atIndexedSubscriptSelector (toMTLAttributeDescriptor attributeDesc) index

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLAttributeDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLAttributeDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

