{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLBufferLayoutDescriptorArray@.
module ObjC.Metal.MTLBufferLayoutDescriptorArray
  ( MTLBufferLayoutDescriptorArray
  , IsMTLBufferLayoutDescriptorArray(..)
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
objectAtIndexedSubscript :: IsMTLBufferLayoutDescriptorArray mtlBufferLayoutDescriptorArray => mtlBufferLayoutDescriptorArray -> CULong -> IO (Id MTLBufferLayoutDescriptor)
objectAtIndexedSubscript mtlBufferLayoutDescriptorArray index =
  sendMessage mtlBufferLayoutDescriptorArray objectAtIndexedSubscriptSelector index

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLBufferLayoutDescriptorArray mtlBufferLayoutDescriptorArray, IsMTLBufferLayoutDescriptor bufferDesc) => mtlBufferLayoutDescriptorArray -> bufferDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlBufferLayoutDescriptorArray bufferDesc index =
  sendMessage mtlBufferLayoutDescriptorArray setObject_atIndexedSubscriptSelector (toMTLBufferLayoutDescriptor bufferDesc) index

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLBufferLayoutDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLBufferLayoutDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

