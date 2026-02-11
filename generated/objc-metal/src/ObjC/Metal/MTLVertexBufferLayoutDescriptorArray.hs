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
objectAtIndexedSubscript :: IsMTLVertexBufferLayoutDescriptorArray mtlVertexBufferLayoutDescriptorArray => mtlVertexBufferLayoutDescriptorArray -> CULong -> IO (Id MTLVertexBufferLayoutDescriptor)
objectAtIndexedSubscript mtlVertexBufferLayoutDescriptorArray  index =
  sendMsg mtlVertexBufferLayoutDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLVertexBufferLayoutDescriptorArray mtlVertexBufferLayoutDescriptorArray, IsMTLVertexBufferLayoutDescriptor bufferDesc) => mtlVertexBufferLayoutDescriptorArray -> bufferDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlVertexBufferLayoutDescriptorArray  bufferDesc index =
withObjCPtr bufferDesc $ \raw_bufferDesc ->
    sendMsg mtlVertexBufferLayoutDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_bufferDesc :: Ptr ()), argCULong (fromIntegral index)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

