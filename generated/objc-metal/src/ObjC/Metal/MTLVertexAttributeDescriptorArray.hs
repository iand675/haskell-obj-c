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
objectAtIndexedSubscript :: IsMTLVertexAttributeDescriptorArray mtlVertexAttributeDescriptorArray => mtlVertexAttributeDescriptorArray -> CULong -> IO (Id MTLVertexAttributeDescriptor)
objectAtIndexedSubscript mtlVertexAttributeDescriptorArray  index =
  sendMsg mtlVertexAttributeDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLVertexAttributeDescriptorArray mtlVertexAttributeDescriptorArray, IsMTLVertexAttributeDescriptor attributeDesc) => mtlVertexAttributeDescriptorArray -> attributeDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlVertexAttributeDescriptorArray  attributeDesc index =
withObjCPtr attributeDesc $ \raw_attributeDesc ->
    sendMsg mtlVertexAttributeDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_attributeDesc :: Ptr ()), argCULong (fromIntegral index)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

