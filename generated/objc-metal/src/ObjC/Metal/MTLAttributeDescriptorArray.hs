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
objectAtIndexedSubscript :: IsMTLAttributeDescriptorArray mtlAttributeDescriptorArray => mtlAttributeDescriptorArray -> CULong -> IO (Id MTLAttributeDescriptor)
objectAtIndexedSubscript mtlAttributeDescriptorArray  index =
  sendMsg mtlAttributeDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLAttributeDescriptorArray mtlAttributeDescriptorArray, IsMTLAttributeDescriptor attributeDesc) => mtlAttributeDescriptorArray -> attributeDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlAttributeDescriptorArray  attributeDesc index =
withObjCPtr attributeDesc $ \raw_attributeDesc ->
    sendMsg mtlAttributeDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_attributeDesc :: Ptr ()), argCULong (fromIntegral index)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

