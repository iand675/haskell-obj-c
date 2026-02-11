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
objectAtIndexedSubscript :: IsMTLBufferLayoutDescriptorArray mtlBufferLayoutDescriptorArray => mtlBufferLayoutDescriptorArray -> CULong -> IO (Id MTLBufferLayoutDescriptor)
objectAtIndexedSubscript mtlBufferLayoutDescriptorArray  index =
  sendMsg mtlBufferLayoutDescriptorArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLBufferLayoutDescriptorArray mtlBufferLayoutDescriptorArray, IsMTLBufferLayoutDescriptor bufferDesc) => mtlBufferLayoutDescriptorArray -> bufferDesc -> CULong -> IO ()
setObject_atIndexedSubscript mtlBufferLayoutDescriptorArray  bufferDesc index =
withObjCPtr bufferDesc $ \raw_bufferDesc ->
    sendMsg mtlBufferLayoutDescriptorArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_bufferDesc :: Ptr ()), argCULong (fromIntegral index)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

