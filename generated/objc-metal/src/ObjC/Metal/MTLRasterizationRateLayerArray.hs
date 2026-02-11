{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLRasterizationRateLayerArray
--
-- Mutable array of MTLRasterizationRateLayerDescriptor
--
-- Generated bindings for @MTLRasterizationRateLayerArray@.
module ObjC.Metal.MTLRasterizationRateLayerArray
  ( MTLRasterizationRateLayerArray
  , IsMTLRasterizationRateLayerArray(..)
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

-- | objectAtIndexedSubscript:
--
-- Returns: The MTLRasterizationRateLayerDescriptor instance for the given layerIndex, or nil if no instance hasn't been set for this index.
--
-- Use setObject:atIndexedSubscript: to set the layer
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTLRasterizationRateLayerArray mtlRasterizationRateLayerArray => mtlRasterizationRateLayerArray -> CULong -> IO (Id MTLRasterizationRateLayerDescriptor)
objectAtIndexedSubscript mtlRasterizationRateLayerArray  layerIndex =
  sendMsg mtlRasterizationRateLayerArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral layerIndex)] >>= retainedObject . castPtr

-- | setObject:atIndexedSubscript:
--
-- Sets the MTLRasterizationRateLayerDescriptor instance for the given layerIndex.
--
-- The previous instance at this index will be overwritten.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRasterizationRateLayerArray mtlRasterizationRateLayerArray, IsMTLRasterizationRateLayerDescriptor layer) => mtlRasterizationRateLayerArray -> layer -> CULong -> IO ()
setObject_atIndexedSubscript mtlRasterizationRateLayerArray  layer layerIndex =
withObjCPtr layer $ \raw_layer ->
    sendMsg mtlRasterizationRateLayerArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_layer :: Ptr ()), argCULong (fromIntegral layerIndex)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

