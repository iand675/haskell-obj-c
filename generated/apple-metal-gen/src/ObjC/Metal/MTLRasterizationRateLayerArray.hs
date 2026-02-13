{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
objectAtIndexedSubscript mtlRasterizationRateLayerArray layerIndex =
  sendMessage mtlRasterizationRateLayerArray objectAtIndexedSubscriptSelector layerIndex

-- | setObject:atIndexedSubscript:
--
-- Sets the MTLRasterizationRateLayerDescriptor instance for the given layerIndex.
--
-- The previous instance at this index will be overwritten.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRasterizationRateLayerArray mtlRasterizationRateLayerArray, IsMTLRasterizationRateLayerDescriptor layer) => mtlRasterizationRateLayerArray -> layer -> CULong -> IO ()
setObject_atIndexedSubscript mtlRasterizationRateLayerArray layer layerIndex =
  sendMessage mtlRasterizationRateLayerArray setObject_atIndexedSubscriptSelector (toMTLRasterizationRateLayerDescriptor layer) layerIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MTLRasterizationRateLayerDescriptor)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id MTLRasterizationRateLayerDescriptor, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

