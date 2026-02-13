{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLRasterizationRateMapDescriptor
--
-- Describes a MTLRasterizationRateMap containing an arbitrary number of MTLRasterizationRateLayerDescriptor instances.
--
-- An MTLRasterizationRateMapDescriptor is compiled into an MTLRasterizationRateMap using MTLDevice.
--
-- Generated bindings for @MTLRasterizationRateMapDescriptor@.
module ObjC.Metal.MTLRasterizationRateMapDescriptor
  ( MTLRasterizationRateMapDescriptor
  , IsMTLRasterizationRateMapDescriptor(..)
  , layerAtIndex
  , setLayer_atIndex
  , layers
  , label
  , setLabel
  , layerCount
  , labelSelector
  , layerAtIndexSelector
  , layerCountSelector
  , layersSelector
  , setLabelSelector
  , setLayer_atIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | layerAtIndex:
--
-- Returns: The MTLRasterizationRateLayerDescriptor instance for the given layerIndex, or nil if no instance hasn't been set for this index.
--
-- Use setLayer:atIndex: to add or set the layer. Identical to "layers[layerIndex]".
--
-- ObjC selector: @- layerAtIndex:@
layerAtIndex :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> CULong -> IO (Id MTLRasterizationRateLayerDescriptor)
layerAtIndex mtlRasterizationRateMapDescriptor layerIndex =
  sendMessage mtlRasterizationRateMapDescriptor layerAtIndexSelector layerIndex

-- | setLayer:atIndex:
--
-- Sets the MTLRasterizationRateLayerDescriptor instance for the given layerIndex.
--
-- The previous instance at the index, if any, will be overwritten. Set nil to an index to remove the layer at that index from the descriptor. Identical to "layers[layerIndex] = layer".
--
-- ObjC selector: @- setLayer:atIndex:@
setLayer_atIndex :: (IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor, IsMTLRasterizationRateLayerDescriptor layer) => mtlRasterizationRateMapDescriptor -> layer -> CULong -> IO ()
setLayer_atIndex mtlRasterizationRateMapDescriptor layer layerIndex =
  sendMessage mtlRasterizationRateMapDescriptor setLayer_atIndexSelector (toMTLRasterizationRateLayerDescriptor layer) layerIndex

-- | layers
--
-- Returns: A modifiable array of layers
--
-- Accesses the layers currently stored in the descriptor. Syntactic sugar around "layerAtIndex:" and "setLayer:atIndex:"
--
-- ObjC selector: @- layers@
layers :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> IO (Id MTLRasterizationRateLayerArray)
layers mtlRasterizationRateMapDescriptor =
  sendMessage mtlRasterizationRateMapDescriptor layersSelector

-- | label
--
-- A string to help identify this object.
--
-- The default value is nil.
--
-- ObjC selector: @- label@
label :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> IO (Id NSString)
label mtlRasterizationRateMapDescriptor =
  sendMessage mtlRasterizationRateMapDescriptor labelSelector

-- | label
--
-- A string to help identify this object.
--
-- The default value is nil.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor, IsNSString value) => mtlRasterizationRateMapDescriptor -> value -> IO ()
setLabel mtlRasterizationRateMapDescriptor value =
  sendMessage mtlRasterizationRateMapDescriptor setLabelSelector (toNSString value)

-- | layerCount
--
-- Returns: The number of subsequent non-nil layer instances stored in the descriptor, starting at index 0.
--
-- This property is modified by setting new layer instances using setLayer:atIndex: or assigning to layers[X]
--
-- ObjC selector: @- layerCount@
layerCount :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> IO CULong
layerCount mtlRasterizationRateMapDescriptor =
  sendMessage mtlRasterizationRateMapDescriptor layerCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerAtIndex:@
layerAtIndexSelector :: Selector '[CULong] (Id MTLRasterizationRateLayerDescriptor)
layerAtIndexSelector = mkSelector "layerAtIndex:"

-- | @Selector@ for @setLayer:atIndex:@
setLayer_atIndexSelector :: Selector '[Id MTLRasterizationRateLayerDescriptor, CULong] ()
setLayer_atIndexSelector = mkSelector "setLayer:atIndex:"

-- | @Selector@ for @layers@
layersSelector :: Selector '[] (Id MTLRasterizationRateLayerArray)
layersSelector = mkSelector "layers"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @layerCount@
layerCountSelector :: Selector '[] CULong
layerCountSelector = mkSelector "layerCount"

