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
  , layerAtIndexSelector
  , setLayer_atIndexSelector
  , layersSelector
  , labelSelector
  , setLabelSelector
  , layerCountSelector


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

-- | layerAtIndex:
--
-- Returns: The MTLRasterizationRateLayerDescriptor instance for the given layerIndex, or nil if no instance hasn't been set for this index.
--
-- Use setLayer:atIndex: to add or set the layer. Identical to "layers[layerIndex]".
--
-- ObjC selector: @- layerAtIndex:@
layerAtIndex :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> CULong -> IO (Id MTLRasterizationRateLayerDescriptor)
layerAtIndex mtlRasterizationRateMapDescriptor  layerIndex =
  sendMsg mtlRasterizationRateMapDescriptor (mkSelector "layerAtIndex:") (retPtr retVoid) [argCULong (fromIntegral layerIndex)] >>= retainedObject . castPtr

-- | setLayer:atIndex:
--
-- Sets the MTLRasterizationRateLayerDescriptor instance for the given layerIndex.
--
-- The previous instance at the index, if any, will be overwritten. Set nil to an index to remove the layer at that index from the descriptor. Identical to "layers[layerIndex] = layer".
--
-- ObjC selector: @- setLayer:atIndex:@
setLayer_atIndex :: (IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor, IsMTLRasterizationRateLayerDescriptor layer) => mtlRasterizationRateMapDescriptor -> layer -> CULong -> IO ()
setLayer_atIndex mtlRasterizationRateMapDescriptor  layer layerIndex =
withObjCPtr layer $ \raw_layer ->
    sendMsg mtlRasterizationRateMapDescriptor (mkSelector "setLayer:atIndex:") retVoid [argPtr (castPtr raw_layer :: Ptr ()), argCULong (fromIntegral layerIndex)]

-- | layers
--
-- Returns: A modifiable array of layers
--
-- Accesses the layers currently stored in the descriptor. Syntactic sugar around "layerAtIndex:" and "setLayer:atIndex:"
--
-- ObjC selector: @- layers@
layers :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> IO (Id MTLRasterizationRateLayerArray)
layers mtlRasterizationRateMapDescriptor  =
  sendMsg mtlRasterizationRateMapDescriptor (mkSelector "layers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- The default value is nil.
--
-- ObjC selector: @- label@
label :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> IO (Id NSString)
label mtlRasterizationRateMapDescriptor  =
  sendMsg mtlRasterizationRateMapDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- The default value is nil.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor, IsNSString value) => mtlRasterizationRateMapDescriptor -> value -> IO ()
setLabel mtlRasterizationRateMapDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlRasterizationRateMapDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | layerCount
--
-- Returns: The number of subsequent non-nil layer instances stored in the descriptor, starting at index 0.
--
-- This property is modified by setting new layer instances using setLayer:atIndex: or assigning to layers[X]
--
-- ObjC selector: @- layerCount@
layerCount :: IsMTLRasterizationRateMapDescriptor mtlRasterizationRateMapDescriptor => mtlRasterizationRateMapDescriptor -> IO CULong
layerCount mtlRasterizationRateMapDescriptor  =
  sendMsg mtlRasterizationRateMapDescriptor (mkSelector "layerCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerAtIndex:@
layerAtIndexSelector :: Selector
layerAtIndexSelector = mkSelector "layerAtIndex:"

-- | @Selector@ for @setLayer:atIndex:@
setLayer_atIndexSelector :: Selector
setLayer_atIndexSelector = mkSelector "setLayer:atIndex:"

-- | @Selector@ for @layers@
layersSelector :: Selector
layersSelector = mkSelector "layers"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @layerCount@
layerCountSelector :: Selector
layerCountSelector = mkSelector "layerCount"

