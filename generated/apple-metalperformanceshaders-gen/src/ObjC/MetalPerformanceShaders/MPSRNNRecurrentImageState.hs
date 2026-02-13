{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNRecurrentImageState
--
-- This depends on Metal.framework
--
-- This class holds all the data that is passed from one sequence iteration of the image-based RNN layer (stack) to the next.
--
-- Generated bindings for @MPSRNNRecurrentImageState@.
module ObjC.MetalPerformanceShaders.MPSRNNRecurrentImageState
  ( MPSRNNRecurrentImageState
  , IsMPSRNNRecurrentImageState(..)
  , getRecurrentOutputImageForLayerIndex
  , getMemoryCellImageForLayerIndex
  , getMemoryCellImageForLayerIndexSelector
  , getRecurrentOutputImageForLayerIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Access the stored recurrent image data.
--
-- @layerIndex@ — Index of the layer whose to get - belongs to { 0, 1,...,
--
-- See: numberOfLayers - 1 }
--
-- Returns: For valid layerIndex the recurrent output image data, otherwise nil.
--
-- ObjC selector: @- getRecurrentOutputImageForLayerIndex:@
getRecurrentOutputImageForLayerIndex :: IsMPSRNNRecurrentImageState mpsrnnRecurrentImageState => mpsrnnRecurrentImageState -> CULong -> IO (Id MPSImage)
getRecurrentOutputImageForLayerIndex mpsrnnRecurrentImageState layerIndex =
  sendMessage mpsrnnRecurrentImageState getRecurrentOutputImageForLayerIndexSelector layerIndex

-- | Access the stored memory cell image data (if present).
--
-- @layerIndex@ — Index of the layer whose to get - belongs to { 0, 1,...,
--
-- See: numberOfLayers - 1 }
--
-- Returns: For valid layerIndex the memory cell image data, otherwise nil.
--
-- ObjC selector: @- getMemoryCellImageForLayerIndex:@
getMemoryCellImageForLayerIndex :: IsMPSRNNRecurrentImageState mpsrnnRecurrentImageState => mpsrnnRecurrentImageState -> CULong -> IO (Id MPSImage)
getMemoryCellImageForLayerIndex mpsrnnRecurrentImageState layerIndex =
  sendMessage mpsrnnRecurrentImageState getMemoryCellImageForLayerIndexSelector layerIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getRecurrentOutputImageForLayerIndex:@
getRecurrentOutputImageForLayerIndexSelector :: Selector '[CULong] (Id MPSImage)
getRecurrentOutputImageForLayerIndexSelector = mkSelector "getRecurrentOutputImageForLayerIndex:"

-- | @Selector@ for @getMemoryCellImageForLayerIndex:@
getMemoryCellImageForLayerIndexSelector :: Selector '[CULong] (Id MPSImage)
getMemoryCellImageForLayerIndexSelector = mkSelector "getMemoryCellImageForLayerIndex:"

