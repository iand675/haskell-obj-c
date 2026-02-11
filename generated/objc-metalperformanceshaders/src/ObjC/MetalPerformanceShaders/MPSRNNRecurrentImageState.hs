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
  , getRecurrentOutputImageForLayerIndexSelector
  , getMemoryCellImageForLayerIndexSelector


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
getRecurrentOutputImageForLayerIndex mpsrnnRecurrentImageState  layerIndex =
  sendMsg mpsrnnRecurrentImageState (mkSelector "getRecurrentOutputImageForLayerIndex:") (retPtr retVoid) [argCULong (fromIntegral layerIndex)] >>= retainedObject . castPtr

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
getMemoryCellImageForLayerIndex mpsrnnRecurrentImageState  layerIndex =
  sendMsg mpsrnnRecurrentImageState (mkSelector "getMemoryCellImageForLayerIndex:") (retPtr retVoid) [argCULong (fromIntegral layerIndex)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getRecurrentOutputImageForLayerIndex:@
getRecurrentOutputImageForLayerIndexSelector :: Selector
getRecurrentOutputImageForLayerIndexSelector = mkSelector "getRecurrentOutputImageForLayerIndex:"

-- | @Selector@ for @getMemoryCellImageForLayerIndex:@
getMemoryCellImageForLayerIndexSelector :: Selector
getMemoryCellImageForLayerIndexSelector = mkSelector "getMemoryCellImageForLayerIndex:"

