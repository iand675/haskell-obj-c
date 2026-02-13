{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNRecurrentMatrixState
--
-- This depends on Metal.framework
--
-- This class holds all the data that is passed from one sequence iteration of the matrix-based RNN layer to the next.
--
-- Generated bindings for @MPSRNNRecurrentMatrixState@.
module ObjC.MetalPerformanceShaders.MPSRNNRecurrentMatrixState
  ( MPSRNNRecurrentMatrixState
  , IsMPSRNNRecurrentMatrixState(..)
  , getRecurrentOutputMatrixForLayerIndex
  , getMemoryCellMatrixForLayerIndex
  , getMemoryCellMatrixForLayerIndexSelector
  , getRecurrentOutputMatrixForLayerIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Access the stored recurrent matrix data.
--
-- @layerIndex@ — Index of the layer whose to get - belongs to { 0, 1,...,
--
-- See: numberOfLayers - 1 }
--
-- Returns: For valid layerIndex the recurrent output matrix data, otherwise nil.
--
-- ObjC selector: @- getRecurrentOutputMatrixForLayerIndex:@
getRecurrentOutputMatrixForLayerIndex :: IsMPSRNNRecurrentMatrixState mpsrnnRecurrentMatrixState => mpsrnnRecurrentMatrixState -> CULong -> IO (Id MPSMatrix)
getRecurrentOutputMatrixForLayerIndex mpsrnnRecurrentMatrixState layerIndex =
  sendMessage mpsrnnRecurrentMatrixState getRecurrentOutputMatrixForLayerIndexSelector layerIndex

-- | Access the stored memory cell matrix data (if present).
--
-- @layerIndex@ — Index of the layer whose to get - belongs to { 0, 1,...,
--
-- See: numberOfLayers - 1 }
--
-- Returns: For valid layerIndex the memory cell image matrix, otherwise nil.
--
-- ObjC selector: @- getMemoryCellMatrixForLayerIndex:@
getMemoryCellMatrixForLayerIndex :: IsMPSRNNRecurrentMatrixState mpsrnnRecurrentMatrixState => mpsrnnRecurrentMatrixState -> CULong -> IO (Id MPSMatrix)
getMemoryCellMatrixForLayerIndex mpsrnnRecurrentMatrixState layerIndex =
  sendMessage mpsrnnRecurrentMatrixState getMemoryCellMatrixForLayerIndexSelector layerIndex

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getRecurrentOutputMatrixForLayerIndex:@
getRecurrentOutputMatrixForLayerIndexSelector :: Selector '[CULong] (Id MPSMatrix)
getRecurrentOutputMatrixForLayerIndexSelector = mkSelector "getRecurrentOutputMatrixForLayerIndex:"

-- | @Selector@ for @getMemoryCellMatrixForLayerIndex:@
getMemoryCellMatrixForLayerIndexSelector :: Selector '[CULong] (Id MPSMatrix)
getMemoryCellMatrixForLayerIndexSelector = mkSelector "getMemoryCellMatrixForLayerIndex:"

