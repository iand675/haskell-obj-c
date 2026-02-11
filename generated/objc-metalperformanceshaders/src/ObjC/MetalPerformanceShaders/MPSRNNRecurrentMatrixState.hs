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
  , getRecurrentOutputMatrixForLayerIndexSelector
  , getMemoryCellMatrixForLayerIndexSelector


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
getRecurrentOutputMatrixForLayerIndex mpsrnnRecurrentMatrixState  layerIndex =
  sendMsg mpsrnnRecurrentMatrixState (mkSelector "getRecurrentOutputMatrixForLayerIndex:") (retPtr retVoid) [argCULong (fromIntegral layerIndex)] >>= retainedObject . castPtr

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
getMemoryCellMatrixForLayerIndex mpsrnnRecurrentMatrixState  layerIndex =
  sendMsg mpsrnnRecurrentMatrixState (mkSelector "getMemoryCellMatrixForLayerIndex:") (retPtr retVoid) [argCULong (fromIntegral layerIndex)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getRecurrentOutputMatrixForLayerIndex:@
getRecurrentOutputMatrixForLayerIndexSelector :: Selector
getRecurrentOutputMatrixForLayerIndexSelector = mkSelector "getRecurrentOutputMatrixForLayerIndex:"

-- | @Selector@ for @getMemoryCellMatrixForLayerIndex:@
getMemoryCellMatrixForLayerIndexSelector :: Selector
getMemoryCellMatrixForLayerIndexSelector = mkSelector "getMemoryCellMatrixForLayerIndex:"

