{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCConcatenationLayer
--
-- A concatenation layer
--
-- Generated bindings for @MLCConcatenationLayer@.
module ObjC.MLCompute.MLCConcatenationLayer
  ( MLCConcatenationLayer
  , IsMLCConcatenationLayer(..)
  , layer
  , layerWithDimension
  , dimension
  , dimensionSelector
  , layerSelector
  , layerWithDimensionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a concatenation layer
--
-- Returns: A new concatenation layer
--
-- ObjC selector: @+ layer@
layer :: IO (Id MLCConcatenationLayer)
layer  =
  do
    cls' <- getRequiredClass "MLCConcatenationLayer"
    sendClassMessage cls' layerSelector

-- | Create a concatenation layer
--
-- @dimension@ — The concatenation dimension
--
-- Returns: A new concatenation layer
--
-- ObjC selector: @+ layerWithDimension:@
layerWithDimension :: CULong -> IO (Id MLCConcatenationLayer)
layerWithDimension dimension =
  do
    cls' <- getRequiredClass "MLCConcatenationLayer"
    sendClassMessage cls' layerWithDimensionSelector dimension

-- | dimension
--
-- The dimension (or axis) along which to concatenate tensors
--
-- The default value is 1 (which typically represents features channels)
--
-- ObjC selector: @- dimension@
dimension :: IsMLCConcatenationLayer mlcConcatenationLayer => mlcConcatenationLayer -> IO CULong
dimension mlcConcatenationLayer =
  sendMessage mlcConcatenationLayer dimensionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layer@
layerSelector :: Selector '[] (Id MLCConcatenationLayer)
layerSelector = mkSelector "layer"

-- | @Selector@ for @layerWithDimension:@
layerWithDimensionSelector :: Selector '[CULong] (Id MLCConcatenationLayer)
layerWithDimensionSelector = mkSelector "layerWithDimension:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

