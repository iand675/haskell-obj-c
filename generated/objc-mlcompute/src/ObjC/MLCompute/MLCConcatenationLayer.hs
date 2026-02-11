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
  , layerSelector
  , layerWithDimensionSelector
  , dimensionSelector


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
    sendClassMsg cls' (mkSelector "layer") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "layerWithDimension:") (retPtr retVoid) [argCULong (fromIntegral dimension)] >>= retainedObject . castPtr

-- | dimension
--
-- The dimension (or axis) along which to concatenate tensors
--
-- The default value is 1 (which typically represents features channels)
--
-- ObjC selector: @- dimension@
dimension :: IsMLCConcatenationLayer mlcConcatenationLayer => mlcConcatenationLayer -> IO CULong
dimension mlcConcatenationLayer  =
  sendMsg mlcConcatenationLayer (mkSelector "dimension") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @layerWithDimension:@
layerWithDimensionSelector :: Selector
layerWithDimensionSelector = mkSelector "layerWithDimension:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

