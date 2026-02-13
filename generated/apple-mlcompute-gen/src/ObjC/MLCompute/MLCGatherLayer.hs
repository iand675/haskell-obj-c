{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCGatherLayer
--
-- A gather layer
--
-- Generated bindings for @MLCGatherLayer@.
module ObjC.MLCompute.MLCGatherLayer
  ( MLCGatherLayer
  , IsMLCGatherLayer(..)
  , layerWithDimension
  , dimension
  , dimensionSelector
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

-- | Create a gather layer
--
-- @dimension@ — The dimension along which to index
--
-- Returns: A new gather layer
--
-- ObjC selector: @+ layerWithDimension:@
layerWithDimension :: CULong -> IO (Id MLCGatherLayer)
layerWithDimension dimension =
  do
    cls' <- getRequiredClass "MLCGatherLayer"
    sendClassMessage cls' layerWithDimensionSelector dimension

-- | dimension
--
-- The dimension along which to index
--
-- ObjC selector: @- dimension@
dimension :: IsMLCGatherLayer mlcGatherLayer => mlcGatherLayer -> IO CULong
dimension mlcGatherLayer =
  sendMessage mlcGatherLayer dimensionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDimension:@
layerWithDimensionSelector :: Selector '[CULong] (Id MLCGatherLayer)
layerWithDimensionSelector = mkSelector "layerWithDimension:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

