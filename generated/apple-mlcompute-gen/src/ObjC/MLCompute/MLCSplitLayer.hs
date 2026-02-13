{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCSplitLayer
--
-- A split layer
--
-- Generated bindings for @MLCSplitLayer@.
module ObjC.MLCompute.MLCSplitLayer
  ( MLCSplitLayer
  , IsMLCSplitLayer(..)
  , layerWithSplitCount_dimension
  , layerWithSplitSectionLengths_dimension
  , dimension
  , splitCount
  , splitSectionLengths
  , dimensionSelector
  , layerWithSplitCount_dimensionSelector
  , layerWithSplitSectionLengths_dimensionSelector
  , splitCountSelector
  , splitSectionLengthsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a split layer
--
-- @splitCount@ — The number of splits.
--
-- @dimension@ — The dimension along which the tensor should be split.
--
-- Returns: A new split layer
--
-- ObjC selector: @+ layerWithSplitCount:dimension:@
layerWithSplitCount_dimension :: CULong -> CULong -> IO (Id MLCSplitLayer)
layerWithSplitCount_dimension splitCount dimension =
  do
    cls' <- getRequiredClass "MLCSplitLayer"
    sendClassMessage cls' layerWithSplitCount_dimensionSelector splitCount dimension

-- | Create a split layer
--
-- @splitSectionLengths@ — Lengths of each split section.
--
-- @dimension@ — The dimension along which the tensor should be split.
--
-- Returns: A new split layer
--
-- ObjC selector: @+ layerWithSplitSectionLengths:dimension:@
layerWithSplitSectionLengths_dimension :: IsNSArray splitSectionLengths => splitSectionLengths -> CULong -> IO (Id MLCSplitLayer)
layerWithSplitSectionLengths_dimension splitSectionLengths dimension =
  do
    cls' <- getRequiredClass "MLCSplitLayer"
    sendClassMessage cls' layerWithSplitSectionLengths_dimensionSelector (toNSArray splitSectionLengths) dimension

-- | dimension
--
-- The dimension (or axis) along which to split tensor
--
-- ObjC selector: @- dimension@
dimension :: IsMLCSplitLayer mlcSplitLayer => mlcSplitLayer -> IO CULong
dimension mlcSplitLayer =
  sendMessage mlcSplitLayer dimensionSelector

-- | splitCount
--
-- The number of splits.
--
-- The tensor will be split into equally sized chunks.  The last chunk may be smaller in size.
--
-- ObjC selector: @- splitCount@
splitCount :: IsMLCSplitLayer mlcSplitLayer => mlcSplitLayer -> IO CULong
splitCount mlcSplitLayer =
  sendMessage mlcSplitLayer splitCountSelector

-- | splitSectionLengths
--
-- Lengths of each split section.
--
-- The tensor will be split into chunks along dimensions with sizes given in @splitSectionLengths@ .
--
-- ObjC selector: @- splitSectionLengths@
splitSectionLengths :: IsMLCSplitLayer mlcSplitLayer => mlcSplitLayer -> IO (Id NSArray)
splitSectionLengths mlcSplitLayer =
  sendMessage mlcSplitLayer splitSectionLengthsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithSplitCount:dimension:@
layerWithSplitCount_dimensionSelector :: Selector '[CULong, CULong] (Id MLCSplitLayer)
layerWithSplitCount_dimensionSelector = mkSelector "layerWithSplitCount:dimension:"

-- | @Selector@ for @layerWithSplitSectionLengths:dimension:@
layerWithSplitSectionLengths_dimensionSelector :: Selector '[Id NSArray, CULong] (Id MLCSplitLayer)
layerWithSplitSectionLengths_dimensionSelector = mkSelector "layerWithSplitSectionLengths:dimension:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @splitCount@
splitCountSelector :: Selector '[] CULong
splitCountSelector = mkSelector "splitCount"

-- | @Selector@ for @splitSectionLengths@
splitSectionLengthsSelector :: Selector '[] (Id NSArray)
splitSectionLengthsSelector = mkSelector "splitSectionLengths"

