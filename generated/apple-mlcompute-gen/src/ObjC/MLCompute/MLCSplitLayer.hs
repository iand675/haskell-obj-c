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
  , layerWithSplitCount_dimensionSelector
  , layerWithSplitSectionLengths_dimensionSelector
  , dimensionSelector
  , splitCountSelector
  , splitSectionLengthsSelector


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
    sendClassMsg cls' (mkSelector "layerWithSplitCount:dimension:") (retPtr retVoid) [argCULong splitCount, argCULong dimension] >>= retainedObject . castPtr

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
    withObjCPtr splitSectionLengths $ \raw_splitSectionLengths ->
      sendClassMsg cls' (mkSelector "layerWithSplitSectionLengths:dimension:") (retPtr retVoid) [argPtr (castPtr raw_splitSectionLengths :: Ptr ()), argCULong dimension] >>= retainedObject . castPtr

-- | dimension
--
-- The dimension (or axis) along which to split tensor
--
-- ObjC selector: @- dimension@
dimension :: IsMLCSplitLayer mlcSplitLayer => mlcSplitLayer -> IO CULong
dimension mlcSplitLayer  =
    sendMsg mlcSplitLayer (mkSelector "dimension") retCULong []

-- | splitCount
--
-- The number of splits.
--
-- The tensor will be split into equally sized chunks.  The last chunk may be smaller in size.
--
-- ObjC selector: @- splitCount@
splitCount :: IsMLCSplitLayer mlcSplitLayer => mlcSplitLayer -> IO CULong
splitCount mlcSplitLayer  =
    sendMsg mlcSplitLayer (mkSelector "splitCount") retCULong []

-- | splitSectionLengths
--
-- Lengths of each split section.
--
-- The tensor will be split into chunks along dimensions with sizes given in @splitSectionLengths@ .
--
-- ObjC selector: @- splitSectionLengths@
splitSectionLengths :: IsMLCSplitLayer mlcSplitLayer => mlcSplitLayer -> IO (Id NSArray)
splitSectionLengths mlcSplitLayer  =
    sendMsg mlcSplitLayer (mkSelector "splitSectionLengths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithSplitCount:dimension:@
layerWithSplitCount_dimensionSelector :: Selector
layerWithSplitCount_dimensionSelector = mkSelector "layerWithSplitCount:dimension:"

-- | @Selector@ for @layerWithSplitSectionLengths:dimension:@
layerWithSplitSectionLengths_dimensionSelector :: Selector
layerWithSplitSectionLengths_dimensionSelector = mkSelector "layerWithSplitSectionLengths:dimension:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @splitCount@
splitCountSelector :: Selector
splitCountSelector = mkSelector "splitCount"

-- | @Selector@ for @splitSectionLengths@
splitSectionLengthsSelector :: Selector
splitSectionLengthsSelector = mkSelector "splitSectionLengths"

