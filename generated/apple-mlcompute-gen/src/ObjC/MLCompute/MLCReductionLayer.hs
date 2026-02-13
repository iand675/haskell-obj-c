{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCReductionLayer
--
-- Reduce tensor values across a given dimension to a scalar value.
--
-- The layer is used to perform reductionType operation on a given dimension.                Result of this layer is a tensor of the same shape as source tensor,                except for the given dimension which is set to 1.
--
-- Generated bindings for @MLCReductionLayer@.
module ObjC.MLCompute.MLCReductionLayer
  ( MLCReductionLayer
  , IsMLCReductionLayer(..)
  , layerWithReductionType_dimension
  , layerWithReductionType_dimensions
  , reductionType
  , dimension
  , dimensions
  , dimensionSelector
  , dimensionsSelector
  , layerWithReductionType_dimensionSelector
  , layerWithReductionType_dimensionsSelector
  , reductionTypeSelector

  -- * Enum types
  , MLCReductionType(MLCReductionType)
  , pattern MLCReductionTypeNone
  , pattern MLCReductionTypeSum
  , pattern MLCReductionTypeMean
  , pattern MLCReductionTypeMax
  , pattern MLCReductionTypeMin
  , pattern MLCReductionTypeArgMax
  , pattern MLCReductionTypeArgMin
  , pattern MLCReductionTypeL1Norm
  , pattern MLCReductionTypeAny
  , pattern MLCReductionTypeAll
  , pattern MLCReductionTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a reduction layer.
--
-- @reductionType@ — The reduction type.
--
-- @dimension@ — The reduction dimension.
--
-- Returns: A new reduction layer.
--
-- ObjC selector: @+ layerWithReductionType:dimension:@
layerWithReductionType_dimension :: MLCReductionType -> CULong -> IO (Id MLCReductionLayer)
layerWithReductionType_dimension reductionType dimension =
  do
    cls' <- getRequiredClass "MLCReductionLayer"
    sendClassMessage cls' layerWithReductionType_dimensionSelector reductionType dimension

-- | Create a reduction layer.
--
-- @reductionType@ — The reduction type.
--
-- @dimensions@ — The list of dimensions to reduce over
--
-- Returns: A new reduction layer.
--
-- ObjC selector: @+ layerWithReductionType:dimensions:@
layerWithReductionType_dimensions :: IsNSArray dimensions => MLCReductionType -> dimensions -> IO (Id MLCReductionLayer)
layerWithReductionType_dimensions reductionType dimensions =
  do
    cls' <- getRequiredClass "MLCReductionLayer"
    sendClassMessage cls' layerWithReductionType_dimensionsSelector reductionType (toNSArray dimensions)

-- | reductionType
--
-- The reduction type
--
-- ObjC selector: @- reductionType@
reductionType :: IsMLCReductionLayer mlcReductionLayer => mlcReductionLayer -> IO MLCReductionType
reductionType mlcReductionLayer =
  sendMessage mlcReductionLayer reductionTypeSelector

-- | dimension
--
-- The dimension over which to perform the reduction operation
--
-- ObjC selector: @- dimension@
dimension :: IsMLCReductionLayer mlcReductionLayer => mlcReductionLayer -> IO CULong
dimension mlcReductionLayer =
  sendMessage mlcReductionLayer dimensionSelector

-- | dimensions
--
-- The dimensions over which to perform the reduction operation
--
-- ObjC selector: @- dimensions@
dimensions :: IsMLCReductionLayer mlcReductionLayer => mlcReductionLayer -> IO (Id NSArray)
dimensions mlcReductionLayer =
  sendMessage mlcReductionLayer dimensionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithReductionType:dimension:@
layerWithReductionType_dimensionSelector :: Selector '[MLCReductionType, CULong] (Id MLCReductionLayer)
layerWithReductionType_dimensionSelector = mkSelector "layerWithReductionType:dimension:"

-- | @Selector@ for @layerWithReductionType:dimensions:@
layerWithReductionType_dimensionsSelector :: Selector '[MLCReductionType, Id NSArray] (Id MLCReductionLayer)
layerWithReductionType_dimensionsSelector = mkSelector "layerWithReductionType:dimensions:"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MLCReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @dimensions@
dimensionsSelector :: Selector '[] (Id NSArray)
dimensionsSelector = mkSelector "dimensions"

