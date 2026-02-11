{-# LANGUAGE PatternSynonyms #-}
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
  , layerWithReductionType_dimensionSelector
  , layerWithReductionType_dimensionsSelector
  , reductionTypeSelector
  , dimensionSelector
  , dimensionsSelector

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
    sendClassMsg cls' (mkSelector "layerWithReductionType:dimension:") (retPtr retVoid) [argCInt (coerce reductionType), argCULong dimension] >>= retainedObject . castPtr

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
    withObjCPtr dimensions $ \raw_dimensions ->
      sendClassMsg cls' (mkSelector "layerWithReductionType:dimensions:") (retPtr retVoid) [argCInt (coerce reductionType), argPtr (castPtr raw_dimensions :: Ptr ())] >>= retainedObject . castPtr

-- | reductionType
--
-- The reduction type
--
-- ObjC selector: @- reductionType@
reductionType :: IsMLCReductionLayer mlcReductionLayer => mlcReductionLayer -> IO MLCReductionType
reductionType mlcReductionLayer  =
    fmap (coerce :: CInt -> MLCReductionType) $ sendMsg mlcReductionLayer (mkSelector "reductionType") retCInt []

-- | dimension
--
-- The dimension over which to perform the reduction operation
--
-- ObjC selector: @- dimension@
dimension :: IsMLCReductionLayer mlcReductionLayer => mlcReductionLayer -> IO CULong
dimension mlcReductionLayer  =
    sendMsg mlcReductionLayer (mkSelector "dimension") retCULong []

-- | dimensions
--
-- The dimensions over which to perform the reduction operation
--
-- ObjC selector: @- dimensions@
dimensions :: IsMLCReductionLayer mlcReductionLayer => mlcReductionLayer -> IO (Id NSArray)
dimensions mlcReductionLayer  =
    sendMsg mlcReductionLayer (mkSelector "dimensions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithReductionType:dimension:@
layerWithReductionType_dimensionSelector :: Selector
layerWithReductionType_dimensionSelector = mkSelector "layerWithReductionType:dimension:"

-- | @Selector@ for @layerWithReductionType:dimensions:@
layerWithReductionType_dimensionsSelector :: Selector
layerWithReductionType_dimensionsSelector = mkSelector "layerWithReductionType:dimensions:"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @dimensions@
dimensionsSelector :: Selector
dimensionsSelector = mkSelector "dimensions"

