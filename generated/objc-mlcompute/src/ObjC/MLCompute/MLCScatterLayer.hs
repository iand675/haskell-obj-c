{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCScatterLayer
--
-- A scatter layer
--
-- Generated bindings for @MLCScatterLayer@.
module ObjC.MLCompute.MLCScatterLayer
  ( MLCScatterLayer
  , IsMLCScatterLayer(..)
  , layerWithDimension_reductionType
  , dimension
  , reductionType
  , layerWithDimension_reductionTypeSelector
  , dimensionSelector
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

-- | Create a scatter layer
--
-- @dimension@ — The dimension along which to index
--
-- @reductionType@ — The reduction type to use
--
-- Returns: A new scatter layer
--
-- ObjC selector: @+ layerWithDimension:reductionType:@
layerWithDimension_reductionType :: CULong -> MLCReductionType -> IO (Id MLCScatterLayer)
layerWithDimension_reductionType dimension reductionType =
  do
    cls' <- getRequiredClass "MLCScatterLayer"
    sendClassMsg cls' (mkSelector "layerWithDimension:reductionType:") (retPtr retVoid) [argCULong (fromIntegral dimension), argCInt (coerce reductionType)] >>= retainedObject . castPtr

-- | dimension
--
-- The dimension along which to index
--
-- ObjC selector: @- dimension@
dimension :: IsMLCScatterLayer mlcScatterLayer => mlcScatterLayer -> IO CULong
dimension mlcScatterLayer  =
  sendMsg mlcScatterLayer (mkSelector "dimension") retCULong []

-- | reductionType
--
-- The reduction type applied for all values in source tensor that are scattered to a specific location in the result tensor.                Must be: MLCReductionTypeNone or MLCReductionTypeSum.
--
-- ObjC selector: @- reductionType@
reductionType :: IsMLCScatterLayer mlcScatterLayer => mlcScatterLayer -> IO MLCReductionType
reductionType mlcScatterLayer  =
  fmap (coerce :: CInt -> MLCReductionType) $ sendMsg mlcScatterLayer (mkSelector "reductionType") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDimension:reductionType:@
layerWithDimension_reductionTypeSelector :: Selector
layerWithDimension_reductionTypeSelector = mkSelector "layerWithDimension:reductionType:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

