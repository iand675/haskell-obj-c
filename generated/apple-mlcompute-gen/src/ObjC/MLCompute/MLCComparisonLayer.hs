{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Compare layer.
--
-- The layer is used to perform element-wise comparison of two tensor. Returns a              tensor with the shape equal to the largest shape of operands and filled              with Boolean values result[i] = op1[i] ? op2[i], where ? corresponds to the              given @MLCComparisonOperation.@
--
-- Generated bindings for @MLCComparisonLayer@.
module ObjC.MLCompute.MLCComparisonLayer
  ( MLCComparisonLayer
  , IsMLCComparisonLayer(..)
  , layerWithOperation
  , operation
  , layerWithOperationSelector
  , operationSelector

  -- * Enum types
  , MLCComparisonOperation(MLCComparisonOperation)
  , pattern MLCComparisonOperationEqual
  , pattern MLCComparisonOperationNotEqual
  , pattern MLCComparisonOperationLess
  , pattern MLCComparisonOperationGreater
  , pattern MLCComparisonOperationLessOrEqual
  , pattern MLCComparisonOperationGreaterOrEqual
  , pattern MLCComparisonOperationLogicalAND
  , pattern MLCComparisonOperationLogicalOR
  , pattern MLCComparisonOperationLogicalNOT
  , pattern MLCComparisonOperationLogicalNAND
  , pattern MLCComparisonOperationLogicalNOR
  , pattern MLCComparisonOperationLogicalXOR
  , pattern MLCComparisonOperationCount

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

-- | Create a comparison layer.
--
-- Returns: A new compare layer.
--
-- ObjC selector: @+ layerWithOperation:@
layerWithOperation :: MLCComparisonOperation -> IO (Id MLCComparisonLayer)
layerWithOperation operation =
  do
    cls' <- getRequiredClass "MLCComparisonLayer"
    sendClassMessage cls' layerWithOperationSelector operation

-- | @- operation@
operation :: IsMLCComparisonLayer mlcComparisonLayer => mlcComparisonLayer -> IO MLCComparisonOperation
operation mlcComparisonLayer =
  sendMessage mlcComparisonLayer operationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithOperation:@
layerWithOperationSelector :: Selector '[MLCComparisonOperation] (Id MLCComparisonLayer)
layerWithOperationSelector = mkSelector "layerWithOperation:"

-- | @Selector@ for @operation@
operationSelector :: Selector '[] MLCComparisonOperation
operationSelector = mkSelector "operation"

