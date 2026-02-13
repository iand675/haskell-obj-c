{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCSoftmaxLayer
--
-- A softmax layer
--
-- Generated bindings for @MLCSoftmaxLayer@.
module ObjC.MLCompute.MLCSoftmaxLayer
  ( MLCSoftmaxLayer
  , IsMLCSoftmaxLayer(..)
  , layerWithOperation
  , layerWithOperation_dimension
  , operation
  , dimension
  , dimensionSelector
  , layerWithOperationSelector
  , layerWithOperation_dimensionSelector
  , operationSelector

  -- * Enum types
  , MLCSoftmaxOperation(MLCSoftmaxOperation)
  , pattern MLCSoftmaxOperationSoftmax
  , pattern MLCSoftmaxOperationLogSoftmax

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

-- | Create a softmax layer
--
-- @operation@ — The softmax operation
--
-- Returns: A new softmax layer
--
-- ObjC selector: @+ layerWithOperation:@
layerWithOperation :: MLCSoftmaxOperation -> IO (Id MLCSoftmaxLayer)
layerWithOperation operation =
  do
    cls' <- getRequiredClass "MLCSoftmaxLayer"
    sendClassMessage cls' layerWithOperationSelector operation

-- | Create a softmax layer
--
-- @operation@ — The softmax operation
--
-- @dimension@ — The  dimension over which softmax operation should be performed
--
-- Returns: A new softmax layer
--
-- ObjC selector: @+ layerWithOperation:dimension:@
layerWithOperation_dimension :: MLCSoftmaxOperation -> CULong -> IO (Id MLCSoftmaxLayer)
layerWithOperation_dimension operation dimension =
  do
    cls' <- getRequiredClass "MLCSoftmaxLayer"
    sendClassMessage cls' layerWithOperation_dimensionSelector operation dimension

-- | operation
--
-- The softmax operation.  Supported values are softmax and log softmax.
--
-- ObjC selector: @- operation@
operation :: IsMLCSoftmaxLayer mlcSoftmaxLayer => mlcSoftmaxLayer -> IO MLCSoftmaxOperation
operation mlcSoftmaxLayer =
  sendMessage mlcSoftmaxLayer operationSelector

-- | dimension
--
-- The  dimension over which softmax operation should be performed
--
-- ObjC selector: @- dimension@
dimension :: IsMLCSoftmaxLayer mlcSoftmaxLayer => mlcSoftmaxLayer -> IO CULong
dimension mlcSoftmaxLayer =
  sendMessage mlcSoftmaxLayer dimensionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithOperation:@
layerWithOperationSelector :: Selector '[MLCSoftmaxOperation] (Id MLCSoftmaxLayer)
layerWithOperationSelector = mkSelector "layerWithOperation:"

-- | @Selector@ for @layerWithOperation:dimension:@
layerWithOperation_dimensionSelector :: Selector '[MLCSoftmaxOperation, CULong] (Id MLCSoftmaxLayer)
layerWithOperation_dimensionSelector = mkSelector "layerWithOperation:dimension:"

-- | @Selector@ for @operation@
operationSelector :: Selector '[] MLCSoftmaxOperation
operationSelector = mkSelector "operation"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

