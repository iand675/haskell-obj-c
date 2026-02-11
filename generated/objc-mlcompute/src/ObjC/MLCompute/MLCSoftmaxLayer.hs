{-# LANGUAGE PatternSynonyms #-}
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
  , layerWithOperationSelector
  , layerWithOperation_dimensionSelector
  , operationSelector
  , dimensionSelector

  -- * Enum types
  , MLCSoftmaxOperation(MLCSoftmaxOperation)
  , pattern MLCSoftmaxOperationSoftmax
  , pattern MLCSoftmaxOperationLogSoftmax

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
    sendClassMsg cls' (mkSelector "layerWithOperation:") (retPtr retVoid) [argCInt (coerce operation)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "layerWithOperation:dimension:") (retPtr retVoid) [argCInt (coerce operation), argCULong (fromIntegral dimension)] >>= retainedObject . castPtr

-- | operation
--
-- The softmax operation.  Supported values are softmax and log softmax.
--
-- ObjC selector: @- operation@
operation :: IsMLCSoftmaxLayer mlcSoftmaxLayer => mlcSoftmaxLayer -> IO MLCSoftmaxOperation
operation mlcSoftmaxLayer  =
  fmap (coerce :: CInt -> MLCSoftmaxOperation) $ sendMsg mlcSoftmaxLayer (mkSelector "operation") retCInt []

-- | dimension
--
-- The  dimension over which softmax operation should be performed
--
-- ObjC selector: @- dimension@
dimension :: IsMLCSoftmaxLayer mlcSoftmaxLayer => mlcSoftmaxLayer -> IO CULong
dimension mlcSoftmaxLayer  =
  sendMsg mlcSoftmaxLayer (mkSelector "dimension") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithOperation:@
layerWithOperationSelector :: Selector
layerWithOperationSelector = mkSelector "layerWithOperation:"

-- | @Selector@ for @layerWithOperation:dimension:@
layerWithOperation_dimensionSelector :: Selector
layerWithOperation_dimensionSelector = mkSelector "layerWithOperation:dimension:"

-- | @Selector@ for @operation@
operationSelector :: Selector
operationSelector = mkSelector "operation"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

