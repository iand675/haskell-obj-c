{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCArithmeticLayer
--
-- An arithmetic layer
--
-- Generated bindings for @MLCArithmeticLayer@.
module ObjC.MLCompute.MLCArithmeticLayer
  ( MLCArithmeticLayer
  , IsMLCArithmeticLayer(..)
  , layerWithOperation
  , operation
  , layerWithOperationSelector
  , operationSelector

  -- * Enum types
  , MLCArithmeticOperation(MLCArithmeticOperation)
  , pattern MLCArithmeticOperationAdd
  , pattern MLCArithmeticOperationSubtract
  , pattern MLCArithmeticOperationMultiply
  , pattern MLCArithmeticOperationDivide
  , pattern MLCArithmeticOperationFloor
  , pattern MLCArithmeticOperationRound
  , pattern MLCArithmeticOperationCeil
  , pattern MLCArithmeticOperationSqrt
  , pattern MLCArithmeticOperationRsqrt
  , pattern MLCArithmeticOperationSin
  , pattern MLCArithmeticOperationCos
  , pattern MLCArithmeticOperationTan
  , pattern MLCArithmeticOperationAsin
  , pattern MLCArithmeticOperationAcos
  , pattern MLCArithmeticOperationAtan
  , pattern MLCArithmeticOperationSinh
  , pattern MLCArithmeticOperationCosh
  , pattern MLCArithmeticOperationTanh
  , pattern MLCArithmeticOperationAsinh
  , pattern MLCArithmeticOperationAcosh
  , pattern MLCArithmeticOperationAtanh
  , pattern MLCArithmeticOperationPow
  , pattern MLCArithmeticOperationExp
  , pattern MLCArithmeticOperationExp2
  , pattern MLCArithmeticOperationLog
  , pattern MLCArithmeticOperationLog2
  , pattern MLCArithmeticOperationMultiplyNoNaN
  , pattern MLCArithmeticOperationDivideNoNaN
  , pattern MLCArithmeticOperationMin
  , pattern MLCArithmeticOperationMax
  , pattern MLCArithmeticOperationCount

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

-- | Create an arithmetic layer
--
-- @operation@ — The arithmetic operation
--
-- Returns: A new arithmetic layer
--
-- ObjC selector: @+ layerWithOperation:@
layerWithOperation :: MLCArithmeticOperation -> IO (Id MLCArithmeticLayer)
layerWithOperation operation =
  do
    cls' <- getRequiredClass "MLCArithmeticLayer"
    sendClassMessage cls' layerWithOperationSelector operation

-- | operation
--
-- The arithmetic operation.
--
-- ObjC selector: @- operation@
operation :: IsMLCArithmeticLayer mlcArithmeticLayer => mlcArithmeticLayer -> IO MLCArithmeticOperation
operation mlcArithmeticLayer =
  sendMessage mlcArithmeticLayer operationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithOperation:@
layerWithOperationSelector :: Selector '[MLCArithmeticOperation] (Id MLCArithmeticLayer)
layerWithOperationSelector = mkSelector "layerWithOperation:"

-- | @Selector@ for @operation@
operationSelector :: Selector '[] MLCArithmeticOperation
operationSelector = mkSelector "operation"

