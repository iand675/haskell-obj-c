{-# LANGUAGE PatternSynonyms #-}
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
    sendClassMsg cls' (mkSelector "layerWithOperation:") (retPtr retVoid) [argCInt (coerce operation)] >>= retainedObject . castPtr

-- | operation
--
-- The arithmetic operation.
--
-- ObjC selector: @- operation@
operation :: IsMLCArithmeticLayer mlcArithmeticLayer => mlcArithmeticLayer -> IO MLCArithmeticOperation
operation mlcArithmeticLayer  =
  fmap (coerce :: CInt -> MLCArithmeticOperation) $ sendMsg mlcArithmeticLayer (mkSelector "operation") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithOperation:@
layerWithOperationSelector :: Selector
layerWithOperationSelector = mkSelector "layerWithOperation:"

-- | @Selector@ for @operation@
operationSelector :: Selector
operationSelector = mkSelector "operation"

