{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixLogSoftMaxGradient
--
-- This depends on Metal.framework.
--
-- Computes the gradient corresponding to a forward MPSMatrixLogSoftMax object.
--
-- A MPSMatrixLogSoftMaxGradient object computes:
--
-- dL_dX_ij = dL_dY_ij - exp(Y_ij * sum_k(dL_dY_ik))
--
-- Where dL_dX is the resulting gradient of the loss function with respect to              the original input to the forward MPSMatrixLogSoftMax operation, Y is              the output of the forward MPSMatrixLogSoftMax operation, and dL_dY is the              gradient of the loss function with respect to Y.
--
-- Generated bindings for @MPSMatrixLogSoftMaxGradient@.
module ObjC.MetalPerformanceShaders.MPSMatrixLogSoftMaxGradient
  ( MPSMatrixLogSoftMaxGradient
  , IsMPSMatrixLogSoftMaxGradient(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

