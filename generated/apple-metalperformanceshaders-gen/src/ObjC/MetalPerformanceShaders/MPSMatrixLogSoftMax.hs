{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixLogSoftMax
--
-- This depends on Metal.framework.
--
-- A logarithmic softmax kernel that operates on matrices.
--
-- A MPSMatrixLogSoftMax object computes:
--
-- B_ij = ln { Exp { A_ij } / ( Sum_k Exp { A_ik } ) } = A_ij - ln { Sum_k Exp { A_ik } }
--
-- A and B are matrices which are represented by MPSMatrix              objects. This filter computes the same result for MPSMatrices as              MPSCNNLogSoftMax filter does for MPSImages by interpreting the columns              of the matrix as feature channels, that is the sum runs over column indices.
--
-- Generated bindings for @MPSMatrixLogSoftMax@.
module ObjC.MetalPerformanceShaders.MPSMatrixLogSoftMax
  ( MPSMatrixLogSoftMax
  , IsMPSMatrixLogSoftMax(..)


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

