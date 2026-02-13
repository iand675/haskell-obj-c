{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixDecompositionCholesky
--
-- This depends on Metal.framework.
--
-- A kernel for computing the Cholesky factorization of a matrix.
--
-- A MPSMatrixDecompositionLU object computes one of the following              factorizations of a matrix A:
--
-- A = L * L**T                  A = U**T * U
--
-- A is a symmetric positive-definite matrix for which the              factorization is to be computed. L and U are lower and upper              triangular matrices respectively.
--
-- Generated bindings for @MPSMatrixDecompositionCholesky@.
module ObjC.MetalPerformanceShaders.MPSMatrixDecompositionCholesky
  ( MPSMatrixDecompositionCholesky
  , IsMPSMatrixDecompositionCholesky(..)
  , initWithDevice_lower_order
  , encodeToCommandBuffer_sourceMatrix_resultMatrix_status
  , encodeToCommandBuffer_sourceMatrix_resultMatrix_statusSelector
  , initWithDevice_lower_orderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MPSMatrixDecompositionCholesky object on a device
--
-- @device@ — The device on which the kernel will execute.
--
-- @lower@ — A boolean value indicating if the lower triangular                              part of the source matrix is stored.  If lower = YES                              the lower triangular part will be used and the factor                              will be written to the lower triangular part of the                              result, otherwise the upper triangular part will be used                              and the factor will be written to the upper triangular                              part.
--
-- @order@ — The number of rows and columns in the source matrix.
--
-- Returns: A valid MPSMatrixDecompositionCholesky object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lower:order:@
initWithDevice_lower_order :: IsMPSMatrixDecompositionCholesky mpsMatrixDecompositionCholesky => mpsMatrixDecompositionCholesky -> RawId -> Bool -> CULong -> IO (Id MPSMatrixDecompositionCholesky)
initWithDevice_lower_order mpsMatrixDecompositionCholesky device lower order =
  sendOwnedMessage mpsMatrixDecompositionCholesky initWithDevice_lower_orderSelector device lower order

-- | Encode a MPSMatrixDecompositionCholesky kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceMatrix@ — A valid MPSMatrix containing the source data.  Must have                                      enough space to hold a order x order matrix.
--
-- @resultMatrix@ — A valid MPSMatrix to contain the result.  Must have enough                                      space to hold a order x order matrix.
--
-- @status@ — A MTLBuffer which indicates the resulting MPSMatrixDecompositionStatus                                      value.
--
-- This function encodes the MPSMatrixDecompositionCholesky object to a valid              command buffer.
--
-- If during the factorization a leading minor of the matrix is found to be              not positive definite, MPSMatrixDecompositionNonPositiveDefinite will be returned              in the provided status buffer.  Previously computed pivots and the non positive              pivot are written to the result, but the factorization does not complete.              The data referenced by the MTLBuffer is not valid until the command buffer has completed              execution.  If the matrix return status is not desired NULL may be provided.
--
-- If the return status is MPSMatrixDecompositionStatusSuccess, resultMatrix              contains the resulting factors in its lower or upper triangular regions              respectively.
--
-- This kernel functions either in-place, if the result matrix              completely aliases the source matrix, or out-of-place.  If there              is any partial overlap between input and output data the results              are undefined.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrix:resultMatrix:status:@
encodeToCommandBuffer_sourceMatrix_resultMatrix_status :: (IsMPSMatrixDecompositionCholesky mpsMatrixDecompositionCholesky, IsMPSMatrix sourceMatrix, IsMPSMatrix resultMatrix) => mpsMatrixDecompositionCholesky -> RawId -> sourceMatrix -> resultMatrix -> RawId -> IO ()
encodeToCommandBuffer_sourceMatrix_resultMatrix_status mpsMatrixDecompositionCholesky commandBuffer sourceMatrix resultMatrix status =
  sendMessage mpsMatrixDecompositionCholesky encodeToCommandBuffer_sourceMatrix_resultMatrix_statusSelector commandBuffer (toMPSMatrix sourceMatrix) (toMPSMatrix resultMatrix) status

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:lower:order:@
initWithDevice_lower_orderSelector :: Selector '[RawId, Bool, CULong] (Id MPSMatrixDecompositionCholesky)
initWithDevice_lower_orderSelector = mkSelector "initWithDevice:lower:order:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:resultMatrix:status:@
encodeToCommandBuffer_sourceMatrix_resultMatrix_statusSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, RawId] ()
encodeToCommandBuffer_sourceMatrix_resultMatrix_statusSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:resultMatrix:status:"

