{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixSolveCholesky
--
-- This depends on Metal.framework.
--
-- A kernel for computing the solution of a linear system of equations              using the Cholesky factorization resulting from a              MPSMatrixDecompositionCholesky kernel.
--
-- A MPSMatrixSolveCholesky finds the solution matrix to the system:
--
-- A * X = B
--
-- Where A is symmetric positive definite.  B is the array of              right hand sides for which the equations are to be solved.              X is the resulting matrix of solutions.
--
-- Generated bindings for @MPSMatrixSolveCholesky@.
module ObjC.MetalPerformanceShaders.MPSMatrixSolveCholesky
  ( MPSMatrixSolveCholesky
  , IsMPSMatrixSolveCholesky(..)
  , initWithDevice_upper_order_numberOfRightHandSides
  , encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix
  , encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector
  , initWithDevice_upper_order_numberOfRightHandSidesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MPSMatrixSolveCholesky object on a device
--
-- @device@ — The device on which the kernel will execute.
--
-- @upper@ — A boolean value which indicates if the source                              matrix stores the lower or upper triangular                              factors.
--
-- @order@ — The order of the source matrix and the number of                              rows in the solution and right hand side matrices.
--
-- @numberOfRightHandSides@ — The number of columns in the solution and right hand side                                      matrices.
--
-- Returns: A valid MPSMatrixSolveCholesky object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:upper:order:numberOfRightHandSides:@
initWithDevice_upper_order_numberOfRightHandSides :: IsMPSMatrixSolveCholesky mpsMatrixSolveCholesky => mpsMatrixSolveCholesky -> RawId -> Bool -> CULong -> CULong -> IO (Id MPSMatrixSolveCholesky)
initWithDevice_upper_order_numberOfRightHandSides mpsMatrixSolveCholesky device upper order numberOfRightHandSides =
  sendOwnedMessage mpsMatrixSolveCholesky initWithDevice_upper_order_numberOfRightHandSidesSelector device upper order numberOfRightHandSides

-- | Encode a MPSMatrixSolveCholesky kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceMatrix@ — A valid MPSMatrix containing the source matrix in factored                                  form as returned by a previous successful execution of a                                  MPSMatrixDecompositionCholesky kernel.
--
-- @rightHandSideMatrix@ — A valid MPSMatrix containing the right hand side values.
--
-- @solutionMatrix@ — A valid MPSMatrix to contain the result.
--
-- This function encodes the MPSMatrixSolveCholesky object to a valid              command buffer. sourceMatrix should contain either the lower or upper triangular              factors corresponding to the factorization returned by a previous execution              of MPSMatrixDecompositionCholesky.
--
-- rightHandSideMatrix and solutionMatrix must be large enough to hold a matrix              of size order x numberOfRightHandSides starting at secondarySourceMatrixOrigin and              resultMatrixOrigin respectively.
--
-- sourceMatrix must be at least size order x order starting at primarySourceMatrixOrigin.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix :: (IsMPSMatrixSolveCholesky mpsMatrixSolveCholesky, IsMPSMatrix sourceMatrix, IsMPSMatrix rightHandSideMatrix, IsMPSMatrix solutionMatrix) => mpsMatrixSolveCholesky -> RawId -> sourceMatrix -> rightHandSideMatrix -> solutionMatrix -> IO ()
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix mpsMatrixSolveCholesky commandBuffer sourceMatrix rightHandSideMatrix solutionMatrix =
  sendMessage mpsMatrixSolveCholesky encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector commandBuffer (toMPSMatrix sourceMatrix) (toMPSMatrix rightHandSideMatrix) (toMPSMatrix solutionMatrix)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:upper:order:numberOfRightHandSides:@
initWithDevice_upper_order_numberOfRightHandSidesSelector :: Selector '[RawId, Bool, CULong, CULong] (Id MPSMatrixSolveCholesky)
initWithDevice_upper_order_numberOfRightHandSidesSelector = mkSelector "initWithDevice:upper:order:numberOfRightHandSides:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSMatrix] ()
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:"

