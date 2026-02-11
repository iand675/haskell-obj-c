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
  , initWithDevice_upper_order_numberOfRightHandSidesSelector
  , encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector


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
initWithDevice_upper_order_numberOfRightHandSides mpsMatrixSolveCholesky  device upper order numberOfRightHandSides =
  sendMsg mpsMatrixSolveCholesky (mkSelector "initWithDevice:upper:order:numberOfRightHandSides:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (if upper then 1 else 0), argCULong (fromIntegral order), argCULong (fromIntegral numberOfRightHandSides)] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix mpsMatrixSolveCholesky  commandBuffer sourceMatrix rightHandSideMatrix solutionMatrix =
withObjCPtr sourceMatrix $ \raw_sourceMatrix ->
  withObjCPtr rightHandSideMatrix $ \raw_rightHandSideMatrix ->
    withObjCPtr solutionMatrix $ \raw_solutionMatrix ->
        sendMsg mpsMatrixSolveCholesky (mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrix :: Ptr ()), argPtr (castPtr raw_rightHandSideMatrix :: Ptr ()), argPtr (castPtr raw_solutionMatrix :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:upper:order:numberOfRightHandSides:@
initWithDevice_upper_order_numberOfRightHandSidesSelector :: Selector
initWithDevice_upper_order_numberOfRightHandSidesSelector = mkSelector "initWithDevice:upper:order:numberOfRightHandSides:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector :: Selector
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:"

