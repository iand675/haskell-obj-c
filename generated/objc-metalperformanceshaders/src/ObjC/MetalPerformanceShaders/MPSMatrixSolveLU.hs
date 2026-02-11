{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixSolveLU
--
-- This depends on Metal.framework.
--
-- A kernel for computing the solution of a linear system of equations              using the LU factorization resulting from a MPSMatrixDecompositionLU              kernel.
--
-- A MPSMatrixSolveLU finds the solution matrix to the system:
--
-- op(A) * X = B
--
-- Where op(A) is A**T or A.  B is the array of right hand sides for which              the equations are to be solved.  X is the resulting matrix of solutions.
--
-- Generated bindings for @MPSMatrixSolveLU@.
module ObjC.MetalPerformanceShaders.MPSMatrixSolveLU
  ( MPSMatrixSolveLU
  , IsMPSMatrixSolveLU(..)
  , initWithDevice_transpose_order_numberOfRightHandSides
  , encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_pivotIndices_solutionMatrix
  , initWithDevice_transpose_order_numberOfRightHandSidesSelector
  , encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_pivotIndices_solutionMatrixSelector


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

-- | Initialize an MPSMatrixSolveLU object on a device
--
-- @device@ — The device on which the kernel will execute.
--
-- @transpose@ — A boolean value which indicates if the source                              matrix should be used in transposed form.
--
-- @order@ — The order of the source matrix and the number of                              rows in the solution and right hand side matrices.
--
-- @numberOfRightHandSides@ — The number of columns in the solution and right hand side                                      matrices.
--
-- Returns: A valid MPSMatrixSolveLU object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:transpose:order:numberOfRightHandSides:@
initWithDevice_transpose_order_numberOfRightHandSides :: IsMPSMatrixSolveLU mpsMatrixSolveLU => mpsMatrixSolveLU -> RawId -> Bool -> CULong -> CULong -> IO (Id MPSMatrixSolveLU)
initWithDevice_transpose_order_numberOfRightHandSides mpsMatrixSolveLU  device transpose order numberOfRightHandSides =
  sendMsg mpsMatrixSolveLU (mkSelector "initWithDevice:transpose:order:numberOfRightHandSides:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (if transpose then 1 else 0), argCULong (fromIntegral order), argCULong (fromIntegral numberOfRightHandSides)] >>= ownedObject . castPtr

-- | Encode a MPSMatrixSolveLU kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceMatrix@ — A valid MPSMatrix containing the source matrix in factored                                  form as returned by a previous successful execution of a                                  MPSMatrixDecompositionLU kernel.
--
-- @rightHandSideMatrix@ — A valid MPSMatrix containing the right hand side values.
--
-- @pivotIndices@ — A valid MPSMatrix which contains the pivot indices as returned by                                  a previous successful execution of a MPSMatrixDecompositionLU                                  kernel.
--
-- @solutionMatrix@ — A valid MPSMatrix to contain the result.
--
-- This function encodes the MPSMatrixSolveLU object to a valid command buffer.              sourceMatrix should contain the lower and upper triangular factors of A as              results from a previous execution of MPSMatrixDecompositionLU.
--
-- pivotIndices is an array of pivots resulting from a previous execution of              MPSMatrixDecompositionLU.
--
-- rightHandSideMatrix and solutionMatrix must be large enough to hold a matrix              of size order x numberOfRightHandSides starting at secondarySourceMatrixOrigin and              resultMatrixOrigin respectively.
--
-- sourceMatrix must be at least size order x order starting at primarySourceMatrixOrigin.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:pivotIndices:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_pivotIndices_solutionMatrix :: (IsMPSMatrixSolveLU mpsMatrixSolveLU, IsMPSMatrix sourceMatrix, IsMPSMatrix rightHandSideMatrix, IsMPSMatrix pivotIndices, IsMPSMatrix solutionMatrix) => mpsMatrixSolveLU -> RawId -> sourceMatrix -> rightHandSideMatrix -> pivotIndices -> solutionMatrix -> IO ()
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_pivotIndices_solutionMatrix mpsMatrixSolveLU  commandBuffer sourceMatrix rightHandSideMatrix pivotIndices solutionMatrix =
withObjCPtr sourceMatrix $ \raw_sourceMatrix ->
  withObjCPtr rightHandSideMatrix $ \raw_rightHandSideMatrix ->
    withObjCPtr pivotIndices $ \raw_pivotIndices ->
      withObjCPtr solutionMatrix $ \raw_solutionMatrix ->
          sendMsg mpsMatrixSolveLU (mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:pivotIndices:solutionMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrix :: Ptr ()), argPtr (castPtr raw_rightHandSideMatrix :: Ptr ()), argPtr (castPtr raw_pivotIndices :: Ptr ()), argPtr (castPtr raw_solutionMatrix :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:transpose:order:numberOfRightHandSides:@
initWithDevice_transpose_order_numberOfRightHandSidesSelector :: Selector
initWithDevice_transpose_order_numberOfRightHandSidesSelector = mkSelector "initWithDevice:transpose:order:numberOfRightHandSides:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:pivotIndices:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_pivotIndices_solutionMatrixSelector :: Selector
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_pivotIndices_solutionMatrixSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:pivotIndices:solutionMatrix:"

