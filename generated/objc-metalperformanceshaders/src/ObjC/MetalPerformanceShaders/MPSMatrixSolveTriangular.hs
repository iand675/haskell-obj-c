{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixSolveTriangular
--
-- This depends on Metal.framework.
--
-- A kernel for computing the solution of a linear system of              equations using a triangular coefficient matrix.
--
-- A MPSMatrixSolveTriangular finds the solution matrix to the              triangular system:
--
-- op(A) * X = alpha * B    or    X * op(A) = alpha * B
--
-- Where A is either upper or lower triangular and op(A) is A**T              or A.  B is the array of right hand sides for which the              equations are to be solved.  X is the resulting matrix of              solutions.
--
-- Generated bindings for @MPSMatrixSolveTriangular@.
module ObjC.MetalPerformanceShaders.MPSMatrixSolveTriangular
  ( MPSMatrixSolveTriangular
  , IsMPSMatrixSolveTriangular(..)
  , initWithDevice_right_upper_transpose_unit_order_numberOfRightHandSides_alpha
  , encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix
  , initWithDevice_right_upper_transpose_unit_order_numberOfRightHandSides_alphaSelector
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

-- | Initialize an MPSMatrixSolveTriangular object on a device
--
-- @device@ — The device on which the kernel will execute.
--
-- @right@ — A boolean value which indicates if the                               coefficient matrix is multiplied on the left                              or right side of the solution.  NO indicates                              the multiplication is on the left.
--
-- @upper@ — A boolean value which indicates if the source                              is lower or upper triangular.  NO indicates                              that the coefficient matrix is lower triangular.
--
-- @transpose@ — A boolean value which indicates if the source                              matrix should be used in transposed form.  NO                              indicates that the coefficient matrix is to be                              used normally.
--
-- @unit@ — A boolean value which indicates if the source                              matrix is unit triangular.
--
-- @order@ — The order of the source matrix and, if                              right == NO, the number of rows in the solution                              and right hand side matrices.  If right == YES                              the number of columns in the solution and right                              hand side matrices.
--
-- @numberOfRightHandSides@ — If right == NO, the number of columns in the                                      solution and right hand side matrices.  The                                      number of rows otherwise.
--
-- @alpha@ — A double precision value used to scale the right                              hand sides.
--
-- This function initializes a MPSMatrixSolveTriangular object.  It              may allocate device side memory.
--
-- Returns: A valid MPSMatrixSolveTriangular object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:right:upper:transpose:unit:order:numberOfRightHandSides:alpha:@
initWithDevice_right_upper_transpose_unit_order_numberOfRightHandSides_alpha :: IsMPSMatrixSolveTriangular mpsMatrixSolveTriangular => mpsMatrixSolveTriangular -> RawId -> Bool -> Bool -> Bool -> Bool -> CULong -> CULong -> CDouble -> IO (Id MPSMatrixSolveTriangular)
initWithDevice_right_upper_transpose_unit_order_numberOfRightHandSides_alpha mpsMatrixSolveTriangular  device right upper transpose unit order numberOfRightHandSides alpha =
  sendMsg mpsMatrixSolveTriangular (mkSelector "initWithDevice:right:upper:transpose:unit:order:numberOfRightHandSides:alpha:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (if right then 1 else 0), argCULong (if upper then 1 else 0), argCULong (if transpose then 1 else 0), argCULong (if unit then 1 else 0), argCULong (fromIntegral order), argCULong (fromIntegral numberOfRightHandSides), argCDouble (fromIntegral alpha)] >>= ownedObject . castPtr

-- | Encode a MPSMatrixSolveTriangular kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the                                  encoded filter
--
-- @sourceMatrix@ — A valid MPSMatrix containing the source                                  matrix.
--
-- @rightHandSideMatrix@ — A valid MPSMatrix containing the right hand                                  side values.
--
-- @solutionMatrix@ — A valid MPSMatrix to contain the result.
--
-- This function encodes the MPSMatrixSolveTriangular object to a              valid command buffer.
--
-- rightHandSideMatrix and solutionMatrix must be large enough to              hold at least order * numberOfRightHandSides values starting at              secondarySourceMatrixOrigin and resultMatrixOrigin respectively.
--
-- sourceMatrix must be at least size order x order starting at              primarySourceMatrixOrigin.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix :: (IsMPSMatrixSolveTriangular mpsMatrixSolveTriangular, IsMPSMatrix sourceMatrix, IsMPSMatrix rightHandSideMatrix, IsMPSMatrix solutionMatrix) => mpsMatrixSolveTriangular -> RawId -> sourceMatrix -> rightHandSideMatrix -> solutionMatrix -> IO ()
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrix mpsMatrixSolveTriangular  commandBuffer sourceMatrix rightHandSideMatrix solutionMatrix =
withObjCPtr sourceMatrix $ \raw_sourceMatrix ->
  withObjCPtr rightHandSideMatrix $ \raw_rightHandSideMatrix ->
    withObjCPtr solutionMatrix $ \raw_solutionMatrix ->
        sendMsg mpsMatrixSolveTriangular (mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrix :: Ptr ()), argPtr (castPtr raw_rightHandSideMatrix :: Ptr ()), argPtr (castPtr raw_solutionMatrix :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:right:upper:transpose:unit:order:numberOfRightHandSides:alpha:@
initWithDevice_right_upper_transpose_unit_order_numberOfRightHandSides_alphaSelector :: Selector
initWithDevice_right_upper_transpose_unit_order_numberOfRightHandSides_alphaSelector = mkSelector "initWithDevice:right:upper:transpose:unit:order:numberOfRightHandSides:alpha:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:@
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector :: Selector
encodeToCommandBuffer_sourceMatrix_rightHandSideMatrix_solutionMatrixSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:rightHandSideMatrix:solutionMatrix:"

