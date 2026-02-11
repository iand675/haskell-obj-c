{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixVectorMultiplication
--
-- This depends on Metal.framework.
--
-- A matrix-vector multiplication kernel.
--
-- A MPSMatrixVectorMultiplication object computes:
--
-- y = alpha * op(A) * x + beta * y
--
-- A is a matrix represented by a MPSMatrix object. alpha and beta              are scalar values (of the same data type as values of y) which are              applied as shown above.  A may have an optional transposition              operation applied.
--
-- A MPSMatrixVectorMultiplication object is initialized with the transpose              operator to apply to A, sizes for the operation to perform,              and the scalar values alpha and beta.
--
-- Generated bindings for @MPSMatrixVectorMultiplication@.
module ObjC.MetalPerformanceShaders.MPSMatrixVectorMultiplication
  ( MPSMatrixVectorMultiplication
  , IsMPSMatrixVectorMultiplication(..)
  , initWithDevice_transpose_rows_columns_alpha_beta
  , initWithDevice_rows_columns
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_inputVector_resultVector
  , initWithDevice_transpose_rows_columns_alpha_betaSelector
  , initWithDevice_rows_columnsSelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_inputMatrix_inputVector_resultVectorSelector


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

-- | Initialize an MPSMatrixVectorMultiplication object on a device for a given size              and desired transpose and scale values.
--
-- @device@ — The device on which the kernel will execute.
--
-- @transpose@ — A boolean value which indicates if the input matrix should be                              used in transposed form.  if 'YES' then op(A) == A**T, otherwise                              op(A) == A.
--
-- @rows@ — The number of rows in the input matrix op(A), and the number of elements                              in the vector y.
--
-- @columns@ — The number of columns in the input matrix op(A), and the number of                              elements in the input vector x.
--
-- @alpha@ — The scale factor to apply to the product.  Specified in double                              precision.  Will be converted to the appropriate precision in the                              implementation subject to rounding and/or clamping as necessary.
--
-- @beta@ — The scale factor to apply to the initial values of y.  Specified                              in double precision.  Will be converted to the appropriate precision in the                              implementation subject to rounding and/or clamping as necessary.
--
-- Returns: A valid MPSMatrixVectorMultiplication object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:transpose:rows:columns:alpha:beta:@
initWithDevice_transpose_rows_columns_alpha_beta :: IsMPSMatrixVectorMultiplication mpsMatrixVectorMultiplication => mpsMatrixVectorMultiplication -> RawId -> Bool -> CULong -> CULong -> CDouble -> CDouble -> IO (Id MPSMatrixVectorMultiplication)
initWithDevice_transpose_rows_columns_alpha_beta mpsMatrixVectorMultiplication  device transpose rows columns alpha beta =
  sendMsg mpsMatrixVectorMultiplication (mkSelector "initWithDevice:transpose:rows:columns:alpha:beta:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (if transpose then 1 else 0), argCULong (fromIntegral rows), argCULong (fromIntegral columns), argCDouble (fromIntegral alpha), argCDouble (fromIntegral beta)] >>= ownedObject . castPtr

-- | Convenience initialization for a matrix-vector multiplication              with no transposition, unit scaling of the product, and no              accumulation of the result.  The scaling factors alpha and beta              are taken to be 1.0 and 0.0 respectively.
--
-- @device@ — The device on which the kernel will execute.
--
-- @rows@ — The number of rows in the input matrix A, and the number of elements                              in the vector y.
--
-- @columns@ — The number of columns in the input matrix A, and the number of                              elements in the input vector x.
--
-- Returns: A valid MPSMatrixVectorMultiplication object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rows:columns:@
initWithDevice_rows_columns :: IsMPSMatrixVectorMultiplication mpsMatrixVectorMultiplication => mpsMatrixVectorMultiplication -> RawId -> CULong -> CULong -> IO (Id MPSMatrixVectorMultiplication)
initWithDevice_rows_columns mpsMatrixVectorMultiplication  device rows columns =
  sendMsg mpsMatrixVectorMultiplication (mkSelector "initWithDevice:rows:columns:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral rows), argCULong (fromIntegral columns)] >>= ownedObject . castPtr

-- | Use the above initialization method instead.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixVectorMultiplication mpsMatrixVectorMultiplication => mpsMatrixVectorMultiplication -> RawId -> IO (Id MPSMatrixVectorMultiplication)
initWithDevice mpsMatrixVectorMultiplication  device =
  sendMsg mpsMatrixVectorMultiplication (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixVectorMultiplication object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input matrix A.
--
-- @inputVector@ — A valid MPSVector object which specifies the input vector x.
--
-- @resultVector@ — A valid MPSVector object which specifies the addend vector which will                              also be overwritten by the result.
--
-- The left input matrix must be large enough to hold an array of size (rows x columns)              elements beginning at primarySourceMatrixOrigin.
--
-- The input vector must be large enough to hold an array of size (columns)              elements beginning at secondarySourceMatrixOrigin.x  secondarySourceMatrixOrigin.y and              secondarySourceMatrixOrigin.z must be zero.
--
-- The result vector must be large enough to hold an array of size (rows)              elements beginning at resultMatrixOrigin.x.  resultMatrixOrigin.y and              resultMatrixOrigin.z must be zero.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:inputVector:resultVector:@
encodeToCommandBuffer_inputMatrix_inputVector_resultVector :: (IsMPSMatrixVectorMultiplication mpsMatrixVectorMultiplication, IsMPSMatrix inputMatrix, IsMPSVector inputVector, IsMPSVector resultVector) => mpsMatrixVectorMultiplication -> RawId -> inputMatrix -> inputVector -> resultVector -> IO ()
encodeToCommandBuffer_inputMatrix_inputVector_resultVector mpsMatrixVectorMultiplication  commandBuffer inputMatrix inputVector resultVector =
withObjCPtr inputMatrix $ \raw_inputMatrix ->
  withObjCPtr inputVector $ \raw_inputVector ->
    withObjCPtr resultVector $ \raw_resultVector ->
        sendMsg mpsMatrixVectorMultiplication (mkSelector "encodeToCommandBuffer:inputMatrix:inputVector:resultVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_inputVector :: Ptr ()), argPtr (castPtr raw_resultVector :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:transpose:rows:columns:alpha:beta:@
initWithDevice_transpose_rows_columns_alpha_betaSelector :: Selector
initWithDevice_transpose_rows_columns_alpha_betaSelector = mkSelector "initWithDevice:transpose:rows:columns:alpha:beta:"

-- | @Selector@ for @initWithDevice:rows:columns:@
initWithDevice_rows_columnsSelector :: Selector
initWithDevice_rows_columnsSelector = mkSelector "initWithDevice:rows:columns:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:inputVector:resultVector:@
encodeToCommandBuffer_inputMatrix_inputVector_resultVectorSelector :: Selector
encodeToCommandBuffer_inputMatrix_inputVector_resultVectorSelector = mkSelector "encodeToCommandBuffer:inputMatrix:inputVector:resultVector:"

