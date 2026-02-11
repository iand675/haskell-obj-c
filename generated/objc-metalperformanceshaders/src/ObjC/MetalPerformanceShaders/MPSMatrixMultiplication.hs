{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixMultiplication
--
-- This depends on Metal.framework.
--
-- A matrix multiplication kernel.
--
-- A MPSMatrixMultiplication object computes:
--
-- C = alpha * op(A) * op(B) + beta * C
--
-- A, B, and C are matrices which are represented by MPSMatrix              objects. alpha and beta are scalar values (of the same data type              as values of C) which are applied as shown above.  A and B may              each have an optional transposition operation applied.
--
-- A, B, and C (also referred to in later discussions as the left input              matrix, the right input matrix, and the result matrix respectively).
--
-- A MPSMatrixMultiplication object is initialized with the transpose              operators to apply to A and B, sizes for the operation to perform,              and the scalar values alpha and beta.
--
-- Generated bindings for @MPSMatrixMultiplication@.
module ObjC.MetalPerformanceShaders.MPSMatrixMultiplication
  ( MPSMatrixMultiplication
  , IsMPSMatrixMultiplication(..)
  , initWithDevice_transposeLeft_transposeRight_resultRows_resultColumns_interiorColumns_alpha_beta
  , initWithDevice_resultRows_resultColumns_interiorColumns
  , initWithDevice
  , encodeToCommandBuffer_leftMatrix_rightMatrix_resultMatrix
  , batchStart
  , setBatchStart
  , batchSize
  , setBatchSize
  , initWithDevice_transposeLeft_transposeRight_resultRows_resultColumns_interiorColumns_alpha_betaSelector
  , initWithDevice_resultRows_resultColumns_interiorColumnsSelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_leftMatrix_rightMatrix_resultMatrixSelector
  , batchStartSelector
  , setBatchStartSelector
  , batchSizeSelector
  , setBatchSizeSelector


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

-- | Initialize an MPSMatrixMultiplication object on a device for a given size              and desired transpose and scale values.
--
-- @device@ — The device on which the kernel will execute.
--
-- @transposeLeft@ — A boolean value which indicates if the left input matrix should be                              used in transposed form.  If 'YES' then op(A) = A**T, otherwise                              op(A) = A.
--
-- @transposeRight@ — A boolean value which indicates if the right input matrix should be                              used in transposed form.  If 'YES' then op(B) = B**T, otherwise                              op(B) = B.
--
-- @resultRows@ — The number of rows in the result matrix, M in BLAS GEMM description.
--
-- @resultColumns@ — The number of columns in the result matrix, N in BLAS GEMM description.
--
-- @interiorColumns@ — The number of columns of the left input matrix after the                              appropriate transpose operation has been applied. K in BLAS                              GEMM description.
--
-- @alpha@ — The scale factor to apply to the product.  Specified in double                              precision.  Will be converted to the appropriate precision in the                              implementation subject to rounding and/or clamping as necessary.
--
-- @beta@ — The scale factor to apply to the initial values of C.  Specified                              in double precision.  Will be converted to the appropriate precision in the                              implementation subject to rounding and/or clamping as necessary.
--
-- Returns: A valid MPSMatrixMultiplication object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:transposeLeft:transposeRight:resultRows:resultColumns:interiorColumns:alpha:beta:@
initWithDevice_transposeLeft_transposeRight_resultRows_resultColumns_interiorColumns_alpha_beta :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> RawId -> Bool -> Bool -> CULong -> CULong -> CULong -> CDouble -> CDouble -> IO (Id MPSMatrixMultiplication)
initWithDevice_transposeLeft_transposeRight_resultRows_resultColumns_interiorColumns_alpha_beta mpsMatrixMultiplication  device transposeLeft transposeRight resultRows resultColumns interiorColumns alpha beta =
  sendMsg mpsMatrixMultiplication (mkSelector "initWithDevice:transposeLeft:transposeRight:resultRows:resultColumns:interiorColumns:alpha:beta:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (if transposeLeft then 1 else 0), argCULong (if transposeRight then 1 else 0), argCULong (fromIntegral resultRows), argCULong (fromIntegral resultColumns), argCULong (fromIntegral interiorColumns), argCDouble (fromIntegral alpha), argCDouble (fromIntegral beta)] >>= ownedObject . castPtr

-- | Convenience initialization for a matrix-matrix multiplication              with no transpositions, unit scaling of the product, and no              accumulation of the result.  The scaling factors alpha and beta              are taken to be 1.0 and 0.0 respectively.
--
-- @device@ — The device on which the kernel will execute.
--
-- @resultRows@ — The number of rows in the result matrix, M in BLAS GEMM description.
--
-- @resultColumns@ — The number of columns in the result matrix, N in BLAS GEMM description.
--
-- @interiorColumns@ — The number of columns of the left input matrix. K in BLAS                              GEMM description.
--
-- Returns: A valid MPSMatrixMultiplication object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:resultRows:resultColumns:interiorColumns:@
initWithDevice_resultRows_resultColumns_interiorColumns :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> RawId -> CULong -> CULong -> CULong -> IO (Id MPSMatrixMultiplication)
initWithDevice_resultRows_resultColumns_interiorColumns mpsMatrixMultiplication  device resultRows resultColumns interiorColumns =
  sendMsg mpsMatrixMultiplication (mkSelector "initWithDevice:resultRows:resultColumns:interiorColumns:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral resultRows), argCULong (fromIntegral resultColumns), argCULong (fromIntegral interiorColumns)] >>= ownedObject . castPtr

-- | Use the above initialization method instead.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> RawId -> IO (Id MPSMatrixMultiplication)
initWithDevice mpsMatrixMultiplication  device =
  sendMsg mpsMatrixMultiplication (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixMultiplication object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @leftMatrix@ — A valid MPSMatrix object which specifies the left input matrix.
--
-- @rightMatrix@ — A valid MPSMatrix object which specifies the right input matrix.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the addend matrix which will                              also be overwritten by the result.
--
-- Certain constraints apply to the sizes of the matrices depending on the transposition              operations and sizes requested at initialization time as well as the origins at the time              this routine is called:
--
-- The left input matrix must be large enough to hold an array of size resultRows x interiorColumns              elements beginning at leftMatrixOrigin.
--
-- The right input matrix must be large enough to hold an array of size interiorColumns x resultColumns              elements beginning at rightMatrixOrigin.
--
-- The result matrix must be large enough to hold an array of size resultRows x resultColumns              elements beginning at resultMatrixOrigin.
--
-- Each matrix within the range specified by batchStart and batchSize, which also specifies              a valid set of matrices within leftMatrix, rightMatrix, and resultMatrix, will              be processed.
--
-- ObjC selector: @- encodeToCommandBuffer:leftMatrix:rightMatrix:resultMatrix:@
encodeToCommandBuffer_leftMatrix_rightMatrix_resultMatrix :: (IsMPSMatrixMultiplication mpsMatrixMultiplication, IsMPSMatrix leftMatrix, IsMPSMatrix rightMatrix, IsMPSMatrix resultMatrix) => mpsMatrixMultiplication -> RawId -> leftMatrix -> rightMatrix -> resultMatrix -> IO ()
encodeToCommandBuffer_leftMatrix_rightMatrix_resultMatrix mpsMatrixMultiplication  commandBuffer leftMatrix rightMatrix resultMatrix =
withObjCPtr leftMatrix $ \raw_leftMatrix ->
  withObjCPtr rightMatrix $ \raw_rightMatrix ->
    withObjCPtr resultMatrix $ \raw_resultMatrix ->
        sendMsg mpsMatrixMultiplication (mkSelector "encodeToCommandBuffer:leftMatrix:rightMatrix:resultMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_leftMatrix :: Ptr ()), argPtr (castPtr raw_rightMatrix :: Ptr ()), argPtr (castPtr raw_resultMatrix :: Ptr ())]

-- | batchStart
--
-- The index of the first matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.  If              batch processing should begin at a different matrix this value              should be modified prior to encoding the kernel.
--
-- ObjC selector: @- batchStart@
batchStart :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> IO CULong
batchStart mpsMatrixMultiplication  =
  sendMsg mpsMatrixMultiplication (mkSelector "batchStart") retCULong []

-- | batchStart
--
-- The index of the first matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.  If              batch processing should begin at a different matrix this value              should be modified prior to encoding the kernel.
--
-- ObjC selector: @- setBatchStart:@
setBatchStart :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> CULong -> IO ()
setBatchStart mpsMatrixMultiplication  value =
  sendMsg mpsMatrixMultiplication (mkSelector "setBatchStart:") retVoid [argCULong (fromIntegral value)]

-- | batchSize
--
-- The number of matrices in the batch to process.  This property              is modifiable and by default allows all matrices available at              encoding time to be processed.
--
-- ObjC selector: @- batchSize@
batchSize :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> IO CULong
batchSize mpsMatrixMultiplication  =
  sendMsg mpsMatrixMultiplication (mkSelector "batchSize") retCULong []

-- | batchSize
--
-- The number of matrices in the batch to process.  This property              is modifiable and by default allows all matrices available at              encoding time to be processed.
--
-- ObjC selector: @- setBatchSize:@
setBatchSize :: IsMPSMatrixMultiplication mpsMatrixMultiplication => mpsMatrixMultiplication -> CULong -> IO ()
setBatchSize mpsMatrixMultiplication  value =
  sendMsg mpsMatrixMultiplication (mkSelector "setBatchSize:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:transposeLeft:transposeRight:resultRows:resultColumns:interiorColumns:alpha:beta:@
initWithDevice_transposeLeft_transposeRight_resultRows_resultColumns_interiorColumns_alpha_betaSelector :: Selector
initWithDevice_transposeLeft_transposeRight_resultRows_resultColumns_interiorColumns_alpha_betaSelector = mkSelector "initWithDevice:transposeLeft:transposeRight:resultRows:resultColumns:interiorColumns:alpha:beta:"

-- | @Selector@ for @initWithDevice:resultRows:resultColumns:interiorColumns:@
initWithDevice_resultRows_resultColumns_interiorColumnsSelector :: Selector
initWithDevice_resultRows_resultColumns_interiorColumnsSelector = mkSelector "initWithDevice:resultRows:resultColumns:interiorColumns:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:leftMatrix:rightMatrix:resultMatrix:@
encodeToCommandBuffer_leftMatrix_rightMatrix_resultMatrixSelector :: Selector
encodeToCommandBuffer_leftMatrix_rightMatrix_resultMatrixSelector = mkSelector "encodeToCommandBuffer:leftMatrix:rightMatrix:resultMatrix:"

-- | @Selector@ for @batchStart@
batchStartSelector :: Selector
batchStartSelector = mkSelector "batchStart"

-- | @Selector@ for @setBatchStart:@
setBatchStartSelector :: Selector
setBatchStartSelector = mkSelector "setBatchStart:"

-- | @Selector@ for @batchSize@
batchSizeSelector :: Selector
batchSizeSelector = mkSelector "batchSize"

-- | @Selector@ for @setBatchSize:@
setBatchSizeSelector :: Selector
setBatchSizeSelector = mkSelector "setBatchSize:"

