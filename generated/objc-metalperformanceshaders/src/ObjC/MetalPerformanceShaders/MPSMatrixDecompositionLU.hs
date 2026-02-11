{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixDecompositionLU
--
-- This depends on Metal.framework.
--
-- A kernel for computing the LU factorization of a matrix using              partial pivoting with row interchanges.
--
-- A MPSMatrixDecompositionLU object computes an LU factorization:
--
-- P * A = L * U
--
-- A is a matrix for which the LU factorization is to be computed.              L is a unit lower triangular matrix and U is an upper triangular              matrix.  P is a permutation matrix.
--
-- Generated bindings for @MPSMatrixDecompositionLU@.
module ObjC.MetalPerformanceShaders.MPSMatrixDecompositionLU
  ( MPSMatrixDecompositionLU
  , IsMPSMatrixDecompositionLU(..)
  , initWithDevice_rows_columns
  , encodeToCommandBuffer_sourceMatrix_resultMatrix_pivotIndices_status
  , initWithDevice_rows_columnsSelector
  , encodeToCommandBuffer_sourceMatrix_resultMatrix_pivotIndices_statusSelector


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

-- | Initialize an MPSMatrixDecompositionLU object on a device
--
-- @device@ — The device on which the kernel will execute.
--
-- @rows@ — The number of rows in the source matrix.
--
-- @columns@ — The number of columns in the source matrix.
--
-- Returns: A valid MPSMatrixDecompositionLU object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rows:columns:@
initWithDevice_rows_columns :: IsMPSMatrixDecompositionLU mpsMatrixDecompositionLU => mpsMatrixDecompositionLU -> RawId -> CULong -> CULong -> IO (Id MPSMatrixDecompositionLU)
initWithDevice_rows_columns mpsMatrixDecompositionLU  device rows columns =
  sendMsg mpsMatrixDecompositionLU (mkSelector "initWithDevice:rows:columns:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral rows), argCULong (fromIntegral columns)] >>= ownedObject . castPtr

-- | Encode a MPSMatrixDecompositionLU kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceMatrix@ — A valid MPSMatrix containing the source data.  Must have                                      enough space to hold a rows x columns matrix.
--
-- @resultMatrix@ — A valid MPSMatrix to contain the result.  Must have enough                                      space to hold a rows x columns matrix.
--
-- @pivotIndices@ — A valid MPSMatrix to contain the pivot indices. Must have enough space                                      to hold an array of size 1xmin(rows, columns) values.                                      Element type must be MPSDataTypeUInt32.
--
-- @status@ — A MTLBuffer which indicates the resulting MPSMatrixDecompositionStatus                                      value.
--
-- This function encodes the MPSMatrixDecompositionLU object to a valid              command buffer.
--
-- Upon completion the array pivotIndices contains, for each index i,              the row interchanged with row i.
--
-- If during the computation U[k, k], for some k, is determined to be              exactly zero MPSMatrixDecompositionStatusSingular will be returned in the              provided status buffer.  The data referenced by the MTLBuffer is not valid              until the command buffer has completed execution.  If the matrix              return status is not desired NULL may be provided.
--
-- Upon successful factorization, resultMatrix contains the resulting              lower triangular factor (without the unit diagonal elements) in its              strictly lower triangular region and the upper triangular factor in              its upper triangular region.
--
-- This kernel functions either in-place, if the result matrix              completely aliases the source matrix, or out-of-place.  If there              is any partial overlap between input and output data the results              are undefined.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrix:resultMatrix:pivotIndices:status:@
encodeToCommandBuffer_sourceMatrix_resultMatrix_pivotIndices_status :: (IsMPSMatrixDecompositionLU mpsMatrixDecompositionLU, IsMPSMatrix sourceMatrix, IsMPSMatrix resultMatrix, IsMPSMatrix pivotIndices) => mpsMatrixDecompositionLU -> RawId -> sourceMatrix -> resultMatrix -> pivotIndices -> RawId -> IO ()
encodeToCommandBuffer_sourceMatrix_resultMatrix_pivotIndices_status mpsMatrixDecompositionLU  commandBuffer sourceMatrix resultMatrix pivotIndices status =
withObjCPtr sourceMatrix $ \raw_sourceMatrix ->
  withObjCPtr resultMatrix $ \raw_resultMatrix ->
    withObjCPtr pivotIndices $ \raw_pivotIndices ->
        sendMsg mpsMatrixDecompositionLU (mkSelector "encodeToCommandBuffer:sourceMatrix:resultMatrix:pivotIndices:status:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrix :: Ptr ()), argPtr (castPtr raw_resultMatrix :: Ptr ()), argPtr (castPtr raw_pivotIndices :: Ptr ()), argPtr (castPtr (unRawId status) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:rows:columns:@
initWithDevice_rows_columnsSelector :: Selector
initWithDevice_rows_columnsSelector = mkSelector "initWithDevice:rows:columns:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:resultMatrix:pivotIndices:status:@
encodeToCommandBuffer_sourceMatrix_resultMatrix_pivotIndices_statusSelector :: Selector
encodeToCommandBuffer_sourceMatrix_resultMatrix_pivotIndices_statusSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:resultMatrix:pivotIndices:status:"

