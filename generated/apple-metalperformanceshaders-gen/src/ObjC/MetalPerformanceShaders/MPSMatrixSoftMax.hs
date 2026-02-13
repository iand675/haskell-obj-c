{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixSoftMax
--
-- This depends on Metal.framework.
--
-- A softmax kernel that operates on matrices.
--
-- A MPSMatrixSoftMax object computes:
--
-- B_ij = Exp { A_ij } / ( Sum_k Exp { A_ik } )
--
-- A and B are matrices which are represented by MPSMatrix              objects. This filter computes the same result for MPSMatrices as              MPSCNNSoftMax filter does for MPSImages by interpreting the columns              of the matrix as feature channels, that is the sum runs over column indices.
--
-- Generated bindings for @MPSMatrixSoftMax@.
module ObjC.MetalPerformanceShaders.MPSMatrixSoftMax
  ( MPSMatrixSoftMax
  , IsMPSMatrixSoftMax(..)
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceRows
  , setSourceRows
  , sourceColumns
  , setSourceColumns
  , copyWithZone_deviceSelector
  , encodeToCommandBuffer_inputMatrix_resultMatrixSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , setSourceColumnsSelector
  , setSourceRowsSelector
  , sourceColumnsSelector
  , sourceRowsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MPSMatrixSoftMax object on a device for a given size.
--
-- @device@ — The device on which the kernel will execute.
--
-- Returns: A valid MPSMatrixSoftMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixSoftMax mpsMatrixSoftMax => mpsMatrixSoftMax -> RawId -> IO (Id MPSMatrixSoftMax)
initWithDevice mpsMatrixSoftMax device =
  sendOwnedMessage mpsMatrixSoftMax initWithDeviceSelector device

-- | Encode a MPSMatrixSoftMax object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input matrix.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the matrix which will                              be overwritten by the result.
--
-- Certain constraints apply to the sizes of the matrices depending on the sizes requested at              initialization time as well as the origins at the time this routine is called:
--
-- The result matrix must be large enough to hold a two dimensional array of 'sourceRows' rows and              'sourceColumns' columns beginning at resultMatrixOrigin.
--
-- Each matrix within the range specified by batchStart and batchSize, which also specifies              a valid set of matrices within inputMatrix and resultMatrix, will              be processed.
--
-- The datatypes of the matrices inputMatrix and resultMatrix must match and be either              MPSDataTypeFloat32 or MPSDataTypeFloat16.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:resultMatrix:@
encodeToCommandBuffer_inputMatrix_resultMatrix :: (IsMPSMatrixSoftMax mpsMatrixSoftMax, IsMPSMatrix inputMatrix, IsMPSMatrix resultMatrix) => mpsMatrixSoftMax -> RawId -> inputMatrix -> resultMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_resultMatrix mpsMatrixSoftMax commandBuffer inputMatrix resultMatrix =
  sendMessage mpsMatrixSoftMax encodeToCommandBuffer_inputMatrix_resultMatrixSelector commandBuffer (toMPSMatrix inputMatrix) (toMPSMatrix resultMatrix)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixSoftMax
--
-- @device@ — The MTLDevice on which to make the MPSMatrixSoftMax
--
-- Returns: A new MPSMatrixSoftMax object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixSoftMax mpsMatrixSoftMax, IsNSCoder aDecoder) => mpsMatrixSoftMax -> aDecoder -> RawId -> IO (Id MPSMatrixSoftMax)
initWithCoder_device mpsMatrixSoftMax aDecoder device =
  sendOwnedMessage mpsMatrixSoftMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Make a copy of this kernel for a new device -
--
-- See: MPSKernel
--
-- @zone@ — The NSZone in which to allocate the object
--
-- @device@ — The device for the new MPSKernel. If nil, then use                          self.device.
--
-- Returns: a pointer to a copy of this MPSKernel. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSMatrixSoftMax mpsMatrixSoftMax => mpsMatrixSoftMax -> Ptr () -> RawId -> IO (Id MPSMatrixSoftMax)
copyWithZone_device mpsMatrixSoftMax zone device =
  sendOwnedMessage mpsMatrixSoftMax copyWithZone_deviceSelector zone device

-- | sourceRows
--
-- The number of rows to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.x,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel. It is the user's responsibility to ensure that the              resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- sourceRows@
sourceRows :: IsMPSMatrixSoftMax mpsMatrixSoftMax => mpsMatrixSoftMax -> IO CULong
sourceRows mpsMatrixSoftMax =
  sendMessage mpsMatrixSoftMax sourceRowsSelector

-- | sourceRows
--
-- The number of rows to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.x,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel. It is the user's responsibility to ensure that the              resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- setSourceRows:@
setSourceRows :: IsMPSMatrixSoftMax mpsMatrixSoftMax => mpsMatrixSoftMax -> CULong -> IO ()
setSourceRows mpsMatrixSoftMax value =
  sendMessage mpsMatrixSoftMax setSourceRowsSelector value

-- | sourceColumns
--
-- The number of columns to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.y,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel. It is the user's responsibility to ensure that the              resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- sourceColumns@
sourceColumns :: IsMPSMatrixSoftMax mpsMatrixSoftMax => mpsMatrixSoftMax -> IO CULong
sourceColumns mpsMatrixSoftMax =
  sendMessage mpsMatrixSoftMax sourceColumnsSelector

-- | sourceColumns
--
-- The number of columns to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.y,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel. It is the user's responsibility to ensure that the              resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- setSourceColumns:@
setSourceColumns :: IsMPSMatrixSoftMax mpsMatrixSoftMax => mpsMatrixSoftMax -> CULong -> IO ()
setSourceColumns mpsMatrixSoftMax value =
  sendMessage mpsMatrixSoftMax setSourceColumnsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixSoftMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:resultMatrix:@
encodeToCommandBuffer_inputMatrix_resultMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix] ()
encodeToCommandBuffer_inputMatrix_resultMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:resultMatrix:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixSoftMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixSoftMax)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @sourceRows@
sourceRowsSelector :: Selector '[] CULong
sourceRowsSelector = mkSelector "sourceRows"

-- | @Selector@ for @setSourceRows:@
setSourceRowsSelector :: Selector '[CULong] ()
setSourceRowsSelector = mkSelector "setSourceRows:"

-- | @Selector@ for @sourceColumns@
sourceColumnsSelector :: Selector '[] CULong
sourceColumnsSelector = mkSelector "sourceColumns"

-- | @Selector@ for @setSourceColumns:@
setSourceColumnsSelector :: Selector '[CULong] ()
setSourceColumnsSelector = mkSelector "setSourceColumns:"

