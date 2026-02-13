{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixSoftMaxGradient
--
-- This depends on Metal.framework.
--
-- Computes the gradient corresponding to a forward MPSMatrixSoftMax object.
--
-- A MPSMatrixSoftMaxGradient object computes:
--
-- dL_dX_ij = Y_ij * (dL_dY_ij - sum_k(dL_dY_ik * Y_ik)
--
-- Where dL_dX is the resulting gradient of the loss function with respect to              the original input to the forward MPSMatrixSoftMax operation, Y is              the output of the forward MPSMatrixSoftMax operation, and dL_dY is the              gradient of the loss function with respect to Y.
--
-- Generated bindings for @MPSMatrixSoftMaxGradient@.
module ObjC.MetalPerformanceShaders.MPSMatrixSoftMaxGradient
  ( MPSMatrixSoftMaxGradient
  , IsMPSMatrixSoftMaxGradient(..)
  , initWithDevice
  , encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceRows
  , setSourceRows
  , sourceColumns
  , setSourceColumns
  , copyWithZone_deviceSelector
  , encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrixSelector
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

-- | Initialize an MPSMatrixSoftMaxGradient object on a device.
--
-- @device@ — The device on which the kernel will execute.
--
-- Returns: A valid MPSMatrixSoftMaxGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient => mpsMatrixSoftMaxGradient -> RawId -> IO (Id MPSMatrixSoftMaxGradient)
initWithDevice mpsMatrixSoftMaxGradient device =
  sendOwnedMessage mpsMatrixSoftMaxGradient initWithDeviceSelector device

-- | Encode a MPSMatrixSoftMaxGradient object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @gradientMatrix@ — A MPSMatrix object containing gradient values with respect                                  to the forward operation's output.  dL_dY in the class                                  description.
--
-- @forwardOutputMatrix@ — A MPSMatrix object containing the output values from the                                  forward operation.  Y in the class description.
--
-- @resultMatrix@ — The MPSMatrix object to hold the resulting gradient values                                  with respect to the forward operation's input.  dL_dX in the                                  class description.
--
-- ObjC selector: @- encodeToCommandBuffer:gradientMatrix:forwardOutputMatrix:resultMatrix:@
encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrix :: (IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient, IsMPSMatrix gradientMatrix, IsMPSMatrix forwardOutputMatrix, IsMPSMatrix resultMatrix) => mpsMatrixSoftMaxGradient -> RawId -> gradientMatrix -> forwardOutputMatrix -> resultMatrix -> IO ()
encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrix mpsMatrixSoftMaxGradient commandBuffer gradientMatrix forwardOutputMatrix resultMatrix =
  sendMessage mpsMatrixSoftMaxGradient encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrixSelector commandBuffer (toMPSMatrix gradientMatrix) (toMPSMatrix forwardOutputMatrix) (toMPSMatrix resultMatrix)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixSoftMaxGradient
--
-- @device@ — The MTLDevice on which to make the MPSMatrixSoftMaxGradient
--
-- Returns: A new MPSMatrixSoftMaxGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient, IsNSCoder aDecoder) => mpsMatrixSoftMaxGradient -> aDecoder -> RawId -> IO (Id MPSMatrixSoftMaxGradient)
initWithCoder_device mpsMatrixSoftMaxGradient aDecoder device =
  sendOwnedMessage mpsMatrixSoftMaxGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
copyWithZone_device :: IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient => mpsMatrixSoftMaxGradient -> Ptr () -> RawId -> IO (Id MPSMatrixSoftMaxGradient)
copyWithZone_device mpsMatrixSoftMaxGradient zone device =
  sendOwnedMessage mpsMatrixSoftMaxGradient copyWithZone_deviceSelector zone device

-- | sourceRows
--
-- The number of rows to consider from the sources in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrices available starting from              [primary/secondary]SourceMatrixOrigin.x, indicating that by default the              whole source matrix is used. If a different size is desired then this should              be modified prior to encoding the kernel. It is the user's responsibility to              ensure that the resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: primarySourceMatrixOrigin, secondarySourceMatrixOrigin and resultMatrixOrigin              from MPSMatrixBinaryKernel can be used to control the starting points in the primary              source, secondary source, and result matrices respectively.
--
-- ObjC selector: @- sourceRows@
sourceRows :: IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient => mpsMatrixSoftMaxGradient -> IO CULong
sourceRows mpsMatrixSoftMaxGradient =
  sendMessage mpsMatrixSoftMaxGradient sourceRowsSelector

-- | sourceRows
--
-- The number of rows to consider from the sources in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrices available starting from              [primary/secondary]SourceMatrixOrigin.x, indicating that by default the              whole source matrix is used. If a different size is desired then this should              be modified prior to encoding the kernel. It is the user's responsibility to              ensure that the resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: primarySourceMatrixOrigin, secondarySourceMatrixOrigin and resultMatrixOrigin              from MPSMatrixBinaryKernel can be used to control the starting points in the primary              source, secondary source, and result matrices respectively.
--
-- ObjC selector: @- setSourceRows:@
setSourceRows :: IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient => mpsMatrixSoftMaxGradient -> CULong -> IO ()
setSourceRows mpsMatrixSoftMaxGradient value =
  sendMessage mpsMatrixSoftMaxGradient setSourceRowsSelector value

-- | sourceColumns
--
-- The number of columns to consider from the sources in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrices available starting from [primary/secondary]SourceMatrixOrigin.y,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel. It is the user's responsibility to ensure that the              resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: primarySourceMatrixOrigin, secondarySourceMatrixOrigin and resultMatrixOrigin              from MPSMatrixBinaryKernel can be used to control the starting points in the primary              source, secondary source, and result matrices respectively.
--
-- ObjC selector: @- sourceColumns@
sourceColumns :: IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient => mpsMatrixSoftMaxGradient -> IO CULong
sourceColumns mpsMatrixSoftMaxGradient =
  sendMessage mpsMatrixSoftMaxGradient sourceColumnsSelector

-- | sourceColumns
--
-- The number of columns to consider from the sources in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrices available starting from [primary/secondary]SourceMatrixOrigin.y,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel. It is the user's responsibility to ensure that the              resultMatrix parameter in encodeToCommandBuffer is large enough              to accommodate the results of this operation, otherwise the results of              the encode call are undefined.              NOTE: primarySourceMatrixOrigin, secondarySourceMatrixOrigin and resultMatrixOrigin              from MPSMatrixBinaryKernel can be used to control the starting points in the primary              source, secondary source, and result matrices respectively.
--
-- ObjC selector: @- setSourceColumns:@
setSourceColumns :: IsMPSMatrixSoftMaxGradient mpsMatrixSoftMaxGradient => mpsMatrixSoftMaxGradient -> CULong -> IO ()
setSourceColumns mpsMatrixSoftMaxGradient value =
  sendMessage mpsMatrixSoftMaxGradient setSourceColumnsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixSoftMaxGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:gradientMatrix:forwardOutputMatrix:resultMatrix:@
encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSMatrix] ()
encodeToCommandBuffer_gradientMatrix_forwardOutputMatrix_resultMatrixSelector = mkSelector "encodeToCommandBuffer:gradientMatrix:forwardOutputMatrix:resultMatrix:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixSoftMaxGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixSoftMaxGradient)
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

