{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixFindTopK
--
-- This depends on Metal.framework.
--
-- A kernel that find top-K values and their corresponding indices withing a row of a matrix
--
-- A MPSMatrixFindTopK object computes finds the 'k' largest values within              a row of a matrix and returns the value found and the index of the entry              in the source matrix. This operation is performed independently on the              rows and matrices in batch of the source matrix.
--
-- Generated bindings for @MPSMatrixFindTopK@.
module ObjC.MetalPerformanceShaders.MPSMatrixFindTopK
  ( MPSMatrixFindTopK
  , IsMPSMatrixFindTopK(..)
  , initWithDevice_numberOfTopKValues
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_resultIndexMatrix_resultValueMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceRows
  , setSourceRows
  , sourceColumns
  , setSourceColumns
  , indexOffset
  , setIndexOffset
  , numberOfTopKValues
  , setNumberOfTopKValues
  , initWithDevice_numberOfTopKValuesSelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_inputMatrix_resultIndexMatrix_resultValueMatrixSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , sourceRowsSelector
  , setSourceRowsSelector
  , sourceColumnsSelector
  , setSourceColumnsSelector
  , indexOffsetSelector
  , setIndexOffsetSelector
  , numberOfTopKValuesSelector
  , setNumberOfTopKValuesSelector


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

-- | Initialize an MPSMatrixFindTopK object on a device for a given size.
--
-- @device@ — The device on which the kernel will execute.
--
-- @numberOfTopKValues@ — The number of largest values to find from each row,                                  must be less or equal to 16.
--
-- Returns: A valid MPSMatrixFindTopK object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:numberOfTopKValues:@
initWithDevice_numberOfTopKValues :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> RawId -> CULong -> IO (Id MPSMatrixFindTopK)
initWithDevice_numberOfTopKValues mpsMatrixFindTopK  device numberOfTopKValues =
  sendMsg mpsMatrixFindTopK (mkSelector "initWithDevice:numberOfTopKValues:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral numberOfTopKValues)] >>= ownedObject . castPtr

-- | Use the above initialization method instead.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> RawId -> IO (Id MPSMatrixFindTopK)
initWithDevice mpsMatrixFindTopK  device =
  sendMsg mpsMatrixFindTopK (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixFindTopK object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input matrix.
--
-- @resultIndexMatrix@ — A valid MPSMatrix object which specifies the matrix which will                                  be overwritten by the result indices.                                  This matrix must have datatype MPSDataTypeUInt32.
--
-- @resultValueMatrix@ — A valid MPSMatrix object which specifies the matrix which will                                  be overwritten by the result values.
--
-- Certain constraints apply to the sizes of the matrices depending on the sizes requested at              initialization time as well as the origins at the time this routine is called:
--
-- Both result matrices must be large enough to hold a two dimensional array of 'sourceRows' rows and              'numberOfTopKValues' columns beginning at resultMatrixOrigin.
--
-- The source matrix must be large enough to contain at least 'numberOfTopKValues' values              starting from sourceMatrixOrigin.y.
--
-- Each matrix within the range specified by batchStart and batchSize, which also specifies a valid              set of matrices within inputMatrix, resultIndexMatrix and resultValueMatrix, will be processed.
--
-- The datatypes of the matrices inputMatrix and resultValueMatrix must match and be either              MPSDataTypeFloat32 or MPSDataTypeFloat16.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:resultIndexMatrix:resultValueMatrix:@
encodeToCommandBuffer_inputMatrix_resultIndexMatrix_resultValueMatrix :: (IsMPSMatrixFindTopK mpsMatrixFindTopK, IsMPSMatrix inputMatrix, IsMPSMatrix resultIndexMatrix, IsMPSMatrix resultValueMatrix) => mpsMatrixFindTopK -> RawId -> inputMatrix -> resultIndexMatrix -> resultValueMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_resultIndexMatrix_resultValueMatrix mpsMatrixFindTopK  commandBuffer inputMatrix resultIndexMatrix resultValueMatrix =
withObjCPtr inputMatrix $ \raw_inputMatrix ->
  withObjCPtr resultIndexMatrix $ \raw_resultIndexMatrix ->
    withObjCPtr resultValueMatrix $ \raw_resultValueMatrix ->
        sendMsg mpsMatrixFindTopK (mkSelector "encodeToCommandBuffer:inputMatrix:resultIndexMatrix:resultValueMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_resultIndexMatrix :: Ptr ()), argPtr (castPtr raw_resultValueMatrix :: Ptr ())]

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixFindTopK
--
-- @device@ — The MTLDevice on which to make the MPSMatrixFindTopK
--
-- Returns: A new MPSMatrixFindTopK object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixFindTopK mpsMatrixFindTopK, IsNSCoder aDecoder) => mpsMatrixFindTopK -> aDecoder -> RawId -> IO (Id MPSMatrixFindTopK)
initWithCoder_device mpsMatrixFindTopK  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixFindTopK (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
copyWithZone_device :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> Ptr () -> RawId -> IO (Id MPSMatrixFindTopK)
copyWithZone_device mpsMatrixFindTopK  zone device =
  sendMsg mpsMatrixFindTopK (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | sourceRows
--
-- The number of rows to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.x,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel.              It is the user's responsibility to ensure that the resultIndexMatrix and resultValueMatrix              parameters in encodeToCommandBuffer are large enough to accommodate the results of this              operation, otherwise the results of the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- sourceRows@
sourceRows :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> IO CULong
sourceRows mpsMatrixFindTopK  =
  sendMsg mpsMatrixFindTopK (mkSelector "sourceRows") retCULong []

-- | sourceRows
--
-- The number of rows to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.x,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel.              It is the user's responsibility to ensure that the resultIndexMatrix and resultValueMatrix              parameters in encodeToCommandBuffer are large enough to accommodate the results of this              operation, otherwise the results of the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- setSourceRows:@
setSourceRows :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> CULong -> IO ()
setSourceRows mpsMatrixFindTopK  value =
  sendMsg mpsMatrixFindTopK (mkSelector "setSourceRows:") retVoid [argCULong (fromIntegral value)]

-- | sourceColumns
--
-- The number of columns to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.y,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel.              It is the user's responsibility to ensure that the resultIndexMatrix and resultValueMatrix              parameters in encodeToCommandBuffer are large enough to accommodate the results of this              operation, otherwise the results of the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- sourceColumns@
sourceColumns :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> IO CULong
sourceColumns mpsMatrixFindTopK  =
  sendMsg mpsMatrixFindTopK (mkSelector "sourceColumns") retCULong []

-- | sourceColumns
--
-- The number of columns to consider from the source in the operation.              This property is modifiable and defaults to NSUIntegerMax and the number is              adjusted dynamically at kernel encode time (see encodeToCommandBuffer) to              fit into the source matrix available starting from sourceMatrixOrigin.y,              indicating that by default the whole source matrix is used.              If a different size is desired then this should be modified prior to              encoding the kernel.              It is the user's responsibility to ensure that the resultIndexMatrix and resultValueMatrix              parameters in encodeToCommandBuffer are large enough to accommodate the results of this              operation, otherwise the results of the encode call are undefined.              NOTE: sourceMatrixOrigin and resultMatrixOrigin from MPSMatrixUnaryKernel              can be used to control the starting points in the source and destination              at kernel encode time (see encodeToCommandBuffer).
--
-- ObjC selector: @- setSourceColumns:@
setSourceColumns :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> CULong -> IO ()
setSourceColumns mpsMatrixFindTopK  value =
  sendMsg mpsMatrixFindTopK (mkSelector "setSourceColumns:") retVoid [argCULong (fromIntegral value)]

-- | indexOffset
--
-- Specifies a number that will be added to all the indices written to              resultIndexMatrix in encodeToCommandBuffer. This value can be used              to offset later computations for example by adding the value for              the source matrix column offset sourceMatrixOrigin.y.              Example: Let numberOfTopKValues be 3, let the source be the following:
--
-- source = [ 6.0, 3.0, 8.0, 1.0, 9.0, 4.0, 5.0 ]
--
-- and let the sourceMatrixOrigin.y = 2.
--
-- Then if indexOffset = 2 then the result value and result index matrices will be:
--
-- result values  = [ 9.0, 8.0, 5.0 ]                  result indices = [  4 ,  2 ,  6  ],
--
-- which gives the user indices into the original source matrix.
--
-- On the other hand if the indexOffset = 0 then the results  are as follows:
--
-- result values  = [ 9.0, 8.0, 5.0 ]                  result indices = [  2 ,  0 ,  4  ],
--
-- which on the other hand gives the user indices into the submatrix starting              from sourceMatrixOrigin.y == 2.
--
-- This property is modifiable and defaults to 0. If a different behavior              is desired then this should be modified prior to encoding the kernel.
--
-- ObjC selector: @- indexOffset@
indexOffset :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> IO CULong
indexOffset mpsMatrixFindTopK  =
  sendMsg mpsMatrixFindTopK (mkSelector "indexOffset") retCULong []

-- | indexOffset
--
-- Specifies a number that will be added to all the indices written to              resultIndexMatrix in encodeToCommandBuffer. This value can be used              to offset later computations for example by adding the value for              the source matrix column offset sourceMatrixOrigin.y.              Example: Let numberOfTopKValues be 3, let the source be the following:
--
-- source = [ 6.0, 3.0, 8.0, 1.0, 9.0, 4.0, 5.0 ]
--
-- and let the sourceMatrixOrigin.y = 2.
--
-- Then if indexOffset = 2 then the result value and result index matrices will be:
--
-- result values  = [ 9.0, 8.0, 5.0 ]                  result indices = [  4 ,  2 ,  6  ],
--
-- which gives the user indices into the original source matrix.
--
-- On the other hand if the indexOffset = 0 then the results  are as follows:
--
-- result values  = [ 9.0, 8.0, 5.0 ]                  result indices = [  2 ,  0 ,  4  ],
--
-- which on the other hand gives the user indices into the submatrix starting              from sourceMatrixOrigin.y == 2.
--
-- This property is modifiable and defaults to 0. If a different behavior              is desired then this should be modified prior to encoding the kernel.
--
-- ObjC selector: @- setIndexOffset:@
setIndexOffset :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> CULong -> IO ()
setIndexOffset mpsMatrixFindTopK  value =
  sendMsg mpsMatrixFindTopK (mkSelector "setIndexOffset:") retVoid [argCULong (fromIntegral value)]

-- | numberOfTopKValues
--
-- The number of highest values (and their indices) to be found in each row              by the kernel. This property is initialized in the kernel initialization call              initWithDevice, but can be modified before encoding the kernel.              Must be less or equal to 16 and requesting more values results in undefined behavior.              It is the user's responsibility to ensure that the resultIndexMatrix and resultValueMatrix              parameters in encodeToCommandBuffer are large enough to accommodate the results of this              operation, otherwise the results of the encode call are undefined.
--
-- ObjC selector: @- numberOfTopKValues@
numberOfTopKValues :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> IO CULong
numberOfTopKValues mpsMatrixFindTopK  =
  sendMsg mpsMatrixFindTopK (mkSelector "numberOfTopKValues") retCULong []

-- | numberOfTopKValues
--
-- The number of highest values (and their indices) to be found in each row              by the kernel. This property is initialized in the kernel initialization call              initWithDevice, but can be modified before encoding the kernel.              Must be less or equal to 16 and requesting more values results in undefined behavior.              It is the user's responsibility to ensure that the resultIndexMatrix and resultValueMatrix              parameters in encodeToCommandBuffer are large enough to accommodate the results of this              operation, otherwise the results of the encode call are undefined.
--
-- ObjC selector: @- setNumberOfTopKValues:@
setNumberOfTopKValues :: IsMPSMatrixFindTopK mpsMatrixFindTopK => mpsMatrixFindTopK -> CULong -> IO ()
setNumberOfTopKValues mpsMatrixFindTopK  value =
  sendMsg mpsMatrixFindTopK (mkSelector "setNumberOfTopKValues:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:numberOfTopKValues:@
initWithDevice_numberOfTopKValuesSelector :: Selector
initWithDevice_numberOfTopKValuesSelector = mkSelector "initWithDevice:numberOfTopKValues:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:resultIndexMatrix:resultValueMatrix:@
encodeToCommandBuffer_inputMatrix_resultIndexMatrix_resultValueMatrixSelector :: Selector
encodeToCommandBuffer_inputMatrix_resultIndexMatrix_resultValueMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:resultIndexMatrix:resultValueMatrix:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @sourceRows@
sourceRowsSelector :: Selector
sourceRowsSelector = mkSelector "sourceRows"

-- | @Selector@ for @setSourceRows:@
setSourceRowsSelector :: Selector
setSourceRowsSelector = mkSelector "setSourceRows:"

-- | @Selector@ for @sourceColumns@
sourceColumnsSelector :: Selector
sourceColumnsSelector = mkSelector "sourceColumns"

-- | @Selector@ for @setSourceColumns:@
setSourceColumnsSelector :: Selector
setSourceColumnsSelector = mkSelector "setSourceColumns:"

-- | @Selector@ for @indexOffset@
indexOffsetSelector :: Selector
indexOffsetSelector = mkSelector "indexOffset"

-- | @Selector@ for @setIndexOffset:@
setIndexOffsetSelector :: Selector
setIndexOffsetSelector = mkSelector "setIndexOffset:"

-- | @Selector@ for @numberOfTopKValues@
numberOfTopKValuesSelector :: Selector
numberOfTopKValuesSelector = mkSelector "numberOfTopKValues"

-- | @Selector@ for @setNumberOfTopKValues:@
setNumberOfTopKValuesSelector :: Selector
setNumberOfTopKValuesSelector = mkSelector "setNumberOfTopKValues:"

