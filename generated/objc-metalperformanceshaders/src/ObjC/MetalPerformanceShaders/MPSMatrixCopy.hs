{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSMatrixCopy@.
module ObjC.MetalPerformanceShaders.MPSMatrixCopy
  ( MPSMatrixCopy
  , IsMPSMatrixCopy(..)
  , initWithDevice
  , initWithDevice_copyRows_copyColumns_sourcesAreTransposed_destinationsAreTransposed
  , encodeToCommandBuffer_copyDescriptor
  , encodeToCommandBuffer_copyDescriptor_rowPermuteIndices_rowPermuteOffset_columnPermuteIndices_columnPermuteOffset
  , initWithCoder_device
  , copyRows
  , copyColumns
  , sourcesAreTransposed
  , destinationsAreTransposed
  , initWithDeviceSelector
  , initWithDevice_copyRows_copyColumns_sourcesAreTransposed_destinationsAreTransposedSelector
  , encodeToCommandBuffer_copyDescriptorSelector
  , encodeToCommandBuffer_copyDescriptor_rowPermuteIndices_rowPermuteOffset_columnPermuteIndices_columnPermuteOffsetSelector
  , initWithCoder_deviceSelector
  , copyRowsSelector
  , copyColumnsSelector
  , sourcesAreTransposedSelector
  , destinationsAreTransposedSelector


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

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixCopy mpsMatrixCopy => mpsMatrixCopy -> RawId -> IO (Id MPSMatrixCopy)
initWithDevice mpsMatrixCopy  device =
  sendMsg mpsMatrixCopy (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a copy operator
--
-- @copyRows@ — The number of rows to copy for each copy operation
--
-- @copyColumns@ — The number of matrix columns to copy in each copy operation
--
-- @sourcesAreTransposed@ — If YES, the sources are in column major storage order
--
-- @destinationsAreTransposed@ — If YES, the destinations are in column major storage order
--
-- ObjC selector: @- initWithDevice:copyRows:copyColumns:sourcesAreTransposed:destinationsAreTransposed:@
initWithDevice_copyRows_copyColumns_sourcesAreTransposed_destinationsAreTransposed :: IsMPSMatrixCopy mpsMatrixCopy => mpsMatrixCopy -> RawId -> CULong -> CULong -> Bool -> Bool -> IO (Id MPSMatrixCopy)
initWithDevice_copyRows_copyColumns_sourcesAreTransposed_destinationsAreTransposed mpsMatrixCopy  device copyRows copyColumns sourcesAreTransposed destinationsAreTransposed =
  sendMsg mpsMatrixCopy (mkSelector "initWithDevice:copyRows:copyColumns:sourcesAreTransposed:destinationsAreTransposed:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral copyRows), argCULong (fromIntegral copyColumns), argCULong (if sourcesAreTransposed then 1 else 0), argCULong (if destinationsAreTransposed then 1 else 0)] >>= ownedObject . castPtr

-- | Encode the copy operations to the command buffer
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @copyDescriptor@ — The descriptor that defines the copy operator
--
-- ObjC selector: @- encodeToCommandBuffer:copyDescriptor:@
encodeToCommandBuffer_copyDescriptor :: (IsMPSMatrixCopy mpsMatrixCopy, IsMPSMatrixCopyDescriptor copyDescriptor) => mpsMatrixCopy -> RawId -> copyDescriptor -> IO ()
encodeToCommandBuffer_copyDescriptor mpsMatrixCopy  commandBuffer copyDescriptor =
withObjCPtr copyDescriptor $ \raw_copyDescriptor ->
    sendMsg mpsMatrixCopy (mkSelector "encodeToCommandBuffer:copyDescriptor:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_copyDescriptor :: Ptr ())]

-- | Encode the copy operations to the command buffer.              This of the encode version support permuting the outputs with custom vectors of indices.              The permutations are defined on the destination indices and are the same for each copy              operation.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @copyDescriptor@ — The descriptor that defines the copy operator
--
-- @rowPermuteIndices@ — If not nil then the output row index is                                      'rowPermuteIndices[i] + rowOffset' instead of 'i + rowOffset',                                      where 'i' is the local row index of the copy operation.                                      Note: if destinationsAreTransposed is set to YES then the destination                                      transpose is performed before permutations.
--
-- @rowPermuteOffset@ — Offset in numbers to apply to the 'rowPermuteIndices' vector.
--
-- @columnPermuteIndices@ — If not nil then the output column index is                                      'columnPermuteIndices[i] + columnOffset' instead of 'i + columnOffset',                                      where 'i' is the local column index of the copy operation.                                      Note: if destinationsAreTransposed is set to YES then the destination                                      transpose is performed before permutations.
--
-- @columnPermuteOffset@ — Offset in numbers to apply to the 'columnPermuteIndices' vector.
--
-- ObjC selector: @- encodeToCommandBuffer:copyDescriptor:rowPermuteIndices:rowPermuteOffset:columnPermuteIndices:columnPermuteOffset:@
encodeToCommandBuffer_copyDescriptor_rowPermuteIndices_rowPermuteOffset_columnPermuteIndices_columnPermuteOffset :: (IsMPSMatrixCopy mpsMatrixCopy, IsMPSMatrixCopyDescriptor copyDescriptor, IsMPSVector rowPermuteIndices, IsMPSVector columnPermuteIndices) => mpsMatrixCopy -> RawId -> copyDescriptor -> rowPermuteIndices -> CULong -> columnPermuteIndices -> CULong -> IO ()
encodeToCommandBuffer_copyDescriptor_rowPermuteIndices_rowPermuteOffset_columnPermuteIndices_columnPermuteOffset mpsMatrixCopy  commandBuffer copyDescriptor rowPermuteIndices rowPermuteOffset columnPermuteIndices columnPermuteOffset =
withObjCPtr copyDescriptor $ \raw_copyDescriptor ->
  withObjCPtr rowPermuteIndices $ \raw_rowPermuteIndices ->
    withObjCPtr columnPermuteIndices $ \raw_columnPermuteIndices ->
        sendMsg mpsMatrixCopy (mkSelector "encodeToCommandBuffer:copyDescriptor:rowPermuteIndices:rowPermuteOffset:columnPermuteIndices:columnPermuteOffset:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_copyDescriptor :: Ptr ()), argPtr (castPtr raw_rowPermuteIndices :: Ptr ()), argCULong (fromIntegral rowPermuteOffset), argPtr (castPtr raw_columnPermuteIndices :: Ptr ()), argCULong (fromIntegral columnPermuteOffset)]

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixLookUpAndCopy
--
-- @device@ — The MTLDevice on which to make the MPSMatrixLookUpAndCopy
--
-- Returns: A new MPSMatrixLookUpAndCopy object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixCopy mpsMatrixCopy, IsNSCoder aDecoder) => mpsMatrixCopy -> aDecoder -> RawId -> IO (Id MPSMatrixCopy)
initWithCoder_device mpsMatrixCopy  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixCopy (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | The number of rows to copy for each copy operation
--
-- ObjC selector: @- copyRows@
copyRows :: IsMPSMatrixCopy mpsMatrixCopy => mpsMatrixCopy -> IO CULong
copyRows mpsMatrixCopy  =
  sendMsg mpsMatrixCopy (mkSelector "copyRows") retCULong []

-- | The number of columns to copy for each copy operation
--
-- ObjC selector: @- copyColumns@
copyColumns :: IsMPSMatrixCopy mpsMatrixCopy => mpsMatrixCopy -> IO CULong
copyColumns mpsMatrixCopy  =
  sendMsg mpsMatrixCopy (mkSelector "copyColumns") retCULong []

-- | If YES, the sources are in row major storage order
--
-- ObjC selector: @- sourcesAreTransposed@
sourcesAreTransposed :: IsMPSMatrixCopy mpsMatrixCopy => mpsMatrixCopy -> IO Bool
sourcesAreTransposed mpsMatrixCopy  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsMatrixCopy (mkSelector "sourcesAreTransposed") retCULong []

-- | If YES, the destinations are in row major storage order
--
-- ObjC selector: @- destinationsAreTransposed@
destinationsAreTransposed :: IsMPSMatrixCopy mpsMatrixCopy => mpsMatrixCopy -> IO Bool
destinationsAreTransposed mpsMatrixCopy  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsMatrixCopy (mkSelector "destinationsAreTransposed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:copyRows:copyColumns:sourcesAreTransposed:destinationsAreTransposed:@
initWithDevice_copyRows_copyColumns_sourcesAreTransposed_destinationsAreTransposedSelector :: Selector
initWithDevice_copyRows_copyColumns_sourcesAreTransposed_destinationsAreTransposedSelector = mkSelector "initWithDevice:copyRows:copyColumns:sourcesAreTransposed:destinationsAreTransposed:"

-- | @Selector@ for @encodeToCommandBuffer:copyDescriptor:@
encodeToCommandBuffer_copyDescriptorSelector :: Selector
encodeToCommandBuffer_copyDescriptorSelector = mkSelector "encodeToCommandBuffer:copyDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:copyDescriptor:rowPermuteIndices:rowPermuteOffset:columnPermuteIndices:columnPermuteOffset:@
encodeToCommandBuffer_copyDescriptor_rowPermuteIndices_rowPermuteOffset_columnPermuteIndices_columnPermuteOffsetSelector :: Selector
encodeToCommandBuffer_copyDescriptor_rowPermuteIndices_rowPermuteOffset_columnPermuteIndices_columnPermuteOffsetSelector = mkSelector "encodeToCommandBuffer:copyDescriptor:rowPermuteIndices:rowPermuteOffset:columnPermuteIndices:columnPermuteOffset:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyRows@
copyRowsSelector :: Selector
copyRowsSelector = mkSelector "copyRows"

-- | @Selector@ for @copyColumns@
copyColumnsSelector :: Selector
copyColumnsSelector = mkSelector "copyColumns"

-- | @Selector@ for @sourcesAreTransposed@
sourcesAreTransposedSelector :: Selector
sourcesAreTransposedSelector = mkSelector "sourcesAreTransposed"

-- | @Selector@ for @destinationsAreTransposed@
destinationsAreTransposedSelector :: Selector
destinationsAreTransposedSelector = mkSelector "destinationsAreTransposed"

