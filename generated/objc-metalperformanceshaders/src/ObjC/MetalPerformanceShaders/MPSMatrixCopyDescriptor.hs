{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A list of copy operations
--
-- The MPSMatrixCopy filter can do multiple copy operations.  For RNN filters, these              copies are often small, and are more efficient when grouped together.              The MPSMatriceCopyDescriptor provides a container to list the operations.              The operations occur in any order, and may not alias.
--
-- Generated bindings for @MPSMatrixCopyDescriptor@.
module ObjC.MetalPerformanceShaders.MPSMatrixCopyDescriptor
  ( MPSMatrixCopyDescriptor
  , IsMPSMatrixCopyDescriptor(..)
  , initWithDevice_count
  , initWithSourceMatrices_destinationMatrices_offsetVector_offset
  , init_
  , initWithDevice_countSelector
  , initWithSourceMatrices_destinationMatrices_offsetVector_offsetSelector
  , initSelector


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

-- | initialize a MPSMatrixCopyDescriptor with default values.
--
-- Use -setCopyOperationAtIndex:sourceMatrix:destinationMatrix:copyOffsets                  to initialize. All indices must be initialized before use.
--
-- @device@ — The device on which the copy will be performed
--
-- @count@ — The number of copy operations the object will encode
--
-- Returns: A MPSMatrixCopyDescriptor. It still needs to be initialized with              -setCopyOperationAtIndex:sourceMatrix:destinationMatrix:copyOffsets
--
-- ObjC selector: @- initWithDevice:count:@
initWithDevice_count :: IsMPSMatrixCopyDescriptor mpsMatrixCopyDescriptor => mpsMatrixCopyDescriptor -> RawId -> CULong -> IO (Id MPSMatrixCopyDescriptor)
initWithDevice_count mpsMatrixCopyDescriptor  device count =
  sendMsg mpsMatrixCopyDescriptor (mkSelector "initWithDevice:count:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count)] >>= ownedObject . castPtr

-- | Initialize a MPSMatrixCopyDescriptor using offsets generated on the GPU
--
-- Use this method when the offsets needed are coming from GPU based computation.
--
-- @sourceMatrices@ — A list of matrices from which the matrix data is read
--
-- @destinationMatrices@ — A list of matrices to which to write the data. The count                                      must match the number of source matrices.
--
-- @offsets@ — A MPSVector of type MPSDataTypeUInt32 containing the list of                                  offsets, stored as a packed array of MPSMatrixCopyOffsets.
--
-- @byteOffset@ — A byte offset into the offsets vector where the data starts in 'offsets'.                                  This value must be a multiple of 16.
--
-- Returns: A valid MPSMatrixCopyDescriptor to represent the list of copy operations
--
-- ObjC selector: @- initWithSourceMatrices:destinationMatrices:offsetVector:offset:@
initWithSourceMatrices_destinationMatrices_offsetVector_offset :: (IsMPSMatrixCopyDescriptor mpsMatrixCopyDescriptor, IsNSArray sourceMatrices, IsNSArray destinationMatrices, IsMPSVector offsets) => mpsMatrixCopyDescriptor -> sourceMatrices -> destinationMatrices -> offsets -> CULong -> IO (Id MPSMatrixCopyDescriptor)
initWithSourceMatrices_destinationMatrices_offsetVector_offset mpsMatrixCopyDescriptor  sourceMatrices destinationMatrices offsets byteOffset =
withObjCPtr sourceMatrices $ \raw_sourceMatrices ->
  withObjCPtr destinationMatrices $ \raw_destinationMatrices ->
    withObjCPtr offsets $ \raw_offsets ->
        sendMsg mpsMatrixCopyDescriptor (mkSelector "initWithSourceMatrices:destinationMatrices:offsetVector:offset:") (retPtr retVoid) [argPtr (castPtr raw_sourceMatrices :: Ptr ()), argPtr (castPtr raw_destinationMatrices :: Ptr ()), argPtr (castPtr raw_offsets :: Ptr ()), argCULong (fromIntegral byteOffset)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSMatrixCopyDescriptor mpsMatrixCopyDescriptor => mpsMatrixCopyDescriptor -> IO (Id MPSMatrixCopyDescriptor)
init_ mpsMatrixCopyDescriptor  =
  sendMsg mpsMatrixCopyDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:count:@
initWithDevice_countSelector :: Selector
initWithDevice_countSelector = mkSelector "initWithDevice:count:"

-- | @Selector@ for @initWithSourceMatrices:destinationMatrices:offsetVector:offset:@
initWithSourceMatrices_destinationMatrices_offsetVector_offsetSelector :: Selector
initWithSourceMatrices_destinationMatrices_offsetVector_offsetSelector = mkSelector "initWithSourceMatrices:destinationMatrices:offsetVector:offset:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

