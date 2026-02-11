{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixDescriptor
--
-- This depends on Metal.framework
--
-- A MPSMatrixDescriptor describes the sizes, strides, and data type of a              an array of 2-dimensional matrices.  All storage is assumed to be in              "matrix-major".  See the description for MPSMatrix for further details.
--
-- Generated bindings for @MPSMatrixDescriptor@.
module ObjC.MetalPerformanceShaders.MPSMatrixDescriptor
  ( MPSMatrixDescriptor
  , IsMPSMatrixDescriptor(..)
  , matrixDescriptorWithDimensions_columns_rowBytes_dataType
  , matrixDescriptorWithRows_columns_rowBytes_dataType
  , matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataType
  , rowBytesFromColumns_dataType
  , rowBytesForColumns_dataType
  , rows
  , setRows
  , columns
  , setColumns
  , matrices
  , dataType
  , setDataType
  , rowBytes
  , setRowBytes
  , matrixBytes
  , matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector
  , matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector
  , matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector
  , rowBytesFromColumns_dataTypeSelector
  , rowBytesForColumns_dataTypeSelector
  , rowsSelector
  , setRowsSelector
  , columnsSelector
  , setColumnsSelector
  , matricesSelector
  , dataTypeSelector
  , setDataTypeSelector
  , rowBytesSelector
  , setRowBytesSelector
  , matrixBytesSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a MPSMatrixDescriptor with the specified dimensions and data type.
--
-- @rows@ — The number of rows of the matrix.
--
-- @columns@ — The number of columns of the matrix.
--
-- @rowBytes@ — The number of bytes between starting elements of consecutive                                  rows.  Must be a multiple of the element size.
--
-- @dataType@ — The type of the data to be stored in the matrix.
--
-- For performance considerations the optimal row stride may not necessarily be equal              to the number of columns in the matrix.  The MPSMatrix class provides a method which              may be used to determine this value, see the rowBytesForColumns API in the MPSMatrix              class.              The number of matrices described is initialized to 1.
--
-- ObjC selector: @+ matrixDescriptorWithDimensions:columns:rowBytes:dataType:@
matrixDescriptorWithDimensions_columns_rowBytes_dataType :: CULong -> CULong -> CULong -> MPSDataType -> IO (Id MPSMatrixDescriptor)
matrixDescriptorWithDimensions_columns_rowBytes_dataType rows columns rowBytes dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMsg cls' (mkSelector "matrixDescriptorWithDimensions:columns:rowBytes:dataType:") (retPtr retVoid) [argCULong (fromIntegral rows), argCULong (fromIntegral columns), argCULong (fromIntegral rowBytes), argCUInt (coerce dataType)] >>= retainedObject . castPtr

-- | @+ matrixDescriptorWithRows:columns:rowBytes:dataType:@
matrixDescriptorWithRows_columns_rowBytes_dataType :: CULong -> CULong -> CULong -> MPSDataType -> IO (Id MPSMatrixDescriptor)
matrixDescriptorWithRows_columns_rowBytes_dataType rows columns rowBytes dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMsg cls' (mkSelector "matrixDescriptorWithRows:columns:rowBytes:dataType:") (retPtr retVoid) [argCULong (fromIntegral rows), argCULong (fromIntegral columns), argCULong (fromIntegral rowBytes), argCUInt (coerce dataType)] >>= retainedObject . castPtr

-- | Create a MPSMatrixDescriptor with the specified dimensions and data type.
--
-- @rows@ — The number of rows of a single matrix.
--
-- @columns@ — The number of columns of a single matrix.
--
-- @matrices@ — The number of matrices in the MPSMatrix object.
--
-- @rowBytes@ — The number of bytes between starting elements of consecutive                                  rows.  Must be a multiple of the element size.
--
-- @matrixBytes@ — The number of bytes between starting elements of consecutive                                  matrices.  Must be a multiple of rowBytes.
--
-- @dataType@ — The type of the data to be stored in the matrix.
--
-- For performance considerations the optimal row stride may not necessarily be equal              to the number of columns in the matrix.  The MPSMatrix class provides a method which              may be used to determine this value, see the rowBytesForColumns API in the MPSMatrix              class.
--
-- ObjC selector: @+ matrixDescriptorWithRows:columns:matrices:rowBytes:matrixBytes:dataType:@
matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataType :: CULong -> CULong -> CULong -> CULong -> CULong -> MPSDataType -> IO (Id MPSMatrixDescriptor)
matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataType rows columns matrices rowBytes matrixBytes dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMsg cls' (mkSelector "matrixDescriptorWithRows:columns:matrices:rowBytes:matrixBytes:dataType:") (retPtr retVoid) [argCULong (fromIntegral rows), argCULong (fromIntegral columns), argCULong (fromIntegral matrices), argCULong (fromIntegral rowBytes), argCULong (fromIntegral matrixBytes), argCUInt (coerce dataType)] >>= retainedObject . castPtr

-- | Return the recommended row stride, in bytes, for a given number of              columns.
--
-- @columns@ — The number of columns in the matrix for which the recommended                              row stride, in bytes, is to be determined.
--
-- @dataType@ — The type of matrix data values.
--
-- To achieve best performance the optimal stride between rows of a matrix is not              necessarily equivalent to the number of columns.  This method returns the row stride, in              bytes, which gives best performance for a given number of columns.  Using this row stride              to construct your array is recommended, but not required (provided that the stride              used is still large enough to allocate a full row of data).
--
-- ObjC selector: @+ rowBytesFromColumns:dataType:@
rowBytesFromColumns_dataType :: CULong -> MPSDataType -> IO CULong
rowBytesFromColumns_dataType columns dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMsg cls' (mkSelector "rowBytesFromColumns:dataType:") retCULong [argCULong (fromIntegral columns), argCUInt (coerce dataType)]

-- | @+ rowBytesForColumns:dataType:@
rowBytesForColumns_dataType :: CULong -> MPSDataType -> IO CULong
rowBytesForColumns_dataType columns dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMsg cls' (mkSelector "rowBytesForColumns:dataType:") retCULong [argCULong (fromIntegral columns), argCUInt (coerce dataType)]

-- | rows
--
-- The number of rows in a matrix.
--
-- ObjC selector: @- rows@
rows :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
rows mpsMatrixDescriptor  =
  sendMsg mpsMatrixDescriptor (mkSelector "rows") retCULong []

-- | rows
--
-- The number of rows in a matrix.
--
-- ObjC selector: @- setRows:@
setRows :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> CULong -> IO ()
setRows mpsMatrixDescriptor  value =
  sendMsg mpsMatrixDescriptor (mkSelector "setRows:") retVoid [argCULong (fromIntegral value)]

-- | columns
--
-- The number of columns in a matrix.
--
-- ObjC selector: @- columns@
columns :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
columns mpsMatrixDescriptor  =
  sendMsg mpsMatrixDescriptor (mkSelector "columns") retCULong []

-- | columns
--
-- The number of columns in a matrix.
--
-- ObjC selector: @- setColumns:@
setColumns :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> CULong -> IO ()
setColumns mpsMatrixDescriptor  value =
  sendMsg mpsMatrixDescriptor (mkSelector "setColumns:") retVoid [argCULong (fromIntegral value)]

-- | matrices
--
-- The number of matrices.
--
-- ObjC selector: @- matrices@
matrices :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
matrices mpsMatrixDescriptor  =
  sendMsg mpsMatrixDescriptor (mkSelector "matrices") retCULong []

-- | dataType
--
-- The type of the data which makes up the values of the matrix.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO MPSDataType
dataType mpsMatrixDescriptor  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsMatrixDescriptor (mkSelector "dataType") retCUInt []

-- | dataType
--
-- The type of the data which makes up the values of the matrix.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> MPSDataType -> IO ()
setDataType mpsMatrixDescriptor  value =
  sendMsg mpsMatrixDescriptor (mkSelector "setDataType:") retVoid [argCUInt (coerce value)]

-- | rowBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive rows.  Must be a multiple of the element size.
--
-- ObjC selector: @- rowBytes@
rowBytes :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
rowBytes mpsMatrixDescriptor  =
  sendMsg mpsMatrixDescriptor (mkSelector "rowBytes") retCULong []

-- | rowBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive rows.  Must be a multiple of the element size.
--
-- ObjC selector: @- setRowBytes:@
setRowBytes :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> CULong -> IO ()
setRowBytes mpsMatrixDescriptor  value =
  sendMsg mpsMatrixDescriptor (mkSelector "setRowBytes:") retVoid [argCULong (fromIntegral value)]

-- | matrixBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive matrices.  Must be a multiple of rowBytes.
--
-- ObjC selector: @- matrixBytes@
matrixBytes :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
matrixBytes mpsMatrixDescriptor  =
  sendMsg mpsMatrixDescriptor (mkSelector "matrixBytes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @matrixDescriptorWithDimensions:columns:rowBytes:dataType:@
matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector :: Selector
matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector = mkSelector "matrixDescriptorWithDimensions:columns:rowBytes:dataType:"

-- | @Selector@ for @matrixDescriptorWithRows:columns:rowBytes:dataType:@
matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector :: Selector
matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector = mkSelector "matrixDescriptorWithRows:columns:rowBytes:dataType:"

-- | @Selector@ for @matrixDescriptorWithRows:columns:matrices:rowBytes:matrixBytes:dataType:@
matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector :: Selector
matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector = mkSelector "matrixDescriptorWithRows:columns:matrices:rowBytes:matrixBytes:dataType:"

-- | @Selector@ for @rowBytesFromColumns:dataType:@
rowBytesFromColumns_dataTypeSelector :: Selector
rowBytesFromColumns_dataTypeSelector = mkSelector "rowBytesFromColumns:dataType:"

-- | @Selector@ for @rowBytesForColumns:dataType:@
rowBytesForColumns_dataTypeSelector :: Selector
rowBytesForColumns_dataTypeSelector = mkSelector "rowBytesForColumns:dataType:"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

-- | @Selector@ for @setRows:@
setRowsSelector :: Selector
setRowsSelector = mkSelector "setRows:"

-- | @Selector@ for @columns@
columnsSelector :: Selector
columnsSelector = mkSelector "columns"

-- | @Selector@ for @setColumns:@
setColumnsSelector :: Selector
setColumnsSelector = mkSelector "setColumns:"

-- | @Selector@ for @matrices@
matricesSelector :: Selector
matricesSelector = mkSelector "matrices"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @rowBytes@
rowBytesSelector :: Selector
rowBytesSelector = mkSelector "rowBytes"

-- | @Selector@ for @setRowBytes:@
setRowBytesSelector :: Selector
setRowBytesSelector = mkSelector "setRowBytes:"

-- | @Selector@ for @matrixBytes@
matrixBytesSelector :: Selector
matrixBytesSelector = mkSelector "matrixBytes"

