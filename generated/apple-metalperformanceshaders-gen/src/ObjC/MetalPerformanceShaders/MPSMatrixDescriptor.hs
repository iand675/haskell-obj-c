{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , columnsSelector
  , dataTypeSelector
  , matricesSelector
  , matrixBytesSelector
  , matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector
  , matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector
  , matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector
  , rowBytesForColumns_dataTypeSelector
  , rowBytesFromColumns_dataTypeSelector
  , rowBytesSelector
  , rowsSelector
  , setColumnsSelector
  , setDataTypeSelector
  , setRowBytesSelector
  , setRowsSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector rows columns rowBytes dataType

-- | @+ matrixDescriptorWithRows:columns:rowBytes:dataType:@
matrixDescriptorWithRows_columns_rowBytes_dataType :: CULong -> CULong -> CULong -> MPSDataType -> IO (Id MPSMatrixDescriptor)
matrixDescriptorWithRows_columns_rowBytes_dataType rows columns rowBytes dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMessage cls' matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector rows columns rowBytes dataType

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
    sendClassMessage cls' matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector rows columns matrices rowBytes matrixBytes dataType

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
    sendClassMessage cls' rowBytesFromColumns_dataTypeSelector columns dataType

-- | @+ rowBytesForColumns:dataType:@
rowBytesForColumns_dataType :: CULong -> MPSDataType -> IO CULong
rowBytesForColumns_dataType columns dataType =
  do
    cls' <- getRequiredClass "MPSMatrixDescriptor"
    sendClassMessage cls' rowBytesForColumns_dataTypeSelector columns dataType

-- | rows
--
-- The number of rows in a matrix.
--
-- ObjC selector: @- rows@
rows :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
rows mpsMatrixDescriptor =
  sendMessage mpsMatrixDescriptor rowsSelector

-- | rows
--
-- The number of rows in a matrix.
--
-- ObjC selector: @- setRows:@
setRows :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> CULong -> IO ()
setRows mpsMatrixDescriptor value =
  sendMessage mpsMatrixDescriptor setRowsSelector value

-- | columns
--
-- The number of columns in a matrix.
--
-- ObjC selector: @- columns@
columns :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
columns mpsMatrixDescriptor =
  sendMessage mpsMatrixDescriptor columnsSelector

-- | columns
--
-- The number of columns in a matrix.
--
-- ObjC selector: @- setColumns:@
setColumns :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> CULong -> IO ()
setColumns mpsMatrixDescriptor value =
  sendMessage mpsMatrixDescriptor setColumnsSelector value

-- | matrices
--
-- The number of matrices.
--
-- ObjC selector: @- matrices@
matrices :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
matrices mpsMatrixDescriptor =
  sendMessage mpsMatrixDescriptor matricesSelector

-- | dataType
--
-- The type of the data which makes up the values of the matrix.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO MPSDataType
dataType mpsMatrixDescriptor =
  sendMessage mpsMatrixDescriptor dataTypeSelector

-- | dataType
--
-- The type of the data which makes up the values of the matrix.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> MPSDataType -> IO ()
setDataType mpsMatrixDescriptor value =
  sendMessage mpsMatrixDescriptor setDataTypeSelector value

-- | rowBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive rows.  Must be a multiple of the element size.
--
-- ObjC selector: @- rowBytes@
rowBytes :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
rowBytes mpsMatrixDescriptor =
  sendMessage mpsMatrixDescriptor rowBytesSelector

-- | rowBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive rows.  Must be a multiple of the element size.
--
-- ObjC selector: @- setRowBytes:@
setRowBytes :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> CULong -> IO ()
setRowBytes mpsMatrixDescriptor value =
  sendMessage mpsMatrixDescriptor setRowBytesSelector value

-- | matrixBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive matrices.  Must be a multiple of rowBytes.
--
-- ObjC selector: @- matrixBytes@
matrixBytes :: IsMPSMatrixDescriptor mpsMatrixDescriptor => mpsMatrixDescriptor -> IO CULong
matrixBytes mpsMatrixDescriptor =
  sendMessage mpsMatrixDescriptor matrixBytesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @matrixDescriptorWithDimensions:columns:rowBytes:dataType:@
matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector :: Selector '[CULong, CULong, CULong, MPSDataType] (Id MPSMatrixDescriptor)
matrixDescriptorWithDimensions_columns_rowBytes_dataTypeSelector = mkSelector "matrixDescriptorWithDimensions:columns:rowBytes:dataType:"

-- | @Selector@ for @matrixDescriptorWithRows:columns:rowBytes:dataType:@
matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector :: Selector '[CULong, CULong, CULong, MPSDataType] (Id MPSMatrixDescriptor)
matrixDescriptorWithRows_columns_rowBytes_dataTypeSelector = mkSelector "matrixDescriptorWithRows:columns:rowBytes:dataType:"

-- | @Selector@ for @matrixDescriptorWithRows:columns:matrices:rowBytes:matrixBytes:dataType:@
matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, MPSDataType] (Id MPSMatrixDescriptor)
matrixDescriptorWithRows_columns_matrices_rowBytes_matrixBytes_dataTypeSelector = mkSelector "matrixDescriptorWithRows:columns:matrices:rowBytes:matrixBytes:dataType:"

-- | @Selector@ for @rowBytesFromColumns:dataType:@
rowBytesFromColumns_dataTypeSelector :: Selector '[CULong, MPSDataType] CULong
rowBytesFromColumns_dataTypeSelector = mkSelector "rowBytesFromColumns:dataType:"

-- | @Selector@ for @rowBytesForColumns:dataType:@
rowBytesForColumns_dataTypeSelector :: Selector '[CULong, MPSDataType] CULong
rowBytesForColumns_dataTypeSelector = mkSelector "rowBytesForColumns:dataType:"

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] CULong
rowsSelector = mkSelector "rows"

-- | @Selector@ for @setRows:@
setRowsSelector :: Selector '[CULong] ()
setRowsSelector = mkSelector "setRows:"

-- | @Selector@ for @columns@
columnsSelector :: Selector '[] CULong
columnsSelector = mkSelector "columns"

-- | @Selector@ for @setColumns:@
setColumnsSelector :: Selector '[CULong] ()
setColumnsSelector = mkSelector "setColumns:"

-- | @Selector@ for @matrices@
matricesSelector :: Selector '[] CULong
matricesSelector = mkSelector "matrices"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MPSDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @rowBytes@
rowBytesSelector :: Selector '[] CULong
rowBytesSelector = mkSelector "rowBytes"

-- | @Selector@ for @setRowBytes:@
setRowBytesSelector :: Selector '[CULong] ()
setRowBytesSelector = mkSelector "setRowBytes:"

-- | @Selector@ for @matrixBytes@
matrixBytesSelector :: Selector '[] CULong
matrixBytesSelector = mkSelector "matrixBytes"

