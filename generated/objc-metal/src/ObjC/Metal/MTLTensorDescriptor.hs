{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A configuration type for creating new tensor instances.
--
-- Generated bindings for @MTLTensorDescriptor@.
module ObjC.Metal.MTLTensorDescriptor
  ( MTLTensorDescriptor
  , IsMTLTensorDescriptor(..)
  , dimensions
  , setDimensions
  , strides
  , setStrides
  , dataType
  , setDataType
  , usage
  , setUsage
  , resourceOptions
  , setResourceOptions
  , cpuCacheMode
  , setCpuCacheMode
  , storageMode
  , setStorageMode
  , hazardTrackingMode
  , setHazardTrackingMode
  , dimensionsSelector
  , setDimensionsSelector
  , stridesSelector
  , setStridesSelector
  , dataTypeSelector
  , setDataTypeSelector
  , usageSelector
  , setUsageSelector
  , resourceOptionsSelector
  , setResourceOptionsSelector
  , cpuCacheModeSelector
  , setCpuCacheModeSelector
  , storageModeSelector
  , setStorageModeSelector
  , hazardTrackingModeSelector
  , setHazardTrackingModeSelector

  -- * Enum types
  , MTLCPUCacheMode(MTLCPUCacheMode)
  , pattern MTLCPUCacheModeDefaultCache
  , pattern MTLCPUCacheModeWriteCombined
  , MTLHazardTrackingMode(MTLHazardTrackingMode)
  , pattern MTLHazardTrackingModeDefault
  , pattern MTLHazardTrackingModeUntracked
  , pattern MTLHazardTrackingModeTracked
  , MTLResourceOptions(MTLResourceOptions)
  , pattern MTLResourceCPUCacheModeDefaultCache
  , pattern MTLResourceCPUCacheModeWriteCombined
  , pattern MTLResourceStorageModeShared
  , pattern MTLResourceStorageModeManaged
  , pattern MTLResourceStorageModePrivate
  , pattern MTLResourceStorageModeMemoryless
  , pattern MTLResourceHazardTrackingModeDefault
  , pattern MTLResourceHazardTrackingModeUntracked
  , pattern MTLResourceHazardTrackingModeTracked
  , pattern MTLResourceOptionCPUCacheModeDefault
  , pattern MTLResourceOptionCPUCacheModeWriteCombined
  , MTLStorageMode(MTLStorageMode)
  , pattern MTLStorageModeShared
  , pattern MTLStorageModeManaged
  , pattern MTLStorageModePrivate
  , pattern MTLStorageModeMemoryless
  , MTLTensorDataType(MTLTensorDataType)
  , pattern MTLTensorDataTypeNone
  , pattern MTLTensorDataTypeFloat32
  , pattern MTLTensorDataTypeFloat16
  , pattern MTLTensorDataTypeBFloat16
  , pattern MTLTensorDataTypeInt8
  , pattern MTLTensorDataTypeUInt8
  , pattern MTLTensorDataTypeInt16
  , pattern MTLTensorDataTypeUInt16
  , pattern MTLTensorDataTypeInt32
  , pattern MTLTensorDataTypeUInt32
  , MTLTensorUsage(MTLTensorUsage)
  , pattern MTLTensorUsageCompute
  , pattern MTLTensorUsageRender
  , pattern MTLTensorUsageMachineLearning

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | An array of sizes, in elements, one for each dimension of the tensors you create with this descriptor.
--
-- The default value of this property is a rank one extents with size one.
--
-- ObjC selector: @- dimensions@
dimensions :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO (Id MTLTensorExtents)
dimensions mtlTensorDescriptor  =
  sendMsg mtlTensorDescriptor (mkSelector "dimensions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of sizes, in elements, one for each dimension of the tensors you create with this descriptor.
--
-- The default value of this property is a rank one extents with size one.
--
-- ObjC selector: @- setDimensions:@
setDimensions :: (IsMTLTensorDescriptor mtlTensorDescriptor, IsMTLTensorExtents value) => mtlTensorDescriptor -> value -> IO ()
setDimensions mtlTensorDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlTensorDescriptor (mkSelector "setDimensions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An array of strides, in elements, one for each dimension in the tensors you create with this descriptor, if applicable.
--
-- This property only applies to tensors you create from a buffer, otherwise it is nil. You are responsible for ensuring @strides@ meets the following requirements: - Elements of @strides@are in monotonically non-decreasing order. - The first element of @strides@ is one. - For any @i@ larger than zero, @strides[i]@ is greater than or equal to @strides[i-1] * dimensions[i-1]@. - If @usage@ contains ``MTLTensorUsage/MTLTensorUsageMachineLearning``, the second element of @strides@ is aligned to 64 bytes, and for any @i@ larger than one, @strides[i]@ is equal to @strides[i-1] * dimensions[i-1]@.
--
-- ObjC selector: @- strides@
strides :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO (Id MTLTensorExtents)
strides mtlTensorDescriptor  =
  sendMsg mtlTensorDescriptor (mkSelector "strides") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of strides, in elements, one for each dimension in the tensors you create with this descriptor, if applicable.
--
-- This property only applies to tensors you create from a buffer, otherwise it is nil. You are responsible for ensuring @strides@ meets the following requirements: - Elements of @strides@are in monotonically non-decreasing order. - The first element of @strides@ is one. - For any @i@ larger than zero, @strides[i]@ is greater than or equal to @strides[i-1] * dimensions[i-1]@. - If @usage@ contains ``MTLTensorUsage/MTLTensorUsageMachineLearning``, the second element of @strides@ is aligned to 64 bytes, and for any @i@ larger than one, @strides[i]@ is equal to @strides[i-1] * dimensions[i-1]@.
--
-- ObjC selector: @- setStrides:@
setStrides :: (IsMTLTensorDescriptor mtlTensorDescriptor, IsMTLTensorExtents value) => mtlTensorDescriptor -> value -> IO ()
setStrides mtlTensorDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlTensorDescriptor (mkSelector "setStrides:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A data format for the tensors you create with this descriptor.
--
-- The default value of this property is ``MTLTensorDataType/MTLTensorDataTypeFloat32``.
--
-- ObjC selector: @- dataType@
dataType :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLTensorDataType
dataType mtlTensorDescriptor  =
  fmap (coerce :: CLong -> MTLTensorDataType) $ sendMsg mtlTensorDescriptor (mkSelector "dataType") retCLong []

-- | A data format for the tensors you create with this descriptor.
--
-- The default value of this property is ``MTLTensorDataType/MTLTensorDataTypeFloat32``.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLTensorDataType -> IO ()
setDataType mtlTensorDescriptor  value =
  sendMsg mtlTensorDescriptor (mkSelector "setDataType:") retVoid [argCLong (coerce value)]

-- | A set of contexts in which you can use tensors you create with this descriptor.
--
-- The default value for this property is a bitwise @OR@ of: - ``MTLTensorUsage/MTLTensorUsageRender`` - ``MTLTensorUsage/MTLTensorUsageCompute``
--
-- ObjC selector: @- usage@
usage :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLTensorUsage
usage mtlTensorDescriptor  =
  fmap (coerce :: CULong -> MTLTensorUsage) $ sendMsg mtlTensorDescriptor (mkSelector "usage") retCULong []

-- | A set of contexts in which you can use tensors you create with this descriptor.
--
-- The default value for this property is a bitwise @OR@ of: - ``MTLTensorUsage/MTLTensorUsageRender`` - ``MTLTensorUsage/MTLTensorUsageCompute``
--
-- ObjC selector: @- setUsage:@
setUsage :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLTensorUsage -> IO ()
setUsage mtlTensorDescriptor  value =
  sendMsg mtlTensorDescriptor (mkSelector "setUsage:") retVoid [argCULong (coerce value)]

-- | A packed set of the @storageMode@, @cpuCacheMode@ and @hazardTrackingMode@ properties.
--
-- ObjC selector: @- resourceOptions@
resourceOptions :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLResourceOptions
resourceOptions mtlTensorDescriptor  =
  fmap (coerce :: CULong -> MTLResourceOptions) $ sendMsg mtlTensorDescriptor (mkSelector "resourceOptions") retCULong []

-- | A packed set of the @storageMode@, @cpuCacheMode@ and @hazardTrackingMode@ properties.
--
-- ObjC selector: @- setResourceOptions:@
setResourceOptions :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLResourceOptions -> IO ()
setResourceOptions mtlTensorDescriptor  value =
  sendMsg mtlTensorDescriptor (mkSelector "setResourceOptions:") retVoid [argCULong (coerce value)]

-- | A value that configures the cache mode of CPU mapping of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLCPUCacheMode/MTLCPUCacheModeDefaultCache``.
--
-- ObjC selector: @- cpuCacheMode@
cpuCacheMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLCPUCacheMode
cpuCacheMode mtlTensorDescriptor  =
  fmap (coerce :: CULong -> MTLCPUCacheMode) $ sendMsg mtlTensorDescriptor (mkSelector "cpuCacheMode") retCULong []

-- | A value that configures the cache mode of CPU mapping of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLCPUCacheMode/MTLCPUCacheModeDefaultCache``.
--
-- ObjC selector: @- setCpuCacheMode:@
setCpuCacheMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLCPUCacheMode -> IO ()
setCpuCacheMode mtlTensorDescriptor  value =
  sendMsg mtlTensorDescriptor (mkSelector "setCpuCacheMode:") retVoid [argCULong (coerce value)]

-- | A value that configures the memory location and access permissions of tensors you create with this descriptor.
--
-- The default value of this property defaults to ``MTLStorageMode/MTLStorageModeShared``.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLStorageMode
storageMode mtlTensorDescriptor  =
  fmap (coerce :: CULong -> MTLStorageMode) $ sendMsg mtlTensorDescriptor (mkSelector "storageMode") retCULong []

-- | A value that configures the memory location and access permissions of tensors you create with this descriptor.
--
-- The default value of this property defaults to ``MTLStorageMode/MTLStorageModeShared``.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlTensorDescriptor  value =
  sendMsg mtlTensorDescriptor (mkSelector "setStorageMode:") retVoid [argCULong (coerce value)]

-- | A value that configures the hazard tracking of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLHazardTrackingMode/MTLHazardTrackingModeDefault``.
--
-- ObjC selector: @- hazardTrackingMode@
hazardTrackingMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLHazardTrackingMode
hazardTrackingMode mtlTensorDescriptor  =
  fmap (coerce :: CULong -> MTLHazardTrackingMode) $ sendMsg mtlTensorDescriptor (mkSelector "hazardTrackingMode") retCULong []

-- | A value that configures the hazard tracking of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLHazardTrackingMode/MTLHazardTrackingModeDefault``.
--
-- ObjC selector: @- setHazardTrackingMode:@
setHazardTrackingMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLHazardTrackingMode -> IO ()
setHazardTrackingMode mtlTensorDescriptor  value =
  sendMsg mtlTensorDescriptor (mkSelector "setHazardTrackingMode:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dimensions@
dimensionsSelector :: Selector
dimensionsSelector = mkSelector "dimensions"

-- | @Selector@ for @setDimensions:@
setDimensionsSelector :: Selector
setDimensionsSelector = mkSelector "setDimensions:"

-- | @Selector@ for @strides@
stridesSelector :: Selector
stridesSelector = mkSelector "strides"

-- | @Selector@ for @setStrides:@
setStridesSelector :: Selector
setStridesSelector = mkSelector "setStrides:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @usage@
usageSelector :: Selector
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector
setUsageSelector = mkSelector "setUsage:"

-- | @Selector@ for @resourceOptions@
resourceOptionsSelector :: Selector
resourceOptionsSelector = mkSelector "resourceOptions"

-- | @Selector@ for @setResourceOptions:@
setResourceOptionsSelector :: Selector
setResourceOptionsSelector = mkSelector "setResourceOptions:"

-- | @Selector@ for @cpuCacheMode@
cpuCacheModeSelector :: Selector
cpuCacheModeSelector = mkSelector "cpuCacheMode"

-- | @Selector@ for @setCpuCacheMode:@
setCpuCacheModeSelector :: Selector
setCpuCacheModeSelector = mkSelector "setCpuCacheMode:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @hazardTrackingMode@
hazardTrackingModeSelector :: Selector
hazardTrackingModeSelector = mkSelector "hazardTrackingMode"

-- | @Selector@ for @setHazardTrackingMode:@
setHazardTrackingModeSelector :: Selector
setHazardTrackingModeSelector = mkSelector "setHazardTrackingMode:"

