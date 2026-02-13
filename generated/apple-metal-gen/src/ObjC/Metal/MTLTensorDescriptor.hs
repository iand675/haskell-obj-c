{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cpuCacheModeSelector
  , dataTypeSelector
  , dimensionsSelector
  , hazardTrackingModeSelector
  , resourceOptionsSelector
  , setCpuCacheModeSelector
  , setDataTypeSelector
  , setDimensionsSelector
  , setHazardTrackingModeSelector
  , setResourceOptionsSelector
  , setStorageModeSelector
  , setStridesSelector
  , setUsageSelector
  , storageModeSelector
  , stridesSelector
  , usageSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
dimensions mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor dimensionsSelector

-- | An array of sizes, in elements, one for each dimension of the tensors you create with this descriptor.
--
-- The default value of this property is a rank one extents with size one.
--
-- ObjC selector: @- setDimensions:@
setDimensions :: (IsMTLTensorDescriptor mtlTensorDescriptor, IsMTLTensorExtents value) => mtlTensorDescriptor -> value -> IO ()
setDimensions mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setDimensionsSelector (toMTLTensorExtents value)

-- | An array of strides, in elements, one for each dimension in the tensors you create with this descriptor, if applicable.
--
-- This property only applies to tensors you create from a buffer, otherwise it is nil. You are responsible for ensuring @strides@ meets the following requirements: - Elements of @strides@are in monotonically non-decreasing order. - The first element of @strides@ is one. - For any @i@ larger than zero, @strides[i]@ is greater than or equal to @strides[i-1] * dimensions[i-1]@. - If @usage@ contains ``MTLTensorUsage/MTLTensorUsageMachineLearning``, the second element of @strides@ is aligned to 64 bytes, and for any @i@ larger than one, @strides[i]@ is equal to @strides[i-1] * dimensions[i-1]@.
--
-- ObjC selector: @- strides@
strides :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO (Id MTLTensorExtents)
strides mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor stridesSelector

-- | An array of strides, in elements, one for each dimension in the tensors you create with this descriptor, if applicable.
--
-- This property only applies to tensors you create from a buffer, otherwise it is nil. You are responsible for ensuring @strides@ meets the following requirements: - Elements of @strides@are in monotonically non-decreasing order. - The first element of @strides@ is one. - For any @i@ larger than zero, @strides[i]@ is greater than or equal to @strides[i-1] * dimensions[i-1]@. - If @usage@ contains ``MTLTensorUsage/MTLTensorUsageMachineLearning``, the second element of @strides@ is aligned to 64 bytes, and for any @i@ larger than one, @strides[i]@ is equal to @strides[i-1] * dimensions[i-1]@.
--
-- ObjC selector: @- setStrides:@
setStrides :: (IsMTLTensorDescriptor mtlTensorDescriptor, IsMTLTensorExtents value) => mtlTensorDescriptor -> value -> IO ()
setStrides mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setStridesSelector (toMTLTensorExtents value)

-- | A data format for the tensors you create with this descriptor.
--
-- The default value of this property is ``MTLTensorDataType/MTLTensorDataTypeFloat32``.
--
-- ObjC selector: @- dataType@
dataType :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLTensorDataType
dataType mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor dataTypeSelector

-- | A data format for the tensors you create with this descriptor.
--
-- The default value of this property is ``MTLTensorDataType/MTLTensorDataTypeFloat32``.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLTensorDataType -> IO ()
setDataType mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setDataTypeSelector value

-- | A set of contexts in which you can use tensors you create with this descriptor.
--
-- The default value for this property is a bitwise @OR@ of: - ``MTLTensorUsage/MTLTensorUsageRender`` - ``MTLTensorUsage/MTLTensorUsageCompute``
--
-- ObjC selector: @- usage@
usage :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLTensorUsage
usage mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor usageSelector

-- | A set of contexts in which you can use tensors you create with this descriptor.
--
-- The default value for this property is a bitwise @OR@ of: - ``MTLTensorUsage/MTLTensorUsageRender`` - ``MTLTensorUsage/MTLTensorUsageCompute``
--
-- ObjC selector: @- setUsage:@
setUsage :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLTensorUsage -> IO ()
setUsage mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setUsageSelector value

-- | A packed set of the @storageMode@, @cpuCacheMode@ and @hazardTrackingMode@ properties.
--
-- ObjC selector: @- resourceOptions@
resourceOptions :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLResourceOptions
resourceOptions mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor resourceOptionsSelector

-- | A packed set of the @storageMode@, @cpuCacheMode@ and @hazardTrackingMode@ properties.
--
-- ObjC selector: @- setResourceOptions:@
setResourceOptions :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLResourceOptions -> IO ()
setResourceOptions mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setResourceOptionsSelector value

-- | A value that configures the cache mode of CPU mapping of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLCPUCacheMode/MTLCPUCacheModeDefaultCache``.
--
-- ObjC selector: @- cpuCacheMode@
cpuCacheMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLCPUCacheMode
cpuCacheMode mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor cpuCacheModeSelector

-- | A value that configures the cache mode of CPU mapping of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLCPUCacheMode/MTLCPUCacheModeDefaultCache``.
--
-- ObjC selector: @- setCpuCacheMode:@
setCpuCacheMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLCPUCacheMode -> IO ()
setCpuCacheMode mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setCpuCacheModeSelector value

-- | A value that configures the memory location and access permissions of tensors you create with this descriptor.
--
-- The default value of this property defaults to ``MTLStorageMode/MTLStorageModeShared``.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLStorageMode
storageMode mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor storageModeSelector

-- | A value that configures the memory location and access permissions of tensors you create with this descriptor.
--
-- The default value of this property defaults to ``MTLStorageMode/MTLStorageModeShared``.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setStorageModeSelector value

-- | A value that configures the hazard tracking of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLHazardTrackingMode/MTLHazardTrackingModeDefault``.
--
-- ObjC selector: @- hazardTrackingMode@
hazardTrackingMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> IO MTLHazardTrackingMode
hazardTrackingMode mtlTensorDescriptor =
  sendMessage mtlTensorDescriptor hazardTrackingModeSelector

-- | A value that configures the hazard tracking of tensors you create with this descriptor.
--
-- The default value of this property is ``MTLHazardTrackingMode/MTLHazardTrackingModeDefault``.
--
-- ObjC selector: @- setHazardTrackingMode:@
setHazardTrackingMode :: IsMTLTensorDescriptor mtlTensorDescriptor => mtlTensorDescriptor -> MTLHazardTrackingMode -> IO ()
setHazardTrackingMode mtlTensorDescriptor value =
  sendMessage mtlTensorDescriptor setHazardTrackingModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dimensions@
dimensionsSelector :: Selector '[] (Id MTLTensorExtents)
dimensionsSelector = mkSelector "dimensions"

-- | @Selector@ for @setDimensions:@
setDimensionsSelector :: Selector '[Id MTLTensorExtents] ()
setDimensionsSelector = mkSelector "setDimensions:"

-- | @Selector@ for @strides@
stridesSelector :: Selector '[] (Id MTLTensorExtents)
stridesSelector = mkSelector "strides"

-- | @Selector@ for @setStrides:@
setStridesSelector :: Selector '[Id MTLTensorExtents] ()
setStridesSelector = mkSelector "setStrides:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MTLTensorDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MTLTensorDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @usage@
usageSelector :: Selector '[] MTLTensorUsage
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector '[MTLTensorUsage] ()
setUsageSelector = mkSelector "setUsage:"

-- | @Selector@ for @resourceOptions@
resourceOptionsSelector :: Selector '[] MTLResourceOptions
resourceOptionsSelector = mkSelector "resourceOptions"

-- | @Selector@ for @setResourceOptions:@
setResourceOptionsSelector :: Selector '[MTLResourceOptions] ()
setResourceOptionsSelector = mkSelector "setResourceOptions:"

-- | @Selector@ for @cpuCacheMode@
cpuCacheModeSelector :: Selector '[] MTLCPUCacheMode
cpuCacheModeSelector = mkSelector "cpuCacheMode"

-- | @Selector@ for @setCpuCacheMode:@
setCpuCacheModeSelector :: Selector '[MTLCPUCacheMode] ()
setCpuCacheModeSelector = mkSelector "setCpuCacheMode:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector '[] MTLStorageMode
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector '[MTLStorageMode] ()
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @hazardTrackingMode@
hazardTrackingModeSelector :: Selector '[] MTLHazardTrackingMode
hazardTrackingModeSelector = mkSelector "hazardTrackingMode"

-- | @Selector@ for @setHazardTrackingMode:@
setHazardTrackingModeSelector :: Selector '[MTLHazardTrackingMode] ()
setHazardTrackingModeSelector = mkSelector "setHazardTrackingMode:"

