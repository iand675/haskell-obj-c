{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLHeapDescriptor
--
-- Generated bindings for @MTLHeapDescriptor@.
module ObjC.Metal.MTLHeapDescriptor
  ( MTLHeapDescriptor
  , IsMTLHeapDescriptor(..)
  , size
  , setSize
  , storageMode
  , setStorageMode
  , cpuCacheMode
  , setCpuCacheMode
  , sparsePageSize
  , setSparsePageSize
  , hazardTrackingMode
  , setHazardTrackingMode
  , resourceOptions
  , setResourceOptions
  , type_
  , setType
  , maxCompatiblePlacementSparsePageSize
  , setMaxCompatiblePlacementSparsePageSize
  , cpuCacheModeSelector
  , hazardTrackingModeSelector
  , maxCompatiblePlacementSparsePageSizeSelector
  , resourceOptionsSelector
  , setCpuCacheModeSelector
  , setHazardTrackingModeSelector
  , setMaxCompatiblePlacementSparsePageSizeSelector
  , setResourceOptionsSelector
  , setSizeSelector
  , setSparsePageSizeSelector
  , setStorageModeSelector
  , setTypeSelector
  , sizeSelector
  , sparsePageSizeSelector
  , storageModeSelector
  , typeSelector

  -- * Enum types
  , MTLCPUCacheMode(MTLCPUCacheMode)
  , pattern MTLCPUCacheModeDefaultCache
  , pattern MTLCPUCacheModeWriteCombined
  , MTLHazardTrackingMode(MTLHazardTrackingMode)
  , pattern MTLHazardTrackingModeDefault
  , pattern MTLHazardTrackingModeUntracked
  , pattern MTLHazardTrackingModeTracked
  , MTLHeapType(MTLHeapType)
  , pattern MTLHeapTypeAutomatic
  , pattern MTLHeapTypePlacement
  , pattern MTLHeapTypeSparse
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
  , MTLSparsePageSize(MTLSparsePageSize)
  , pattern MTLSparsePageSize16
  , pattern MTLSparsePageSize64
  , pattern MTLSparsePageSize256
  , MTLStorageMode(MTLStorageMode)
  , pattern MTLStorageModeShared
  , pattern MTLStorageModeManaged
  , pattern MTLStorageModePrivate
  , pattern MTLStorageModeMemoryless

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

-- | size
--
-- Requested size of the heap's backing memory.
--
-- The size may be rounded up to GPU page granularity.
--
-- ObjC selector: @- size@
size :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO CULong
size mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor sizeSelector

-- | size
--
-- Requested size of the heap's backing memory.
--
-- The size may be rounded up to GPU page granularity.
--
-- ObjC selector: @- setSize:@
setSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> CULong -> IO ()
setSize mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setSizeSelector value

-- | storageMode
--
-- Storage mode for the heap. Default is MTLStorageModePrivate.
--
-- All resources created from this heap share the same storage mode. MTLStorageModeManaged and MTLStorageModeMemoryless are disallowed.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLStorageMode
storageMode mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor storageModeSelector

-- | storageMode
--
-- Storage mode for the heap. Default is MTLStorageModePrivate.
--
-- All resources created from this heap share the same storage mode. MTLStorageModeManaged and MTLStorageModeMemoryless are disallowed.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setStorageModeSelector value

-- | cpuCacheMode
--
-- CPU cache mode for the heap. Default is MTLCPUCacheModeDefaultCache.
--
-- All resources created from this heap share the same cache mode. CPU cache mode is ignored for MTLStorageModePrivate.
--
-- ObjC selector: @- cpuCacheMode@
cpuCacheMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLCPUCacheMode
cpuCacheMode mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor cpuCacheModeSelector

-- | cpuCacheMode
--
-- CPU cache mode for the heap. Default is MTLCPUCacheModeDefaultCache.
--
-- All resources created from this heap share the same cache mode. CPU cache mode is ignored for MTLStorageModePrivate.
--
-- ObjC selector: @- setCpuCacheMode:@
setCpuCacheMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLCPUCacheMode -> IO ()
setCpuCacheMode mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setCpuCacheModeSelector value

-- | sparsePageSize
--
-- The sparse page size to use for resources created from the heap.
--
-- ObjC selector: @- sparsePageSize@
sparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLSparsePageSize
sparsePageSize mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor sparsePageSizeSelector

-- | sparsePageSize
--
-- The sparse page size to use for resources created from the heap.
--
-- ObjC selector: @- setSparsePageSize:@
setSparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLSparsePageSize -> IO ()
setSparsePageSize mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setSparsePageSizeSelector value

-- | hazardTrackingMode
--
-- Set hazard tracking mode for the heap. The default value is MTLHazardTrackingModeDefault.
--
-- For heaps, MTLHazardTrackingModeDefault is treated as MTLHazardTrackingModeUntracked. Setting hazardTrackingMode to MTLHazardTrackingModeTracked causes hazard tracking to be enabled heap. When a resource on a hazard tracked heap is modified, reads and writes from all resources suballocated on that heap will be delayed until the modification is complete. Similarly, modifying heap resources will be delayed until all in-flight reads and writes from all resources suballocated on that heap have completed. For optimal performance, perform hazard tracking manually through MTLFence or MTLEvent instead. All resources created from this heap shared the same hazard tracking mode.
--
-- ObjC selector: @- hazardTrackingMode@
hazardTrackingMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLHazardTrackingMode
hazardTrackingMode mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor hazardTrackingModeSelector

-- | hazardTrackingMode
--
-- Set hazard tracking mode for the heap. The default value is MTLHazardTrackingModeDefault.
--
-- For heaps, MTLHazardTrackingModeDefault is treated as MTLHazardTrackingModeUntracked. Setting hazardTrackingMode to MTLHazardTrackingModeTracked causes hazard tracking to be enabled heap. When a resource on a hazard tracked heap is modified, reads and writes from all resources suballocated on that heap will be delayed until the modification is complete. Similarly, modifying heap resources will be delayed until all in-flight reads and writes from all resources suballocated on that heap have completed. For optimal performance, perform hazard tracking manually through MTLFence or MTLEvent instead. All resources created from this heap shared the same hazard tracking mode.
--
-- ObjC selector: @- setHazardTrackingMode:@
setHazardTrackingMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLHazardTrackingMode -> IO ()
setHazardTrackingMode mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setHazardTrackingModeSelector value

-- | resourceOptions
--
-- A packed tuple of the storageMode, cpuCacheMode and hazardTrackingMode properties.
--
-- Modifications to this property are reflected in the other properties and vice versa.
--
-- ObjC selector: @- resourceOptions@
resourceOptions :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLResourceOptions
resourceOptions mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor resourceOptionsSelector

-- | resourceOptions
--
-- A packed tuple of the storageMode, cpuCacheMode and hazardTrackingMode properties.
--
-- Modifications to this property are reflected in the other properties and vice versa.
--
-- ObjC selector: @- setResourceOptions:@
setResourceOptions :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLResourceOptions -> IO ()
setResourceOptions mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setResourceOptionsSelector value

-- | type
--
-- The type of the heap. The default value is MTLHeapTypeAutomatic.
--
-- This constrains the resource creation functions that are available.
--
-- ObjC selector: @- type@
type_ :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLHeapType
type_ mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor typeSelector

-- | type
--
-- The type of the heap. The default value is MTLHeapTypeAutomatic.
--
-- This constrains the resource creation functions that are available.
--
-- ObjC selector: @- setType:@
setType :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLHeapType -> IO ()
setType mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setTypeSelector value

-- | Specifies the largest sparse page size that the Metal heap supports.
--
-- This parameter only affects the heap if you set the ``type`` property of this descriptor to ``MTLHeapType/MTLHeapTypePlacement``.
--
-- The value you assign to this property determines the compatibility of the Metal heap with with placement sparse resources, because placement sparse resources require that their sparse page size be less than or equal to the placement sparse page of the Metal heap that this property controls.
--
-- ObjC selector: @- maxCompatiblePlacementSparsePageSize@
maxCompatiblePlacementSparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLSparsePageSize
maxCompatiblePlacementSparsePageSize mtlHeapDescriptor =
  sendMessage mtlHeapDescriptor maxCompatiblePlacementSparsePageSizeSelector

-- | Specifies the largest sparse page size that the Metal heap supports.
--
-- This parameter only affects the heap if you set the ``type`` property of this descriptor to ``MTLHeapType/MTLHeapTypePlacement``.
--
-- The value you assign to this property determines the compatibility of the Metal heap with with placement sparse resources, because placement sparse resources require that their sparse page size be less than or equal to the placement sparse page of the Metal heap that this property controls.
--
-- ObjC selector: @- setMaxCompatiblePlacementSparsePageSize:@
setMaxCompatiblePlacementSparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLSparsePageSize -> IO ()
setMaxCompatiblePlacementSparsePageSize mtlHeapDescriptor value =
  sendMessage mtlHeapDescriptor setMaxCompatiblePlacementSparsePageSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @size@
sizeSelector :: Selector '[] CULong
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[CULong] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector '[] MTLStorageMode
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector '[MTLStorageMode] ()
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @cpuCacheMode@
cpuCacheModeSelector :: Selector '[] MTLCPUCacheMode
cpuCacheModeSelector = mkSelector "cpuCacheMode"

-- | @Selector@ for @setCpuCacheMode:@
setCpuCacheModeSelector :: Selector '[MTLCPUCacheMode] ()
setCpuCacheModeSelector = mkSelector "setCpuCacheMode:"

-- | @Selector@ for @sparsePageSize@
sparsePageSizeSelector :: Selector '[] MTLSparsePageSize
sparsePageSizeSelector = mkSelector "sparsePageSize"

-- | @Selector@ for @setSparsePageSize:@
setSparsePageSizeSelector :: Selector '[MTLSparsePageSize] ()
setSparsePageSizeSelector = mkSelector "setSparsePageSize:"

-- | @Selector@ for @hazardTrackingMode@
hazardTrackingModeSelector :: Selector '[] MTLHazardTrackingMode
hazardTrackingModeSelector = mkSelector "hazardTrackingMode"

-- | @Selector@ for @setHazardTrackingMode:@
setHazardTrackingModeSelector :: Selector '[MTLHazardTrackingMode] ()
setHazardTrackingModeSelector = mkSelector "setHazardTrackingMode:"

-- | @Selector@ for @resourceOptions@
resourceOptionsSelector :: Selector '[] MTLResourceOptions
resourceOptionsSelector = mkSelector "resourceOptions"

-- | @Selector@ for @setResourceOptions:@
setResourceOptionsSelector :: Selector '[MTLResourceOptions] ()
setResourceOptionsSelector = mkSelector "setResourceOptions:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MTLHeapType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[MTLHeapType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @maxCompatiblePlacementSparsePageSize@
maxCompatiblePlacementSparsePageSizeSelector :: Selector '[] MTLSparsePageSize
maxCompatiblePlacementSparsePageSizeSelector = mkSelector "maxCompatiblePlacementSparsePageSize"

-- | @Selector@ for @setMaxCompatiblePlacementSparsePageSize:@
setMaxCompatiblePlacementSparsePageSizeSelector :: Selector '[MTLSparsePageSize] ()
setMaxCompatiblePlacementSparsePageSizeSelector = mkSelector "setMaxCompatiblePlacementSparsePageSize:"

