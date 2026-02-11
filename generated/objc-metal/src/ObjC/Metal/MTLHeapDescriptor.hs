{-# LANGUAGE PatternSynonyms #-}
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
  , sizeSelector
  , setSizeSelector
  , storageModeSelector
  , setStorageModeSelector
  , cpuCacheModeSelector
  , setCpuCacheModeSelector
  , sparsePageSizeSelector
  , setSparsePageSizeSelector
  , hazardTrackingModeSelector
  , setHazardTrackingModeSelector
  , resourceOptionsSelector
  , setResourceOptionsSelector
  , typeSelector
  , setTypeSelector
  , maxCompatiblePlacementSparsePageSizeSelector
  , setMaxCompatiblePlacementSparsePageSizeSelector

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

-- | size
--
-- Requested size of the heap's backing memory.
--
-- The size may be rounded up to GPU page granularity.
--
-- ObjC selector: @- size@
size :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO CULong
size mtlHeapDescriptor  =
  sendMsg mtlHeapDescriptor (mkSelector "size") retCULong []

-- | size
--
-- Requested size of the heap's backing memory.
--
-- The size may be rounded up to GPU page granularity.
--
-- ObjC selector: @- setSize:@
setSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> CULong -> IO ()
setSize mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setSize:") retVoid [argCULong (fromIntegral value)]

-- | storageMode
--
-- Storage mode for the heap. Default is MTLStorageModePrivate.
--
-- All resources created from this heap share the same storage mode. MTLStorageModeManaged and MTLStorageModeMemoryless are disallowed.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLStorageMode
storageMode mtlHeapDescriptor  =
  fmap (coerce :: CULong -> MTLStorageMode) $ sendMsg mtlHeapDescriptor (mkSelector "storageMode") retCULong []

-- | storageMode
--
-- Storage mode for the heap. Default is MTLStorageModePrivate.
--
-- All resources created from this heap share the same storage mode. MTLStorageModeManaged and MTLStorageModeMemoryless are disallowed.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setStorageMode:") retVoid [argCULong (coerce value)]

-- | cpuCacheMode
--
-- CPU cache mode for the heap. Default is MTLCPUCacheModeDefaultCache.
--
-- All resources created from this heap share the same cache mode. CPU cache mode is ignored for MTLStorageModePrivate.
--
-- ObjC selector: @- cpuCacheMode@
cpuCacheMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLCPUCacheMode
cpuCacheMode mtlHeapDescriptor  =
  fmap (coerce :: CULong -> MTLCPUCacheMode) $ sendMsg mtlHeapDescriptor (mkSelector "cpuCacheMode") retCULong []

-- | cpuCacheMode
--
-- CPU cache mode for the heap. Default is MTLCPUCacheModeDefaultCache.
--
-- All resources created from this heap share the same cache mode. CPU cache mode is ignored for MTLStorageModePrivate.
--
-- ObjC selector: @- setCpuCacheMode:@
setCpuCacheMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLCPUCacheMode -> IO ()
setCpuCacheMode mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setCpuCacheMode:") retVoid [argCULong (coerce value)]

-- | sparsePageSize
--
-- The sparse page size to use for resources created from the heap.
--
-- ObjC selector: @- sparsePageSize@
sparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLSparsePageSize
sparsePageSize mtlHeapDescriptor  =
  fmap (coerce :: CLong -> MTLSparsePageSize) $ sendMsg mtlHeapDescriptor (mkSelector "sparsePageSize") retCLong []

-- | sparsePageSize
--
-- The sparse page size to use for resources created from the heap.
--
-- ObjC selector: @- setSparsePageSize:@
setSparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLSparsePageSize -> IO ()
setSparsePageSize mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setSparsePageSize:") retVoid [argCLong (coerce value)]

-- | hazardTrackingMode
--
-- Set hazard tracking mode for the heap. The default value is MTLHazardTrackingModeDefault.
--
-- For heaps, MTLHazardTrackingModeDefault is treated as MTLHazardTrackingModeUntracked. Setting hazardTrackingMode to MTLHazardTrackingModeTracked causes hazard tracking to be enabled heap. When a resource on a hazard tracked heap is modified, reads and writes from all resources suballocated on that heap will be delayed until the modification is complete. Similarly, modifying heap resources will be delayed until all in-flight reads and writes from all resources suballocated on that heap have completed. For optimal performance, perform hazard tracking manually through MTLFence or MTLEvent instead. All resources created from this heap shared the same hazard tracking mode.
--
-- ObjC selector: @- hazardTrackingMode@
hazardTrackingMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLHazardTrackingMode
hazardTrackingMode mtlHeapDescriptor  =
  fmap (coerce :: CULong -> MTLHazardTrackingMode) $ sendMsg mtlHeapDescriptor (mkSelector "hazardTrackingMode") retCULong []

-- | hazardTrackingMode
--
-- Set hazard tracking mode for the heap. The default value is MTLHazardTrackingModeDefault.
--
-- For heaps, MTLHazardTrackingModeDefault is treated as MTLHazardTrackingModeUntracked. Setting hazardTrackingMode to MTLHazardTrackingModeTracked causes hazard tracking to be enabled heap. When a resource on a hazard tracked heap is modified, reads and writes from all resources suballocated on that heap will be delayed until the modification is complete. Similarly, modifying heap resources will be delayed until all in-flight reads and writes from all resources suballocated on that heap have completed. For optimal performance, perform hazard tracking manually through MTLFence or MTLEvent instead. All resources created from this heap shared the same hazard tracking mode.
--
-- ObjC selector: @- setHazardTrackingMode:@
setHazardTrackingMode :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLHazardTrackingMode -> IO ()
setHazardTrackingMode mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setHazardTrackingMode:") retVoid [argCULong (coerce value)]

-- | resourceOptions
--
-- A packed tuple of the storageMode, cpuCacheMode and hazardTrackingMode properties.
--
-- Modifications to this property are reflected in the other properties and vice versa.
--
-- ObjC selector: @- resourceOptions@
resourceOptions :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLResourceOptions
resourceOptions mtlHeapDescriptor  =
  fmap (coerce :: CULong -> MTLResourceOptions) $ sendMsg mtlHeapDescriptor (mkSelector "resourceOptions") retCULong []

-- | resourceOptions
--
-- A packed tuple of the storageMode, cpuCacheMode and hazardTrackingMode properties.
--
-- Modifications to this property are reflected in the other properties and vice versa.
--
-- ObjC selector: @- setResourceOptions:@
setResourceOptions :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLResourceOptions -> IO ()
setResourceOptions mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setResourceOptions:") retVoid [argCULong (coerce value)]

-- | type
--
-- The type of the heap. The default value is MTLHeapTypeAutomatic.
--
-- This constrains the resource creation functions that are available.
--
-- ObjC selector: @- type@
type_ :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLHeapType
type_ mtlHeapDescriptor  =
  fmap (coerce :: CLong -> MTLHeapType) $ sendMsg mtlHeapDescriptor (mkSelector "type") retCLong []

-- | type
--
-- The type of the heap. The default value is MTLHeapTypeAutomatic.
--
-- This constrains the resource creation functions that are available.
--
-- ObjC selector: @- setType:@
setType :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLHeapType -> IO ()
setType mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | Specifies the largest sparse page size that the Metal heap supports.
--
-- This parameter only affects the heap if you set the ``type`` property of this descriptor to ``MTLHeapType/MTLHeapTypePlacement``.
--
-- The value you assign to this property determines the compatibility of the Metal heap with with placement sparse resources, because placement sparse resources require that their sparse page size be less than or equal to the placement sparse page of the Metal heap that this property controls.
--
-- ObjC selector: @- maxCompatiblePlacementSparsePageSize@
maxCompatiblePlacementSparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> IO MTLSparsePageSize
maxCompatiblePlacementSparsePageSize mtlHeapDescriptor  =
  fmap (coerce :: CLong -> MTLSparsePageSize) $ sendMsg mtlHeapDescriptor (mkSelector "maxCompatiblePlacementSparsePageSize") retCLong []

-- | Specifies the largest sparse page size that the Metal heap supports.
--
-- This parameter only affects the heap if you set the ``type`` property of this descriptor to ``MTLHeapType/MTLHeapTypePlacement``.
--
-- The value you assign to this property determines the compatibility of the Metal heap with with placement sparse resources, because placement sparse resources require that their sparse page size be less than or equal to the placement sparse page of the Metal heap that this property controls.
--
-- ObjC selector: @- setMaxCompatiblePlacementSparsePageSize:@
setMaxCompatiblePlacementSparsePageSize :: IsMTLHeapDescriptor mtlHeapDescriptor => mtlHeapDescriptor -> MTLSparsePageSize -> IO ()
setMaxCompatiblePlacementSparsePageSize mtlHeapDescriptor  value =
  sendMsg mtlHeapDescriptor (mkSelector "setMaxCompatiblePlacementSparsePageSize:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @cpuCacheMode@
cpuCacheModeSelector :: Selector
cpuCacheModeSelector = mkSelector "cpuCacheMode"

-- | @Selector@ for @setCpuCacheMode:@
setCpuCacheModeSelector :: Selector
setCpuCacheModeSelector = mkSelector "setCpuCacheMode:"

-- | @Selector@ for @sparsePageSize@
sparsePageSizeSelector :: Selector
sparsePageSizeSelector = mkSelector "sparsePageSize"

-- | @Selector@ for @setSparsePageSize:@
setSparsePageSizeSelector :: Selector
setSparsePageSizeSelector = mkSelector "setSparsePageSize:"

-- | @Selector@ for @hazardTrackingMode@
hazardTrackingModeSelector :: Selector
hazardTrackingModeSelector = mkSelector "hazardTrackingMode"

-- | @Selector@ for @setHazardTrackingMode:@
setHazardTrackingModeSelector :: Selector
setHazardTrackingModeSelector = mkSelector "setHazardTrackingMode:"

-- | @Selector@ for @resourceOptions@
resourceOptionsSelector :: Selector
resourceOptionsSelector = mkSelector "resourceOptions"

-- | @Selector@ for @setResourceOptions:@
setResourceOptionsSelector :: Selector
setResourceOptionsSelector = mkSelector "setResourceOptions:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @maxCompatiblePlacementSparsePageSize@
maxCompatiblePlacementSparsePageSizeSelector :: Selector
maxCompatiblePlacementSparsePageSizeSelector = mkSelector "maxCompatiblePlacementSparsePageSize"

-- | @Selector@ for @setMaxCompatiblePlacementSparsePageSize:@
setMaxCompatiblePlacementSparsePageSizeSelector :: Selector
setMaxCompatiblePlacementSparsePageSizeSelector = mkSelector "setMaxCompatiblePlacementSparsePageSize:"

