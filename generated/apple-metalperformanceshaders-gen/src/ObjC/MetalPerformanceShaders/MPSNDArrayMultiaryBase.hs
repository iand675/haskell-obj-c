{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayMultiaryBase@.
module ObjC.MetalPerformanceShaders.MPSNDArrayMultiaryBase
  ( MPSNDArrayMultiaryBase
  , IsMPSNDArrayMultiaryBase(..)
  , edgeModeAtSourceIndex
  , initWithDevice
  , initWithDevice_sourceCount
  , initWithCoder_device
  , encodeWithCoder
  , copyWithZone_device
  , resultStateForSourceArrays_sourceStates_destinationArray
  , destinationArrayDescriptorForSourceArrays_sourceState
  , destinationArrayAllocator
  , setDestinationArrayAllocator
  , copyWithZone_deviceSelector
  , destinationArrayAllocatorSelector
  , destinationArrayDescriptorForSourceArrays_sourceStateSelector
  , edgeModeAtSourceIndexSelector
  , encodeWithCoderSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector
  , resultStateForSourceArrays_sourceStates_destinationArraySelector
  , setDestinationArrayAllocatorSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

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

-- | The edge mode used for each source NDArray
--
-- @sourceIndex@ — The index of the source image
--
-- Returns: The MPSImageEdgeMode for that image
--
-- ObjC selector: @- edgeModeAtSourceIndex:@
edgeModeAtSourceIndex :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> CULong -> IO MPSImageEdgeMode
edgeModeAtSourceIndex mpsndArrayMultiaryBase sourceIndex =
  sendMessage mpsndArrayMultiaryBase edgeModeAtSourceIndexSelector sourceIndex

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> RawId -> IO (Id MPSNDArrayMultiaryBase)
initWithDevice mpsndArrayMultiaryBase device =
  sendOwnedMessage mpsndArrayMultiaryBase initWithDeviceSelector device

-- | Initialize a MPSNDArrayMultiaryKernel
--
-- @device@ — The device on which the kernel will run
--
-- @count@ — The maximum number of NDArrays read by the kernel
--
-- Returns: A valid MPSNDArrayMultiaryKernel, or nil if allocation failure.
--
-- ObjC selector: @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> RawId -> CULong -> IO (Id MPSNDArrayMultiaryBase)
initWithDevice_sourceCount mpsndArrayMultiaryBase device count =
  sendOwnedMessage mpsndArrayMultiaryBase initWithDevice_sourceCountSelector device count

-- | Initialize a MPSNDArrayMultiaryKernel from a NSCoder
--
-- @coder@ — The NSCoder that contains the serialized object
--
-- @device@ — The device on which the kernel will run
--
-- Returns: A valid MPSNDArrayMultiaryKernel, or nil if allocation failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase, IsNSCoder coder) => mpsndArrayMultiaryBase -> coder -> RawId -> IO (Id MPSNDArrayMultiaryBase)
initWithCoder_device mpsndArrayMultiaryBase coder device =
  sendOwnedMessage mpsndArrayMultiaryBase initWithCoder_deviceSelector (toNSCoder coder) device

-- | Initialize a MPSNDArrayMultiaryKernel from a NSCoder
--
-- @coder@ — The NSCoder that contains the serialized object
--
-- ObjC selector: @- encodeWithCoder:@
encodeWithCoder :: (IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase, IsNSCoder coder) => mpsndArrayMultiaryBase -> coder -> IO ()
encodeWithCoder mpsndArrayMultiaryBase coder =
  sendMessage mpsndArrayMultiaryBase encodeWithCoderSelector (toNSCoder coder)

-- | Create a copy with
--
-- @zone@ — The NSZone in which to allocate the MPSNDArrayMultiaryKernel object
--
-- @device@ — The device on which the new kernel will run. Pass nil for same device.
--
-- Returns: A valid MPSNDArrayMultiaryKernel, or nil if allocation failure.
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> Ptr () -> RawId -> IO (Id MPSNDArrayMultiaryBase)
copyWithZone_device mpsndArrayMultiaryBase zone device =
  sendOwnedMessage mpsndArrayMultiaryBase copyWithZone_deviceSelector zone device

-- | @- resultStateForSourceArrays:sourceStates:destinationArray:@
resultStateForSourceArrays_sourceStates_destinationArray :: (IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase, IsNSArray sourceArrays, IsNSArray sourceStates, IsMPSNDArray destinationArray) => mpsndArrayMultiaryBase -> sourceArrays -> sourceStates -> destinationArray -> IO (Id MPSState)
resultStateForSourceArrays_sourceStates_destinationArray mpsndArrayMultiaryBase sourceArrays sourceStates destinationArray =
  sendMessage mpsndArrayMultiaryBase resultStateForSourceArrays_sourceStates_destinationArraySelector (toNSArray sourceArrays) (toNSArray sourceStates) (toMPSNDArray destinationArray)

-- | Return a descriptor suitable for allocating a NSArray to receive the result
--
-- The object properties (kernelSize, offsets, edgeMode, etc.) should be properly              configured as if the -encode call was about to be made, before this method is              called. Those properties may affect the results.
--
-- @sources@ — The list of sources passed into the -encode call
--
-- @state@ — The source state object, if any passed to the -encode call
--
-- Returns: a valid MPSNDArrayDescriptor that may be used to create a MPSNDArray to used to              hold the results of this kernel.
--
-- ObjC selector: @- destinationArrayDescriptorForSourceArrays:sourceState:@
destinationArrayDescriptorForSourceArrays_sourceState :: (IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase, IsNSArray sources, IsMPSState state) => mpsndArrayMultiaryBase -> sources -> state -> IO (Id MPSNDArrayDescriptor)
destinationArrayDescriptorForSourceArrays_sourceState mpsndArrayMultiaryBase sources state =
  sendMessage mpsndArrayMultiaryBase destinationArrayDescriptorForSourceArrays_sourceStateSelector (toNSArray sources) (toMPSState state)

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- destinationArrayAllocator@
destinationArrayAllocator :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> IO RawId
destinationArrayAllocator mpsndArrayMultiaryBase =
  sendMessage mpsndArrayMultiaryBase destinationArrayAllocatorSelector

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- setDestinationArrayAllocator:@
setDestinationArrayAllocator :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> RawId -> IO ()
setDestinationArrayAllocator mpsndArrayMultiaryBase value =
  sendMessage mpsndArrayMultiaryBase setDestinationArrayAllocatorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @edgeModeAtSourceIndex:@
edgeModeAtSourceIndexSelector :: Selector '[CULong] MPSImageEdgeMode
edgeModeAtSourceIndexSelector = mkSelector "edgeModeAtSourceIndex:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayMultiaryBase)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayMultiaryBase)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayMultiaryBase)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector '[Id NSCoder] ()
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSNDArrayMultiaryBase)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @resultStateForSourceArrays:sourceStates:destinationArray:@
resultStateForSourceArrays_sourceStates_destinationArraySelector :: Selector '[Id NSArray, Id NSArray, Id MPSNDArray] (Id MPSState)
resultStateForSourceArrays_sourceStates_destinationArraySelector = mkSelector "resultStateForSourceArrays:sourceStates:destinationArray:"

-- | @Selector@ for @destinationArrayDescriptorForSourceArrays:sourceState:@
destinationArrayDescriptorForSourceArrays_sourceStateSelector :: Selector '[Id NSArray, Id MPSState] (Id MPSNDArrayDescriptor)
destinationArrayDescriptorForSourceArrays_sourceStateSelector = mkSelector "destinationArrayDescriptorForSourceArrays:sourceState:"

-- | @Selector@ for @destinationArrayAllocator@
destinationArrayAllocatorSelector :: Selector '[] RawId
destinationArrayAllocatorSelector = mkSelector "destinationArrayAllocator"

-- | @Selector@ for @setDestinationArrayAllocator:@
setDestinationArrayAllocatorSelector :: Selector '[RawId] ()
setDestinationArrayAllocatorSelector = mkSelector "setDestinationArrayAllocator:"

