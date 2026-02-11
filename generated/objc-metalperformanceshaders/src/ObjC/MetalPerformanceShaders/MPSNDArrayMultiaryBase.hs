{-# LANGUAGE PatternSynonyms #-}
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
  , edgeModeAtSourceIndexSelector
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector
  , initWithCoder_deviceSelector
  , encodeWithCoderSelector
  , copyWithZone_deviceSelector
  , resultStateForSourceArrays_sourceStates_destinationArraySelector
  , destinationArrayDescriptorForSourceArrays_sourceStateSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

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

-- | The edge mode used for each source NDArray
--
-- @sourceIndex@ — The index of the source image
--
-- Returns: The MPSImageEdgeMode for that image
--
-- ObjC selector: @- edgeModeAtSourceIndex:@
edgeModeAtSourceIndex :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> CULong -> IO MPSImageEdgeMode
edgeModeAtSourceIndex mpsndArrayMultiaryBase  sourceIndex =
  fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpsndArrayMultiaryBase (mkSelector "edgeModeAtSourceIndex:") retCULong [argCULong (fromIntegral sourceIndex)]

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase => mpsndArrayMultiaryBase -> RawId -> IO (Id MPSNDArrayMultiaryBase)
initWithDevice mpsndArrayMultiaryBase  device =
  sendMsg mpsndArrayMultiaryBase (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_sourceCount mpsndArrayMultiaryBase  device count =
  sendMsg mpsndArrayMultiaryBase (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count)] >>= ownedObject . castPtr

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
initWithCoder_device mpsndArrayMultiaryBase  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayMultiaryBase (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a MPSNDArrayMultiaryKernel from a NSCoder
--
-- @coder@ — The NSCoder that contains the serialized object
--
-- ObjC selector: @- encodeWithCoder:@
encodeWithCoder :: (IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase, IsNSCoder coder) => mpsndArrayMultiaryBase -> coder -> IO ()
encodeWithCoder mpsndArrayMultiaryBase  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayMultiaryBase (mkSelector "encodeWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

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
copyWithZone_device mpsndArrayMultiaryBase  zone device =
  sendMsg mpsndArrayMultiaryBase (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- resultStateForSourceArrays:sourceStates:destinationArray:@
resultStateForSourceArrays_sourceStates_destinationArray :: (IsMPSNDArrayMultiaryBase mpsndArrayMultiaryBase, IsNSArray sourceArrays, IsNSArray sourceStates, IsMPSNDArray destinationArray) => mpsndArrayMultiaryBase -> sourceArrays -> sourceStates -> destinationArray -> IO (Id MPSState)
resultStateForSourceArrays_sourceStates_destinationArray mpsndArrayMultiaryBase  sourceArrays sourceStates destinationArray =
withObjCPtr sourceArrays $ \raw_sourceArrays ->
  withObjCPtr sourceStates $ \raw_sourceStates ->
    withObjCPtr destinationArray $ \raw_destinationArray ->
        sendMsg mpsndArrayMultiaryBase (mkSelector "resultStateForSourceArrays:sourceStates:destinationArray:") (retPtr retVoid) [argPtr (castPtr raw_sourceArrays :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationArray :: Ptr ())] >>= retainedObject . castPtr

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
destinationArrayDescriptorForSourceArrays_sourceState mpsndArrayMultiaryBase  sources state =
withObjCPtr sources $ \raw_sources ->
  withObjCPtr state $ \raw_state ->
      sendMsg mpsndArrayMultiaryBase (mkSelector "destinationArrayDescriptorForSourceArrays:sourceState:") (retPtr retVoid) [argPtr (castPtr raw_sources :: Ptr ()), argPtr (castPtr raw_state :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @edgeModeAtSourceIndex:@
edgeModeAtSourceIndexSelector :: Selector
edgeModeAtSourceIndexSelector = mkSelector "edgeModeAtSourceIndex:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @resultStateForSourceArrays:sourceStates:destinationArray:@
resultStateForSourceArrays_sourceStates_destinationArraySelector :: Selector
resultStateForSourceArrays_sourceStates_destinationArraySelector = mkSelector "resultStateForSourceArrays:sourceStates:destinationArray:"

-- | @Selector@ for @destinationArrayDescriptorForSourceArrays:sourceState:@
destinationArrayDescriptorForSourceArrays_sourceStateSelector :: Selector
destinationArrayDescriptorForSourceArrays_sourceStateSelector = mkSelector "destinationArrayDescriptorForSourceArrays:sourceState:"

