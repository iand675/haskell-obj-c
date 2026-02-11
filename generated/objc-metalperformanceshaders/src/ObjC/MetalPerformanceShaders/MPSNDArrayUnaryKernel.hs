{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayUnaryKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayUnaryKernel
  ( MPSNDArrayUnaryKernel
  , IsMPSNDArrayUnaryKernel(..)
  , initWithDevice
  , initWithDevice_sourceCount
  , initWithCoder_device
  , encodeToCommandBuffer_sourceArray
  , encodeToCommandBuffer_sourceArray_destinationArray
  , encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporary
  , encodeToCommandBuffer_sourceArray_resultState_destinationArray
  , edgeMode
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceArraySelector
  , encodeToCommandBuffer_sourceArray_destinationArraySelector
  , encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector
  , encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector
  , edgeModeSelector

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel => mpsndArrayUnaryKernel -> RawId -> IO (Id MPSNDArrayUnaryKernel)
initWithDevice mpsndArrayUnaryKernel  device =
  sendMsg mpsndArrayUnaryKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel => mpsndArrayUnaryKernel -> RawId -> CULong -> IO (Id MPSNDArrayUnaryKernel)
initWithDevice_sourceCount mpsndArrayUnaryKernel  device count =
  sendMsg mpsndArrayUnaryKernel (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsNSCoder coder) => mpsndArrayUnaryKernel -> coder -> RawId -> IO (Id MPSNDArrayUnaryKernel)
initWithCoder_device mpsndArrayUnaryKernel  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayUnaryKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:@
encodeToCommandBuffer_sourceArray :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArray mpsndArrayUnaryKernel  cmdBuf sourceArray =
withObjCPtr sourceArray $ \raw_sourceArray ->
    sendMsg mpsndArrayUnaryKernel (mkSelector "encodeToCommandBuffer:sourceArray:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ())] >>= retainedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- @destination@ — The NDArray to receive the result
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:destinationArray:@
encodeToCommandBuffer_sourceArray_destinationArray :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray, IsMPSNDArray destination) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> destination -> IO ()
encodeToCommandBuffer_sourceArray_destinationArray mpsndArrayUnaryKernel  cmdBuf sourceArray destination =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr destination $ \raw_destination ->
      sendMsg mpsndArrayUnaryKernel (mkSelector "encodeToCommandBuffer:sourceArray:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- @outGradientState@ — If non-nil, the address output gradient state is written to this address
--
-- @outputStateIsTemporary@ — If YES, the state if any will be allocated to contain temporary textures and buffers as needed
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporary :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray, IsMPSState outGradientState) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> outGradientState -> Bool -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporary mpsndArrayUnaryKernel  cmdBuf sourceArray outGradientState outputStateIsTemporary =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr outGradientState $ \raw_outGradientState ->
      sendMsg mpsndArrayUnaryKernel (mkSelector "encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argPtr (castPtr raw_outGradientState :: Ptr ()), argCULong (if outputStateIsTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- @outGradientState@ — The output gradient state to record the operation for later use by gradient
--
-- @destination@ — A destination array to contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:resultState:destinationArray:@
encodeToCommandBuffer_sourceArray_resultState_destinationArray :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray, IsMPSState outGradientState, IsMPSNDArray destination) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> outGradientState -> destination -> IO ()
encodeToCommandBuffer_sourceArray_resultState_destinationArray mpsndArrayUnaryKernel  cmdBuf sourceArray outGradientState destination =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr outGradientState $ \raw_outGradientState ->
    withObjCPtr destination $ \raw_destination ->
        sendMsg mpsndArrayUnaryKernel (mkSelector "encodeToCommandBuffer:sourceArray:resultState:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argPtr (castPtr raw_outGradientState :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- | edgeMode
--
-- The edge mode used for a source NDArray             Default: MPSImageEdgeModeZero
--
-- ObjC selector: @- edgeMode@
edgeMode :: IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel => mpsndArrayUnaryKernel -> IO MPSImageEdgeMode
edgeMode mpsndArrayUnaryKernel  =
  fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpsndArrayUnaryKernel (mkSelector "edgeMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:@
encodeToCommandBuffer_sourceArraySelector :: Selector
encodeToCommandBuffer_sourceArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:destinationArray:@
encodeToCommandBuffer_sourceArray_destinationArraySelector :: Selector
encodeToCommandBuffer_sourceArray_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:destinationArray:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector :: Selector
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:resultState:destinationArray:@
encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector :: Selector
encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:resultState:destinationArray:"

-- | @Selector@ for @edgeMode@
edgeModeSelector :: Selector
edgeModeSelector = mkSelector "edgeMode"

