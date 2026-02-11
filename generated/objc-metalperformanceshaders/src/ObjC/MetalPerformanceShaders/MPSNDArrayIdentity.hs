{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayIdentityKernel
--
-- This depends on Metal.framework.
--
-- An efficient kernel to handle copies, transposed-copies and reshapes.
--
-- Generated bindings for @MPSNDArrayIdentity@.
module ObjC.MetalPerformanceShaders.MPSNDArrayIdentity
  ( MPSNDArrayIdentity
  , IsMPSNDArrayIdentity(..)
  , initWithDevice
  , reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray
  , reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray
  , initWithDeviceSelector
  , reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector
  , reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector


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

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayIdentity mpsndArrayIdentity => mpsndArrayIdentity -> RawId -> IO (Id MPSNDArrayIdentity)
initWithDevice mpsndArrayIdentity  device =
  sendMsg mpsndArrayIdentity (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray :: (IsMPSNDArrayIdentity mpsndArrayIdentity, IsMPSNDArray sourceArray, IsMPSNDArray destinationArray) => mpsndArrayIdentity -> RawId -> sourceArray -> CULong -> Ptr CULong -> destinationArray -> IO (Id MPSNDArray)
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray mpsndArrayIdentity  cmdBuf sourceArray numberOfDimensions dimensionSizes destinationArray =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr destinationArray $ \raw_destinationArray ->
      sendMsg mpsndArrayIdentity (mkSelector "reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argCULong (fromIntegral numberOfDimensions), argPtr dimensionSizes, argPtr (castPtr raw_destinationArray :: Ptr ())] >>= retainedObject . castPtr

-- | @- reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray :: (IsMPSNDArrayIdentity mpsndArrayIdentity, IsMPSNDArray sourceArray, IsMPSNDArray destinationArray) => mpsndArrayIdentity -> RawId -> RawId -> sourceArray -> CULong -> Ptr CULong -> destinationArray -> IO (Id MPSNDArray)
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray mpsndArrayIdentity  encoder cmdBuf sourceArray numberOfDimensions dimensionSizes destinationArray =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr destinationArray $ \raw_destinationArray ->
      sendMsg mpsndArrayIdentity (mkSelector "reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:") (retPtr retVoid) [argPtr (castPtr (unRawId encoder) :: Ptr ()), argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argCULong (fromIntegral numberOfDimensions), argPtr dimensionSizes, argPtr (castPtr raw_destinationArray :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector :: Selector
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector = mkSelector "reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:"

-- | @Selector@ for @reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector :: Selector
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector = mkSelector "reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:"

