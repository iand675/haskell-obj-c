{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSPredicate
--
-- This depends on Metal.framework
--
-- A MPSPredicate can be used to run MPS kernels subject to a predicate.
--
-- The MPSPredicate defines a way to refrain running a kernel on the GPU              based on values computed on the GPU. That way one can build control flow operations              that do the decisions on the GPU side mitigating the need to synchronize CPU and GPU              execution. The predicate is used with the version of encode calls that take              a object of type MPSKernelEncodeOptions as a parameter (
--
-- See: MPSCNNKernel for example).              The code associated with the kernel's encode call is executed on the GPU if and only if              the predicate is considered to be true.              NOTE: It is advisable to release MPSPredicate objects promptly as they take a reference              to a MTLBuffer object and therefore can keep the memory allocated for long periods of time.
--
-- Generated bindings for @MPSPredicate@.
module ObjC.MetalPerformanceShaders.MPSPredicate
  ( MPSPredicate
  , IsMPSPredicate(..)
  , predicateWithBuffer_offset
  , initWithBuffer_offset
  , initWithDevice
  , predicateOffset
  , predicateWithBuffer_offsetSelector
  , initWithBuffer_offsetSelector
  , initWithDeviceSelector
  , predicateOffsetSelector


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

-- | Initializes a MPSPredicate object with a buffer and given offset.
--
-- @buffer@ — The buffer to use as a predicate.
--
-- @offset@ — Byteoffset to the predicate buffer where the predicate is stored.
--
-- Returns: A pointer to the newly initialized MPSPredicate object.
--
-- ObjC selector: @+ predicateWithBuffer:offset:@
predicateWithBuffer_offset :: RawId -> CULong -> IO (Id MPSPredicate)
predicateWithBuffer_offset buffer offset =
  do
    cls' <- getRequiredClass "MPSPredicate"
    sendClassMsg cls' (mkSelector "predicateWithBuffer:offset:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong (fromIntegral offset)] >>= retainedObject . castPtr

-- | Initializes a MPSPredicate object with a buffer and given offset.
--
-- @buffer@ — The buffer to use as a predicate.
--
-- @offset@ — Byteoffset to the predicate buffer where the predicate is stored.
--
-- Returns: A pointer to the newly initialized MPSPredicate object.
--
-- ObjC selector: @- initWithBuffer:offset:@
initWithBuffer_offset :: IsMPSPredicate mpsPredicate => mpsPredicate -> RawId -> CULong -> IO (Id MPSPredicate)
initWithBuffer_offset mpsPredicate  buffer offset =
  sendMsg mpsPredicate (mkSelector "initWithBuffer:offset:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong (fromIntegral offset)] >>= ownedObject . castPtr

-- | Initializes a MPSPredicate object for a given device.
--
-- NOTE: The metal buffer used by the resulting MPSPredicate object may be              shared among many MPSPredicate objects and therefore care must be used when              writing to this buffer: writing to any other location in this buffer than the              four bytes at the offset predicateOffset results in undefined behavior.
--
-- @device@ — The device the predicate is used with
--
-- Returns: A pointer to the newly initialized MPSPredicate object.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSPredicate mpsPredicate => mpsPredicate -> RawId -> IO (Id MPSPredicate)
initWithDevice mpsPredicate  device =
  sendMsg mpsPredicate (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | predicateOffset
--
-- Location of the predicate in bytes, must be multiple of four.
--
-- If the uint32_t value stored at this location in predicateBuffer is other than zero,              then the predicate is considered to be true and the code is executed on the GPU.              With this property a single MPSPredicate object can be used with multiple different predication              operations.              Default = 0;
--
-- ObjC selector: @- predicateOffset@
predicateOffset :: IsMPSPredicate mpsPredicate => mpsPredicate -> IO CULong
predicateOffset mpsPredicate  =
  sendMsg mpsPredicate (mkSelector "predicateOffset") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithBuffer:offset:@
predicateWithBuffer_offsetSelector :: Selector
predicateWithBuffer_offsetSelector = mkSelector "predicateWithBuffer:offset:"

-- | @Selector@ for @initWithBuffer:offset:@
initWithBuffer_offsetSelector :: Selector
initWithBuffer_offsetSelector = mkSelector "initWithBuffer:offset:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @predicateOffset@
predicateOffsetSelector :: Selector
predicateOffsetSelector = mkSelector "predicateOffset"

