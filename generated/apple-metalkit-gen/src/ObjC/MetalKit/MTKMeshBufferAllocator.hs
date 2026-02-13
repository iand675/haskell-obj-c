{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTKMeshBufferAllocator
--
-- Allocator passed to MDLAsset init method to load vertex and index data directly into Metal buffers.
--
-- Generated bindings for @MTKMeshBufferAllocator@.
module ObjC.MetalKit.MTKMeshBufferAllocator
  ( MTKMeshBufferAllocator
  , IsMTKMeshBufferAllocator(..)
  , init_
  , initWithDevice
  , device
  , deviceSelector
  , initSelector
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Must initialize with device
--
-- ObjC selector: @- init@
init_ :: IsMTKMeshBufferAllocator mtkMeshBufferAllocator => mtkMeshBufferAllocator -> IO (Id MTKMeshBufferAllocator)
init_ mtkMeshBufferAllocator =
  sendOwnedMessage mtkMeshBufferAllocator initSelector

-- | initWithDevice
--
-- Initialize the allocator with a device to be used to create buffers.
--
-- The designated initializer for this class.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMTKMeshBufferAllocator mtkMeshBufferAllocator => mtkMeshBufferAllocator -> RawId -> IO (Id MTKMeshBufferAllocator)
initWithDevice mtkMeshBufferAllocator device =
  sendOwnedMessage mtkMeshBufferAllocator initWithDeviceSelector device

-- | device
--
-- Device used to create buffers.
--
-- ObjC selector: @- device@
device :: IsMTKMeshBufferAllocator mtkMeshBufferAllocator => mtkMeshBufferAllocator -> IO RawId
device mtkMeshBufferAllocator =
  sendMessage mtkMeshBufferAllocator deviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTKMeshBufferAllocator)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MTKMeshBufferAllocator)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

