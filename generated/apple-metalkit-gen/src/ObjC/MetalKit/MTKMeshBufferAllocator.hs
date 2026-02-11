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
  , initSelector
  , initWithDeviceSelector
  , deviceSelector


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

import ObjC.MetalKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Must initialize with device
--
-- ObjC selector: @- init@
init_ :: IsMTKMeshBufferAllocator mtkMeshBufferAllocator => mtkMeshBufferAllocator -> IO (Id MTKMeshBufferAllocator)
init_ mtkMeshBufferAllocator  =
    sendMsg mtkMeshBufferAllocator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithDevice
--
-- Initialize the allocator with a device to be used to create buffers.
--
-- The designated initializer for this class.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMTKMeshBufferAllocator mtkMeshBufferAllocator => mtkMeshBufferAllocator -> RawId -> IO (Id MTKMeshBufferAllocator)
initWithDevice mtkMeshBufferAllocator  device =
    sendMsg mtkMeshBufferAllocator (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | device
--
-- Device used to create buffers.
--
-- ObjC selector: @- device@
device :: IsMTKMeshBufferAllocator mtkMeshBufferAllocator => mtkMeshBufferAllocator -> IO RawId
device mtkMeshBufferAllocator  =
    fmap (RawId . castPtr) $ sendMsg mtkMeshBufferAllocator (mkSelector "device") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

