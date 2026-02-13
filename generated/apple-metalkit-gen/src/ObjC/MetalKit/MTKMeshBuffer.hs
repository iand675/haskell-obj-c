{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTKMeshBuffer
--
-- Mesh buffer created by MTKMeshBufferAllocator when Model I/O needs to memory for vertex or index data backing.
--
-- Memory backing these buffer are Metal buffers.  Model I/O will load index and vertex data from from a model asset directly in to the Metal buffer.
--
-- Generated bindings for @MTKMeshBuffer@.
module ObjC.MetalKit.MTKMeshBuffer
  ( MTKMeshBuffer
  , IsMTKMeshBuffer(..)
  , init_
  , length_
  , allocator
  , zone
  , buffer
  , offset
  , type_
  , allocatorSelector
  , bufferSelector
  , initSelector
  , lengthSelector
  , offsetSelector
  , typeSelector
  , zoneSelector

  -- * Enum types
  , MDLMeshBufferType(MDLMeshBufferType)
  , pattern MDLMeshBufferTypeVertex
  , pattern MDLMeshBufferTypeIndex
  , pattern MDLMeshBufferTypeCustom

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalKit.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Only an MTKMeshBufferAllocator object can initilize a MTKMeshBuffer object
--
-- ObjC selector: @- init@
init_ :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO (Id MTKMeshBuffer)
init_ mtkMeshBuffer =
  sendOwnedMessage mtkMeshBuffer initSelector

-- | length
--
-- Size in bytes of the buffer allocation.
--
-- ObjC selector: @- length@
length_ :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO CULong
length_ mtkMeshBuffer =
  sendMessage mtkMeshBuffer lengthSelector

-- | allocator
--
-- Allocator object used to create this buffer.
--
-- This allcoator is stored so that it can be used by Model I/O for copy and relayout operations (such as when a new vertex descriptor is applied to a vertex buffer).
--
-- ObjC selector: @- allocator@
allocator :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO (Id MTKMeshBufferAllocator)
allocator mtkMeshBuffer =
  sendOwnedMessage mtkMeshBuffer allocatorSelector

-- | zone
--
-- Zone from which this buffer was created (if it was created from a zone).
--
-- A single MetalBuffer is allocated for each zone.  Each zone could have many MTKMeshBuffers, each with it's own offset.  If a MTKMeshBufferAllocator is used, Model I/O will attempt to load all vertex and index data of a single mesh into a single zone.  This allows the GPU to achieve a higher cache hit rate when drawing the mesh.  So although there maybe many MTKMeshBuffers for a model they will be backed with the same contigous MetalBuffer.
--
-- ObjC selector: @- zone@
zone :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO RawId
zone mtkMeshBuffer =
  sendMessage mtkMeshBuffer zoneSelector

-- | buffer
--
-- Metal Buffer backing vertex/index data.
--
-- Many MTKMeshBuffers may reference the same buffer, but each with it's own offset.  (i.e. Many MTKMeshBuffers may be suballocated from a single buffer)
--
-- ObjC selector: @- buffer@
buffer :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO RawId
buffer mtkMeshBuffer =
  sendMessage mtkMeshBuffer bufferSelector

-- | offset
--
-- Byte offset of the data within the metal buffer.
--
-- ObjC selector: @- offset@
offset :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO CULong
offset mtkMeshBuffer =
  sendMessage mtkMeshBuffer offsetSelector

-- | type
--
-- the intended type of the buffer
--
-- ObjC selector: @- type@
type_ :: IsMTKMeshBuffer mtkMeshBuffer => mtkMeshBuffer -> IO MDLMeshBufferType
type_ mtkMeshBuffer =
  sendMessage mtkMeshBuffer typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTKMeshBuffer)
initSelector = mkSelector "init"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @allocator@
allocatorSelector :: Selector '[] (Id MTKMeshBufferAllocator)
allocatorSelector = mkSelector "allocator"

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] RawId
zoneSelector = mkSelector "zone"

-- | @Selector@ for @buffer@
bufferSelector :: Selector '[] RawId
bufferSelector = mkSelector "buffer"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MDLMeshBufferType
typeSelector = mkSelector "type"

