{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GLKMeshBuffer
--
-- Mesh buffers created when  needs to allocate memory to back vertex or index data
--
-- Memory backing these buffer are OpenGL buffers. Model I/O will load index and vertex data from from a model asset directly in to the OpenGL buffer object.
--
-- Generated bindings for @GLKMeshBuffer@.
module ObjC.GLKit.GLKMeshBuffer
  ( GLKMeshBuffer
  , IsGLKMeshBuffer(..)
  , length_
  , allocator
  , glBufferName
  , offset
  , zone
  , type_
  , allocatorSelector
  , glBufferNameSelector
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

import ObjC.GLKit.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | length
--
-- Size in bytes of the buffer allocation
--
-- ObjC selector: @- length@
length_ :: IsGLKMeshBuffer glkMeshBuffer => glkMeshBuffer -> IO CULong
length_ glkMeshBuffer =
  sendMessage glkMeshBuffer lengthSelector

-- | allocator
--
-- Allocator object used to create this buffer.
--
-- This allcoator used for copy and relayout operations (such as when a new vertex descriptor is applied to a vertex buffer)
--
-- ObjC selector: @- allocator@
allocator :: IsGLKMeshBuffer glkMeshBuffer => glkMeshBuffer -> IO (Id GLKMeshBufferAllocator)
allocator glkMeshBuffer =
  sendOwnedMessage glkMeshBuffer allocatorSelector

-- | glBufferName
--
-- glBufferName for buffer object backing vertex/index data
--
-- Many GLKMeshBuffers may reference the same OpenGL buffer object, but each with its own offset.  (i.e. Many GLKMeshBuffers may be suballocated from a single OpenGL buffer object)
--
-- ObjC selector: @- glBufferName@
glBufferName :: IsGLKMeshBuffer glkMeshBuffer => glkMeshBuffer -> IO CUInt
glBufferName glkMeshBuffer =
  sendMessage glkMeshBuffer glBufferNameSelector

-- | offset
--
-- Byte offset of the data within the OpenGL buffer
--
-- ObjC selector: @- offset@
offset :: IsGLKMeshBuffer glkMeshBuffer => glkMeshBuffer -> IO CULong
offset glkMeshBuffer =
  sendMessage glkMeshBuffer offsetSelector

-- | zone
--
-- Zone from which this buffer was created (if it was created witha zone)
--
-- A single GL buffer is allocated for each zone.  Each zone could have many GLKMeshBuffers, each with it's own offset.  If a GLKMeshBufferAllocator is used, Model I/O will attempt to load all vertex and indexData of a single model into a single zone.  So although there maybe many GLKMeshBuffers for a model they will be backed with the same contigous GL buffer.
--
-- ObjC selector: @- zone@
zone :: IsGLKMeshBuffer glkMeshBuffer => glkMeshBuffer -> IO RawId
zone glkMeshBuffer =
  sendMessage glkMeshBuffer zoneSelector

-- | type
--
-- the intended type of the buffer
--
-- ObjC selector: @- type@
type_ :: IsGLKMeshBuffer glkMeshBuffer => glkMeshBuffer -> IO MDLMeshBufferType
type_ glkMeshBuffer =
  sendMessage glkMeshBuffer typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @allocator@
allocatorSelector :: Selector '[] (Id GLKMeshBufferAllocator)
allocatorSelector = mkSelector "allocator"

-- | @Selector@ for @glBufferName@
glBufferNameSelector :: Selector '[] CUInt
glBufferNameSelector = mkSelector "glBufferName"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] RawId
zoneSelector = mkSelector "zone"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MDLMeshBufferType
typeSelector = mkSelector "type"

