{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MetalKit.Internal.Classes (
    module ObjC.MetalKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.ModelIO.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- ---------- MTKMesh ----------

-- | MTKMesh
--
-- Container for vertex data of a mesh and submeshes to render it.
-- 
-- Phantom type for @MTKMesh@.
data MTKMesh

instance IsObjCObject (Id MTKMesh) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTKMesh"

class IsNSObject a => IsMTKMesh a where
  toMTKMesh :: a -> Id MTKMesh

instance IsMTKMesh (Id MTKMesh) where
  toMTKMesh = unsafeCastId

instance IsNSObject (Id MTKMesh) where
  toNSObject = unsafeCastId

-- ---------- MTKMeshBuffer ----------

-- | MTKMeshBuffer
--
-- Mesh buffer created by MTKMeshBufferAllocator when Model I/O needs to memory for vertex or index data backing.
--
-- Memory backing these buffer are Metal buffers.  Model I/O will load index and vertex data from from a model asset directly in to the Metal buffer.
-- 
-- Phantom type for @MTKMeshBuffer@.
data MTKMeshBuffer

instance IsObjCObject (Id MTKMeshBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTKMeshBuffer"

class IsNSObject a => IsMTKMeshBuffer a where
  toMTKMeshBuffer :: a -> Id MTKMeshBuffer

instance IsMTKMeshBuffer (Id MTKMeshBuffer) where
  toMTKMeshBuffer = unsafeCastId

instance IsNSObject (Id MTKMeshBuffer) where
  toNSObject = unsafeCastId

-- ---------- MTKMeshBufferAllocator ----------

-- | MTKMeshBufferAllocator
--
-- Allocator passed to MDLAsset init method to load vertex and index data directly into Metal buffers.
-- 
-- Phantom type for @MTKMeshBufferAllocator@.
data MTKMeshBufferAllocator

instance IsObjCObject (Id MTKMeshBufferAllocator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTKMeshBufferAllocator"

class IsNSObject a => IsMTKMeshBufferAllocator a where
  toMTKMeshBufferAllocator :: a -> Id MTKMeshBufferAllocator

instance IsMTKMeshBufferAllocator (Id MTKMeshBufferAllocator) where
  toMTKMeshBufferAllocator = unsafeCastId

instance IsNSObject (Id MTKMeshBufferAllocator) where
  toNSObject = unsafeCastId

-- ---------- MTKSubmesh ----------

-- | MTKSubmesh
--
-- A segment of a mesh and properties to render the segement.
--
-- Container for data that can be rendered in a single draw call. 1:1 mapping to MDLSubmesh.  Each submesh contains an index Buffer with which the parents mesh data can be rendered.  Actual vertex data resides in the submesh's parent MTKMesh object.
-- 
-- Phantom type for @MTKSubmesh@.
data MTKSubmesh

instance IsObjCObject (Id MTKSubmesh) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTKSubmesh"

class IsNSObject a => IsMTKSubmesh a where
  toMTKSubmesh :: a -> Id MTKSubmesh

instance IsMTKSubmesh (Id MTKSubmesh) where
  toMTKSubmesh = unsafeCastId

instance IsNSObject (Id MTKSubmesh) where
  toNSObject = unsafeCastId

-- ---------- MTKTextureLoader ----------

-- | MTKTextureLoader
--
-- Load Metal textures from files with the device specified at initialization
-- 
-- Phantom type for @MTKTextureLoader@.
data MTKTextureLoader

instance IsObjCObject (Id MTKTextureLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTKTextureLoader"

class IsNSObject a => IsMTKTextureLoader a where
  toMTKTextureLoader :: a -> Id MTKTextureLoader

instance IsMTKTextureLoader (Id MTKTextureLoader) where
  toMTKTextureLoader = unsafeCastId

instance IsNSObject (Id MTKTextureLoader) where
  toNSObject = unsafeCastId

-- ---------- MTKView ----------

-- | MTKView
--
-- View for rendering metal content
-- 
-- Phantom type for @MTKView@.
data MTKView

instance IsObjCObject (Id MTKView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTKView"

class IsNSView a => IsMTKView a where
  toMTKView :: a -> Id MTKView

instance IsMTKView (Id MTKView) where
  toMTKView = unsafeCastId

instance IsNSObject (Id MTKView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MTKView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MTKView) where
  toNSView = unsafeCastId
