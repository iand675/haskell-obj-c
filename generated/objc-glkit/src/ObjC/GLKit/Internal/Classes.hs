{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.GLKit.Internal.Classes (
    module ObjC.GLKit.Internal.Classes,
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

-- ---------- GLKBaseEffect ----------

-- | Phantom type for @GLKBaseEffect@.
data GLKBaseEffect

instance IsObjCObject (Id GLKBaseEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKBaseEffect"

class IsNSObject a => IsGLKBaseEffect a where
  toGLKBaseEffect :: a -> Id GLKBaseEffect

instance IsGLKBaseEffect (Id GLKBaseEffect) where
  toGLKBaseEffect = unsafeCastId

instance IsNSObject (Id GLKBaseEffect) where
  toNSObject = unsafeCastId

-- ---------- GLKEffectProperty ----------

-- | Phantom type for @GLKEffectProperty@.
data GLKEffectProperty

instance IsObjCObject (Id GLKEffectProperty) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKEffectProperty"

class IsNSObject a => IsGLKEffectProperty a where
  toGLKEffectProperty :: a -> Id GLKEffectProperty

instance IsGLKEffectProperty (Id GLKEffectProperty) where
  toGLKEffectProperty = unsafeCastId

instance IsNSObject (Id GLKEffectProperty) where
  toNSObject = unsafeCastId

-- ---------- GLKMesh ----------

-- | Phantom type for @GLKMesh@.
data GLKMesh

instance IsObjCObject (Id GLKMesh) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKMesh"

class IsNSObject a => IsGLKMesh a where
  toGLKMesh :: a -> Id GLKMesh

instance IsGLKMesh (Id GLKMesh) where
  toGLKMesh = unsafeCastId

instance IsNSObject (Id GLKMesh) where
  toNSObject = unsafeCastId

-- ---------- GLKMeshBuffer ----------

-- | GLKMeshBuffer
--
-- Mesh buffers created when  needs to allocate memory to back vertex or index data
--
-- Memory backing these buffer are OpenGL buffers. Model I/O will load index and vertex data from from a model asset directly in to the OpenGL buffer object.
-- 
-- Phantom type for @GLKMeshBuffer@.
data GLKMeshBuffer

instance IsObjCObject (Id GLKMeshBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKMeshBuffer"

class IsNSObject a => IsGLKMeshBuffer a where
  toGLKMeshBuffer :: a -> Id GLKMeshBuffer

instance IsGLKMeshBuffer (Id GLKMeshBuffer) where
  toGLKMeshBuffer = unsafeCastId

instance IsNSObject (Id GLKMeshBuffer) where
  toNSObject = unsafeCastId

-- ---------- GLKMeshBufferAllocator ----------

-- | GLKMeshBufferAllocator
--
-- Allocator passed to MDLAsset init method to load vertex and index data directly into OpenGL buffer object
-- 
-- Phantom type for @GLKMeshBufferAllocator@.
data GLKMeshBufferAllocator

instance IsObjCObject (Id GLKMeshBufferAllocator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKMeshBufferAllocator"

class IsNSObject a => IsGLKMeshBufferAllocator a where
  toGLKMeshBufferAllocator :: a -> Id GLKMeshBufferAllocator

instance IsGLKMeshBufferAllocator (Id GLKMeshBufferAllocator) where
  toGLKMeshBufferAllocator = unsafeCastId

instance IsNSObject (Id GLKMeshBufferAllocator) where
  toNSObject = unsafeCastId

-- ---------- GLKSkyboxEffect ----------

-- | Phantom type for @GLKSkyboxEffect@.
data GLKSkyboxEffect

instance IsObjCObject (Id GLKSkyboxEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKSkyboxEffect"

class IsNSObject a => IsGLKSkyboxEffect a where
  toGLKSkyboxEffect :: a -> Id GLKSkyboxEffect

instance IsGLKSkyboxEffect (Id GLKSkyboxEffect) where
  toGLKSkyboxEffect = unsafeCastId

instance IsNSObject (Id GLKSkyboxEffect) where
  toNSObject = unsafeCastId

-- ---------- GLKSubmesh ----------

-- | Phantom type for @GLKSubmesh@.
data GLKSubmesh

instance IsObjCObject (Id GLKSubmesh) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKSubmesh"

class IsNSObject a => IsGLKSubmesh a where
  toGLKSubmesh :: a -> Id GLKSubmesh

instance IsGLKSubmesh (Id GLKSubmesh) where
  toGLKSubmesh = unsafeCastId

instance IsNSObject (Id GLKSubmesh) where
  toNSObject = unsafeCastId

-- ---------- GLKTextureInfo ----------

-- | Phantom type for @GLKTextureInfo@.
data GLKTextureInfo

instance IsObjCObject (Id GLKTextureInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKTextureInfo"

class IsNSObject a => IsGLKTextureInfo a where
  toGLKTextureInfo :: a -> Id GLKTextureInfo

instance IsGLKTextureInfo (Id GLKTextureInfo) where
  toGLKTextureInfo = unsafeCastId

instance IsNSObject (Id GLKTextureInfo) where
  toNSObject = unsafeCastId

-- ---------- GLKTextureLoader ----------

-- | Phantom type for @GLKTextureLoader@.
data GLKTextureLoader

instance IsObjCObject (Id GLKTextureLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKTextureLoader"

class IsNSObject a => IsGLKTextureLoader a where
  toGLKTextureLoader :: a -> Id GLKTextureLoader

instance IsGLKTextureLoader (Id GLKTextureLoader) where
  toGLKTextureLoader = unsafeCastId

instance IsNSObject (Id GLKTextureLoader) where
  toNSObject = unsafeCastId

-- ---------- GLKReflectionMapEffect ----------

-- | Phantom type for @GLKReflectionMapEffect@.
data GLKReflectionMapEffect

instance IsObjCObject (Id GLKReflectionMapEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKReflectionMapEffect"

class IsGLKBaseEffect a => IsGLKReflectionMapEffect a where
  toGLKReflectionMapEffect :: a -> Id GLKReflectionMapEffect

instance IsGLKReflectionMapEffect (Id GLKReflectionMapEffect) where
  toGLKReflectionMapEffect = unsafeCastId

instance IsGLKBaseEffect (Id GLKReflectionMapEffect) where
  toGLKBaseEffect = unsafeCastId

instance IsNSObject (Id GLKReflectionMapEffect) where
  toNSObject = unsafeCastId

-- ---------- GLKEffectPropertyFog ----------

-- | Phantom type for @GLKEffectPropertyFog@.
data GLKEffectPropertyFog

instance IsObjCObject (Id GLKEffectPropertyFog) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKEffectPropertyFog"

class IsGLKEffectProperty a => IsGLKEffectPropertyFog a where
  toGLKEffectPropertyFog :: a -> Id GLKEffectPropertyFog

instance IsGLKEffectPropertyFog (Id GLKEffectPropertyFog) where
  toGLKEffectPropertyFog = unsafeCastId

instance IsGLKEffectProperty (Id GLKEffectPropertyFog) where
  toGLKEffectProperty = unsafeCastId

instance IsNSObject (Id GLKEffectPropertyFog) where
  toNSObject = unsafeCastId

-- ---------- GLKEffectPropertyLight ----------

-- | Phantom type for @GLKEffectPropertyLight@.
data GLKEffectPropertyLight

instance IsObjCObject (Id GLKEffectPropertyLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKEffectPropertyLight"

class IsGLKEffectProperty a => IsGLKEffectPropertyLight a where
  toGLKEffectPropertyLight :: a -> Id GLKEffectPropertyLight

instance IsGLKEffectPropertyLight (Id GLKEffectPropertyLight) where
  toGLKEffectPropertyLight = unsafeCastId

instance IsGLKEffectProperty (Id GLKEffectPropertyLight) where
  toGLKEffectProperty = unsafeCastId

instance IsNSObject (Id GLKEffectPropertyLight) where
  toNSObject = unsafeCastId

-- ---------- GLKEffectPropertyMaterial ----------

-- | Phantom type for @GLKEffectPropertyMaterial@.
data GLKEffectPropertyMaterial

instance IsObjCObject (Id GLKEffectPropertyMaterial) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKEffectPropertyMaterial"

class IsGLKEffectProperty a => IsGLKEffectPropertyMaterial a where
  toGLKEffectPropertyMaterial :: a -> Id GLKEffectPropertyMaterial

instance IsGLKEffectPropertyMaterial (Id GLKEffectPropertyMaterial) where
  toGLKEffectPropertyMaterial = unsafeCastId

instance IsGLKEffectProperty (Id GLKEffectPropertyMaterial) where
  toGLKEffectProperty = unsafeCastId

instance IsNSObject (Id GLKEffectPropertyMaterial) where
  toNSObject = unsafeCastId

-- ---------- GLKEffectPropertyTexture ----------

-- | Phantom type for @GLKEffectPropertyTexture@.
data GLKEffectPropertyTexture

instance IsObjCObject (Id GLKEffectPropertyTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKEffectPropertyTexture"

class IsGLKEffectProperty a => IsGLKEffectPropertyTexture a where
  toGLKEffectPropertyTexture :: a -> Id GLKEffectPropertyTexture

instance IsGLKEffectPropertyTexture (Id GLKEffectPropertyTexture) where
  toGLKEffectPropertyTexture = unsafeCastId

instance IsGLKEffectProperty (Id GLKEffectPropertyTexture) where
  toGLKEffectProperty = unsafeCastId

instance IsNSObject (Id GLKEffectPropertyTexture) where
  toNSObject = unsafeCastId

-- ---------- GLKEffectPropertyTransform ----------

-- | Phantom type for @GLKEffectPropertyTransform@.
data GLKEffectPropertyTransform

instance IsObjCObject (Id GLKEffectPropertyTransform) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GLKEffectPropertyTransform"

class IsGLKEffectProperty a => IsGLKEffectPropertyTransform a where
  toGLKEffectPropertyTransform :: a -> Id GLKEffectPropertyTransform

instance IsGLKEffectPropertyTransform (Id GLKEffectPropertyTransform) where
  toGLKEffectPropertyTransform = unsafeCastId

instance IsGLKEffectProperty (Id GLKEffectPropertyTransform) where
  toGLKEffectProperty = unsafeCastId

instance IsNSObject (Id GLKEffectPropertyTransform) where
  toNSObject = unsafeCastId
