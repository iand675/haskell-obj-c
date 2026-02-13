{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKMesh@.
module ObjC.GLKit.GLKMesh
  ( GLKMesh
  , IsGLKMesh(..)
  , init_
  , initWithMesh_error
  , newMeshesFromAsset_sourceMeshes_error
  , vertexCount
  , vertexBuffers
  , vertexDescriptor
  , submeshes
  , name
  , initSelector
  , initWithMesh_errorSelector
  , nameSelector
  , newMeshesFromAsset_sourceMeshes_errorSelector
  , submeshesSelector
  , vertexBuffersSelector
  , vertexCountSelector
  , vertexDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- | init
--
-- Must initialize with a mesh
--
-- ObjC selector: @- init@
init_ :: IsGLKMesh glkMesh => glkMesh -> IO (Id GLKMesh)
init_ glkMesh =
  sendOwnedMessage glkMesh initSelector

-- | initWithMesh:error:
--
-- Initialize the mesh and the mesh's submeshes
--
-- This does NOT initialize any meshes that are children of the Model I/O mesh  Pointer to an NSError object which will be set if an error occurred
--
-- @mesh@ — Model I/O Mesh from which to create this GLKit mesh
--
-- ObjC selector: @- initWithMesh:error:@
initWithMesh_error :: (IsGLKMesh glkMesh, IsMDLMesh mesh, IsNSError error_) => glkMesh -> mesh -> error_ -> IO (Id GLKMesh)
initWithMesh_error glkMesh mesh error_ =
  sendOwnedMessage glkMesh initWithMesh_errorSelector (toMDLMesh mesh) (toNSError error_)

-- | newMeshesFromAsset:sourceMeshes:error:
--
-- Initialize all meshes in a Model I/O asset.
--
-- Returns: An array of GLKit meshes built an asset
--
-- @asset@ — Model I/O asset from which to create GLKit meshes
--
-- @sourceMeshes@ — Model I/O meshes corresponding the newly created GLKMeshes
--
-- @error@ — Pointer to an NSError object set if an error occurred
--
-- @return@ — GLKit meshes created from the Model I/O asset
--
-- A convenience method to create GLKit meshes from each mesh in a Model/IO asset.  Resulting meshes are returned while Model I/O meshes from which they were generated will appear in the sourceMeshes array.
--
-- ObjC selector: @+ newMeshesFromAsset:sourceMeshes:error:@
newMeshesFromAsset_sourceMeshes_error :: (IsMDLAsset asset, IsNSArray sourceMeshes, IsNSError error_) => asset -> sourceMeshes -> error_ -> IO (Id NSArray)
newMeshesFromAsset_sourceMeshes_error asset sourceMeshes error_ =
  do
    cls' <- getRequiredClass "GLKMesh"
    sendOwnedClassMessage cls' newMeshesFromAsset_sourceMeshes_errorSelector (toMDLAsset asset) (toNSArray sourceMeshes) (toNSError error_)

-- | vertexCount
--
-- Number of verticies in the vertexBuffers
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsGLKMesh glkMesh => glkMesh -> IO CULong
vertexCount glkMesh =
  sendMessage glkMesh vertexCountSelector

-- | vertexBuffers
--
-- Array of buffers in which mesh vertex data resides
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsGLKMesh glkMesh => glkMesh -> IO (Id NSArray)
vertexBuffers glkMesh =
  sendMessage glkMesh vertexBuffersSelector

-- | vertexDescriptor
--
-- Model I/O vertex descriptor specifying the layout of data in vertexBuffers
--
-- This is not directly used by this object, but the application can use this information to determine rendering state or setup a vertex attribute object.
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsGLKMesh glkMesh => glkMesh -> IO (Id MDLVertexDescriptor)
vertexDescriptor glkMesh =
  sendMessage glkMesh vertexDescriptorSelector

-- | submeshes
--
-- Submeshes containing index buffers to rendering mesh verticies.
--
-- Submeshes may also contain texture materials to apply when rendering this object
--
-- ObjC selector: @- submeshes@
submeshes :: IsGLKMesh glkMesh => glkMesh -> IO (Id NSArray)
submeshes glkMesh =
  sendMessage glkMesh submeshesSelector

-- | name
--
-- Name of the mesh copies from the originating Model I/O mesh
--
-- Can be used by the app to identiry the mesh in it's scene/world/renderer etc.
--
-- ObjC selector: @- name@
name :: IsGLKMesh glkMesh => glkMesh -> IO (Id NSString)
name glkMesh =
  sendMessage glkMesh nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GLKMesh)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMesh:error:@
initWithMesh_errorSelector :: Selector '[Id MDLMesh, Id NSError] (Id GLKMesh)
initWithMesh_errorSelector = mkSelector "initWithMesh:error:"

-- | @Selector@ for @newMeshesFromAsset:sourceMeshes:error:@
newMeshesFromAsset_sourceMeshes_errorSelector :: Selector '[Id MDLAsset, Id NSArray, Id NSError] (Id NSArray)
newMeshesFromAsset_sourceMeshes_errorSelector = mkSelector "newMeshesFromAsset:sourceMeshes:error:"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector '[] CULong
vertexCountSelector = mkSelector "vertexCount"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector '[] (Id NSArray)
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector '[] (Id MDLVertexDescriptor)
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @submeshes@
submeshesSelector :: Selector '[] (Id NSArray)
submeshesSelector = mkSelector "submeshes"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

