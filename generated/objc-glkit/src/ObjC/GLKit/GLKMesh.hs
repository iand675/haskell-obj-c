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
  , newMeshesFromAsset_sourceMeshes_errorSelector
  , vertexCountSelector
  , vertexBuffersSelector
  , vertexDescriptorSelector
  , submeshesSelector
  , nameSelector


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

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- | init
--
-- Must initialize with a mesh
--
-- ObjC selector: @- init@
init_ :: IsGLKMesh glkMesh => glkMesh -> IO (Id GLKMesh)
init_ glkMesh  =
  sendMsg glkMesh (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithMesh_error glkMesh  mesh error_ =
withObjCPtr mesh $ \raw_mesh ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg glkMesh (mkSelector "initWithMesh:error:") (retPtr retVoid) [argPtr (castPtr raw_mesh :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr sourceMeshes $ \raw_sourceMeshes ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "newMeshesFromAsset:sourceMeshes:error:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_sourceMeshes :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | vertexCount
--
-- Number of verticies in the vertexBuffers
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsGLKMesh glkMesh => glkMesh -> IO CULong
vertexCount glkMesh  =
  sendMsg glkMesh (mkSelector "vertexCount") retCULong []

-- | vertexBuffers
--
-- Array of buffers in which mesh vertex data resides
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsGLKMesh glkMesh => glkMesh -> IO (Id NSArray)
vertexBuffers glkMesh  =
  sendMsg glkMesh (mkSelector "vertexBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexDescriptor
--
-- Model I/O vertex descriptor specifying the layout of data in vertexBuffers
--
-- This is not directly used by this object, but the application can use this information to determine rendering state or setup a vertex attribute object.
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsGLKMesh glkMesh => glkMesh -> IO (Id MDLVertexDescriptor)
vertexDescriptor glkMesh  =
  sendMsg glkMesh (mkSelector "vertexDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | submeshes
--
-- Submeshes containing index buffers to rendering mesh verticies.
--
-- Submeshes may also contain texture materials to apply when rendering this object
--
-- ObjC selector: @- submeshes@
submeshes :: IsGLKMesh glkMesh => glkMesh -> IO (Id NSArray)
submeshes glkMesh  =
  sendMsg glkMesh (mkSelector "submeshes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Name of the mesh copies from the originating Model I/O mesh
--
-- Can be used by the app to identiry the mesh in it's scene/world/renderer etc.
--
-- ObjC selector: @- name@
name :: IsGLKMesh glkMesh => glkMesh -> IO (Id NSString)
name glkMesh  =
  sendMsg glkMesh (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMesh:error:@
initWithMesh_errorSelector :: Selector
initWithMesh_errorSelector = mkSelector "initWithMesh:error:"

-- | @Selector@ for @newMeshesFromAsset:sourceMeshes:error:@
newMeshesFromAsset_sourceMeshes_errorSelector :: Selector
newMeshesFromAsset_sourceMeshes_errorSelector = mkSelector "newMeshesFromAsset:sourceMeshes:error:"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector
vertexCountSelector = mkSelector "vertexCount"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @submeshes@
submeshesSelector :: Selector
submeshesSelector = mkSelector "submeshes"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

