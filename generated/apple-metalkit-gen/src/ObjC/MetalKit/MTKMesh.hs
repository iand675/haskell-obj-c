{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTKMesh
--
-- Container for vertex data of a mesh and submeshes to render it.
--
-- Generated bindings for @MTKMesh@.
module ObjC.MetalKit.MTKMesh
  ( MTKMesh
  , IsMTKMesh(..)
  , init_
  , initWithMesh_device_error
  , newMeshesFromAsset_device_sourceMeshes_error
  , vertexBuffers
  , vertexDescriptor
  , submeshes
  , vertexCount
  , name
  , setName
  , initSelector
  , initWithMesh_device_errorSelector
  , nameSelector
  , newMeshesFromAsset_device_sourceMeshes_errorSelector
  , setNameSelector
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

import ObjC.MetalKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- | init
--
-- Cannot use default init.  Must initialize with mesh and metal device.
--
-- ObjC selector: @- init@
init_ :: IsMTKMesh mtkMesh => mtkMesh -> IO (Id MTKMesh)
init_ mtkMesh =
  sendOwnedMessage mtkMesh initSelector

-- | initWithMesh:device:error:
--
-- Initialize the mesh and the mesh's submeshes.
--
-- @mesh@ — Model I/O Mesh from which to create this MetalKit mesh
--
-- @device@ — Metal device on which to create mesh resources
--
-- @error@ — Pointer to an NSError object set if an error occurred
--
-- The designated initializer for this class.  This does NOT initialize any meshes that are children of the Model I/O mesh, only submeshes that are part of the given mesh.  An exception is raised if vertexBuffer objects in the given mesh and the indexBuffer of any submesh in this mesh have not been created with a MTKMeshBufferAllocator object.  If a submesh using MDLGeometryTypeQuads or MDLGeometryTypeTopology is used, that submesh will be copied, and recreated to use MDLGeometryTypeTriangles, before this routine creates the MTKSubmesh.
--
-- ObjC selector: @- initWithMesh:device:error:@
initWithMesh_device_error :: (IsMTKMesh mtkMesh, IsMDLMesh mesh, IsNSError error_) => mtkMesh -> mesh -> RawId -> error_ -> IO (Id MTKMesh)
initWithMesh_device_error mtkMesh mesh device error_ =
  sendOwnedMessage mtkMesh initWithMesh_device_errorSelector (toMDLMesh mesh) device (toNSError error_)

-- | newMeshesFromAsset:device:sourceMeshes:error:
--
-- Initialize all meshes in a Model I/O asset.
--
-- @asset@ — Model I/O asset from which to create MetalKit meshes
--
-- @device@ — Metal device on which to create mesh resources
--
-- @sourceMeshes@ — Array built by this method containing MDLMesh objects corresponding the returned MTKMesh objects
--
-- @error@ — Pointer to an NSError object set if an error occurred
--
-- Returns: MetalKit Meshes created from the Model I/O asset
--
-- A convenience method to create MetalKit meshes from each mesh in a Model I/O asset.  resulting meshes are returned while the corresponding Model I/O meshes from which they were generated will appear in the sourceMeshes array.  All vertexBuffer objects in each MDLMesh object in the asset and the indexBuffer of each submesh within each of these meshes must have been created using a MTKMeshBufferAllocator object.  Thus
--
-- ObjC selector: @+ newMeshesFromAsset:device:sourceMeshes:error:@
newMeshesFromAsset_device_sourceMeshes_error :: (IsMDLAsset asset, IsNSArray sourceMeshes, IsNSError error_) => asset -> RawId -> sourceMeshes -> error_ -> IO (Id NSArray)
newMeshesFromAsset_device_sourceMeshes_error asset device sourceMeshes error_ =
  do
    cls' <- getRequiredClass "MTKMesh"
    sendOwnedClassMessage cls' newMeshesFromAsset_device_sourceMeshes_errorSelector (toMDLAsset asset) device (toNSArray sourceMeshes) (toNSError error_)

-- | vertexBuffers
--
-- Array of buffers in which mesh vertex data resides.
--
-- This is filled with mesh buffer objects using the layout described by the vertexDescriptor property.  Elements in this array can be [NSNull null] if the vertexDescriptor does not specify elements for buffer for the given index
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsMTKMesh mtkMesh => mtkMesh -> IO (Id NSArray)
vertexBuffers mtkMesh =
  sendMessage mtkMesh vertexBuffersSelector

-- | vertexDescriptor
--
-- Model I/O vertex descriptor specifying the layout of data in vertexBuffers.
--
-- This is not directly used by this object, but the application can use this information to determine rendering state or create a Metal vertex descriptor to build a RenderPipelineState object capable of interpreting data in 'vertexBuffers'.  Changing propties in the object will not result in the relayout data in vertex descriptor and thus will make the vertex descriptor no loger describe the layout of vertes data and verticies. (i.e. don't change properties in this vertexDescriptor)
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsMTKMesh mtkMesh => mtkMesh -> IO (Id MDLVertexDescriptor)
vertexDescriptor mtkMesh =
  sendMessage mtkMesh vertexDescriptorSelector

-- | submeshes
--
-- Submeshes containing index buffers to rendering mesh vertices.
--
-- ObjC selector: @- submeshes@
submeshes :: IsMTKMesh mtkMesh => mtkMesh -> IO (Id NSArray)
submeshes mtkMesh =
  sendMessage mtkMesh submeshesSelector

-- | vertexCount
--
-- Number of vertices in the vertexBuffers.
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsMTKMesh mtkMesh => mtkMesh -> IO CULong
vertexCount mtkMesh =
  sendMessage mtkMesh vertexCountSelector

-- | name
--
-- Name of the mesh copies from the originating Model I/O mesh.
--
-- Can be used by the app to identify the mesh in its scene/world/renderer etc.
--
-- ObjC selector: @- name@
name :: IsMTKMesh mtkMesh => mtkMesh -> IO (Id NSString)
name mtkMesh =
  sendMessage mtkMesh nameSelector

-- | name
--
-- Name of the mesh copies from the originating Model I/O mesh.
--
-- Can be used by the app to identify the mesh in its scene/world/renderer etc.
--
-- ObjC selector: @- setName:@
setName :: (IsMTKMesh mtkMesh, IsNSString value) => mtkMesh -> value -> IO ()
setName mtkMesh value =
  sendMessage mtkMesh setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTKMesh)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMesh:device:error:@
initWithMesh_device_errorSelector :: Selector '[Id MDLMesh, RawId, Id NSError] (Id MTKMesh)
initWithMesh_device_errorSelector = mkSelector "initWithMesh:device:error:"

-- | @Selector@ for @newMeshesFromAsset:device:sourceMeshes:error:@
newMeshesFromAsset_device_sourceMeshes_errorSelector :: Selector '[Id MDLAsset, RawId, Id NSArray, Id NSError] (Id NSArray)
newMeshesFromAsset_device_sourceMeshes_errorSelector = mkSelector "newMeshesFromAsset:device:sourceMeshes:error:"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector '[] (Id NSArray)
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector '[] (Id MDLVertexDescriptor)
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @submeshes@
submeshesSelector :: Selector '[] (Id NSArray)
submeshesSelector = mkSelector "submeshes"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector '[] CULong
vertexCountSelector = mkSelector "vertexCount"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

