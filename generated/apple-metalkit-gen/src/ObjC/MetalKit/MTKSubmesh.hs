{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTKSubmesh
--
-- A segment of a mesh and properties to render the segement.
--
-- Container for data that can be rendered in a single draw call. 1:1 mapping to MDLSubmesh.  Each submesh contains an index Buffer with which the parents mesh data can be rendered.  Actual vertex data resides in the submesh's parent MTKMesh object.
--
-- Generated bindings for @MTKSubmesh@.
module ObjC.MetalKit.MTKSubmesh
  ( MTKSubmesh
  , IsMTKSubmesh(..)
  , init_
  , primitiveType
  , indexType
  , indexBuffer
  , indexCount
  , mesh
  , name
  , setName
  , indexBufferSelector
  , indexCountSelector
  , indexTypeSelector
  , initSelector
  , meshSelector
  , nameSelector
  , primitiveTypeSelector
  , setNameSelector


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
-- Applicatiohs must not explicity allocate or initialize.  Must initialize as part of MTKMesh object.
--
-- ObjC selector: @- init@
init_ :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id MTKSubmesh)
init_ mtkSubmesh =
  sendOwnedMessage mtkSubmesh initSelector

-- | primitiveType
--
-- Metal primitive type with which to draw this object.
--
-- Value to use for primitiveType parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- primitiveType@
primitiveType :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO CInt
primitiveType mtkSubmesh =
  sendMessage mtkSubmesh primitiveTypeSelector

-- | indexType
--
-- Metal index type of data in indexBuffer.
--
-- Value to use for indexType parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- indexType@
indexType :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO CInt
indexType mtkSubmesh =
  sendMessage mtkSubmesh indexTypeSelector

-- | indexBuffer
--
-- IndexBuffer (including indexCount) to render the object.
--
-- The MTLBuffer to use for indexBuffer parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id MTKMeshBuffer)
indexBuffer mtkSubmesh =
  sendMessage mtkSubmesh indexBufferSelector

-- | indexCount
--
-- Number of indicies in indexBuffer.
--
-- Value to use for indexCount parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- indexCount@
indexCount :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO CULong
indexCount mtkSubmesh =
  sendMessage mtkSubmesh indexCountSelector

-- | mesh
--
-- Parent MTKMesh object containing vertex data of this object.
--
-- The buffer of this parent mesh should be set in the encoder before a drawIndexedPrimitives call is made.
--
-- ObjC selector: @- mesh@
mesh :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id MTKMesh)
mesh mtkSubmesh =
  sendMessage mtkSubmesh meshSelector

-- | name
--
-- Name from the original MDLSubmesh object.
--
-- Although not directly used by this object, the application may use this to identify the submesh in the renderer/scene/world.
--
-- ObjC selector: @- name@
name :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id NSString)
name mtkSubmesh =
  sendMessage mtkSubmesh nameSelector

-- | name
--
-- Name from the original MDLSubmesh object.
--
-- Although not directly used by this object, the application may use this to identify the submesh in the renderer/scene/world.
--
-- ObjC selector: @- setName:@
setName :: (IsMTKSubmesh mtkSubmesh, IsNSString value) => mtkSubmesh -> value -> IO ()
setName mtkSubmesh value =
  sendMessage mtkSubmesh setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTKSubmesh)
initSelector = mkSelector "init"

-- | @Selector@ for @primitiveType@
primitiveTypeSelector :: Selector '[] CInt
primitiveTypeSelector = mkSelector "primitiveType"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector '[] CInt
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector '[] (Id MTKMeshBuffer)
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @indexCount@
indexCountSelector :: Selector '[] CULong
indexCountSelector = mkSelector "indexCount"

-- | @Selector@ for @mesh@
meshSelector :: Selector '[] (Id MTKMesh)
meshSelector = mkSelector "mesh"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

