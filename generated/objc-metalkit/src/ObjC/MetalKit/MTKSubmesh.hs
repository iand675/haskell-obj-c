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
  , initSelector
  , primitiveTypeSelector
  , indexTypeSelector
  , indexBufferSelector
  , indexCountSelector
  , meshSelector
  , nameSelector
  , setNameSelector


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
-- Applicatiohs must not explicity allocate or initialize.  Must initialize as part of MTKMesh object.
--
-- ObjC selector: @- init@
init_ :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id MTKSubmesh)
init_ mtkSubmesh  =
  sendMsg mtkSubmesh (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | primitiveType
--
-- Metal primitive type with which to draw this object.
--
-- Value to use for primitiveType parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- primitiveType@
primitiveType :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO MTLPrimitiveType
primitiveType mtkSubmesh  =
  fmap (coerce :: CULong -> MTLPrimitiveType) $ sendMsg mtkSubmesh (mkSelector "primitiveType") retCULong []

-- | indexType
--
-- Metal index type of data in indexBuffer.
--
-- Value to use for indexType parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- indexType@
indexType :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO MTLIndexType
indexType mtkSubmesh  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtkSubmesh (mkSelector "indexType") retCULong []

-- | indexBuffer
--
-- IndexBuffer (including indexCount) to render the object.
--
-- The MTLBuffer to use for indexBuffer parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id MTKMeshBuffer)
indexBuffer mtkSubmesh  =
  sendMsg mtkSubmesh (mkSelector "indexBuffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | indexCount
--
-- Number of indicies in indexBuffer.
--
-- Value to use for indexCount parameter in a [MTLRenderCommandEncoder drawIndexedPrimitives] call.
--
-- ObjC selector: @- indexCount@
indexCount :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO CULong
indexCount mtkSubmesh  =
  sendMsg mtkSubmesh (mkSelector "indexCount") retCULong []

-- | mesh
--
-- Parent MTKMesh object containing vertex data of this object.
--
-- The buffer of this parent mesh should be set in the encoder before a drawIndexedPrimitives call is made.
--
-- ObjC selector: @- mesh@
mesh :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id MTKMesh)
mesh mtkSubmesh  =
  sendMsg mtkSubmesh (mkSelector "mesh") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Name from the original MDLSubmesh object.
--
-- Although not directly used by this object, the application may use this to identify the submesh in the renderer/scene/world.
--
-- ObjC selector: @- name@
name :: IsMTKSubmesh mtkSubmesh => mtkSubmesh -> IO (Id NSString)
name mtkSubmesh  =
  sendMsg mtkSubmesh (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Name from the original MDLSubmesh object.
--
-- Although not directly used by this object, the application may use this to identify the submesh in the renderer/scene/world.
--
-- ObjC selector: @- setName:@
setName :: (IsMTKSubmesh mtkSubmesh, IsNSString value) => mtkSubmesh -> value -> IO ()
setName mtkSubmesh  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtkSubmesh (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @primitiveType@
primitiveTypeSelector :: Selector
primitiveTypeSelector = mkSelector "primitiveType"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @indexCount@
indexCountSelector :: Selector
indexCountSelector = mkSelector "indexCount"

-- | @Selector@ for @mesh@
meshSelector :: Selector
meshSelector = mkSelector "mesh"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

