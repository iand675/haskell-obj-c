{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKSubmesh@.
module ObjC.GLKit.GLKSubmesh
  ( GLKSubmesh
  , IsGLKSubmesh(..)
  , init_
  , type_
  , mode
  , elementCount
  , elementBuffer
  , mesh
  , name
  , initSelector
  , typeSelector
  , modeSelector
  , elementCountSelector
  , elementBufferSelector
  , meshSelector
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

-- | init
--
-- Must be initialized by a GLKMesh object
--
-- ObjC selector: @- init@
init_ :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id GLKSubmesh)
init_ glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | type
--
-- Type of data in the elementBuffer (aka indexBuffer)
--
-- This value should be used for the type parameter of glDrawElements
--
-- ObjC selector: @- type@
type_ :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO CUInt
type_ glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "type") retCUInt []

-- | mode
--
-- Primitive type mode value of data in the elementBuffer (aka indexBuffer)
--
-- This value should be used for the mode parameter in glDrawElements
--
-- ObjC selector: @- mode@
mode :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO CUInt
mode glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "mode") retCUInt []

-- | elementCount
--
-- Number of elements (aka indicies) in the elementBuffer (aka indexBuffer)
--
-- This value should be used for the count parameter in glDrawElements
--
-- ObjC selector: @- elementCount@
elementCount :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO CInt
elementCount glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "elementCount") retCInt []

-- | elementBuffer
--
-- Name of buffer object with index data
--
-- The buffer name to be used with DrawElements
--
-- ObjC selector: @- elementBuffer@
elementBuffer :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id GLKMeshBuffer)
elementBuffer glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "elementBuffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mesh
--
-- Parent GLKit mesh containing vertex data of this object
--
-- Buffer of this parent mesh should be set in the encoder before a drawIndexedPrimitives call is made
--
-- ObjC selector: @- mesh@
mesh :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id GLKMesh)
mesh glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "mesh") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Name from the original MDLSubmesh object.
--
-- Although not directly used by this object, the application may use this to identify the submesh in it renderer/scene/world.
--
-- ObjC selector: @- name@
name :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id NSString)
name glkSubmesh  =
  sendMsg glkSubmesh (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

-- | @Selector@ for @elementBuffer@
elementBufferSelector :: Selector
elementBufferSelector = mkSelector "elementBuffer"

-- | @Selector@ for @mesh@
meshSelector :: Selector
meshSelector = mkSelector "mesh"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

