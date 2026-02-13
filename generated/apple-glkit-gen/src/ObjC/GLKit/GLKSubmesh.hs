{-# LANGUAGE DataKinds #-}
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
  , elementBufferSelector
  , elementCountSelector
  , initSelector
  , meshSelector
  , modeSelector
  , nameSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
init_ glkSubmesh =
  sendOwnedMessage glkSubmesh initSelector

-- | type
--
-- Type of data in the elementBuffer (aka indexBuffer)
--
-- This value should be used for the type parameter of glDrawElements
--
-- ObjC selector: @- type@
type_ :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO CUInt
type_ glkSubmesh =
  sendMessage glkSubmesh typeSelector

-- | mode
--
-- Primitive type mode value of data in the elementBuffer (aka indexBuffer)
--
-- This value should be used for the mode parameter in glDrawElements
--
-- ObjC selector: @- mode@
mode :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO CUInt
mode glkSubmesh =
  sendMessage glkSubmesh modeSelector

-- | elementCount
--
-- Number of elements (aka indicies) in the elementBuffer (aka indexBuffer)
--
-- This value should be used for the count parameter in glDrawElements
--
-- ObjC selector: @- elementCount@
elementCount :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO CInt
elementCount glkSubmesh =
  sendMessage glkSubmesh elementCountSelector

-- | elementBuffer
--
-- Name of buffer object with index data
--
-- The buffer name to be used with DrawElements
--
-- ObjC selector: @- elementBuffer@
elementBuffer :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id GLKMeshBuffer)
elementBuffer glkSubmesh =
  sendMessage glkSubmesh elementBufferSelector

-- | mesh
--
-- Parent GLKit mesh containing vertex data of this object
--
-- Buffer of this parent mesh should be set in the encoder before a drawIndexedPrimitives call is made
--
-- ObjC selector: @- mesh@
mesh :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id GLKMesh)
mesh glkSubmesh =
  sendMessage glkSubmesh meshSelector

-- | name
--
-- Name from the original MDLSubmesh object.
--
-- Although not directly used by this object, the application may use this to identify the submesh in it renderer/scene/world.
--
-- ObjC selector: @- name@
name :: IsGLKSubmesh glkSubmesh => glkSubmesh -> IO (Id NSString)
name glkSubmesh =
  sendMessage glkSubmesh nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GLKSubmesh)
initSelector = mkSelector "init"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CUInt
typeSelector = mkSelector "type"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] CUInt
modeSelector = mkSelector "mode"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CInt
elementCountSelector = mkSelector "elementCount"

-- | @Selector@ for @elementBuffer@
elementBufferSelector :: Selector '[] (Id GLKMeshBuffer)
elementBufferSelector = mkSelector "elementBuffer"

-- | @Selector@ for @mesh@
meshSelector :: Selector '[] (Id GLKMesh)
meshSelector = mkSelector "mesh"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

