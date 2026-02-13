{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEShape
--
-- The physical representation of an object within the simulated acoustic scene.
--
-- Generated bindings for @PHASEShape@.
module ObjC.PHASE.PHASEShape
  ( PHASEShape
  , IsPHASEShape(..)
  , init_
  , new
  , initWithEngine_mesh
  , initWithEngine_mesh_materials
  , elements
  , elementsSelector
  , initSelector
  , initWithEngine_meshSelector
  , initWithEngine_mesh_materialsSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- | @- init@
init_ :: IsPHASEShape phaseShape => phaseShape -> IO (Id PHASEShape)
init_ phaseShape =
  sendOwnedMessage phaseShape initSelector

-- | @+ new@
new :: IO (Id PHASEShape)
new  =
  do
    cls' <- getRequiredClass "PHASEShape"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:mesh
--
-- Initialize a shape from a mesh.
--
-- One PHASEShapeElement will be created for every submesh within the mesh.
--
-- Note: A single shape can be used to create multiple instances of sources and occluders.        For example, a client could create a single shape for a window, then create multiple occluders from it.        The same can be done with with sources.
--
-- @engine@ — The engine this shape will be used with.
--
-- @mesh@ — A Model I/O mesh object.
--
-- Returns: A new shape object
--
-- ObjC selector: @- initWithEngine:mesh:@
initWithEngine_mesh :: (IsPHASEShape phaseShape, IsPHASEEngine engine, IsMDLMesh mesh) => phaseShape -> engine -> mesh -> IO (Id PHASEShape)
initWithEngine_mesh phaseShape engine mesh =
  sendOwnedMessage phaseShape initWithEngine_meshSelector (toPHASEEngine engine) (toMDLMesh mesh)

-- | initWithEngine:mesh
--
-- Initialize a shape from an MDLMesh and a list of materials
--
-- @engine@ — The engine this shape will be used with
--
-- @mesh@ — A Model I/O mesh object.
--
-- @materials@ — An array of PHASEMaterial objects that overrides any acoustical materials within the mesh object
--
-- Returns: A new shape object
--
-- The materials array cannot be empty and cannot contain nil entries, otherwise an exception is thrown.        If the number of submeshes within the mesh are less than or equal to the size of the material array, the material will be assigned        to the corresponding element. If the number of submeshes within the mesh is greater than the size of the material array, the material        assigned to the element will be the index of the element modulo the number of materials. IE: given a mesh with 6 submeshes and an array        of 3 materials, the element at index 5 will be assigned the material at index: 5 % 3 = 2.
--
-- ObjC selector: @- initWithEngine:mesh:materials:@
initWithEngine_mesh_materials :: (IsPHASEShape phaseShape, IsPHASEEngine engine, IsMDLMesh mesh, IsNSArray materials) => phaseShape -> engine -> mesh -> materials -> IO (Id PHASEShape)
initWithEngine_mesh_materials phaseShape engine mesh materials =
  sendOwnedMessage phaseShape initWithEngine_mesh_materialsSelector (toPHASEEngine engine) (toMDLMesh mesh) (toNSArray materials)

-- | elements
--
-- List of all the shape elements associated with this shape.
--
-- ObjC selector: @- elements@
elements :: IsPHASEShape phaseShape => phaseShape -> IO (Id NSArray)
elements phaseShape =
  sendMessage phaseShape elementsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEShape)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEShape)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:mesh:@
initWithEngine_meshSelector :: Selector '[Id PHASEEngine, Id MDLMesh] (Id PHASEShape)
initWithEngine_meshSelector = mkSelector "initWithEngine:mesh:"

-- | @Selector@ for @initWithEngine:mesh:materials:@
initWithEngine_mesh_materialsSelector :: Selector '[Id PHASEEngine, Id MDLMesh, Id NSArray] (Id PHASEShape)
initWithEngine_mesh_materialsSelector = mkSelector "initWithEngine:mesh:materials:"

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id NSArray)
elementsSelector = mkSelector "elements"

