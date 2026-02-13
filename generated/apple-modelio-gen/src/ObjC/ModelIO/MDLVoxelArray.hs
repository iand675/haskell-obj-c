{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLVoxelArray
--
-- Voxel data represented on a three dimensional grid. Voxel data can          include voxels considered to be on the surface of an object, and a           series of shells on the outside and inside of the surface.
--
-- Generated bindings for @MDLVoxelArray@.
module ObjC.ModelIO.MDLVoxelArray
  ( MDLVoxelArray
  , IsMDLVoxelArray(..)
  , initWithAsset_divisions_patchRadius
  , initWithAsset_divisions_interiorShells_exteriorShells_patchRadius
  , initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadius
  , voxelIndices
  , setVoxelsForMesh_divisions_patchRadius
  , setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadius
  , setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadius
  , unionWithVoxels
  , intersectWithVoxels
  , differenceWithVoxels
  , convertToSignedShellField
  , coarseMesh
  , coarseMeshUsingAllocator
  , meshUsingAllocator
  , count
  , isValidSignedShellField
  , shellFieldInteriorThickness
  , setShellFieldInteriorThickness
  , shellFieldExteriorThickness
  , setShellFieldExteriorThickness
  , coarseMeshSelector
  , coarseMeshUsingAllocatorSelector
  , convertToSignedShellFieldSelector
  , countSelector
  , differenceWithVoxelsSelector
  , initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector
  , initWithAsset_divisions_interiorShells_exteriorShells_patchRadiusSelector
  , initWithAsset_divisions_patchRadiusSelector
  , intersectWithVoxelsSelector
  , isValidSignedShellFieldSelector
  , meshUsingAllocatorSelector
  , setShellFieldExteriorThicknessSelector
  , setShellFieldInteriorThicknessSelector
  , setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector
  , setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadiusSelector
  , setVoxelsForMesh_divisions_patchRadiusSelector
  , shellFieldExteriorThicknessSelector
  , shellFieldInteriorThicknessSelector
  , unionWithVoxelsSelector
  , voxelIndicesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a voxel grid from an MDLAsset. Attempts to create a closed volume model by applying "patches" of radius patchRadius to any holes found in the orginal mesh. Choose a patch radius that will be large enough to fill in the largest hole in the model.
--
-- ObjC selector: @- initWithAsset:divisions:patchRadius:@
initWithAsset_divisions_patchRadius :: (IsMDLVoxelArray mdlVoxelArray, IsMDLAsset asset) => mdlVoxelArray -> asset -> CInt -> CFloat -> IO (Id MDLVoxelArray)
initWithAsset_divisions_patchRadius mdlVoxelArray asset divisions patchRadius =
  sendOwnedMessage mdlVoxelArray initWithAsset_divisions_patchRadiusSelector (toMDLAsset asset) divisions patchRadius

-- | Initialize a voxel grid from an MDLAsset and dilate the resulting voxels by a number of interior and exterior shells. Routine will attempt to create a closed volume model by applying patches of a given radius to any holes it may find in the asset.
--
-- @divisions@ — The number of divisions to divide the vertical extent of the model by.
--
-- @interiorShells@ — The number of shells to compute inside the surface shell
--
-- @exteriorShells@ — The number of shells to compute outside the surface shell
--
-- @patchRadius@ — The radius of the largest model mending patch in world space units
--
-- ObjC selector: @- initWithAsset:divisions:interiorShells:exteriorShells:patchRadius:@
initWithAsset_divisions_interiorShells_exteriorShells_patchRadius :: (IsMDLVoxelArray mdlVoxelArray, IsMDLAsset asset) => mdlVoxelArray -> asset -> CInt -> CInt -> CInt -> CFloat -> IO (Id MDLVoxelArray)
initWithAsset_divisions_interiorShells_exteriorShells_patchRadius mdlVoxelArray asset divisions interiorShells exteriorShells patchRadius =
  sendOwnedMessage mdlVoxelArray initWithAsset_divisions_interiorShells_exteriorShells_patchRadiusSelector (toMDLAsset asset) divisions interiorShells exteriorShells patchRadius

-- | Initialize a voxel grid from an MDLAsset and dilate the resulting voxels by a spatial distance in the interior and exterior directions. Routine will attempt to create a closed volume model by applying "patches" of a given radius to any holes it may find in the asset.
--
-- @divisions@ — The number of divisions to divide the vertical extent of the model by.
--
-- @interiorNBWidth@ — The interior narrow band width in world space units
--
-- @exteriorNBWidth@ — The exterior narrow band width in world space units
--
-- @patchRadius@ — The radius of the largest model mending patch in world space units
--
-- ObjC selector: @- initWithAsset:divisions:interiorNBWidth:exteriorNBWidth:patchRadius:@
initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadius :: (IsMDLVoxelArray mdlVoxelArray, IsMDLAsset asset) => mdlVoxelArray -> asset -> CInt -> CFloat -> CFloat -> CFloat -> IO (Id MDLVoxelArray)
initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadius mdlVoxelArray asset divisions interiorNBWidth exteriorNBWidth patchRadius =
  sendOwnedMessage mdlVoxelArray initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector (toMDLAsset asset) divisions interiorNBWidth exteriorNBWidth patchRadius

-- | Returns an NSData containing the indices of all voxels in the voxel grid
--
-- ObjC selector: @- voxelIndices@
voxelIndices :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO (Id NSData)
voxelIndices mdlVoxelArray =
  sendMessage mdlVoxelArray voxelIndicesSelector

-- | Set voxels corresponding to a mesh. Routine will attempt to create a closed volume model by applying "patches" of a given radius to any holes it may find in the mesh.
--
-- ObjC selector: @- setVoxelsForMesh:divisions:patchRadius:@
setVoxelsForMesh_divisions_patchRadius :: (IsMDLVoxelArray mdlVoxelArray, IsMDLMesh mesh) => mdlVoxelArray -> mesh -> CInt -> CFloat -> IO ()
setVoxelsForMesh_divisions_patchRadius mdlVoxelArray mesh divisions patchRadius =
  sendMessage mdlVoxelArray setVoxelsForMesh_divisions_patchRadiusSelector (toMDLMesh mesh) divisions patchRadius

-- | Set voxels corresponding to a mesh Routine will attempt to create a closed volume model by applying "patches" of a given radius to any holes it may find in the mesh.
--
-- @divisions@ — The number of divisions to divide the vertical extent of the model by.
--
-- @interiorShells@ — The number of shells to compute inside the surface shell
--
-- @exteriorShells@ — The number of shells to compute outside the surface shell
--
-- @patchRadius@ — The radius of the largest model mending patch in world space units
--
-- ObjC selector: @- setVoxelsForMesh:divisions:interiorShells:exteriorShells:patchRadius:@
setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadius :: (IsMDLVoxelArray mdlVoxelArray, IsMDLMesh mesh) => mdlVoxelArray -> mesh -> CInt -> CInt -> CInt -> CFloat -> IO ()
setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadius mdlVoxelArray mesh divisions interiorShells exteriorShells patchRadius =
  sendMessage mdlVoxelArray setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadiusSelector (toMDLMesh mesh) divisions interiorShells exteriorShells patchRadius

-- | Set voxels corresponding to a mesh Routine will attempt to create a closed volume model by applying "patches" of a given radius to any holes it may find in the mesh.
--
-- @divisions@ — The number of divisions to divide the vertical extent of the model by.
--
-- @interiorNBWidth@ — The interior narrow band width in world space units
--
-- @exteriorNBWidth@ — The exterior narrow band width in world space units
--
-- @patchRadius@ — The radius of the largest model mending patch in world space units
--
-- ObjC selector: @- setVoxelsForMesh:divisions:interiorNBWidth:exteriorNBWidth:patchRadius:@
setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadius :: (IsMDLVoxelArray mdlVoxelArray, IsMDLMesh mesh) => mdlVoxelArray -> mesh -> CInt -> CFloat -> CFloat -> CFloat -> IO ()
setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadius mdlVoxelArray mesh divisions interiorNBWidth exteriorNBWidth patchRadius =
  sendMessage mdlVoxelArray setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector (toMDLMesh mesh) divisions interiorNBWidth exteriorNBWidth patchRadius

-- | Union modifies the voxel grid to be the merger with the supplied voxel grid. It is assumed that the spatial voxel extent of one voxel in the supplied grid is the same as that of the voxel grid. Note that the shell level data will be cleared.
--
-- ObjC selector: @- unionWithVoxels:@
unionWithVoxels :: (IsMDLVoxelArray mdlVoxelArray, IsMDLVoxelArray voxels) => mdlVoxelArray -> voxels -> IO ()
unionWithVoxels mdlVoxelArray voxels =
  sendMessage mdlVoxelArray unionWithVoxelsSelector (toMDLVoxelArray voxels)

-- | Intersection modifies the voxel grid so that only voxels that are also in the supplied voxel grid are retained. It is assumed that the spatial voxel extent of one voxel in the supplied grid is the same as that of the voxel grid. Note that the shell level data will be cleared.
--
-- ObjC selector: @- intersectWithVoxels:@
intersectWithVoxels :: (IsMDLVoxelArray mdlVoxelArray, IsMDLVoxelArray voxels) => mdlVoxelArray -> voxels -> IO ()
intersectWithVoxels mdlVoxelArray voxels =
  sendMessage mdlVoxelArray intersectWithVoxelsSelector (toMDLVoxelArray voxels)

-- | Difference modifies the voxel grid so that voxels also in the supplied voxel grid are removed. It is assumed that the spatial voxel extent of one voxel in the supplied grid is the same as that of the voxel grid. Note that the shell level data will be cleared.
--
-- ObjC selector: @- differenceWithVoxels:@
differenceWithVoxels :: (IsMDLVoxelArray mdlVoxelArray, IsMDLVoxelArray voxels) => mdlVoxelArray -> voxels -> IO ()
differenceWithVoxels mdlVoxelArray voxels =
  sendMessage mdlVoxelArray differenceWithVoxelsSelector (toMDLVoxelArray voxels)

-- | Converts volume grid into a signed shell field by surrounding the surface voxels, which have shell  level values of zero, by an inner layer of voxels with shell level values of negative one and an  outer layer of voxels with shell level values of positive one.
--
-- The volume model must be closed in order to generate a signed shell field.
--
-- ObjC selector: @- convertToSignedShellField@
convertToSignedShellField :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO ()
convertToSignedShellField mdlVoxelArray =
  sendMessage mdlVoxelArray convertToSignedShellFieldSelector

-- | Creates a coarse mesh from the voxel grid
--
-- ObjC selector: @- coarseMesh@
coarseMesh :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO (Id MDLMesh)
coarseMesh mdlVoxelArray =
  sendMessage mdlVoxelArray coarseMeshSelector

-- | @- coarseMeshUsingAllocator:@
coarseMeshUsingAllocator :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> RawId -> IO (Id MDLMesh)
coarseMeshUsingAllocator mdlVoxelArray allocator =
  sendMessage mdlVoxelArray coarseMeshUsingAllocatorSelector allocator

-- | Creates a smooth mesh from the voxel grid
--
-- ObjC selector: @- meshUsingAllocator:@
meshUsingAllocator :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> RawId -> IO (Id MDLMesh)
meshUsingAllocator mdlVoxelArray allocator =
  sendMessage mdlVoxelArray meshUsingAllocatorSelector allocator

-- | The number of voxels in the grid
--
-- ObjC selector: @- count@
count :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO CULong
count mdlVoxelArray =
  sendMessage mdlVoxelArray countSelector

-- | Returns whether or not the volume grid is in a valid signed shell field form.
--
-- This property will be set to YES after calling generateSignedShellField. All other  methods that modify the voxel grid will cause this property to be set to NO. Setting shellFieldInteriorThickness and shellFieldExteriorThickness will not affect the value of this property.
--
-- ObjC selector: @- isValidSignedShellField@
isValidSignedShellField :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO Bool
isValidSignedShellField mdlVoxelArray =
  sendMessage mdlVoxelArray isValidSignedShellFieldSelector

-- | If voxel grid is in a valid signed shell field form, sets the interior thickness to the desired width, as measured from the model surface. If the voxel grid is not in a valid signed shell field form, the value of this property is zero.
--
-- ObjC selector: @- shellFieldInteriorThickness@
shellFieldInteriorThickness :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO CFloat
shellFieldInteriorThickness mdlVoxelArray =
  sendMessage mdlVoxelArray shellFieldInteriorThicknessSelector

-- | If voxel grid is in a valid signed shell field form, sets the interior thickness to the desired width, as measured from the model surface. If the voxel grid is not in a valid signed shell field form, the value of this property is zero.
--
-- ObjC selector: @- setShellFieldInteriorThickness:@
setShellFieldInteriorThickness :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> CFloat -> IO ()
setShellFieldInteriorThickness mdlVoxelArray value =
  sendMessage mdlVoxelArray setShellFieldInteriorThicknessSelector value

-- | If voxel grid is in a valid signed shell field form, sets the exterior thickness to the desired width, as measured from the model surface. If the voxel grid is not in a valid signed shell field form, the value of this property is zero.
--
-- ObjC selector: @- shellFieldExteriorThickness@
shellFieldExteriorThickness :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> IO CFloat
shellFieldExteriorThickness mdlVoxelArray =
  sendMessage mdlVoxelArray shellFieldExteriorThicknessSelector

-- | If voxel grid is in a valid signed shell field form, sets the exterior thickness to the desired width, as measured from the model surface. If the voxel grid is not in a valid signed shell field form, the value of this property is zero.
--
-- ObjC selector: @- setShellFieldExteriorThickness:@
setShellFieldExteriorThickness :: IsMDLVoxelArray mdlVoxelArray => mdlVoxelArray -> CFloat -> IO ()
setShellFieldExteriorThickness mdlVoxelArray value =
  sendMessage mdlVoxelArray setShellFieldExteriorThicknessSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAsset:divisions:patchRadius:@
initWithAsset_divisions_patchRadiusSelector :: Selector '[Id MDLAsset, CInt, CFloat] (Id MDLVoxelArray)
initWithAsset_divisions_patchRadiusSelector = mkSelector "initWithAsset:divisions:patchRadius:"

-- | @Selector@ for @initWithAsset:divisions:interiorShells:exteriorShells:patchRadius:@
initWithAsset_divisions_interiorShells_exteriorShells_patchRadiusSelector :: Selector '[Id MDLAsset, CInt, CInt, CInt, CFloat] (Id MDLVoxelArray)
initWithAsset_divisions_interiorShells_exteriorShells_patchRadiusSelector = mkSelector "initWithAsset:divisions:interiorShells:exteriorShells:patchRadius:"

-- | @Selector@ for @initWithAsset:divisions:interiorNBWidth:exteriorNBWidth:patchRadius:@
initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector :: Selector '[Id MDLAsset, CInt, CFloat, CFloat, CFloat] (Id MDLVoxelArray)
initWithAsset_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector = mkSelector "initWithAsset:divisions:interiorNBWidth:exteriorNBWidth:patchRadius:"

-- | @Selector@ for @voxelIndices@
voxelIndicesSelector :: Selector '[] (Id NSData)
voxelIndicesSelector = mkSelector "voxelIndices"

-- | @Selector@ for @setVoxelsForMesh:divisions:patchRadius:@
setVoxelsForMesh_divisions_patchRadiusSelector :: Selector '[Id MDLMesh, CInt, CFloat] ()
setVoxelsForMesh_divisions_patchRadiusSelector = mkSelector "setVoxelsForMesh:divisions:patchRadius:"

-- | @Selector@ for @setVoxelsForMesh:divisions:interiorShells:exteriorShells:patchRadius:@
setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadiusSelector :: Selector '[Id MDLMesh, CInt, CInt, CInt, CFloat] ()
setVoxelsForMesh_divisions_interiorShells_exteriorShells_patchRadiusSelector = mkSelector "setVoxelsForMesh:divisions:interiorShells:exteriorShells:patchRadius:"

-- | @Selector@ for @setVoxelsForMesh:divisions:interiorNBWidth:exteriorNBWidth:patchRadius:@
setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector :: Selector '[Id MDLMesh, CInt, CFloat, CFloat, CFloat] ()
setVoxelsForMesh_divisions_interiorNBWidth_exteriorNBWidth_patchRadiusSelector = mkSelector "setVoxelsForMesh:divisions:interiorNBWidth:exteriorNBWidth:patchRadius:"

-- | @Selector@ for @unionWithVoxels:@
unionWithVoxelsSelector :: Selector '[Id MDLVoxelArray] ()
unionWithVoxelsSelector = mkSelector "unionWithVoxels:"

-- | @Selector@ for @intersectWithVoxels:@
intersectWithVoxelsSelector :: Selector '[Id MDLVoxelArray] ()
intersectWithVoxelsSelector = mkSelector "intersectWithVoxels:"

-- | @Selector@ for @differenceWithVoxels:@
differenceWithVoxelsSelector :: Selector '[Id MDLVoxelArray] ()
differenceWithVoxelsSelector = mkSelector "differenceWithVoxels:"

-- | @Selector@ for @convertToSignedShellField@
convertToSignedShellFieldSelector :: Selector '[] ()
convertToSignedShellFieldSelector = mkSelector "convertToSignedShellField"

-- | @Selector@ for @coarseMesh@
coarseMeshSelector :: Selector '[] (Id MDLMesh)
coarseMeshSelector = mkSelector "coarseMesh"

-- | @Selector@ for @coarseMeshUsingAllocator:@
coarseMeshUsingAllocatorSelector :: Selector '[RawId] (Id MDLMesh)
coarseMeshUsingAllocatorSelector = mkSelector "coarseMeshUsingAllocator:"

-- | @Selector@ for @meshUsingAllocator:@
meshUsingAllocatorSelector :: Selector '[RawId] (Id MDLMesh)
meshUsingAllocatorSelector = mkSelector "meshUsingAllocator:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @isValidSignedShellField@
isValidSignedShellFieldSelector :: Selector '[] Bool
isValidSignedShellFieldSelector = mkSelector "isValidSignedShellField"

-- | @Selector@ for @shellFieldInteriorThickness@
shellFieldInteriorThicknessSelector :: Selector '[] CFloat
shellFieldInteriorThicknessSelector = mkSelector "shellFieldInteriorThickness"

-- | @Selector@ for @setShellFieldInteriorThickness:@
setShellFieldInteriorThicknessSelector :: Selector '[CFloat] ()
setShellFieldInteriorThicknessSelector = mkSelector "setShellFieldInteriorThickness:"

-- | @Selector@ for @shellFieldExteriorThickness@
shellFieldExteriorThicknessSelector :: Selector '[] CFloat
shellFieldExteriorThicknessSelector = mkSelector "shellFieldExteriorThickness"

-- | @Selector@ for @setShellFieldExteriorThickness:@
setShellFieldExteriorThicknessSelector :: Selector '[CFloat] ()
setShellFieldExteriorThicknessSelector = mkSelector "setShellFieldExteriorThickness:"

