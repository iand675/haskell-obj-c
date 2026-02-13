{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLSubmesh
--
-- A drawable subset of an MDLMesh, with its own material
--
-- Generated bindings for @MDLSubmesh@.
module ObjC.ModelIO.MDLSubmesh
  ( MDLSubmesh
  , IsMDLSubmesh(..)
  , initWithName_indexBuffer_indexCount_indexType_geometryType_material
  , initWithIndexBuffer_indexCount_indexType_geometryType_material
  , initWithName_indexBuffer_indexCount_indexType_geometryType_material_topology
  , initWithMDLSubmesh_indexType_geometryType
  , indexBufferAsIndexType
  , indexBuffer
  , indexCount
  , indexType
  , geometryType
  , material
  , setMaterial
  , topology
  , setTopology
  , name
  , setName
  , geometryTypeSelector
  , indexBufferAsIndexTypeSelector
  , indexBufferSelector
  , indexCountSelector
  , indexTypeSelector
  , initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector
  , initWithMDLSubmesh_indexType_geometryTypeSelector
  , initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector
  , initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector
  , materialSelector
  , nameSelector
  , setMaterialSelector
  , setNameSelector
  , setTopologySelector
  , topologySelector

  -- * Enum types
  , MDLGeometryType(MDLGeometryType)
  , pattern MDLGeometryTypePoints
  , pattern MDLGeometryTypeLines
  , pattern MDLGeometryTypeTriangles
  , pattern MDLGeometryTypeTriangleStrips
  , pattern MDLGeometryTypeQuads
  , pattern MDLGeometryTypeVariableTopology
  , MDLIndexBitDepth(MDLIndexBitDepth)
  , pattern MDLIndexBitDepthInvalid
  , pattern MDLIndexBitDepthUInt8
  , pattern MDLIndexBitDepthUint8
  , pattern MDLIndexBitDepthUInt16
  , pattern MDLIndexBitDepthUint16
  , pattern MDLIndexBitDepthUInt32
  , pattern MDLIndexBitDepthUint32

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithName:indexBuffer:indexCount:indexType:geometryType:material:
--
-- Initialize submesh with all data necessary to make properties valid
--
-- ObjC selector: @- initWithName:indexBuffer:indexCount:indexType:geometryType:material:@
initWithName_indexBuffer_indexCount_indexType_geometryType_material :: (IsMDLSubmesh mdlSubmesh, IsNSString name, IsMDLMaterial material) => mdlSubmesh -> name -> RawId -> CULong -> MDLIndexBitDepth -> MDLGeometryType -> material -> IO (Id MDLSubmesh)
initWithName_indexBuffer_indexCount_indexType_geometryType_material mdlSubmesh name indexBuffer indexCount indexType geometryType material =
  sendOwnedMessage mdlSubmesh initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector (toNSString name) indexBuffer indexCount indexType geometryType (toMDLMaterial material)

-- | initWithIndexBuffer:indexCount:indexType:geometryType:material:
--
-- Initialize submesh with all data necessary to make properties valid
--
-- ObjC selector: @- initWithIndexBuffer:indexCount:indexType:geometryType:material:@
initWithIndexBuffer_indexCount_indexType_geometryType_material :: (IsMDLSubmesh mdlSubmesh, IsMDLMaterial material) => mdlSubmesh -> RawId -> CULong -> MDLIndexBitDepth -> MDLGeometryType -> material -> IO (Id MDLSubmesh)
initWithIndexBuffer_indexCount_indexType_geometryType_material mdlSubmesh indexBuffer indexCount indexType geometryType material =
  sendOwnedMessage mdlSubmesh initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector indexBuffer indexCount indexType geometryType (toMDLMaterial material)

-- | initWithIndexBuffer:indexCount:indexType:faceTopologyBuffer:geometryType:material:
--
-- Initialize submesh with all data necessary to make properties valid
--
-- The geometry type will typically be MDLGeometryTypeVariableTopology,             if other types are used the faceTopologyBuffer contents should             reflect that.
--
-- ObjC selector: @- initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:@
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topology :: (IsMDLSubmesh mdlSubmesh, IsNSString name, IsMDLMaterial material, IsMDLSubmeshTopology topology) => mdlSubmesh -> name -> RawId -> CULong -> MDLIndexBitDepth -> MDLGeometryType -> material -> topology -> IO (Id MDLSubmesh)
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topology mdlSubmesh name indexBuffer indexCount indexType geometryType material topology =
  sendOwnedMessage mdlSubmesh initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector (toNSString name) indexBuffer indexCount indexType geometryType (toMDLMaterial material) (toMDLSubmeshTopology topology)

-- | initWithMDLSubmesh:indexType:geometryType:
--
-- Initialize submesh using another submesh as input.
--
-- the resulting submesh will have a new index type if necessary.  If a conversion from the source submesh's geometry type to the requested  geometry type is possible, conversion will be performed. Otherwise nil will  be returned.
--
-- ObjC selector: @- initWithMDLSubmesh:indexType:geometryType:@
initWithMDLSubmesh_indexType_geometryType :: (IsMDLSubmesh mdlSubmesh, IsMDLSubmesh submesh) => mdlSubmesh -> submesh -> MDLIndexBitDepth -> MDLGeometryType -> IO (Id MDLSubmesh)
initWithMDLSubmesh_indexType_geometryType mdlSubmesh submesh indexType geometryType =
  sendOwnedMessage mdlSubmesh initWithMDLSubmesh_indexType_geometryTypeSelector (toMDLSubmesh submesh) indexType geometryType

-- | @- indexBufferAsIndexType:@
indexBufferAsIndexType :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> MDLIndexBitDepth -> IO RawId
indexBufferAsIndexType mdlSubmesh indexType =
  sendMessage mdlSubmesh indexBufferAsIndexTypeSelector indexType

-- | indexBuffer
--
-- Index data referencing vertex data in parent mesh
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO RawId
indexBuffer mdlSubmesh =
  sendMessage mdlSubmesh indexBufferSelector

-- | indexCount
--
-- Number of indices in the indexBuffer
--
-- ObjC selector: @- indexCount@
indexCount :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO CULong
indexCount mdlSubmesh =
  sendMessage mdlSubmesh indexCountSelector

-- | indexType
--
-- Data type of indices in indexBuffer
--
-- Support 8, 16, and 32 bit unsigned integer values
--
-- ObjC selector: @- indexType@
indexType :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO MDLIndexBitDepth
indexType mdlSubmesh =
  sendMessage mdlSubmesh indexTypeSelector

-- | geometryType
--
-- Type of primitive that vertices referenced by the indexBuffer are            assembled into
--
-- ObjC selector: @- geometryType@
geometryType :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO MDLGeometryType
geometryType mdlSubmesh =
  sendMessage mdlSubmesh geometryTypeSelector

-- | material
--
-- Material to apply when rendering this object
--
-- ObjC selector: @- material@
material :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO (Id MDLMaterial)
material mdlSubmesh =
  sendMessage mdlSubmesh materialSelector

-- | material
--
-- Material to apply when rendering this object
--
-- ObjC selector: @- setMaterial:@
setMaterial :: (IsMDLSubmesh mdlSubmesh, IsMDLMaterial value) => mdlSubmesh -> value -> IO ()
setMaterial mdlSubmesh value =
  sendMessage mdlSubmesh setMaterialSelector (toMDLMaterial value)

-- | topology
--
-- Topology data structure for use with MDLGeometryTypeVariableTopology
--
-- ignored for geometry types other than MDLGeometryTypeVariableTopology.             A submesh of type MDLGeometryTypeVariableTopology with no topology             data is an empty submesh.
--
-- ObjC selector: @- topology@
topology :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO (Id MDLSubmeshTopology)
topology mdlSubmesh =
  sendMessage mdlSubmesh topologySelector

-- | topology
--
-- Topology data structure for use with MDLGeometryTypeVariableTopology
--
-- ignored for geometry types other than MDLGeometryTypeVariableTopology.             A submesh of type MDLGeometryTypeVariableTopology with no topology             data is an empty submesh.
--
-- ObjC selector: @- setTopology:@
setTopology :: (IsMDLSubmesh mdlSubmesh, IsMDLSubmeshTopology value) => mdlSubmesh -> value -> IO ()
setTopology mdlSubmesh value =
  sendMessage mdlSubmesh setTopologySelector (toMDLSubmeshTopology value)

-- | name
--
-- Identifying name for this object
--
-- ObjC selector: @- name@
name :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO (Id NSString)
name mdlSubmesh =
  sendMessage mdlSubmesh nameSelector

-- | name
--
-- Identifying name for this object
--
-- ObjC selector: @- setName:@
setName :: (IsMDLSubmesh mdlSubmesh, IsNSString value) => mdlSubmesh -> value -> IO ()
setName mdlSubmesh value =
  sendMessage mdlSubmesh setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:indexBuffer:indexCount:indexType:geometryType:material:@
initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector :: Selector '[Id NSString, RawId, CULong, MDLIndexBitDepth, MDLGeometryType, Id MDLMaterial] (Id MDLSubmesh)
initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector = mkSelector "initWithName:indexBuffer:indexCount:indexType:geometryType:material:"

-- | @Selector@ for @initWithIndexBuffer:indexCount:indexType:geometryType:material:@
initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector :: Selector '[RawId, CULong, MDLIndexBitDepth, MDLGeometryType, Id MDLMaterial] (Id MDLSubmesh)
initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector = mkSelector "initWithIndexBuffer:indexCount:indexType:geometryType:material:"

-- | @Selector@ for @initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:@
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector :: Selector '[Id NSString, RawId, CULong, MDLIndexBitDepth, MDLGeometryType, Id MDLMaterial, Id MDLSubmeshTopology] (Id MDLSubmesh)
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector = mkSelector "initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:"

-- | @Selector@ for @initWithMDLSubmesh:indexType:geometryType:@
initWithMDLSubmesh_indexType_geometryTypeSelector :: Selector '[Id MDLSubmesh, MDLIndexBitDepth, MDLGeometryType] (Id MDLSubmesh)
initWithMDLSubmesh_indexType_geometryTypeSelector = mkSelector "initWithMDLSubmesh:indexType:geometryType:"

-- | @Selector@ for @indexBufferAsIndexType:@
indexBufferAsIndexTypeSelector :: Selector '[MDLIndexBitDepth] RawId
indexBufferAsIndexTypeSelector = mkSelector "indexBufferAsIndexType:"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector '[] RawId
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @indexCount@
indexCountSelector :: Selector '[] CULong
indexCountSelector = mkSelector "indexCount"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector '[] MDLIndexBitDepth
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @geometryType@
geometryTypeSelector :: Selector '[] MDLGeometryType
geometryTypeSelector = mkSelector "geometryType"

-- | @Selector@ for @material@
materialSelector :: Selector '[] (Id MDLMaterial)
materialSelector = mkSelector "material"

-- | @Selector@ for @setMaterial:@
setMaterialSelector :: Selector '[Id MDLMaterial] ()
setMaterialSelector = mkSelector "setMaterial:"

-- | @Selector@ for @topology@
topologySelector :: Selector '[] (Id MDLSubmeshTopology)
topologySelector = mkSelector "topology"

-- | @Selector@ for @setTopology:@
setTopologySelector :: Selector '[Id MDLSubmeshTopology] ()
setTopologySelector = mkSelector "setTopology:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

