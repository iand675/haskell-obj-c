{-# LANGUAGE PatternSynonyms #-}
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
  , initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector
  , initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector
  , initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector
  , initWithMDLSubmesh_indexType_geometryTypeSelector
  , indexBufferAsIndexTypeSelector
  , indexBufferSelector
  , indexCountSelector
  , indexTypeSelector
  , geometryTypeSelector
  , materialSelector
  , setMaterialSelector
  , topologySelector
  , setTopologySelector
  , nameSelector
  , setNameSelector

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithName:indexBuffer:indexCount:indexType:geometryType:material:
--
-- Initialize submesh with all data necessary to make properties valid
--
-- ObjC selector: @- initWithName:indexBuffer:indexCount:indexType:geometryType:material:@
initWithName_indexBuffer_indexCount_indexType_geometryType_material :: (IsMDLSubmesh mdlSubmesh, IsNSString name, IsMDLMaterial material) => mdlSubmesh -> name -> RawId -> CULong -> MDLIndexBitDepth -> MDLGeometryType -> material -> IO (Id MDLSubmesh)
initWithName_indexBuffer_indexCount_indexType_geometryType_material mdlSubmesh  name indexBuffer indexCount indexType geometryType material =
  withObjCPtr name $ \raw_name ->
    withObjCPtr material $ \raw_material ->
        sendMsg mdlSubmesh (mkSelector "initWithName:indexBuffer:indexCount:indexType:geometryType:material:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId indexBuffer) :: Ptr ()), argCULong indexCount, argCULong (coerce indexType), argCLong (coerce geometryType), argPtr (castPtr raw_material :: Ptr ())] >>= ownedObject . castPtr

-- | initWithIndexBuffer:indexCount:indexType:geometryType:material:
--
-- Initialize submesh with all data necessary to make properties valid
--
-- ObjC selector: @- initWithIndexBuffer:indexCount:indexType:geometryType:material:@
initWithIndexBuffer_indexCount_indexType_geometryType_material :: (IsMDLSubmesh mdlSubmesh, IsMDLMaterial material) => mdlSubmesh -> RawId -> CULong -> MDLIndexBitDepth -> MDLGeometryType -> material -> IO (Id MDLSubmesh)
initWithIndexBuffer_indexCount_indexType_geometryType_material mdlSubmesh  indexBuffer indexCount indexType geometryType material =
  withObjCPtr material $ \raw_material ->
      sendMsg mdlSubmesh (mkSelector "initWithIndexBuffer:indexCount:indexType:geometryType:material:") (retPtr retVoid) [argPtr (castPtr (unRawId indexBuffer) :: Ptr ()), argCULong indexCount, argCULong (coerce indexType), argCLong (coerce geometryType), argPtr (castPtr raw_material :: Ptr ())] >>= ownedObject . castPtr

-- | initWithIndexBuffer:indexCount:indexType:faceTopologyBuffer:geometryType:material:
--
-- Initialize submesh with all data necessary to make properties valid
--
-- The geometry type will typically be MDLGeometryTypeVariableTopology,             if other types are used the faceTopologyBuffer contents should             reflect that.
--
-- ObjC selector: @- initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:@
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topology :: (IsMDLSubmesh mdlSubmesh, IsNSString name, IsMDLMaterial material, IsMDLSubmeshTopology topology) => mdlSubmesh -> name -> RawId -> CULong -> MDLIndexBitDepth -> MDLGeometryType -> material -> topology -> IO (Id MDLSubmesh)
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topology mdlSubmesh  name indexBuffer indexCount indexType geometryType material topology =
  withObjCPtr name $ \raw_name ->
    withObjCPtr material $ \raw_material ->
      withObjCPtr topology $ \raw_topology ->
          sendMsg mdlSubmesh (mkSelector "initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId indexBuffer) :: Ptr ()), argCULong indexCount, argCULong (coerce indexType), argCLong (coerce geometryType), argPtr (castPtr raw_material :: Ptr ()), argPtr (castPtr raw_topology :: Ptr ())] >>= ownedObject . castPtr

-- | initWithMDLSubmesh:indexType:geometryType:
--
-- Initialize submesh using another submesh as input.
--
-- the resulting submesh will have a new index type if necessary.  If a conversion from the source submesh's geometry type to the requested  geometry type is possible, conversion will be performed. Otherwise nil will  be returned.
--
-- ObjC selector: @- initWithMDLSubmesh:indexType:geometryType:@
initWithMDLSubmesh_indexType_geometryType :: (IsMDLSubmesh mdlSubmesh, IsMDLSubmesh submesh) => mdlSubmesh -> submesh -> MDLIndexBitDepth -> MDLGeometryType -> IO (Id MDLSubmesh)
initWithMDLSubmesh_indexType_geometryType mdlSubmesh  submesh indexType geometryType =
  withObjCPtr submesh $ \raw_submesh ->
      sendMsg mdlSubmesh (mkSelector "initWithMDLSubmesh:indexType:geometryType:") (retPtr retVoid) [argPtr (castPtr raw_submesh :: Ptr ()), argCULong (coerce indexType), argCLong (coerce geometryType)] >>= ownedObject . castPtr

-- | @- indexBufferAsIndexType:@
indexBufferAsIndexType :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> MDLIndexBitDepth -> IO RawId
indexBufferAsIndexType mdlSubmesh  indexType =
    fmap (RawId . castPtr) $ sendMsg mdlSubmesh (mkSelector "indexBufferAsIndexType:") (retPtr retVoid) [argCULong (coerce indexType)]

-- | indexBuffer
--
-- Index data referencing vertex data in parent mesh
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO RawId
indexBuffer mdlSubmesh  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmesh (mkSelector "indexBuffer") (retPtr retVoid) []

-- | indexCount
--
-- Number of indices in the indexBuffer
--
-- ObjC selector: @- indexCount@
indexCount :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO CULong
indexCount mdlSubmesh  =
    sendMsg mdlSubmesh (mkSelector "indexCount") retCULong []

-- | indexType
--
-- Data type of indices in indexBuffer
--
-- Support 8, 16, and 32 bit unsigned integer values
--
-- ObjC selector: @- indexType@
indexType :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO MDLIndexBitDepth
indexType mdlSubmesh  =
    fmap (coerce :: CULong -> MDLIndexBitDepth) $ sendMsg mdlSubmesh (mkSelector "indexType") retCULong []

-- | geometryType
--
-- Type of primitive that vertices referenced by the indexBuffer are            assembled into
--
-- ObjC selector: @- geometryType@
geometryType :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO MDLGeometryType
geometryType mdlSubmesh  =
    fmap (coerce :: CLong -> MDLGeometryType) $ sendMsg mdlSubmesh (mkSelector "geometryType") retCLong []

-- | material
--
-- Material to apply when rendering this object
--
-- ObjC selector: @- material@
material :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO (Id MDLMaterial)
material mdlSubmesh  =
    sendMsg mdlSubmesh (mkSelector "material") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | material
--
-- Material to apply when rendering this object
--
-- ObjC selector: @- setMaterial:@
setMaterial :: (IsMDLSubmesh mdlSubmesh, IsMDLMaterial value) => mdlSubmesh -> value -> IO ()
setMaterial mdlSubmesh  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlSubmesh (mkSelector "setMaterial:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | topology
--
-- Topology data structure for use with MDLGeometryTypeVariableTopology
--
-- ignored for geometry types other than MDLGeometryTypeVariableTopology.             A submesh of type MDLGeometryTypeVariableTopology with no topology             data is an empty submesh.
--
-- ObjC selector: @- topology@
topology :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO (Id MDLSubmeshTopology)
topology mdlSubmesh  =
    sendMsg mdlSubmesh (mkSelector "topology") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | topology
--
-- Topology data structure for use with MDLGeometryTypeVariableTopology
--
-- ignored for geometry types other than MDLGeometryTypeVariableTopology.             A submesh of type MDLGeometryTypeVariableTopology with no topology             data is an empty submesh.
--
-- ObjC selector: @- setTopology:@
setTopology :: (IsMDLSubmesh mdlSubmesh, IsMDLSubmeshTopology value) => mdlSubmesh -> value -> IO ()
setTopology mdlSubmesh  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlSubmesh (mkSelector "setTopology:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | name
--
-- Identifying name for this object
--
-- ObjC selector: @- name@
name :: IsMDLSubmesh mdlSubmesh => mdlSubmesh -> IO (Id NSString)
name mdlSubmesh  =
    sendMsg mdlSubmesh (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Identifying name for this object
--
-- ObjC selector: @- setName:@
setName :: (IsMDLSubmesh mdlSubmesh, IsNSString value) => mdlSubmesh -> value -> IO ()
setName mdlSubmesh  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlSubmesh (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:indexBuffer:indexCount:indexType:geometryType:material:@
initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector :: Selector
initWithName_indexBuffer_indexCount_indexType_geometryType_materialSelector = mkSelector "initWithName:indexBuffer:indexCount:indexType:geometryType:material:"

-- | @Selector@ for @initWithIndexBuffer:indexCount:indexType:geometryType:material:@
initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector :: Selector
initWithIndexBuffer_indexCount_indexType_geometryType_materialSelector = mkSelector "initWithIndexBuffer:indexCount:indexType:geometryType:material:"

-- | @Selector@ for @initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:@
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector :: Selector
initWithName_indexBuffer_indexCount_indexType_geometryType_material_topologySelector = mkSelector "initWithName:indexBuffer:indexCount:indexType:geometryType:material:topology:"

-- | @Selector@ for @initWithMDLSubmesh:indexType:geometryType:@
initWithMDLSubmesh_indexType_geometryTypeSelector :: Selector
initWithMDLSubmesh_indexType_geometryTypeSelector = mkSelector "initWithMDLSubmesh:indexType:geometryType:"

-- | @Selector@ for @indexBufferAsIndexType:@
indexBufferAsIndexTypeSelector :: Selector
indexBufferAsIndexTypeSelector = mkSelector "indexBufferAsIndexType:"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @indexCount@
indexCountSelector :: Selector
indexCountSelector = mkSelector "indexCount"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @geometryType@
geometryTypeSelector :: Selector
geometryTypeSelector = mkSelector "geometryType"

-- | @Selector@ for @material@
materialSelector :: Selector
materialSelector = mkSelector "material"

-- | @Selector@ for @setMaterial:@
setMaterialSelector :: Selector
setMaterialSelector = mkSelector "setMaterial:"

-- | @Selector@ for @topology@
topologySelector :: Selector
topologySelector = mkSelector "topology"

-- | @Selector@ for @setTopology:@
setTopologySelector :: Selector
setTopologySelector = mkSelector "setTopology:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

