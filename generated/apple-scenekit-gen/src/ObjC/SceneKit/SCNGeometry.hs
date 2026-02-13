{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNGeometry
--
-- SCNGeometry is an abstract class that represents the geometry that can be attached to a SCNNode.
--
-- Generated bindings for @SCNGeometry@.
module ObjC.SceneKit.SCNGeometry
  ( SCNGeometry
  , IsSCNGeometry(..)
  , geometry
  , insertMaterial_atIndex
  , removeMaterialAtIndex
  , replaceMaterialAtIndex_withMaterial
  , materialWithName
  , geometryWithSources_elements
  , geometryWithSources_elements_sourceChannels
  , geometrySourcesForSemantic
  , geometryElementAtIndex
  , name
  , setName
  , materials
  , setMaterials
  , firstMaterial
  , setFirstMaterial
  , geometrySources
  , geometryElements
  , geometryElementCount
  , geometrySourceChannels
  , levelsOfDetail
  , setLevelsOfDetail
  , tessellator
  , setTessellator
  , subdivisionLevel
  , setSubdivisionLevel
  , wantsAdaptiveSubdivision
  , setWantsAdaptiveSubdivision
  , edgeCreasesElement
  , setEdgeCreasesElement
  , edgeCreasesSource
  , setEdgeCreasesSource
  , edgeCreasesElementSelector
  , edgeCreasesSourceSelector
  , firstMaterialSelector
  , geometryElementAtIndexSelector
  , geometryElementCountSelector
  , geometryElementsSelector
  , geometrySelector
  , geometrySourceChannelsSelector
  , geometrySourcesForSemanticSelector
  , geometrySourcesSelector
  , geometryWithSources_elementsSelector
  , geometryWithSources_elements_sourceChannelsSelector
  , insertMaterial_atIndexSelector
  , levelsOfDetailSelector
  , materialWithNameSelector
  , materialsSelector
  , nameSelector
  , removeMaterialAtIndexSelector
  , replaceMaterialAtIndex_withMaterialSelector
  , setEdgeCreasesElementSelector
  , setEdgeCreasesSourceSelector
  , setFirstMaterialSelector
  , setLevelsOfDetailSelector
  , setMaterialsSelector
  , setNameSelector
  , setSubdivisionLevelSelector
  , setTessellatorSelector
  , setWantsAdaptiveSubdivisionSelector
  , subdivisionLevelSelector
  , tessellatorSelector
  , wantsAdaptiveSubdivisionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | geometry
--
-- Creates and returns an empty geometry object.
--
-- An empty geometry may be used as the lowest level of detail of a geometry.
--
-- ObjC selector: @+ geometry@
geometry :: IO (Id SCNGeometry)
geometry  =
  do
    cls' <- getRequiredClass "SCNGeometry"
    sendClassMessage cls' geometrySelector

-- | insertMaterial:atIndex:
--
-- Insert a material in the materials array at the specified index.
--
-- @material@ — The material to insert.
--
-- @index@ — Index in the materials array to insert the new material.
--
-- ObjC selector: @- insertMaterial:atIndex:@
insertMaterial_atIndex :: (IsSCNGeometry scnGeometry, IsSCNMaterial material) => scnGeometry -> material -> CULong -> IO ()
insertMaterial_atIndex scnGeometry material index =
  sendMessage scnGeometry insertMaterial_atIndexSelector (toSCNMaterial material) index

-- | removeMaterialAtIndex:
--
-- Remove the material at the specified index from the materials array.
--
-- @index@ — The index of the material to remove from the 'materials' array.
--
-- ObjC selector: @- removeMaterialAtIndex:@
removeMaterialAtIndex :: IsSCNGeometry scnGeometry => scnGeometry -> CULong -> IO ()
removeMaterialAtIndex scnGeometry index =
  sendMessage scnGeometry removeMaterialAtIndexSelector index

-- | replaceMaterialAtIndex:withMaterial:
--
-- Remove the material at the index 'index' from the materials array of the receiver and insert 'material' in its position.
--
-- @index@ — The index of the material to replace in the materials array.
--
-- @material@ — The new material that will replace the previous one.
--
-- ObjC selector: @- replaceMaterialAtIndex:withMaterial:@
replaceMaterialAtIndex_withMaterial :: (IsSCNGeometry scnGeometry, IsSCNMaterial material) => scnGeometry -> CULong -> material -> IO ()
replaceMaterialAtIndex_withMaterial scnGeometry index material =
  sendMessage scnGeometry replaceMaterialAtIndex_withMaterialSelector index (toSCNMaterial material)

-- | materialWithName:
--
-- Return the first material from the materials array of the receiver with the specified name.
--
-- @name@ — The name of the material to retrieve.
--
-- ObjC selector: @- materialWithName:@
materialWithName :: (IsSCNGeometry scnGeometry, IsNSString name) => scnGeometry -> name -> IO (Id SCNMaterial)
materialWithName scnGeometry name =
  sendMessage scnGeometry materialWithNameSelector (toNSString name)

-- | geometryWithSources:elements:
--
-- Creates and returns a new geometry built from geometry sources and geometry elements.
--
-- @sources@ — An array of geometry sources. If several geometry sources have the same semantic, only the first one is taken into account.
--
-- @elements@ — An array of geometry elements. The sort order in the array determines the mapping between materials and geometry elements.
--
-- A geometry is made of geometry sources (at least @SCNGeometrySourceSemanticVertex@) and at least one geometry element. Multiple sources for texture coordinates are accepted. In that case the @mappingChannel@ is implicitly set based on the order of the texture sources, starting at index 0.
--
-- ObjC selector: @+ geometryWithSources:elements:@
geometryWithSources_elements :: (IsNSArray sources, IsNSArray elements) => sources -> elements -> IO (Id SCNGeometry)
geometryWithSources_elements sources elements =
  do
    cls' <- getRequiredClass "SCNGeometry"
    sendClassMessage cls' geometryWithSources_elementsSelector (toNSArray sources) (toNSArray elements)

-- | geometryWithSources:elements:sourceChannels:
--
-- Creates and returns a new geometry built from geometry sources and geometry elements, with per-source indexed geometry data.
--
-- @sources@ — An array of geometry sources. If several geometry sources have the same semantic, only the first one is taken into account.
--
-- @elements@ — An array of geometry elements. The sort order in the array determines the mapping between materials and geometry elements.
--
-- @sourceChannels@ — An array of indices that describes, for each geometry source, which channel of the geometry elements to use.
--
-- ``` Example: geometry made of 3 primitives (2 quads, 1 pentagon) using different indices to reference position and UV data (2 channels)
--
-- Positions         ┆   POS0           POS3           POS4    ┆             quad   quad   pentagon    quad   quad   pentagon    ┆   SCNGeometryElement *element = [SCNGeometryElement geometryElementWithData:…    0 │ (0.0, 0.0, 0.0)   ┆        ┌───────────┬───────────┐        ┆           ┌─────┐ ┌─────┐ ┌───────┐ ┌─────┐ ┌─────┐ ┌───────┐   ┆                                                               primitiveType:SCNGeometryPrimitiveTypePolygon    1 │ (0.0, 1.0, 0.0)   ┆        │UV0     UV3│UV0     UV3│        ┆     4 4 5 0 1 2 3 5 4 3 2 7 6 5 2 1 0 1 2 3 2 3 0 1 1 2 3 4 0   ┆                                                              primitiveCount:3    2 │ (1.0, 0.0, 0.0)   ┆        │           │           │        ┆     └───┘ └───────────────────────┘ └───────────────────────┘   ┆                                                         indicesChannelCount:2    3 │ (1.0, 1.0, 0.0)   ┆        │     A     │     B     │        ┆   polygons        channel 0                 channel 1           ┆                                                  interleavedIndicesChannels:…    4 │ (2.0, 0.0, 0.0)   ┆        │           │           │        ┆                  (positions)                  (UVs)             ┆                                                               bytesPerIndex:…];    5 │ (2.0, 1.0, 0.0)   ┆        │UV1     UV2│UV1     UV2│        ┆                                                                 ┆    6 │ (2.0, 2.0, 0.0)   ┆   POS1 ├───────────┴───────────┤ POS5   ┆                                                                 ┆   SCNGeometry *geometry = [SCNGeometry geometryWithSources:\@[positionSource, texcoordsSource]    7 │ (0.0, 2.0, 0.0)   ┆        │UVO       UV4       UV3│        ┆                                                                 ┆                                                   elements:\@[element]                          ┆        │         POS2          │        ┆                quad A          quad B          pentagon C       ┆                                             sourceChannels:\@[0, 1]];        UVs               ┆        │                       │        ┆           ┌─────────────┐ ┌─────────────┐ ┌─────────────────┐   ┆    0 │ (0.0, 0.0)        ┆        │           C           │        ┆     4 4 5 0 0 1 1 2 2 3 3 5 2 4 3 3 0 2 1 7 1 6 2 5 3 2 4 1 0   ┆    1 │ (0.0, 1.0)        ┆        │                       │        ┆     └───┘└──────────────────────────────────────────────────┘   ┆    2 │ (1.0, 1.0)        ┆        │UV1                 UV2│        ┆   polygons               interleaved  channels                  ┆    3 │ (1.0, 0.0)        ┆        └───────────────────────┘        ┆                           (positions and UVs)                   ┆    4 │ (0.5, 0.0)        ┆   POS7                          POS6    ┆                                                                 ┆
--
-- Example: geometry made of 3 primitives (2 quads, 1 pentagon) using the same indices to reference position and UV data (1 channel)
--
-- Positions         ┆   POS0           POS3           POS4    ┆             quad A      quad B      pentagon C                  ┆   SCNGeometryElement *element = [SCNGeometryElement geometryElementWithData:…    0 │ (0.0, 4.0, 0.0)   ┆        ┌───────────┬───────────┐        ┆           ┌────────┐  ┌────────┐  ┌───────────┐                 ┆                                                               primitiveType:SCNGeometryPrimitiveTypePolygon    1 │ (0.0, 2.0, 0.0)   ┆        │UV0     UV3│UV3     UV4│        ┆     4 4 5 0  1  2  3  5  4  3  2  7  6  5  2  1                 ┆                                                              primitiveCount:3    2 │ (2.0, 2.0, 0.0)   ┆        │           │           │        ┆     └───┘ └───────────────────────────────────┘                 ┆                                                               bytesPerIndex:…];    3 │ (2.0, 4.0, 0.0)   ┆        │     A     │     B     │        ┆   polygons              channel 0                               ┆    4 │ (4.0, 4.0, 0.0)   ┆        │           │           │        ┆                    (positions and UVs)                          ┆   SCNGeometry *geometry = [SCNGeometry geometryWithSources:\@[positionSource, texcoordsSource]    5 │ (4.0, 2.0, 0.0)   ┆        │UV1     UV2│UV2     UV5│        ┆                                                                 ┆                                                   elements:\@[element]];    6 │ (4.0, 0.0, 0.0)   ┆   POS1 ├───────────┴───────────┤ POS5   ┆                                                                 ┆    7 │ (0.0, 0.0, 0.0)   ┆        │UV1       UV2       UV5│        ┆                                                                 ┆                                            === or equivalently ===                          ┆        │         POS2          │        ┆                                                                 ┆        UVs               ┆        │                       │        ┆                                                                 ┆   SCNGeometryElement *element = [SCNGeometryElement geometryElementWithData:…    0 │ (0.0, 0.0)        ┆        │           C           │        ┆                                                                 ┆                                                               primitiveType:SCNGeometryPrimitiveTypePolygon    1 │ (0.0, 0.5)        ┆        │                       │        ┆                                                                 ┆                                                              primitiveCount:3    2 │ (0.5, 0.5)        ┆        │UV7                 UV6│        ┆                                                                 ┆                                                         indicesChannelCount:1    3 │ (0.5, 0.0)        ┆        └───────────────────────┘        ┆                                                                 ┆                                                  interleavedIndicesChannels:…    4 │ (1.0, 0.0)        ┆   POS7                          POS6    ┆                                                                 ┆                                                               bytesPerIndex:…];    5 │ (1.0, 0.5)        ┆                                         ┆                                                                 ┆    6 │ (1.0, 1.0)        ┆                                         ┆                                                                 ┆   SCNGeometry *geometry = [SCNGeometry geometryWithSources:\@[positionSource, texcoordsSource]    7 │ (0.0, 1.0)        ┆                                         ┆                                                                 ┆                                                   elements:\@[element]                          ┆                                         ┆                                                                 ┆                                             sourceChannels:\@[0, 0]];                                                                                                                                               ┆ ```
--
-- ObjC selector: @+ geometryWithSources:elements:sourceChannels:@
geometryWithSources_elements_sourceChannels :: (IsNSArray sources, IsNSArray elements, IsNSArray sourceChannels) => sources -> elements -> sourceChannels -> IO (Id SCNGeometry)
geometryWithSources_elements_sourceChannels sources elements sourceChannels =
  do
    cls' <- getRequiredClass "SCNGeometry"
    sendClassMessage cls' geometryWithSources_elements_sourceChannelsSelector (toNSArray sources) (toNSArray elements) (toNSArray sourceChannels)

-- | geometrySourcesForSemantic:
--
-- Returns the geometry sources for a given semantic.
--
-- @semantic@ — The semantic of the geometry sources that should be retrieved.
--
-- Returns nil if no geometry source is found for the given semantic. May return more than one source, typically for multiple texture coordinate sources.
--
-- ObjC selector: @- geometrySourcesForSemantic:@
geometrySourcesForSemantic :: (IsSCNGeometry scnGeometry, IsNSString semantic) => scnGeometry -> semantic -> IO (Id NSArray)
geometrySourcesForSemantic scnGeometry semantic =
  sendMessage scnGeometry geometrySourcesForSemanticSelector (toNSString semantic)

-- | geometryElementAtIndex:
--
-- Returns the geometry element at a given index.
--
-- @elementIndex@ — The index of the geometry element.
--
-- ObjC selector: @- geometryElementAtIndex:@
geometryElementAtIndex :: IsSCNGeometry scnGeometry => scnGeometry -> CLong -> IO (Id SCNGeometryElement)
geometryElementAtIndex scnGeometry elementIndex =
  sendMessage scnGeometry geometryElementAtIndexSelector elementIndex

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id NSString)
name scnGeometry =
  sendMessage scnGeometry nameSelector

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNGeometry scnGeometry, IsNSString value) => scnGeometry -> value -> IO ()
setName scnGeometry value =
  sendMessage scnGeometry setNameSelector (toNSString value)

-- | materials
--
-- Specifies the receiver's materials array.
--
-- Each geometry element can be rendered using a different material. The index of the material used for a geometry element is equal to the index of that element modulo the number of materials.
--
-- ObjC selector: @- materials@
materials :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id NSArray)
materials scnGeometry =
  sendMessage scnGeometry materialsSelector

-- | materials
--
-- Specifies the receiver's materials array.
--
-- Each geometry element can be rendered using a different material. The index of the material used for a geometry element is equal to the index of that element modulo the number of materials.
--
-- ObjC selector: @- setMaterials:@
setMaterials :: (IsSCNGeometry scnGeometry, IsNSArray value) => scnGeometry -> value -> IO ()
setMaterials scnGeometry value =
  sendMessage scnGeometry setMaterialsSelector (toNSArray value)

-- | firstMaterial
--
-- Determines the first material of the geometry. Returns nil if the geometry has no material.
--
-- This method is here for convenience. It is equivalent to the first object in the "materials" array above.
--
-- ObjC selector: @- firstMaterial@
firstMaterial :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id SCNMaterial)
firstMaterial scnGeometry =
  sendMessage scnGeometry firstMaterialSelector

-- | firstMaterial
--
-- Determines the first material of the geometry. Returns nil if the geometry has no material.
--
-- This method is here for convenience. It is equivalent to the first object in the "materials" array above.
--
-- ObjC selector: @- setFirstMaterial:@
setFirstMaterial :: (IsSCNGeometry scnGeometry, IsSCNMaterial value) => scnGeometry -> value -> IO ()
setFirstMaterial scnGeometry value =
  sendMessage scnGeometry setFirstMaterialSelector (toSCNMaterial value)

-- | geometrySources
--
-- The array of geometry sources of the receiver.
--
-- ObjC selector: @- geometrySources@
geometrySources :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id NSArray)
geometrySources scnGeometry =
  sendMessage scnGeometry geometrySourcesSelector

-- | geometryElements
--
-- The array of geometry elements of the receiver.
--
-- ObjC selector: @- geometryElements@
geometryElements :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id NSArray)
geometryElements scnGeometry =
  sendMessage scnGeometry geometryElementsSelector

-- | geometryElementCount
--
-- Returns the number of geometry elements owned by the geometry.
--
-- ObjC selector: @- geometryElementCount@
geometryElementCount :: IsSCNGeometry scnGeometry => scnGeometry -> IO CLong
geometryElementCount scnGeometry =
  sendMessage scnGeometry geometryElementCountSelector

-- | geometrySourceChannels
--
-- An array of indices that describes, for each geometry source, which channel of the geometry elements to use.
--
-- ObjC selector: @- geometrySourceChannels@
geometrySourceChannels :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id NSArray)
geometrySourceChannels scnGeometry =
  sendMessage scnGeometry geometrySourceChannelsSelector

-- | levelsOfDetail
--
-- Determines the receiver's levels of detail. Defaults to nil.
--
-- ObjC selector: @- levelsOfDetail@
levelsOfDetail :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id NSArray)
levelsOfDetail scnGeometry =
  sendMessage scnGeometry levelsOfDetailSelector

-- | levelsOfDetail
--
-- Determines the receiver's levels of detail. Defaults to nil.
--
-- ObjC selector: @- setLevelsOfDetail:@
setLevelsOfDetail :: (IsSCNGeometry scnGeometry, IsNSArray value) => scnGeometry -> value -> IO ()
setLevelsOfDetail scnGeometry value =
  sendMessage scnGeometry setLevelsOfDetailSelector (toNSArray value)

-- | @- tessellator@
tessellator :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id SCNGeometryTessellator)
tessellator scnGeometry =
  sendMessage scnGeometry tessellatorSelector

-- | @- setTessellator:@
setTessellator :: (IsSCNGeometry scnGeometry, IsSCNGeometryTessellator value) => scnGeometry -> value -> IO ()
setTessellator scnGeometry value =
  sendMessage scnGeometry setTessellatorSelector (toSCNGeometryTessellator value)

-- | subdivisionLevel
--
-- Specifies the subdivision level of the receiver. Defaults to 0.
--
-- A subdivision level of 0 means no subdivision. When the @tessellator@ property of the receiver is not nil, the refinement is done on the GPU.
--
-- ObjC selector: @- subdivisionLevel@
subdivisionLevel :: IsSCNGeometry scnGeometry => scnGeometry -> IO CULong
subdivisionLevel scnGeometry =
  sendMessage scnGeometry subdivisionLevelSelector

-- | subdivisionLevel
--
-- Specifies the subdivision level of the receiver. Defaults to 0.
--
-- A subdivision level of 0 means no subdivision. When the @tessellator@ property of the receiver is not nil, the refinement is done on the GPU.
--
-- ObjC selector: @- setSubdivisionLevel:@
setSubdivisionLevel :: IsSCNGeometry scnGeometry => scnGeometry -> CULong -> IO ()
setSubdivisionLevel scnGeometry value =
  sendMessage scnGeometry setSubdivisionLevelSelector value

-- | wantsAdaptiveSubdivision
--
-- Specifies if the subdivision is adaptive or uniform. Defaults to YES.
--
-- Adaptive subdivision requires that the @tessellator@ property of the receiver is not nil.
--
-- ObjC selector: @- wantsAdaptiveSubdivision@
wantsAdaptiveSubdivision :: IsSCNGeometry scnGeometry => scnGeometry -> IO Bool
wantsAdaptiveSubdivision scnGeometry =
  sendMessage scnGeometry wantsAdaptiveSubdivisionSelector

-- | wantsAdaptiveSubdivision
--
-- Specifies if the subdivision is adaptive or uniform. Defaults to YES.
--
-- Adaptive subdivision requires that the @tessellator@ property of the receiver is not nil.
--
-- ObjC selector: @- setWantsAdaptiveSubdivision:@
setWantsAdaptiveSubdivision :: IsSCNGeometry scnGeometry => scnGeometry -> Bool -> IO ()
setWantsAdaptiveSubdivision scnGeometry value =
  sendMessage scnGeometry setWantsAdaptiveSubdivisionSelector value

-- | edgeCreasesElement
--
-- Specifies the edges creases that control the subdivision. Defaults to nil.
--
-- The primitive type of this geometry element must be SCNGeometryPrimitiveTypeLine. See subdivisionLevel above to control the level of subdivision. See edgeCreasesSource below to specify sharpness of the creases.
--
-- ObjC selector: @- edgeCreasesElement@
edgeCreasesElement :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id SCNGeometryElement)
edgeCreasesElement scnGeometry =
  sendMessage scnGeometry edgeCreasesElementSelector

-- | edgeCreasesElement
--
-- Specifies the edges creases that control the subdivision. Defaults to nil.
--
-- The primitive type of this geometry element must be SCNGeometryPrimitiveTypeLine. See subdivisionLevel above to control the level of subdivision. See edgeCreasesSource below to specify sharpness of the creases.
--
-- ObjC selector: @- setEdgeCreasesElement:@
setEdgeCreasesElement :: (IsSCNGeometry scnGeometry, IsSCNGeometryElement value) => scnGeometry -> value -> IO ()
setEdgeCreasesElement scnGeometry value =
  sendMessage scnGeometry setEdgeCreasesElementSelector (toSCNGeometryElement value)

-- | edgeCreasesSource
--
-- Specifies the crease value of the edges specified by edgeCreasesElement. Defaults to nil.
--
-- The semantic of this geometry source must be "SCNGeometrySourceSemanticEdgeCrease". The creases values are floating values between 0 and 10, where 0 means smooth and 10 means infinitely sharp. See subdivisionLevel above to control the level of subdivision. See edgeCreasesElement above to specify edges for edge creases.
--
-- ObjC selector: @- edgeCreasesSource@
edgeCreasesSource :: IsSCNGeometry scnGeometry => scnGeometry -> IO (Id SCNGeometrySource)
edgeCreasesSource scnGeometry =
  sendMessage scnGeometry edgeCreasesSourceSelector

-- | edgeCreasesSource
--
-- Specifies the crease value of the edges specified by edgeCreasesElement. Defaults to nil.
--
-- The semantic of this geometry source must be "SCNGeometrySourceSemanticEdgeCrease". The creases values are floating values between 0 and 10, where 0 means smooth and 10 means infinitely sharp. See subdivisionLevel above to control the level of subdivision. See edgeCreasesElement above to specify edges for edge creases.
--
-- ObjC selector: @- setEdgeCreasesSource:@
setEdgeCreasesSource :: (IsSCNGeometry scnGeometry, IsSCNGeometrySource value) => scnGeometry -> value -> IO ()
setEdgeCreasesSource scnGeometry value =
  sendMessage scnGeometry setEdgeCreasesSourceSelector (toSCNGeometrySource value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geometry@
geometrySelector :: Selector '[] (Id SCNGeometry)
geometrySelector = mkSelector "geometry"

-- | @Selector@ for @insertMaterial:atIndex:@
insertMaterial_atIndexSelector :: Selector '[Id SCNMaterial, CULong] ()
insertMaterial_atIndexSelector = mkSelector "insertMaterial:atIndex:"

-- | @Selector@ for @removeMaterialAtIndex:@
removeMaterialAtIndexSelector :: Selector '[CULong] ()
removeMaterialAtIndexSelector = mkSelector "removeMaterialAtIndex:"

-- | @Selector@ for @replaceMaterialAtIndex:withMaterial:@
replaceMaterialAtIndex_withMaterialSelector :: Selector '[CULong, Id SCNMaterial] ()
replaceMaterialAtIndex_withMaterialSelector = mkSelector "replaceMaterialAtIndex:withMaterial:"

-- | @Selector@ for @materialWithName:@
materialWithNameSelector :: Selector '[Id NSString] (Id SCNMaterial)
materialWithNameSelector = mkSelector "materialWithName:"

-- | @Selector@ for @geometryWithSources:elements:@
geometryWithSources_elementsSelector :: Selector '[Id NSArray, Id NSArray] (Id SCNGeometry)
geometryWithSources_elementsSelector = mkSelector "geometryWithSources:elements:"

-- | @Selector@ for @geometryWithSources:elements:sourceChannels:@
geometryWithSources_elements_sourceChannelsSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id SCNGeometry)
geometryWithSources_elements_sourceChannelsSelector = mkSelector "geometryWithSources:elements:sourceChannels:"

-- | @Selector@ for @geometrySourcesForSemantic:@
geometrySourcesForSemanticSelector :: Selector '[Id NSString] (Id NSArray)
geometrySourcesForSemanticSelector = mkSelector "geometrySourcesForSemantic:"

-- | @Selector@ for @geometryElementAtIndex:@
geometryElementAtIndexSelector :: Selector '[CLong] (Id SCNGeometryElement)
geometryElementAtIndexSelector = mkSelector "geometryElementAtIndex:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @materials@
materialsSelector :: Selector '[] (Id NSArray)
materialsSelector = mkSelector "materials"

-- | @Selector@ for @setMaterials:@
setMaterialsSelector :: Selector '[Id NSArray] ()
setMaterialsSelector = mkSelector "setMaterials:"

-- | @Selector@ for @firstMaterial@
firstMaterialSelector :: Selector '[] (Id SCNMaterial)
firstMaterialSelector = mkSelector "firstMaterial"

-- | @Selector@ for @setFirstMaterial:@
setFirstMaterialSelector :: Selector '[Id SCNMaterial] ()
setFirstMaterialSelector = mkSelector "setFirstMaterial:"

-- | @Selector@ for @geometrySources@
geometrySourcesSelector :: Selector '[] (Id NSArray)
geometrySourcesSelector = mkSelector "geometrySources"

-- | @Selector@ for @geometryElements@
geometryElementsSelector :: Selector '[] (Id NSArray)
geometryElementsSelector = mkSelector "geometryElements"

-- | @Selector@ for @geometryElementCount@
geometryElementCountSelector :: Selector '[] CLong
geometryElementCountSelector = mkSelector "geometryElementCount"

-- | @Selector@ for @geometrySourceChannels@
geometrySourceChannelsSelector :: Selector '[] (Id NSArray)
geometrySourceChannelsSelector = mkSelector "geometrySourceChannels"

-- | @Selector@ for @levelsOfDetail@
levelsOfDetailSelector :: Selector '[] (Id NSArray)
levelsOfDetailSelector = mkSelector "levelsOfDetail"

-- | @Selector@ for @setLevelsOfDetail:@
setLevelsOfDetailSelector :: Selector '[Id NSArray] ()
setLevelsOfDetailSelector = mkSelector "setLevelsOfDetail:"

-- | @Selector@ for @tessellator@
tessellatorSelector :: Selector '[] (Id SCNGeometryTessellator)
tessellatorSelector = mkSelector "tessellator"

-- | @Selector@ for @setTessellator:@
setTessellatorSelector :: Selector '[Id SCNGeometryTessellator] ()
setTessellatorSelector = mkSelector "setTessellator:"

-- | @Selector@ for @subdivisionLevel@
subdivisionLevelSelector :: Selector '[] CULong
subdivisionLevelSelector = mkSelector "subdivisionLevel"

-- | @Selector@ for @setSubdivisionLevel:@
setSubdivisionLevelSelector :: Selector '[CULong] ()
setSubdivisionLevelSelector = mkSelector "setSubdivisionLevel:"

-- | @Selector@ for @wantsAdaptiveSubdivision@
wantsAdaptiveSubdivisionSelector :: Selector '[] Bool
wantsAdaptiveSubdivisionSelector = mkSelector "wantsAdaptiveSubdivision"

-- | @Selector@ for @setWantsAdaptiveSubdivision:@
setWantsAdaptiveSubdivisionSelector :: Selector '[Bool] ()
setWantsAdaptiveSubdivisionSelector = mkSelector "setWantsAdaptiveSubdivision:"

-- | @Selector@ for @edgeCreasesElement@
edgeCreasesElementSelector :: Selector '[] (Id SCNGeometryElement)
edgeCreasesElementSelector = mkSelector "edgeCreasesElement"

-- | @Selector@ for @setEdgeCreasesElement:@
setEdgeCreasesElementSelector :: Selector '[Id SCNGeometryElement] ()
setEdgeCreasesElementSelector = mkSelector "setEdgeCreasesElement:"

-- | @Selector@ for @edgeCreasesSource@
edgeCreasesSourceSelector :: Selector '[] (Id SCNGeometrySource)
edgeCreasesSourceSelector = mkSelector "edgeCreasesSource"

-- | @Selector@ for @setEdgeCreasesSource:@
setEdgeCreasesSourceSelector :: Selector '[Id SCNGeometrySource] ()
setEdgeCreasesSourceSelector = mkSelector "setEdgeCreasesSource:"

