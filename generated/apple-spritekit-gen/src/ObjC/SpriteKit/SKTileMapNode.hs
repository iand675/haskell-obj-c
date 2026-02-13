{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A SpriteKit node used to render a 2D array of textured sprites. Uses SKTileSet to determine what textures it can use to render. Separate tile map nodes can be layered on top of one another to achieve various effects, such as parallax scrolling.
--
-- Generated bindings for @SKTileMapNode@.
module ObjC.SpriteKit.SKTileMapNode
  ( SKTileMapNode
  , IsSKTileMapNode(..)
  , valueForAttributeNamed
  , setValue_forAttributeNamed
  , fillWithTileGroup
  , tileDefinitionAtColumn_row
  , tileGroupAtColumn_row
  , setTileGroup_forColumn_row
  , setTileGroup_andTileDefinition_forColumn_row
  , numberOfColumns
  , setNumberOfColumns
  , numberOfRows
  , setNumberOfRows
  , tileSet
  , setTileSet
  , colorBlendFactor
  , setColorBlendFactor
  , color
  , setColor
  , blendMode
  , setBlendMode
  , shader
  , setShader
  , attributeValues
  , setAttributeValues
  , lightingBitMask
  , setLightingBitMask
  , enableAutomapping
  , setEnableAutomapping
  , attributeValuesSelector
  , blendModeSelector
  , colorBlendFactorSelector
  , colorSelector
  , enableAutomappingSelector
  , fillWithTileGroupSelector
  , lightingBitMaskSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , setAttributeValuesSelector
  , setBlendModeSelector
  , setColorBlendFactorSelector
  , setColorSelector
  , setEnableAutomappingSelector
  , setLightingBitMaskSelector
  , setNumberOfColumnsSelector
  , setNumberOfRowsSelector
  , setShaderSelector
  , setTileGroup_andTileDefinition_forColumn_rowSelector
  , setTileGroup_forColumn_rowSelector
  , setTileSetSelector
  , setValue_forAttributeNamedSelector
  , shaderSelector
  , tileDefinitionAtColumn_rowSelector
  , tileGroupAtColumn_rowSelector
  , tileSetSelector
  , valueForAttributeNamedSelector

  -- * Enum types
  , SKBlendMode(SKBlendMode)
  , pattern SKBlendModeAlpha
  , pattern SKBlendModeAdd
  , pattern SKBlendModeSubtract
  , pattern SKBlendModeMultiply
  , pattern SKBlendModeMultiplyX2
  , pattern SKBlendModeScreen
  , pattern SKBlendModeReplace
  , pattern SKBlendModeMultiplyAlpha

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKTileMapNode skTileMapNode, IsNSString key) => skTileMapNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skTileMapNode key =
  sendMessage skTileMapNode valueForAttributeNamedSelector (toNSString key)

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKTileMapNode skTileMapNode, IsSKAttributeValue value, IsNSString key) => skTileMapNode -> value -> key -> IO ()
setValue_forAttributeNamed skTileMapNode value key =
  sendMessage skTileMapNode setValue_forAttributeNamedSelector (toSKAttributeValue value) (toNSString key)

-- | Fill the entire tile map with the provided tile group.
--
-- @tileGroup@ — the tile group that will be used to fill the map
--
-- ObjC selector: @- fillWithTileGroup:@
fillWithTileGroup :: (IsSKTileMapNode skTileMapNode, IsSKTileGroup tileGroup) => skTileMapNode -> tileGroup -> IO ()
fillWithTileGroup skTileMapNode tileGroup =
  sendMessage skTileMapNode fillWithTileGroupSelector (toSKTileGroup tileGroup)

-- | Look up the tile definition at the specified tile index.
--
-- @column@ — the column index of the tile
--
-- @row@ — the row index of the tile
--
-- ObjC selector: @- tileDefinitionAtColumn:row:@
tileDefinitionAtColumn_row :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> CULong -> IO (Id SKTileDefinition)
tileDefinitionAtColumn_row skTileMapNode column row =
  sendMessage skTileMapNode tileDefinitionAtColumn_rowSelector column row

-- | Look up the tile group at the specified tile index.
--
-- @column@ — the column index of the tile
--
-- @row@ — the row index of the tile
--
-- ObjC selector: @- tileGroupAtColumn:row:@
tileGroupAtColumn_row :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> CULong -> IO (Id SKTileGroup)
tileGroupAtColumn_row skTileMapNode column row =
  sendMessage skTileMapNode tileGroupAtColumn_rowSelector column row

-- | Set the tile group at the specified tile index. When automapping is enabled, the appropriate tile definitions will automatically be selected and placed, possibly modifying neighboring tiles. When automapping is disabled, it will simply place the default center tile definition for the group, and will not modify any of the neihboring tiles.
--
-- @tileGroup@ — the tile group we want to place in the map
--
-- @column@ — the column index of the tile
--
-- @row@ — the row index of the tile
--
-- ObjC selector: @- setTileGroup:forColumn:row:@
setTileGroup_forColumn_row :: (IsSKTileMapNode skTileMapNode, IsSKTileGroup tileGroup) => skTileMapNode -> tileGroup -> CULong -> CULong -> IO ()
setTileGroup_forColumn_row skTileMapNode tileGroup column row =
  sendMessage skTileMapNode setTileGroup_forColumn_rowSelector (toSKTileGroup tileGroup) column row

-- | Set the tile group and tile defintion at the specified tile index. When automapping is enabled, it will attempt to resolve the surrounding tiles to allow the specified tile definition to be placed. When automapping is disabled, it will simply place the tile definition and not modify any of the neighboring tiles.
--
-- @tileGroup@ — the tile group we want to place in the map
--
-- @tileDefinition@ — the tile definition we want to place in the map
--
-- @column@ — the column index of the tile
--
-- @row@ — the row index of the tile
--
-- ObjC selector: @- setTileGroup:andTileDefinition:forColumn:row:@
setTileGroup_andTileDefinition_forColumn_row :: (IsSKTileMapNode skTileMapNode, IsSKTileGroup tileGroup, IsSKTileDefinition tileDefinition) => skTileMapNode -> tileGroup -> tileDefinition -> CULong -> CULong -> IO ()
setTileGroup_andTileDefinition_forColumn_row skTileMapNode tileGroup tileDefinition column row =
  sendMessage skTileMapNode setTileGroup_andTileDefinition_forColumn_rowSelector (toSKTileGroup tileGroup) (toSKTileDefinition tileDefinition) column row

-- | The number of columns in the tile map.
--
-- ObjC selector: @- numberOfColumns@
numberOfColumns :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CULong
numberOfColumns skTileMapNode =
  sendMessage skTileMapNode numberOfColumnsSelector

-- | The number of columns in the tile map.
--
-- ObjC selector: @- setNumberOfColumns:@
setNumberOfColumns :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> IO ()
setNumberOfColumns skTileMapNode value =
  sendMessage skTileMapNode setNumberOfColumnsSelector value

-- | The number of rows in the tile map.
--
-- ObjC selector: @- numberOfRows@
numberOfRows :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CULong
numberOfRows skTileMapNode =
  sendMessage skTileMapNode numberOfRowsSelector

-- | The number of rows in the tile map.
--
-- ObjC selector: @- setNumberOfRows:@
setNumberOfRows :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> IO ()
setNumberOfRows skTileMapNode value =
  sendMessage skTileMapNode setNumberOfRowsSelector value

-- | The tile set being used by this tile map.
--
-- ObjC selector: @- tileSet@
tileSet :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id SKTileSet)
tileSet skTileMapNode =
  sendMessage skTileMapNode tileSetSelector

-- | The tile set being used by this tile map.
--
-- ObjC selector: @- setTileSet:@
setTileSet :: (IsSKTileMapNode skTileMapNode, IsSKTileSet value) => skTileMapNode -> value -> IO ()
setTileSet skTileMapNode value =
  sendMessage skTileMapNode setTileSetSelector (toSKTileSet value)

-- | Controls the blending between the texture and the tile map color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- colorBlendFactor@
colorBlendFactor :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CDouble
colorBlendFactor skTileMapNode =
  sendMessage skTileMapNode colorBlendFactorSelector

-- | Controls the blending between the texture and the tile map color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- setColorBlendFactor:@
setColorBlendFactor :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CDouble -> IO ()
setColorBlendFactor skTileMapNode value =
  sendMessage skTileMapNode setColorBlendFactorSelector value

-- | Base color for the tile map (If no texture is present, the color still is drawn).
--
-- ObjC selector: @- color@
color :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id NSColor)
color skTileMapNode =
  sendMessage skTileMapNode colorSelector

-- | Base color for the tile map (If no texture is present, the color still is drawn).
--
-- ObjC selector: @- setColor:@
setColor :: (IsSKTileMapNode skTileMapNode, IsNSColor value) => skTileMapNode -> value -> IO ()
setColor skTileMapNode value =
  sendMessage skTileMapNode setColorSelector (toNSColor value)

-- | Sets the blend mode to use when composing the tile map with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO SKBlendMode
blendMode skTileMapNode =
  sendMessage skTileMapNode blendModeSelector

-- | Sets the blend mode to use when composing the tile map with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKTileMapNode skTileMapNode => skTileMapNode -> SKBlendMode -> IO ()
setBlendMode skTileMapNode value =
  sendMessage skTileMapNode setBlendModeSelector value

-- | A property that determines whether the tile map is rendered using a custom shader.
--
-- ObjC selector: @- shader@
shader :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id SKShader)
shader skTileMapNode =
  sendMessage skTileMapNode shaderSelector

-- | A property that determines whether the tile map is rendered using a custom shader.
--
-- ObjC selector: @- setShader:@
setShader :: (IsSKTileMapNode skTileMapNode, IsSKShader value) => skTileMapNode -> value -> IO ()
setShader skTileMapNode value =
  sendMessage skTileMapNode setShaderSelector (toSKShader value)

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id NSDictionary)
attributeValues skTileMapNode =
  sendMessage skTileMapNode attributeValuesSelector

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKTileMapNode skTileMapNode, IsNSDictionary value) => skTileMapNode -> value -> IO ()
setAttributeValues skTileMapNode value =
  sendMessage skTileMapNode setAttributeValuesSelector (toNSDictionary value)

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- lightingBitMask@
lightingBitMask :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CUInt
lightingBitMask skTileMapNode =
  sendMessage skTileMapNode lightingBitMaskSelector

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- setLightingBitMask:@
setLightingBitMask :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CUInt -> IO ()
setLightingBitMask skTileMapNode value =
  sendMessage skTileMapNode setLightingBitMaskSelector value

-- | @- enableAutomapping@
enableAutomapping :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO Bool
enableAutomapping skTileMapNode =
  sendMessage skTileMapNode enableAutomappingSelector

-- | @- setEnableAutomapping:@
setEnableAutomapping :: IsSKTileMapNode skTileMapNode => skTileMapNode -> Bool -> IO ()
setEnableAutomapping skTileMapNode value =
  sendMessage skTileMapNode setEnableAutomappingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector '[Id NSString] (Id SKAttributeValue)
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector '[Id SKAttributeValue, Id NSString] ()
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @fillWithTileGroup:@
fillWithTileGroupSelector :: Selector '[Id SKTileGroup] ()
fillWithTileGroupSelector = mkSelector "fillWithTileGroup:"

-- | @Selector@ for @tileDefinitionAtColumn:row:@
tileDefinitionAtColumn_rowSelector :: Selector '[CULong, CULong] (Id SKTileDefinition)
tileDefinitionAtColumn_rowSelector = mkSelector "tileDefinitionAtColumn:row:"

-- | @Selector@ for @tileGroupAtColumn:row:@
tileGroupAtColumn_rowSelector :: Selector '[CULong, CULong] (Id SKTileGroup)
tileGroupAtColumn_rowSelector = mkSelector "tileGroupAtColumn:row:"

-- | @Selector@ for @setTileGroup:forColumn:row:@
setTileGroup_forColumn_rowSelector :: Selector '[Id SKTileGroup, CULong, CULong] ()
setTileGroup_forColumn_rowSelector = mkSelector "setTileGroup:forColumn:row:"

-- | @Selector@ for @setTileGroup:andTileDefinition:forColumn:row:@
setTileGroup_andTileDefinition_forColumn_rowSelector :: Selector '[Id SKTileGroup, Id SKTileDefinition, CULong, CULong] ()
setTileGroup_andTileDefinition_forColumn_rowSelector = mkSelector "setTileGroup:andTileDefinition:forColumn:row:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CULong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setNumberOfColumns:@
setNumberOfColumnsSelector :: Selector '[CULong] ()
setNumberOfColumnsSelector = mkSelector "setNumberOfColumns:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CULong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @setNumberOfRows:@
setNumberOfRowsSelector :: Selector '[CULong] ()
setNumberOfRowsSelector = mkSelector "setNumberOfRows:"

-- | @Selector@ for @tileSet@
tileSetSelector :: Selector '[] (Id SKTileSet)
tileSetSelector = mkSelector "tileSet"

-- | @Selector@ for @setTileSet:@
setTileSetSelector :: Selector '[Id SKTileSet] ()
setTileSetSelector = mkSelector "setTileSet:"

-- | @Selector@ for @colorBlendFactor@
colorBlendFactorSelector :: Selector '[] CDouble
colorBlendFactorSelector = mkSelector "colorBlendFactor"

-- | @Selector@ for @setColorBlendFactor:@
setColorBlendFactorSelector :: Selector '[CDouble] ()
setColorBlendFactorSelector = mkSelector "setColorBlendFactor:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] SKBlendMode
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[SKBlendMode] ()
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @shader@
shaderSelector :: Selector '[] (Id SKShader)
shaderSelector = mkSelector "shader"

-- | @Selector@ for @setShader:@
setShaderSelector :: Selector '[Id SKShader] ()
setShaderSelector = mkSelector "setShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector '[] (Id NSDictionary)
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector '[Id NSDictionary] ()
setAttributeValuesSelector = mkSelector "setAttributeValues:"

-- | @Selector@ for @lightingBitMask@
lightingBitMaskSelector :: Selector '[] CUInt
lightingBitMaskSelector = mkSelector "lightingBitMask"

-- | @Selector@ for @setLightingBitMask:@
setLightingBitMaskSelector :: Selector '[CUInt] ()
setLightingBitMaskSelector = mkSelector "setLightingBitMask:"

-- | @Selector@ for @enableAutomapping@
enableAutomappingSelector :: Selector '[] Bool
enableAutomappingSelector = mkSelector "enableAutomapping"

-- | @Selector@ for @setEnableAutomapping:@
setEnableAutomappingSelector :: Selector '[Bool] ()
setEnableAutomappingSelector = mkSelector "setEnableAutomapping:"

