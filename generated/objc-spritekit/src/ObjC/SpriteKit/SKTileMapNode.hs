{-# LANGUAGE PatternSynonyms #-}
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
  , valueForAttributeNamedSelector
  , setValue_forAttributeNamedSelector
  , fillWithTileGroupSelector
  , tileDefinitionAtColumn_rowSelector
  , tileGroupAtColumn_rowSelector
  , setTileGroup_forColumn_rowSelector
  , setTileGroup_andTileDefinition_forColumn_rowSelector
  , numberOfColumnsSelector
  , setNumberOfColumnsSelector
  , numberOfRowsSelector
  , setNumberOfRowsSelector
  , tileSetSelector
  , setTileSetSelector
  , colorBlendFactorSelector
  , setColorBlendFactorSelector
  , colorSelector
  , setColorSelector
  , blendModeSelector
  , setBlendModeSelector
  , shaderSelector
  , setShaderSelector
  , attributeValuesSelector
  , setAttributeValuesSelector
  , lightingBitMaskSelector
  , setLightingBitMaskSelector
  , enableAutomappingSelector
  , setEnableAutomappingSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKTileMapNode skTileMapNode, IsNSString key) => skTileMapNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skTileMapNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skTileMapNode (mkSelector "valueForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKTileMapNode skTileMapNode, IsSKAttributeValue value, IsNSString key) => skTileMapNode -> value -> key -> IO ()
setValue_forAttributeNamed skTileMapNode  value key =
withObjCPtr value $ \raw_value ->
  withObjCPtr key $ \raw_key ->
      sendMsg skTileMapNode (mkSelector "setValue:forAttributeNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | Fill the entire tile map with the provided tile group.
--
-- @tileGroup@ — the tile group that will be used to fill the map
--
-- ObjC selector: @- fillWithTileGroup:@
fillWithTileGroup :: (IsSKTileMapNode skTileMapNode, IsSKTileGroup tileGroup) => skTileMapNode -> tileGroup -> IO ()
fillWithTileGroup skTileMapNode  tileGroup =
withObjCPtr tileGroup $ \raw_tileGroup ->
    sendMsg skTileMapNode (mkSelector "fillWithTileGroup:") retVoid [argPtr (castPtr raw_tileGroup :: Ptr ())]

-- | Look up the tile definition at the specified tile index.
--
-- @column@ — the column index of the tile
--
-- @row@ — the row index of the tile
--
-- ObjC selector: @- tileDefinitionAtColumn:row:@
tileDefinitionAtColumn_row :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> CULong -> IO (Id SKTileDefinition)
tileDefinitionAtColumn_row skTileMapNode  column row =
  sendMsg skTileMapNode (mkSelector "tileDefinitionAtColumn:row:") (retPtr retVoid) [argCULong (fromIntegral column), argCULong (fromIntegral row)] >>= retainedObject . castPtr

-- | Look up the tile group at the specified tile index.
--
-- @column@ — the column index of the tile
--
-- @row@ — the row index of the tile
--
-- ObjC selector: @- tileGroupAtColumn:row:@
tileGroupAtColumn_row :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> CULong -> IO (Id SKTileGroup)
tileGroupAtColumn_row skTileMapNode  column row =
  sendMsg skTileMapNode (mkSelector "tileGroupAtColumn:row:") (retPtr retVoid) [argCULong (fromIntegral column), argCULong (fromIntegral row)] >>= retainedObject . castPtr

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
setTileGroup_forColumn_row skTileMapNode  tileGroup column row =
withObjCPtr tileGroup $ \raw_tileGroup ->
    sendMsg skTileMapNode (mkSelector "setTileGroup:forColumn:row:") retVoid [argPtr (castPtr raw_tileGroup :: Ptr ()), argCULong (fromIntegral column), argCULong (fromIntegral row)]

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
setTileGroup_andTileDefinition_forColumn_row skTileMapNode  tileGroup tileDefinition column row =
withObjCPtr tileGroup $ \raw_tileGroup ->
  withObjCPtr tileDefinition $ \raw_tileDefinition ->
      sendMsg skTileMapNode (mkSelector "setTileGroup:andTileDefinition:forColumn:row:") retVoid [argPtr (castPtr raw_tileGroup :: Ptr ()), argPtr (castPtr raw_tileDefinition :: Ptr ()), argCULong (fromIntegral column), argCULong (fromIntegral row)]

-- | The number of columns in the tile map.
--
-- ObjC selector: @- numberOfColumns@
numberOfColumns :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CULong
numberOfColumns skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "numberOfColumns") retCULong []

-- | The number of columns in the tile map.
--
-- ObjC selector: @- setNumberOfColumns:@
setNumberOfColumns :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> IO ()
setNumberOfColumns skTileMapNode  value =
  sendMsg skTileMapNode (mkSelector "setNumberOfColumns:") retVoid [argCULong (fromIntegral value)]

-- | The number of rows in the tile map.
--
-- ObjC selector: @- numberOfRows@
numberOfRows :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CULong
numberOfRows skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "numberOfRows") retCULong []

-- | The number of rows in the tile map.
--
-- ObjC selector: @- setNumberOfRows:@
setNumberOfRows :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CULong -> IO ()
setNumberOfRows skTileMapNode  value =
  sendMsg skTileMapNode (mkSelector "setNumberOfRows:") retVoid [argCULong (fromIntegral value)]

-- | The tile set being used by this tile map.
--
-- ObjC selector: @- tileSet@
tileSet :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id SKTileSet)
tileSet skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "tileSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The tile set being used by this tile map.
--
-- ObjC selector: @- setTileSet:@
setTileSet :: (IsSKTileMapNode skTileMapNode, IsSKTileSet value) => skTileMapNode -> value -> IO ()
setTileSet skTileMapNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileMapNode (mkSelector "setTileSet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls the blending between the texture and the tile map color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- colorBlendFactor@
colorBlendFactor :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CDouble
colorBlendFactor skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "colorBlendFactor") retCDouble []

-- | Controls the blending between the texture and the tile map color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- setColorBlendFactor:@
setColorBlendFactor :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CDouble -> IO ()
setColorBlendFactor skTileMapNode  value =
  sendMsg skTileMapNode (mkSelector "setColorBlendFactor:") retVoid [argCDouble (fromIntegral value)]

-- | Base color for the tile map (If no texture is present, the color still is drawn).
--
-- ObjC selector: @- color@
color :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id NSColor)
color skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Base color for the tile map (If no texture is present, the color still is drawn).
--
-- ObjC selector: @- setColor:@
setColor :: (IsSKTileMapNode skTileMapNode, IsNSColor value) => skTileMapNode -> value -> IO ()
setColor skTileMapNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileMapNode (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the blend mode to use when composing the tile map with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO SKBlendMode
blendMode skTileMapNode  =
  fmap (coerce :: CLong -> SKBlendMode) $ sendMsg skTileMapNode (mkSelector "blendMode") retCLong []

-- | Sets the blend mode to use when composing the tile map with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKTileMapNode skTileMapNode => skTileMapNode -> SKBlendMode -> IO ()
setBlendMode skTileMapNode  value =
  sendMsg skTileMapNode (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- | A property that determines whether the tile map is rendered using a custom shader.
--
-- ObjC selector: @- shader@
shader :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id SKShader)
shader skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "shader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A property that determines whether the tile map is rendered using a custom shader.
--
-- ObjC selector: @- setShader:@
setShader :: (IsSKTileMapNode skTileMapNode, IsSKShader value) => skTileMapNode -> value -> IO ()
setShader skTileMapNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileMapNode (mkSelector "setShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO (Id NSDictionary)
attributeValues skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "attributeValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKTileMapNode skTileMapNode, IsNSDictionary value) => skTileMapNode -> value -> IO ()
setAttributeValues skTileMapNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileMapNode (mkSelector "setAttributeValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- lightingBitMask@
lightingBitMask :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO CUInt
lightingBitMask skTileMapNode  =
  sendMsg skTileMapNode (mkSelector "lightingBitMask") retCUInt []

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- setLightingBitMask:@
setLightingBitMask :: IsSKTileMapNode skTileMapNode => skTileMapNode -> CUInt -> IO ()
setLightingBitMask skTileMapNode  value =
  sendMsg skTileMapNode (mkSelector "setLightingBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | @- enableAutomapping@
enableAutomapping :: IsSKTileMapNode skTileMapNode => skTileMapNode -> IO Bool
enableAutomapping skTileMapNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skTileMapNode (mkSelector "enableAutomapping") retCULong []

-- | @- setEnableAutomapping:@
setEnableAutomapping :: IsSKTileMapNode skTileMapNode => skTileMapNode -> Bool -> IO ()
setEnableAutomapping skTileMapNode  value =
  sendMsg skTileMapNode (mkSelector "setEnableAutomapping:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @fillWithTileGroup:@
fillWithTileGroupSelector :: Selector
fillWithTileGroupSelector = mkSelector "fillWithTileGroup:"

-- | @Selector@ for @tileDefinitionAtColumn:row:@
tileDefinitionAtColumn_rowSelector :: Selector
tileDefinitionAtColumn_rowSelector = mkSelector "tileDefinitionAtColumn:row:"

-- | @Selector@ for @tileGroupAtColumn:row:@
tileGroupAtColumn_rowSelector :: Selector
tileGroupAtColumn_rowSelector = mkSelector "tileGroupAtColumn:row:"

-- | @Selector@ for @setTileGroup:forColumn:row:@
setTileGroup_forColumn_rowSelector :: Selector
setTileGroup_forColumn_rowSelector = mkSelector "setTileGroup:forColumn:row:"

-- | @Selector@ for @setTileGroup:andTileDefinition:forColumn:row:@
setTileGroup_andTileDefinition_forColumn_rowSelector :: Selector
setTileGroup_andTileDefinition_forColumn_rowSelector = mkSelector "setTileGroup:andTileDefinition:forColumn:row:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setNumberOfColumns:@
setNumberOfColumnsSelector :: Selector
setNumberOfColumnsSelector = mkSelector "setNumberOfColumns:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @setNumberOfRows:@
setNumberOfRowsSelector :: Selector
setNumberOfRowsSelector = mkSelector "setNumberOfRows:"

-- | @Selector@ for @tileSet@
tileSetSelector :: Selector
tileSetSelector = mkSelector "tileSet"

-- | @Selector@ for @setTileSet:@
setTileSetSelector :: Selector
setTileSetSelector = mkSelector "setTileSet:"

-- | @Selector@ for @colorBlendFactor@
colorBlendFactorSelector :: Selector
colorBlendFactorSelector = mkSelector "colorBlendFactor"

-- | @Selector@ for @setColorBlendFactor:@
setColorBlendFactorSelector :: Selector
setColorBlendFactorSelector = mkSelector "setColorBlendFactor:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @shader@
shaderSelector :: Selector
shaderSelector = mkSelector "shader"

-- | @Selector@ for @setShader:@
setShaderSelector :: Selector
setShaderSelector = mkSelector "setShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector
setAttributeValuesSelector = mkSelector "setAttributeValues:"

-- | @Selector@ for @lightingBitMask@
lightingBitMaskSelector :: Selector
lightingBitMaskSelector = mkSelector "lightingBitMask"

-- | @Selector@ for @setLightingBitMask:@
setLightingBitMaskSelector :: Selector
setLightingBitMaskSelector = mkSelector "setLightingBitMask:"

-- | @Selector@ for @enableAutomapping@
enableAutomappingSelector :: Selector
enableAutomappingSelector = mkSelector "enableAutomapping"

-- | @Selector@ for @setEnableAutomapping:@
setEnableAutomappingSelector :: Selector
setEnableAutomappingSelector = mkSelector "setEnableAutomapping:"

