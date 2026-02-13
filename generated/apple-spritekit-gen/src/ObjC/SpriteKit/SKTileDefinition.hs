{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tile definition contains the information needed to represent a single type of tile within a tile map.
--
-- Generated bindings for @SKTileDefinition@.
module ObjC.SpriteKit.SKTileDefinition
  ( SKTileDefinition
  , IsSKTileDefinition(..)
  , tileDefinitionWithTexture
  , initWithTexture
  , textures
  , setTextures
  , normalTextures
  , setNormalTextures
  , userData
  , setUserData
  , name
  , setName
  , timePerFrame
  , setTimePerFrame
  , placementWeight
  , setPlacementWeight
  , rotation
  , setRotation
  , flipVertically
  , setFlipVertically
  , flipHorizontally
  , setFlipHorizontally
  , flipHorizontallySelector
  , flipVerticallySelector
  , initWithTextureSelector
  , nameSelector
  , normalTexturesSelector
  , placementWeightSelector
  , rotationSelector
  , setFlipHorizontallySelector
  , setFlipVerticallySelector
  , setNameSelector
  , setNormalTexturesSelector
  , setPlacementWeightSelector
  , setRotationSelector
  , setTexturesSelector
  , setTimePerFrameSelector
  , setUserDataSelector
  , texturesSelector
  , tileDefinitionWithTextureSelector
  , timePerFrameSelector
  , userDataSelector

  -- * Enum types
  , SKTileDefinitionRotation(SKTileDefinitionRotation)
  , pattern SKTileDefinitionRotation0
  , pattern SKTileDefinitionRotation90
  , pattern SKTileDefinitionRotation180
  , pattern SKTileDefinitionRotation270

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a tile definition with an SKTexture, and set its size to the SKTexture's width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @+ tileDefinitionWithTexture:@
tileDefinitionWithTexture :: IsSKTexture texture => texture -> IO (Id SKTileDefinition)
tileDefinitionWithTexture texture =
  do
    cls' <- getRequiredClass "SKTileDefinition"
    sendClassMessage cls' tileDefinitionWithTextureSelector (toSKTexture texture)

-- | Initilize a tile definition with an SKTexture, and set its size to the SKTexture's width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @- initWithTexture:@
initWithTexture :: (IsSKTileDefinition skTileDefinition, IsSKTexture texture) => skTileDefinition -> texture -> IO (Id SKTileDefinition)
initWithTexture skTileDefinition texture =
  sendOwnedMessage skTileDefinition initWithTextureSelector (toSKTexture texture)

-- | The textures used to draw the tile. Non-animated tiles use only one texture. When more than one texture is present, the tile will swap through them in sequence, showing each for the duration specified in the timePerFrame property. After displaying the last texture in the array, the sequence is repeated from the first texture.
--
-- ObjC selector: @- textures@
textures :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSArray)
textures skTileDefinition =
  sendMessage skTileDefinition texturesSelector

-- | The textures used to draw the tile. Non-animated tiles use only one texture. When more than one texture is present, the tile will swap through them in sequence, showing each for the duration specified in the timePerFrame property. After displaying the last texture in the array, the sequence is repeated from the first texture.
--
-- ObjC selector: @- setTextures:@
setTextures :: (IsSKTileDefinition skTileDefinition, IsNSArray value) => skTileDefinition -> value -> IO ()
setTextures skTileDefinition value =
  sendMessage skTileDefinition setTexturesSelector (toNSArray value)

-- | The textures to use for generating normals that lights use to light this tile. These will only be used if the tile is lit by at least one light. Each normal texture corresponds to a texture in the textures property.
--
-- ObjC selector: @- normalTextures@
normalTextures :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSArray)
normalTextures skTileDefinition =
  sendMessage skTileDefinition normalTexturesSelector

-- | The textures to use for generating normals that lights use to light this tile. These will only be used if the tile is lit by at least one light. Each normal texture corresponds to a texture in the textures property.
--
-- ObjC selector: @- setNormalTextures:@
setNormalTextures :: (IsSKTileDefinition skTileDefinition, IsNSArray value) => skTileDefinition -> value -> IO ()
setNormalTextures skTileDefinition value =
  sendMessage skTileDefinition setNormalTexturesSelector (toNSArray value)

-- | An optional dictionary that can be used to store your own data for each tile definition. Defaults to nil.
--
-- ObjC selector: @- userData@
userData :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSMutableDictionary)
userData skTileDefinition =
  sendMessage skTileDefinition userDataSelector

-- | An optional dictionary that can be used to store your own data for each tile definition. Defaults to nil.
--
-- ObjC selector: @- setUserData:@
setUserData :: (IsSKTileDefinition skTileDefinition, IsNSMutableDictionary value) => skTileDefinition -> value -> IO ()
setUserData skTileDefinition value =
  sendMessage skTileDefinition setUserDataSelector (toNSMutableDictionary value)

-- | Client-assignable name for the tile definition. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSString)
name skTileDefinition =
  sendMessage skTileDefinition nameSelector

-- | Client-assignable name for the tile definition. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileDefinition skTileDefinition, IsNSString value) => skTileDefinition -> value -> IO ()
setName skTileDefinition value =
  sendMessage skTileDefinition setNameSelector (toNSString value)

-- | The duration, in seconds, that each texture in the textures array is displayed before switching to the next texture in the sequence. Only used when there is more than one texture available.
--
-- ObjC selector: @- timePerFrame@
timePerFrame :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO CDouble
timePerFrame skTileDefinition =
  sendMessage skTileDefinition timePerFrameSelector

-- | The duration, in seconds, that each texture in the textures array is displayed before switching to the next texture in the sequence. Only used when there is more than one texture available.
--
-- ObjC selector: @- setTimePerFrame:@
setTimePerFrame :: IsSKTileDefinition skTileDefinition => skTileDefinition -> CDouble -> IO ()
setTimePerFrame skTileDefinition value =
  sendMessage skTileDefinition setTimePerFrameSelector value

-- | This value is used to determine how likely this tile definition is to be chosen for placement when a SKTileGroupRule has mulitple tile definitions assigned to it. A higher value relative to the other definitions assigned to the rule make it more likely for this definition to be selected; lower values make it less likely. Defaults to 1. When set to 0, the definition will never be chosen as long as there is at least one other definition with a placementWeight above 0.
--
-- ObjC selector: @- placementWeight@
placementWeight :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO CULong
placementWeight skTileDefinition =
  sendMessage skTileDefinition placementWeightSelector

-- | This value is used to determine how likely this tile definition is to be chosen for placement when a SKTileGroupRule has mulitple tile definitions assigned to it. A higher value relative to the other definitions assigned to the rule make it more likely for this definition to be selected; lower values make it less likely. Defaults to 1. When set to 0, the definition will never be chosen as long as there is at least one other definition with a placementWeight above 0.
--
-- ObjC selector: @- setPlacementWeight:@
setPlacementWeight :: IsSKTileDefinition skTileDefinition => skTileDefinition -> CULong -> IO ()
setPlacementWeight skTileDefinition value =
  sendMessage skTileDefinition setPlacementWeightSelector value

-- | The rotation of the tile definition's images can be set in 90 degree increments. Defaults to SKTileDefinitionRotation0.
--
-- ObjC selector: @- rotation@
rotation :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO SKTileDefinitionRotation
rotation skTileDefinition =
  sendMessage skTileDefinition rotationSelector

-- | The rotation of the tile definition's images can be set in 90 degree increments. Defaults to SKTileDefinitionRotation0.
--
-- ObjC selector: @- setRotation:@
setRotation :: IsSKTileDefinition skTileDefinition => skTileDefinition -> SKTileDefinitionRotation -> IO ()
setRotation skTileDefinition value =
  sendMessage skTileDefinition setRotationSelector value

-- | When set to YES, the tile definition's images will be flipped vertically (i.e., the top of the image becomes the bottom). Defaults to NO.
--
-- ObjC selector: @- flipVertically@
flipVertically :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO Bool
flipVertically skTileDefinition =
  sendMessage skTileDefinition flipVerticallySelector

-- | When set to YES, the tile definition's images will be flipped vertically (i.e., the top of the image becomes the bottom). Defaults to NO.
--
-- ObjC selector: @- setFlipVertically:@
setFlipVertically :: IsSKTileDefinition skTileDefinition => skTileDefinition -> Bool -> IO ()
setFlipVertically skTileDefinition value =
  sendMessage skTileDefinition setFlipVerticallySelector value

-- | When set to YES, the tile definition's images will be flipped horizontally (i.e., the left of the image becomes the right). Defaults to NO.
--
-- ObjC selector: @- flipHorizontally@
flipHorizontally :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO Bool
flipHorizontally skTileDefinition =
  sendMessage skTileDefinition flipHorizontallySelector

-- | When set to YES, the tile definition's images will be flipped horizontally (i.e., the left of the image becomes the right). Defaults to NO.
--
-- ObjC selector: @- setFlipHorizontally:@
setFlipHorizontally :: IsSKTileDefinition skTileDefinition => skTileDefinition -> Bool -> IO ()
setFlipHorizontally skTileDefinition value =
  sendMessage skTileDefinition setFlipHorizontallySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileDefinitionWithTexture:@
tileDefinitionWithTextureSelector :: Selector '[Id SKTexture] (Id SKTileDefinition)
tileDefinitionWithTextureSelector = mkSelector "tileDefinitionWithTexture:"

-- | @Selector@ for @initWithTexture:@
initWithTextureSelector :: Selector '[Id SKTexture] (Id SKTileDefinition)
initWithTextureSelector = mkSelector "initWithTexture:"

-- | @Selector@ for @textures@
texturesSelector :: Selector '[] (Id NSArray)
texturesSelector = mkSelector "textures"

-- | @Selector@ for @setTextures:@
setTexturesSelector :: Selector '[Id NSArray] ()
setTexturesSelector = mkSelector "setTextures:"

-- | @Selector@ for @normalTextures@
normalTexturesSelector :: Selector '[] (Id NSArray)
normalTexturesSelector = mkSelector "normalTextures"

-- | @Selector@ for @setNormalTextures:@
setNormalTexturesSelector :: Selector '[Id NSArray] ()
setNormalTexturesSelector = mkSelector "setNormalTextures:"

-- | @Selector@ for @userData@
userDataSelector :: Selector '[] (Id NSMutableDictionary)
userDataSelector = mkSelector "userData"

-- | @Selector@ for @setUserData:@
setUserDataSelector :: Selector '[Id NSMutableDictionary] ()
setUserDataSelector = mkSelector "setUserData:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @timePerFrame@
timePerFrameSelector :: Selector '[] CDouble
timePerFrameSelector = mkSelector "timePerFrame"

-- | @Selector@ for @setTimePerFrame:@
setTimePerFrameSelector :: Selector '[CDouble] ()
setTimePerFrameSelector = mkSelector "setTimePerFrame:"

-- | @Selector@ for @placementWeight@
placementWeightSelector :: Selector '[] CULong
placementWeightSelector = mkSelector "placementWeight"

-- | @Selector@ for @setPlacementWeight:@
setPlacementWeightSelector :: Selector '[CULong] ()
setPlacementWeightSelector = mkSelector "setPlacementWeight:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector '[] SKTileDefinitionRotation
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector '[SKTileDefinitionRotation] ()
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @flipVertically@
flipVerticallySelector :: Selector '[] Bool
flipVerticallySelector = mkSelector "flipVertically"

-- | @Selector@ for @setFlipVertically:@
setFlipVerticallySelector :: Selector '[Bool] ()
setFlipVerticallySelector = mkSelector "setFlipVertically:"

-- | @Selector@ for @flipHorizontally@
flipHorizontallySelector :: Selector '[] Bool
flipHorizontallySelector = mkSelector "flipHorizontally"

-- | @Selector@ for @setFlipHorizontally:@
setFlipHorizontallySelector :: Selector '[Bool] ()
setFlipHorizontallySelector = mkSelector "setFlipHorizontally:"

