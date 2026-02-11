{-# LANGUAGE PatternSynonyms #-}
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
  , tileDefinitionWithTextureSelector
  , initWithTextureSelector
  , texturesSelector
  , setTexturesSelector
  , normalTexturesSelector
  , setNormalTexturesSelector
  , userDataSelector
  , setUserDataSelector
  , nameSelector
  , setNameSelector
  , timePerFrameSelector
  , setTimePerFrameSelector
  , placementWeightSelector
  , setPlacementWeightSelector
  , rotationSelector
  , setRotationSelector
  , flipVerticallySelector
  , setFlipVerticallySelector
  , flipHorizontallySelector
  , setFlipHorizontallySelector

  -- * Enum types
  , SKTileDefinitionRotation(SKTileDefinitionRotation)
  , pattern SKTileDefinitionRotation0
  , pattern SKTileDefinitionRotation90
  , pattern SKTileDefinitionRotation180
  , pattern SKTileDefinitionRotation270

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
    withObjCPtr texture $ \raw_texture ->
      sendClassMsg cls' (mkSelector "tileDefinitionWithTexture:") (retPtr retVoid) [argPtr (castPtr raw_texture :: Ptr ())] >>= retainedObject . castPtr

-- | Initilize a tile definition with an SKTexture, and set its size to the SKTexture's width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @- initWithTexture:@
initWithTexture :: (IsSKTileDefinition skTileDefinition, IsSKTexture texture) => skTileDefinition -> texture -> IO (Id SKTileDefinition)
initWithTexture skTileDefinition  texture =
withObjCPtr texture $ \raw_texture ->
    sendMsg skTileDefinition (mkSelector "initWithTexture:") (retPtr retVoid) [argPtr (castPtr raw_texture :: Ptr ())] >>= ownedObject . castPtr

-- | The textures used to draw the tile. Non-animated tiles use only one texture. When more than one texture is present, the tile will swap through them in sequence, showing each for the duration specified in the timePerFrame property. After displaying the last texture in the array, the sequence is repeated from the first texture.
--
-- ObjC selector: @- textures@
textures :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSArray)
textures skTileDefinition  =
  sendMsg skTileDefinition (mkSelector "textures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The textures used to draw the tile. Non-animated tiles use only one texture. When more than one texture is present, the tile will swap through them in sequence, showing each for the duration specified in the timePerFrame property. After displaying the last texture in the array, the sequence is repeated from the first texture.
--
-- ObjC selector: @- setTextures:@
setTextures :: (IsSKTileDefinition skTileDefinition, IsNSArray value) => skTileDefinition -> value -> IO ()
setTextures skTileDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileDefinition (mkSelector "setTextures:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The textures to use for generating normals that lights use to light this tile. These will only be used if the tile is lit by at least one light. Each normal texture corresponds to a texture in the textures property.
--
-- ObjC selector: @- normalTextures@
normalTextures :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSArray)
normalTextures skTileDefinition  =
  sendMsg skTileDefinition (mkSelector "normalTextures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The textures to use for generating normals that lights use to light this tile. These will only be used if the tile is lit by at least one light. Each normal texture corresponds to a texture in the textures property.
--
-- ObjC selector: @- setNormalTextures:@
setNormalTextures :: (IsSKTileDefinition skTileDefinition, IsNSArray value) => skTileDefinition -> value -> IO ()
setNormalTextures skTileDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileDefinition (mkSelector "setNormalTextures:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An optional dictionary that can be used to store your own data for each tile definition. Defaults to nil.
--
-- ObjC selector: @- userData@
userData :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSMutableDictionary)
userData skTileDefinition  =
  sendMsg skTileDefinition (mkSelector "userData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional dictionary that can be used to store your own data for each tile definition. Defaults to nil.
--
-- ObjC selector: @- setUserData:@
setUserData :: (IsSKTileDefinition skTileDefinition, IsNSMutableDictionary value) => skTileDefinition -> value -> IO ()
setUserData skTileDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileDefinition (mkSelector "setUserData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Client-assignable name for the tile definition. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO (Id NSString)
name skTileDefinition  =
  sendMsg skTileDefinition (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Client-assignable name for the tile definition. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileDefinition skTileDefinition, IsNSString value) => skTileDefinition -> value -> IO ()
setName skTileDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileDefinition (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The duration, in seconds, that each texture in the textures array is displayed before switching to the next texture in the sequence. Only used when there is more than one texture available.
--
-- ObjC selector: @- timePerFrame@
timePerFrame :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO CDouble
timePerFrame skTileDefinition  =
  sendMsg skTileDefinition (mkSelector "timePerFrame") retCDouble []

-- | The duration, in seconds, that each texture in the textures array is displayed before switching to the next texture in the sequence. Only used when there is more than one texture available.
--
-- ObjC selector: @- setTimePerFrame:@
setTimePerFrame :: IsSKTileDefinition skTileDefinition => skTileDefinition -> CDouble -> IO ()
setTimePerFrame skTileDefinition  value =
  sendMsg skTileDefinition (mkSelector "setTimePerFrame:") retVoid [argCDouble (fromIntegral value)]

-- | This value is used to determine how likely this tile definition is to be chosen for placement when a SKTileGroupRule has mulitple tile definitions assigned to it. A higher value relative to the other definitions assigned to the rule make it more likely for this definition to be selected; lower values make it less likely. Defaults to 1. When set to 0, the definition will never be chosen as long as there is at least one other definition with a placementWeight above 0.
--
-- ObjC selector: @- placementWeight@
placementWeight :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO CULong
placementWeight skTileDefinition  =
  sendMsg skTileDefinition (mkSelector "placementWeight") retCULong []

-- | This value is used to determine how likely this tile definition is to be chosen for placement when a SKTileGroupRule has mulitple tile definitions assigned to it. A higher value relative to the other definitions assigned to the rule make it more likely for this definition to be selected; lower values make it less likely. Defaults to 1. When set to 0, the definition will never be chosen as long as there is at least one other definition with a placementWeight above 0.
--
-- ObjC selector: @- setPlacementWeight:@
setPlacementWeight :: IsSKTileDefinition skTileDefinition => skTileDefinition -> CULong -> IO ()
setPlacementWeight skTileDefinition  value =
  sendMsg skTileDefinition (mkSelector "setPlacementWeight:") retVoid [argCULong (fromIntegral value)]

-- | The rotation of the tile definition's images can be set in 90 degree increments. Defaults to SKTileDefinitionRotation0.
--
-- ObjC selector: @- rotation@
rotation :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO SKTileDefinitionRotation
rotation skTileDefinition  =
  fmap (coerce :: CULong -> SKTileDefinitionRotation) $ sendMsg skTileDefinition (mkSelector "rotation") retCULong []

-- | The rotation of the tile definition's images can be set in 90 degree increments. Defaults to SKTileDefinitionRotation0.
--
-- ObjC selector: @- setRotation:@
setRotation :: IsSKTileDefinition skTileDefinition => skTileDefinition -> SKTileDefinitionRotation -> IO ()
setRotation skTileDefinition  value =
  sendMsg skTileDefinition (mkSelector "setRotation:") retVoid [argCULong (coerce value)]

-- | When set to YES, the tile definition's images will be flipped vertically (i.e., the top of the image becomes the bottom). Defaults to NO.
--
-- ObjC selector: @- flipVertically@
flipVertically :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO Bool
flipVertically skTileDefinition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skTileDefinition (mkSelector "flipVertically") retCULong []

-- | When set to YES, the tile definition's images will be flipped vertically (i.e., the top of the image becomes the bottom). Defaults to NO.
--
-- ObjC selector: @- setFlipVertically:@
setFlipVertically :: IsSKTileDefinition skTileDefinition => skTileDefinition -> Bool -> IO ()
setFlipVertically skTileDefinition  value =
  sendMsg skTileDefinition (mkSelector "setFlipVertically:") retVoid [argCULong (if value then 1 else 0)]

-- | When set to YES, the tile definition's images will be flipped horizontally (i.e., the left of the image becomes the right). Defaults to NO.
--
-- ObjC selector: @- flipHorizontally@
flipHorizontally :: IsSKTileDefinition skTileDefinition => skTileDefinition -> IO Bool
flipHorizontally skTileDefinition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skTileDefinition (mkSelector "flipHorizontally") retCULong []

-- | When set to YES, the tile definition's images will be flipped horizontally (i.e., the left of the image becomes the right). Defaults to NO.
--
-- ObjC selector: @- setFlipHorizontally:@
setFlipHorizontally :: IsSKTileDefinition skTileDefinition => skTileDefinition -> Bool -> IO ()
setFlipHorizontally skTileDefinition  value =
  sendMsg skTileDefinition (mkSelector "setFlipHorizontally:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileDefinitionWithTexture:@
tileDefinitionWithTextureSelector :: Selector
tileDefinitionWithTextureSelector = mkSelector "tileDefinitionWithTexture:"

-- | @Selector@ for @initWithTexture:@
initWithTextureSelector :: Selector
initWithTextureSelector = mkSelector "initWithTexture:"

-- | @Selector@ for @textures@
texturesSelector :: Selector
texturesSelector = mkSelector "textures"

-- | @Selector@ for @setTextures:@
setTexturesSelector :: Selector
setTexturesSelector = mkSelector "setTextures:"

-- | @Selector@ for @normalTextures@
normalTexturesSelector :: Selector
normalTexturesSelector = mkSelector "normalTextures"

-- | @Selector@ for @setNormalTextures:@
setNormalTexturesSelector :: Selector
setNormalTexturesSelector = mkSelector "setNormalTextures:"

-- | @Selector@ for @userData@
userDataSelector :: Selector
userDataSelector = mkSelector "userData"

-- | @Selector@ for @setUserData:@
setUserDataSelector :: Selector
setUserDataSelector = mkSelector "setUserData:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @timePerFrame@
timePerFrameSelector :: Selector
timePerFrameSelector = mkSelector "timePerFrame"

-- | @Selector@ for @setTimePerFrame:@
setTimePerFrameSelector :: Selector
setTimePerFrameSelector = mkSelector "setTimePerFrame:"

-- | @Selector@ for @placementWeight@
placementWeightSelector :: Selector
placementWeightSelector = mkSelector "placementWeight"

-- | @Selector@ for @setPlacementWeight:@
setPlacementWeightSelector :: Selector
setPlacementWeightSelector = mkSelector "setPlacementWeight:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @flipVertically@
flipVerticallySelector :: Selector
flipVerticallySelector = mkSelector "flipVertically"

-- | @Selector@ for @setFlipVertically:@
setFlipVerticallySelector :: Selector
setFlipVerticallySelector = mkSelector "setFlipVertically:"

-- | @Selector@ for @flipHorizontally@
flipHorizontallySelector :: Selector
flipHorizontallySelector = mkSelector "flipHorizontally"

-- | @Selector@ for @setFlipHorizontally:@
setFlipHorizontallySelector :: Selector
setFlipHorizontallySelector = mkSelector "setFlipHorizontally:"

