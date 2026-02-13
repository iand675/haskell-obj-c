{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKUniform@.
module ObjC.SpriteKit.SKUniform
  ( SKUniform
  , IsSKUniform(..)
  , uniformWithName
  , uniformWithName_texture
  , uniformWithName_float
  , initWithName
  , initWithName_texture
  , initWithName_float
  , name
  , uniformType
  , textureValue
  , setTextureValue
  , floatValue
  , setFloatValue
  , floatValueSelector
  , initWithNameSelector
  , initWithName_floatSelector
  , initWithName_textureSelector
  , nameSelector
  , setFloatValueSelector
  , setTextureValueSelector
  , textureValueSelector
  , uniformTypeSelector
  , uniformWithNameSelector
  , uniformWithName_floatSelector
  , uniformWithName_textureSelector

  -- * Enum types
  , SKUniformType(SKUniformType)
  , pattern SKUniformTypeNone
  , pattern SKUniformTypeFloat
  , pattern SKUniformTypeFloatVector2
  , pattern SKUniformTypeFloatVector3
  , pattern SKUniformTypeFloatVector4
  , pattern SKUniformTypeFloatMatrix2
  , pattern SKUniformTypeFloatMatrix3
  , pattern SKUniformTypeFloatMatrix4
  , pattern SKUniformTypeTexture

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

-- | Create a shader uniform with a given name.
--
-- @name@ — the name of the shader uniform.
--
-- ObjC selector: @+ uniformWithName:@
uniformWithName :: IsNSString name => name -> IO (Id SKUniform)
uniformWithName name =
  do
    cls' <- getRequiredClass "SKUniform"
    sendClassMessage cls' uniformWithNameSelector (toNSString name)

-- | Create a shader uniform with a given name, and texture data
--
-- @name@ — the name of the shader uniform.
--
-- @texture@ — the texture data associated with this uniform.
--
-- ObjC selector: @+ uniformWithName:texture:@
uniformWithName_texture :: (IsNSString name, IsSKTexture texture) => name -> texture -> IO (Id SKUniform)
uniformWithName_texture name texture =
  do
    cls' <- getRequiredClass "SKUniform"
    sendClassMessage cls' uniformWithName_textureSelector (toNSString name) (toSKTexture texture)

-- | Create a shader uniform with a given name, and a float value
--
-- @name@ — the name of the shader uniform.
--
-- @value@ — the floating point value associated with this uniform.
--
-- ObjC selector: @+ uniformWithName:float:@
uniformWithName_float :: IsNSString name => name -> CFloat -> IO (Id SKUniform)
uniformWithName_float name value =
  do
    cls' <- getRequiredClass "SKUniform"
    sendClassMessage cls' uniformWithName_floatSelector (toNSString name) value

-- | @- initWithName:@
initWithName :: (IsSKUniform skUniform, IsNSString name) => skUniform -> name -> IO (Id SKUniform)
initWithName skUniform name =
  sendOwnedMessage skUniform initWithNameSelector (toNSString name)

-- | @- initWithName:texture:@
initWithName_texture :: (IsSKUniform skUniform, IsNSString name, IsSKTexture texture) => skUniform -> name -> texture -> IO (Id SKUniform)
initWithName_texture skUniform name texture =
  sendOwnedMessage skUniform initWithName_textureSelector (toNSString name) (toSKTexture texture)

-- | @- initWithName:float:@
initWithName_float :: (IsSKUniform skUniform, IsNSString name) => skUniform -> name -> CFloat -> IO (Id SKUniform)
initWithName_float skUniform name value =
  sendOwnedMessage skUniform initWithName_floatSelector (toNSString name) value

-- | @- name@
name :: IsSKUniform skUniform => skUniform -> IO (Id NSString)
name skUniform =
  sendMessage skUniform nameSelector

-- | @- uniformType@
uniformType :: IsSKUniform skUniform => skUniform -> IO SKUniformType
uniformType skUniform =
  sendMessage skUniform uniformTypeSelector

-- | @- textureValue@
textureValue :: IsSKUniform skUniform => skUniform -> IO (Id SKTexture)
textureValue skUniform =
  sendMessage skUniform textureValueSelector

-- | @- setTextureValue:@
setTextureValue :: (IsSKUniform skUniform, IsSKTexture value) => skUniform -> value -> IO ()
setTextureValue skUniform value =
  sendMessage skUniform setTextureValueSelector (toSKTexture value)

-- | @- floatValue@
floatValue :: IsSKUniform skUniform => skUniform -> IO CFloat
floatValue skUniform =
  sendMessage skUniform floatValueSelector

-- | @- setFloatValue:@
setFloatValue :: IsSKUniform skUniform => skUniform -> CFloat -> IO ()
setFloatValue skUniform value =
  sendMessage skUniform setFloatValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniformWithName:@
uniformWithNameSelector :: Selector '[Id NSString] (Id SKUniform)
uniformWithNameSelector = mkSelector "uniformWithName:"

-- | @Selector@ for @uniformWithName:texture:@
uniformWithName_textureSelector :: Selector '[Id NSString, Id SKTexture] (Id SKUniform)
uniformWithName_textureSelector = mkSelector "uniformWithName:texture:"

-- | @Selector@ for @uniformWithName:float:@
uniformWithName_floatSelector :: Selector '[Id NSString, CFloat] (Id SKUniform)
uniformWithName_floatSelector = mkSelector "uniformWithName:float:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id SKUniform)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:texture:@
initWithName_textureSelector :: Selector '[Id NSString, Id SKTexture] (Id SKUniform)
initWithName_textureSelector = mkSelector "initWithName:texture:"

-- | @Selector@ for @initWithName:float:@
initWithName_floatSelector :: Selector '[Id NSString, CFloat] (Id SKUniform)
initWithName_floatSelector = mkSelector "initWithName:float:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @uniformType@
uniformTypeSelector :: Selector '[] SKUniformType
uniformTypeSelector = mkSelector "uniformType"

-- | @Selector@ for @textureValue@
textureValueSelector :: Selector '[] (Id SKTexture)
textureValueSelector = mkSelector "textureValue"

-- | @Selector@ for @setTextureValue:@
setTextureValueSelector :: Selector '[Id SKTexture] ()
setTextureValueSelector = mkSelector "setTextureValue:"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector '[] CFloat
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector '[CFloat] ()
setFloatValueSelector = mkSelector "setFloatValue:"

