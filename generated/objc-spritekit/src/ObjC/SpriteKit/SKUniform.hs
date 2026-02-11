{-# LANGUAGE PatternSynonyms #-}
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
  , uniformWithNameSelector
  , uniformWithName_textureSelector
  , uniformWithName_floatSelector
  , initWithNameSelector
  , initWithName_textureSelector
  , initWithName_floatSelector
  , nameSelector
  , uniformTypeSelector
  , textureValueSelector
  , setTextureValueSelector
  , floatValueSelector
  , setFloatValueSelector

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
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "uniformWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr name $ \raw_name ->
      withObjCPtr texture $ \raw_texture ->
        sendClassMsg cls' (mkSelector "uniformWithName:texture:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_texture :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "uniformWithName:float:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCFloat (fromIntegral value)] >>= retainedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsSKUniform skUniform, IsNSString name) => skUniform -> name -> IO (Id SKUniform)
initWithName skUniform  name =
withObjCPtr name $ \raw_name ->
    sendMsg skUniform (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:texture:@
initWithName_texture :: (IsSKUniform skUniform, IsNSString name, IsSKTexture texture) => skUniform -> name -> texture -> IO (Id SKUniform)
initWithName_texture skUniform  name texture =
withObjCPtr name $ \raw_name ->
  withObjCPtr texture $ \raw_texture ->
      sendMsg skUniform (mkSelector "initWithName:texture:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_texture :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:float:@
initWithName_float :: (IsSKUniform skUniform, IsNSString name) => skUniform -> name -> CFloat -> IO (Id SKUniform)
initWithName_float skUniform  name value =
withObjCPtr name $ \raw_name ->
    sendMsg skUniform (mkSelector "initWithName:float:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCFloat (fromIntegral value)] >>= ownedObject . castPtr

-- | @- name@
name :: IsSKUniform skUniform => skUniform -> IO (Id NSString)
name skUniform  =
  sendMsg skUniform (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- uniformType@
uniformType :: IsSKUniform skUniform => skUniform -> IO SKUniformType
uniformType skUniform  =
  fmap (coerce :: CLong -> SKUniformType) $ sendMsg skUniform (mkSelector "uniformType") retCLong []

-- | @- textureValue@
textureValue :: IsSKUniform skUniform => skUniform -> IO (Id SKTexture)
textureValue skUniform  =
  sendMsg skUniform (mkSelector "textureValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextureValue:@
setTextureValue :: (IsSKUniform skUniform, IsSKTexture value) => skUniform -> value -> IO ()
setTextureValue skUniform  value =
withObjCPtr value $ \raw_value ->
    sendMsg skUniform (mkSelector "setTextureValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- floatValue@
floatValue :: IsSKUniform skUniform => skUniform -> IO CFloat
floatValue skUniform  =
  sendMsg skUniform (mkSelector "floatValue") retCFloat []

-- | @- setFloatValue:@
setFloatValue :: IsSKUniform skUniform => skUniform -> CFloat -> IO ()
setFloatValue skUniform  value =
  sendMsg skUniform (mkSelector "setFloatValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniformWithName:@
uniformWithNameSelector :: Selector
uniformWithNameSelector = mkSelector "uniformWithName:"

-- | @Selector@ for @uniformWithName:texture:@
uniformWithName_textureSelector :: Selector
uniformWithName_textureSelector = mkSelector "uniformWithName:texture:"

-- | @Selector@ for @uniformWithName:float:@
uniformWithName_floatSelector :: Selector
uniformWithName_floatSelector = mkSelector "uniformWithName:float:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:texture:@
initWithName_textureSelector :: Selector
initWithName_textureSelector = mkSelector "initWithName:texture:"

-- | @Selector@ for @initWithName:float:@
initWithName_floatSelector :: Selector
initWithName_floatSelector = mkSelector "initWithName:float:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @uniformType@
uniformTypeSelector :: Selector
uniformTypeSelector = mkSelector "uniformType"

-- | @Selector@ for @textureValue@
textureValueSelector :: Selector
textureValueSelector = mkSelector "textureValue"

-- | @Selector@ for @setTextureValue:@
setTextureValueSelector :: Selector
setTextureValueSelector = mkSelector "setTextureValue:"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector
setFloatValueSelector = mkSelector "setFloatValue:"

