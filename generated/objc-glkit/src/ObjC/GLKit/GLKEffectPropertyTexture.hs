{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKEffectPropertyTexture@.
module ObjC.GLKit.GLKEffectPropertyTexture
  ( GLKEffectPropertyTexture
  , IsGLKEffectPropertyTexture(..)
  , enabled
  , setEnabled
  , name
  , setName
  , target
  , setTarget
  , envMode
  , setEnvMode
  , enabledSelector
  , setEnabledSelector
  , nameSelector
  , setNameSelector
  , targetSelector
  , setTargetSelector
  , envModeSelector
  , setEnvModeSelector

  -- * Enum types
  , GLKTextureEnvMode(GLKTextureEnvMode)
  , pattern GLKTextureEnvModeReplace
  , pattern GLKTextureEnvModeModulate
  , pattern GLKTextureEnvModeDecal
  , GLKTextureTarget(GLKTextureTarget)
  , pattern GLKTextureTarget2D
  , pattern GLKTextureTargetCubeMap
  , pattern GLKTextureTargetCt

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

import ObjC.GLKit.Internal.Classes
import ObjC.GLKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO CUChar
enabled glkEffectPropertyTexture  =
  sendMsg glkEffectPropertyTexture (mkSelector "enabled") retCUChar []

-- | @- setEnabled:@
setEnabled :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> CUChar -> IO ()
setEnabled glkEffectPropertyTexture  value =
  sendMsg glkEffectPropertyTexture (mkSelector "setEnabled:") retVoid [argCUChar (fromIntegral value)]

-- | @- name@
name :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO CUInt
name glkEffectPropertyTexture  =
  sendMsg glkEffectPropertyTexture (mkSelector "name") retCUInt []

-- | @- setName:@
setName :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> CUInt -> IO ()
setName glkEffectPropertyTexture  value =
  sendMsg glkEffectPropertyTexture (mkSelector "setName:") retVoid [argCUInt (fromIntegral value)]

-- | @- target@
target :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO GLKTextureTarget
target glkEffectPropertyTexture  =
  fmap (coerce :: CUInt -> GLKTextureTarget) $ sendMsg glkEffectPropertyTexture (mkSelector "target") retCUInt []

-- | @- setTarget:@
setTarget :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> GLKTextureTarget -> IO ()
setTarget glkEffectPropertyTexture  value =
  sendMsg glkEffectPropertyTexture (mkSelector "setTarget:") retVoid [argCUInt (coerce value)]

-- | @- envMode@
envMode :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO GLKTextureEnvMode
envMode glkEffectPropertyTexture  =
  fmap (coerce :: CInt -> GLKTextureEnvMode) $ sendMsg glkEffectPropertyTexture (mkSelector "envMode") retCInt []

-- | @- setEnvMode:@
setEnvMode :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> GLKTextureEnvMode -> IO ()
setEnvMode glkEffectPropertyTexture  value =
  sendMsg glkEffectPropertyTexture (mkSelector "setEnvMode:") retVoid [argCInt (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @envMode@
envModeSelector :: Selector
envModeSelector = mkSelector "envMode"

-- | @Selector@ for @setEnvMode:@
setEnvModeSelector :: Selector
setEnvModeSelector = mkSelector "setEnvMode:"

