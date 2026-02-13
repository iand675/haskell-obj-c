{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , envModeSelector
  , nameSelector
  , setEnabledSelector
  , setEnvModeSelector
  , setNameSelector
  , setTargetSelector
  , targetSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.GLKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO CUChar
enabled glkEffectPropertyTexture =
  sendMessage glkEffectPropertyTexture enabledSelector

-- | @- setEnabled:@
setEnabled :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> CUChar -> IO ()
setEnabled glkEffectPropertyTexture value =
  sendMessage glkEffectPropertyTexture setEnabledSelector value

-- | @- name@
name :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO CUInt
name glkEffectPropertyTexture =
  sendMessage glkEffectPropertyTexture nameSelector

-- | @- setName:@
setName :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> CUInt -> IO ()
setName glkEffectPropertyTexture value =
  sendMessage glkEffectPropertyTexture setNameSelector value

-- | @- target@
target :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO GLKTextureTarget
target glkEffectPropertyTexture =
  sendMessage glkEffectPropertyTexture targetSelector

-- | @- setTarget:@
setTarget :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> GLKTextureTarget -> IO ()
setTarget glkEffectPropertyTexture value =
  sendMessage glkEffectPropertyTexture setTargetSelector value

-- | @- envMode@
envMode :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> IO GLKTextureEnvMode
envMode glkEffectPropertyTexture =
  sendMessage glkEffectPropertyTexture envModeSelector

-- | @- setEnvMode:@
setEnvMode :: IsGLKEffectPropertyTexture glkEffectPropertyTexture => glkEffectPropertyTexture -> GLKTextureEnvMode -> IO ()
setEnvMode glkEffectPropertyTexture value =
  sendMessage glkEffectPropertyTexture setEnvModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] CUChar
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[CUChar] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] CUInt
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[CUInt] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] GLKTextureTarget
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[GLKTextureTarget] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @envMode@
envModeSelector :: Selector '[] GLKTextureEnvMode
envModeSelector = mkSelector "envMode"

-- | @Selector@ for @setEnvMode:@
setEnvModeSelector :: Selector '[GLKTextureEnvMode] ()
setEnvModeSelector = mkSelector "setEnvMode:"

