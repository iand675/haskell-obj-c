{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKTextureInfo@.
module ObjC.GLKit.GLKTextureInfo
  ( GLKTextureInfo
  , IsGLKTextureInfo(..)
  , name
  , target
  , width
  , height
  , depth
  , alphaState
  , textureOrigin
  , containsMipmaps
  , mimapLevelCount
  , arrayLength
  , alphaStateSelector
  , arrayLengthSelector
  , containsMipmapsSelector
  , depthSelector
  , heightSelector
  , mimapLevelCountSelector
  , nameSelector
  , targetSelector
  , textureOriginSelector
  , widthSelector

  -- * Enum types
  , GLKTextureInfoAlphaState(GLKTextureInfoAlphaState)
  , pattern GLKTextureInfoAlphaStateNone
  , pattern GLKTextureInfoAlphaStateNonPremultiplied
  , pattern GLKTextureInfoAlphaStatePremultiplied
  , GLKTextureInfoOrigin(GLKTextureInfoOrigin)
  , pattern GLKTextureInfoOriginUnknown
  , pattern GLKTextureInfoOriginTopLeft
  , pattern GLKTextureInfoOriginBottomLeft

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

-- | @- name@
name :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
name glkTextureInfo =
  sendMessage glkTextureInfo nameSelector

-- | @- target@
target :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
target glkTextureInfo =
  sendMessage glkTextureInfo targetSelector

-- | @- width@
width :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
width glkTextureInfo =
  sendMessage glkTextureInfo widthSelector

-- | @- height@
height :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
height glkTextureInfo =
  sendMessage glkTextureInfo heightSelector

-- | @- depth@
depth :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
depth glkTextureInfo =
  sendMessage glkTextureInfo depthSelector

-- | @- alphaState@
alphaState :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO GLKTextureInfoAlphaState
alphaState glkTextureInfo =
  sendMessage glkTextureInfo alphaStateSelector

-- | @- textureOrigin@
textureOrigin :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO GLKTextureInfoOrigin
textureOrigin glkTextureInfo =
  sendMessage glkTextureInfo textureOriginSelector

-- | @- containsMipmaps@
containsMipmaps :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO Bool
containsMipmaps glkTextureInfo =
  sendMessage glkTextureInfo containsMipmapsSelector

-- | @- mimapLevelCount@
mimapLevelCount :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
mimapLevelCount glkTextureInfo =
  sendMessage glkTextureInfo mimapLevelCountSelector

-- | @- arrayLength@
arrayLength :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
arrayLength glkTextureInfo =
  sendMessage glkTextureInfo arrayLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] CUInt
nameSelector = mkSelector "name"

-- | @Selector@ for @target@
targetSelector :: Selector '[] CUInt
targetSelector = mkSelector "target"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CUInt
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CUInt
heightSelector = mkSelector "height"

-- | @Selector@ for @depth@
depthSelector :: Selector '[] CUInt
depthSelector = mkSelector "depth"

-- | @Selector@ for @alphaState@
alphaStateSelector :: Selector '[] GLKTextureInfoAlphaState
alphaStateSelector = mkSelector "alphaState"

-- | @Selector@ for @textureOrigin@
textureOriginSelector :: Selector '[] GLKTextureInfoOrigin
textureOriginSelector = mkSelector "textureOrigin"

-- | @Selector@ for @containsMipmaps@
containsMipmapsSelector :: Selector '[] Bool
containsMipmapsSelector = mkSelector "containsMipmaps"

-- | @Selector@ for @mimapLevelCount@
mimapLevelCountSelector :: Selector '[] CUInt
mimapLevelCountSelector = mkSelector "mimapLevelCount"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector '[] CUInt
arrayLengthSelector = mkSelector "arrayLength"

