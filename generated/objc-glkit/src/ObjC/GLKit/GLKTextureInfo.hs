{-# LANGUAGE PatternSynonyms #-}
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
  , nameSelector
  , targetSelector
  , widthSelector
  , heightSelector
  , depthSelector
  , alphaStateSelector
  , textureOriginSelector
  , containsMipmapsSelector
  , mimapLevelCountSelector
  , arrayLengthSelector

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

-- | @- name@
name :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
name glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "name") retCUInt []

-- | @- target@
target :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
target glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "target") retCUInt []

-- | @- width@
width :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
width glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "width") retCUInt []

-- | @- height@
height :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
height glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "height") retCUInt []

-- | @- depth@
depth :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
depth glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "depth") retCUInt []

-- | @- alphaState@
alphaState :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO GLKTextureInfoAlphaState
alphaState glkTextureInfo  =
  fmap (coerce :: CInt -> GLKTextureInfoAlphaState) $ sendMsg glkTextureInfo (mkSelector "alphaState") retCInt []

-- | @- textureOrigin@
textureOrigin :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO GLKTextureInfoOrigin
textureOrigin glkTextureInfo  =
  fmap (coerce :: CInt -> GLKTextureInfoOrigin) $ sendMsg glkTextureInfo (mkSelector "textureOrigin") retCInt []

-- | @- containsMipmaps@
containsMipmaps :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO Bool
containsMipmaps glkTextureInfo  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg glkTextureInfo (mkSelector "containsMipmaps") retCULong []

-- | @- mimapLevelCount@
mimapLevelCount :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
mimapLevelCount glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "mimapLevelCount") retCUInt []

-- | @- arrayLength@
arrayLength :: IsGLKTextureInfo glkTextureInfo => glkTextureInfo -> IO CUInt
arrayLength glkTextureInfo  =
  sendMsg glkTextureInfo (mkSelector "arrayLength") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @depth@
depthSelector :: Selector
depthSelector = mkSelector "depth"

-- | @Selector@ for @alphaState@
alphaStateSelector :: Selector
alphaStateSelector = mkSelector "alphaState"

-- | @Selector@ for @textureOrigin@
textureOriginSelector :: Selector
textureOriginSelector = mkSelector "textureOrigin"

-- | @Selector@ for @containsMipmaps@
containsMipmapsSelector :: Selector
containsMipmapsSelector = mkSelector "containsMipmaps"

-- | @Selector@ for @mimapLevelCount@
mimapLevelCountSelector :: Selector
mimapLevelCountSelector = mkSelector "mimapLevelCount"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector
arrayLengthSelector = mkSelector "arrayLength"

