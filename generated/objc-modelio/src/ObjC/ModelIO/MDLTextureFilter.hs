{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTextureFilter@.
module ObjC.ModelIO.MDLTextureFilter
  ( MDLTextureFilter
  , IsMDLTextureFilter(..)
  , sWrapMode
  , setSWrapMode
  , tWrapMode
  , setTWrapMode
  , rWrapMode
  , setRWrapMode
  , minFilter
  , setMinFilter
  , magFilter
  , setMagFilter
  , mipFilter
  , setMipFilter
  , sWrapModeSelector
  , setSWrapModeSelector
  , tWrapModeSelector
  , setTWrapModeSelector
  , rWrapModeSelector
  , setRWrapModeSelector
  , minFilterSelector
  , setMinFilterSelector
  , magFilterSelector
  , setMagFilterSelector
  , mipFilterSelector
  , setMipFilterSelector

  -- * Enum types
  , MDLMaterialMipMapFilterMode(MDLMaterialMipMapFilterMode)
  , pattern MDLMaterialMipMapFilterModeNearest
  , pattern MDLMaterialMipMapFilterModeLinear
  , MDLMaterialTextureFilterMode(MDLMaterialTextureFilterMode)
  , pattern MDLMaterialTextureFilterModeNearest
  , pattern MDLMaterialTextureFilterModeLinear
  , MDLMaterialTextureWrapMode(MDLMaterialTextureWrapMode)
  , pattern MDLMaterialTextureWrapModeClamp
  , pattern MDLMaterialTextureWrapModeRepeat
  , pattern MDLMaterialTextureWrapModeMirror

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- sWrapMode@
sWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureWrapMode
sWrapMode mdlTextureFilter  =
  fmap (coerce :: CULong -> MDLMaterialTextureWrapMode) $ sendMsg mdlTextureFilter (mkSelector "sWrapMode") retCULong []

-- | @- setSWrapMode:@
setSWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureWrapMode -> IO ()
setSWrapMode mdlTextureFilter  value =
  sendMsg mdlTextureFilter (mkSelector "setSWrapMode:") retVoid [argCULong (coerce value)]

-- | @- tWrapMode@
tWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureWrapMode
tWrapMode mdlTextureFilter  =
  fmap (coerce :: CULong -> MDLMaterialTextureWrapMode) $ sendMsg mdlTextureFilter (mkSelector "tWrapMode") retCULong []

-- | @- setTWrapMode:@
setTWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureWrapMode -> IO ()
setTWrapMode mdlTextureFilter  value =
  sendMsg mdlTextureFilter (mkSelector "setTWrapMode:") retVoid [argCULong (coerce value)]

-- | @- rWrapMode@
rWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureWrapMode
rWrapMode mdlTextureFilter  =
  fmap (coerce :: CULong -> MDLMaterialTextureWrapMode) $ sendMsg mdlTextureFilter (mkSelector "rWrapMode") retCULong []

-- | @- setRWrapMode:@
setRWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureWrapMode -> IO ()
setRWrapMode mdlTextureFilter  value =
  sendMsg mdlTextureFilter (mkSelector "setRWrapMode:") retVoid [argCULong (coerce value)]

-- | @- minFilter@
minFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureFilterMode
minFilter mdlTextureFilter  =
  fmap (coerce :: CULong -> MDLMaterialTextureFilterMode) $ sendMsg mdlTextureFilter (mkSelector "minFilter") retCULong []

-- | @- setMinFilter:@
setMinFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureFilterMode -> IO ()
setMinFilter mdlTextureFilter  value =
  sendMsg mdlTextureFilter (mkSelector "setMinFilter:") retVoid [argCULong (coerce value)]

-- | @- magFilter@
magFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureFilterMode
magFilter mdlTextureFilter  =
  fmap (coerce :: CULong -> MDLMaterialTextureFilterMode) $ sendMsg mdlTextureFilter (mkSelector "magFilter") retCULong []

-- | @- setMagFilter:@
setMagFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureFilterMode -> IO ()
setMagFilter mdlTextureFilter  value =
  sendMsg mdlTextureFilter (mkSelector "setMagFilter:") retVoid [argCULong (coerce value)]

-- | @- mipFilter@
mipFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialMipMapFilterMode
mipFilter mdlTextureFilter  =
  fmap (coerce :: CULong -> MDLMaterialMipMapFilterMode) $ sendMsg mdlTextureFilter (mkSelector "mipFilter") retCULong []

-- | @- setMipFilter:@
setMipFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialMipMapFilterMode -> IO ()
setMipFilter mdlTextureFilter  value =
  sendMsg mdlTextureFilter (mkSelector "setMipFilter:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sWrapMode@
sWrapModeSelector :: Selector
sWrapModeSelector = mkSelector "sWrapMode"

-- | @Selector@ for @setSWrapMode:@
setSWrapModeSelector :: Selector
setSWrapModeSelector = mkSelector "setSWrapMode:"

-- | @Selector@ for @tWrapMode@
tWrapModeSelector :: Selector
tWrapModeSelector = mkSelector "tWrapMode"

-- | @Selector@ for @setTWrapMode:@
setTWrapModeSelector :: Selector
setTWrapModeSelector = mkSelector "setTWrapMode:"

-- | @Selector@ for @rWrapMode@
rWrapModeSelector :: Selector
rWrapModeSelector = mkSelector "rWrapMode"

-- | @Selector@ for @setRWrapMode:@
setRWrapModeSelector :: Selector
setRWrapModeSelector = mkSelector "setRWrapMode:"

-- | @Selector@ for @minFilter@
minFilterSelector :: Selector
minFilterSelector = mkSelector "minFilter"

-- | @Selector@ for @setMinFilter:@
setMinFilterSelector :: Selector
setMinFilterSelector = mkSelector "setMinFilter:"

-- | @Selector@ for @magFilter@
magFilterSelector :: Selector
magFilterSelector = mkSelector "magFilter"

-- | @Selector@ for @setMagFilter:@
setMagFilterSelector :: Selector
setMagFilterSelector = mkSelector "setMagFilter:"

-- | @Selector@ for @mipFilter@
mipFilterSelector :: Selector
mipFilterSelector = mkSelector "mipFilter"

-- | @Selector@ for @setMipFilter:@
setMipFilterSelector :: Selector
setMipFilterSelector = mkSelector "setMipFilter:"

