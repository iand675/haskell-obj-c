{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , magFilterSelector
  , minFilterSelector
  , mipFilterSelector
  , rWrapModeSelector
  , sWrapModeSelector
  , setMagFilterSelector
  , setMinFilterSelector
  , setMipFilterSelector
  , setRWrapModeSelector
  , setSWrapModeSelector
  , setTWrapModeSelector
  , tWrapModeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- sWrapMode@
sWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureWrapMode
sWrapMode mdlTextureFilter =
  sendMessage mdlTextureFilter sWrapModeSelector

-- | @- setSWrapMode:@
setSWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureWrapMode -> IO ()
setSWrapMode mdlTextureFilter value =
  sendMessage mdlTextureFilter setSWrapModeSelector value

-- | @- tWrapMode@
tWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureWrapMode
tWrapMode mdlTextureFilter =
  sendMessage mdlTextureFilter tWrapModeSelector

-- | @- setTWrapMode:@
setTWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureWrapMode -> IO ()
setTWrapMode mdlTextureFilter value =
  sendMessage mdlTextureFilter setTWrapModeSelector value

-- | @- rWrapMode@
rWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureWrapMode
rWrapMode mdlTextureFilter =
  sendMessage mdlTextureFilter rWrapModeSelector

-- | @- setRWrapMode:@
setRWrapMode :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureWrapMode -> IO ()
setRWrapMode mdlTextureFilter value =
  sendMessage mdlTextureFilter setRWrapModeSelector value

-- | @- minFilter@
minFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureFilterMode
minFilter mdlTextureFilter =
  sendMessage mdlTextureFilter minFilterSelector

-- | @- setMinFilter:@
setMinFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureFilterMode -> IO ()
setMinFilter mdlTextureFilter value =
  sendMessage mdlTextureFilter setMinFilterSelector value

-- | @- magFilter@
magFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialTextureFilterMode
magFilter mdlTextureFilter =
  sendMessage mdlTextureFilter magFilterSelector

-- | @- setMagFilter:@
setMagFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialTextureFilterMode -> IO ()
setMagFilter mdlTextureFilter value =
  sendMessage mdlTextureFilter setMagFilterSelector value

-- | @- mipFilter@
mipFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> IO MDLMaterialMipMapFilterMode
mipFilter mdlTextureFilter =
  sendMessage mdlTextureFilter mipFilterSelector

-- | @- setMipFilter:@
setMipFilter :: IsMDLTextureFilter mdlTextureFilter => mdlTextureFilter -> MDLMaterialMipMapFilterMode -> IO ()
setMipFilter mdlTextureFilter value =
  sendMessage mdlTextureFilter setMipFilterSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sWrapMode@
sWrapModeSelector :: Selector '[] MDLMaterialTextureWrapMode
sWrapModeSelector = mkSelector "sWrapMode"

-- | @Selector@ for @setSWrapMode:@
setSWrapModeSelector :: Selector '[MDLMaterialTextureWrapMode] ()
setSWrapModeSelector = mkSelector "setSWrapMode:"

-- | @Selector@ for @tWrapMode@
tWrapModeSelector :: Selector '[] MDLMaterialTextureWrapMode
tWrapModeSelector = mkSelector "tWrapMode"

-- | @Selector@ for @setTWrapMode:@
setTWrapModeSelector :: Selector '[MDLMaterialTextureWrapMode] ()
setTWrapModeSelector = mkSelector "setTWrapMode:"

-- | @Selector@ for @rWrapMode@
rWrapModeSelector :: Selector '[] MDLMaterialTextureWrapMode
rWrapModeSelector = mkSelector "rWrapMode"

-- | @Selector@ for @setRWrapMode:@
setRWrapModeSelector :: Selector '[MDLMaterialTextureWrapMode] ()
setRWrapModeSelector = mkSelector "setRWrapMode:"

-- | @Selector@ for @minFilter@
minFilterSelector :: Selector '[] MDLMaterialTextureFilterMode
minFilterSelector = mkSelector "minFilter"

-- | @Selector@ for @setMinFilter:@
setMinFilterSelector :: Selector '[MDLMaterialTextureFilterMode] ()
setMinFilterSelector = mkSelector "setMinFilter:"

-- | @Selector@ for @magFilter@
magFilterSelector :: Selector '[] MDLMaterialTextureFilterMode
magFilterSelector = mkSelector "magFilter"

-- | @Selector@ for @setMagFilter:@
setMagFilterSelector :: Selector '[MDLMaterialTextureFilterMode] ()
setMagFilterSelector = mkSelector "setMagFilter:"

-- | @Selector@ for @mipFilter@
mipFilterSelector :: Selector '[] MDLMaterialMipMapFilterMode
mipFilterSelector = mkSelector "mipFilter"

-- | @Selector@ for @setMipFilter:@
setMipFilterSelector :: Selector '[MDLMaterialMipMapFilterMode] ()
setMipFilterSelector = mkSelector "setMipFilter:"

