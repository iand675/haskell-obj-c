{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPassAttachmentDescriptor
  ( MTLRenderPassAttachmentDescriptor
  , IsMTLRenderPassAttachmentDescriptor(..)
  , texture
  , setTexture
  , level
  , setLevel
  , slice
  , setSlice
  , depthPlane
  , setDepthPlane
  , resolveTexture
  , setResolveTexture
  , resolveLevel
  , setResolveLevel
  , resolveSlice
  , setResolveSlice
  , resolveDepthPlane
  , setResolveDepthPlane
  , loadAction
  , setLoadAction
  , storeAction
  , setStoreAction
  , storeActionOptions
  , setStoreActionOptions
  , depthPlaneSelector
  , levelSelector
  , loadActionSelector
  , resolveDepthPlaneSelector
  , resolveLevelSelector
  , resolveSliceSelector
  , resolveTextureSelector
  , setDepthPlaneSelector
  , setLevelSelector
  , setLoadActionSelector
  , setResolveDepthPlaneSelector
  , setResolveLevelSelector
  , setResolveSliceSelector
  , setResolveTextureSelector
  , setSliceSelector
  , setStoreActionOptionsSelector
  , setStoreActionSelector
  , setTextureSelector
  , sliceSelector
  , storeActionOptionsSelector
  , storeActionSelector
  , textureSelector

  -- * Enum types
  , MTLLoadAction(MTLLoadAction)
  , pattern MTLLoadActionDontCare
  , pattern MTLLoadActionLoad
  , pattern MTLLoadActionClear
  , MTLStoreAction(MTLStoreAction)
  , pattern MTLStoreActionDontCare
  , pattern MTLStoreActionStore
  , pattern MTLStoreActionMultisampleResolve
  , pattern MTLStoreActionStoreAndMultisampleResolve
  , pattern MTLStoreActionUnknown
  , pattern MTLStoreActionCustomSampleDepthStore
  , MTLStoreActionOptions(MTLStoreActionOptions)
  , pattern MTLStoreActionOptionNone
  , pattern MTLStoreActionOptionCustomSamplePositions

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | texture
--
-- The MTLTexture object for this attachment.
--
-- ObjC selector: @- texture@
texture :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO RawId
texture mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor textureSelector

-- | texture
--
-- The MTLTexture object for this attachment.
--
-- ObjC selector: @- setTexture:@
setTexture :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> RawId -> IO ()
setTexture mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setTextureSelector value

-- | level
--
-- The mipmap level of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- level@
level :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
level mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor levelSelector

-- | level
--
-- The mipmap level of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- setLevel:@
setLevel :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setLevel mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setLevelSelector value

-- | slice
--
-- The slice of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- slice@
slice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
slice mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor sliceSelector

-- | slice
--
-- The slice of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- setSlice:@
setSlice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setSlice mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setSliceSelector value

-- | depthPlane
--
-- The depth plane of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- depthPlane@
depthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
depthPlane mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor depthPlaneSelector

-- | depthPlane
--
-- The depth plane of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- setDepthPlane:@
setDepthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setDepthPlane mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setDepthPlaneSelector value

-- | resolveTexture
--
-- The texture used for multisample resolve operations.  Only used (and required) if the store action is set to MTLStoreActionMultisampleResolve.
--
-- ObjC selector: @- resolveTexture@
resolveTexture :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO RawId
resolveTexture mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor resolveTextureSelector

-- | resolveTexture
--
-- The texture used for multisample resolve operations.  Only used (and required) if the store action is set to MTLStoreActionMultisampleResolve.
--
-- ObjC selector: @- setResolveTexture:@
setResolveTexture :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> RawId -> IO ()
setResolveTexture mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setResolveTextureSelector value

-- | resolveLevel
--
-- The mipmap level of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- resolveLevel@
resolveLevel :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
resolveLevel mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor resolveLevelSelector

-- | resolveLevel
--
-- The mipmap level of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- setResolveLevel:@
setResolveLevel :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setResolveLevel mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setResolveLevelSelector value

-- | resolveLevel
--
-- The texture slice of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- resolveSlice@
resolveSlice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
resolveSlice mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor resolveSliceSelector

-- | resolveLevel
--
-- The texture slice of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- setResolveSlice:@
setResolveSlice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setResolveSlice mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setResolveSliceSelector value

-- | resolveDepthPlane
--
-- The texture depth plane of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- resolveDepthPlane@
resolveDepthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
resolveDepthPlane mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor resolveDepthPlaneSelector

-- | resolveDepthPlane
--
-- The texture depth plane of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- setResolveDepthPlane:@
setResolveDepthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setResolveDepthPlane mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setResolveDepthPlaneSelector value

-- | loadAction
--
-- The action to be performed with this attachment at the beginning of a render pass.  Default is MTLLoadActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- loadAction@
loadAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO MTLLoadAction
loadAction mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor loadActionSelector

-- | loadAction
--
-- The action to be performed with this attachment at the beginning of a render pass.  Default is MTLLoadActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- setLoadAction:@
setLoadAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> MTLLoadAction -> IO ()
setLoadAction mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setLoadActionSelector value

-- | storeAction
--
-- The action to be performed with this attachment at the end of a render pass.  Default is MTLStoreActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- storeAction@
storeAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO MTLStoreAction
storeAction mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor storeActionSelector

-- | storeAction
--
-- The action to be performed with this attachment at the end of a render pass.  Default is MTLStoreActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- setStoreAction:@
setStoreAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> MTLStoreAction -> IO ()
setStoreAction mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setStoreActionSelector value

-- | storeActionOptions
--
-- Optional configuration for the store action performed with this attachment at the end of a render pass.  Default is MTLStoreActionOptionNone.
--
-- ObjC selector: @- storeActionOptions@
storeActionOptions :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO MTLStoreActionOptions
storeActionOptions mtlRenderPassAttachmentDescriptor =
  sendMessage mtlRenderPassAttachmentDescriptor storeActionOptionsSelector

-- | storeActionOptions
--
-- Optional configuration for the store action performed with this attachment at the end of a render pass.  Default is MTLStoreActionOptionNone.
--
-- ObjC selector: @- setStoreActionOptions:@
setStoreActionOptions :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> MTLStoreActionOptions -> IO ()
setStoreActionOptions mtlRenderPassAttachmentDescriptor value =
  sendMessage mtlRenderPassAttachmentDescriptor setStoreActionOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @texture@
textureSelector :: Selector '[] RawId
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector '[RawId] ()
setTextureSelector = mkSelector "setTexture:"

-- | @Selector@ for @level@
levelSelector :: Selector '[] CULong
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector '[CULong] ()
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @slice@
sliceSelector :: Selector '[] CULong
sliceSelector = mkSelector "slice"

-- | @Selector@ for @setSlice:@
setSliceSelector :: Selector '[CULong] ()
setSliceSelector = mkSelector "setSlice:"

-- | @Selector@ for @depthPlane@
depthPlaneSelector :: Selector '[] CULong
depthPlaneSelector = mkSelector "depthPlane"

-- | @Selector@ for @setDepthPlane:@
setDepthPlaneSelector :: Selector '[CULong] ()
setDepthPlaneSelector = mkSelector "setDepthPlane:"

-- | @Selector@ for @resolveTexture@
resolveTextureSelector :: Selector '[] RawId
resolveTextureSelector = mkSelector "resolveTexture"

-- | @Selector@ for @setResolveTexture:@
setResolveTextureSelector :: Selector '[RawId] ()
setResolveTextureSelector = mkSelector "setResolveTexture:"

-- | @Selector@ for @resolveLevel@
resolveLevelSelector :: Selector '[] CULong
resolveLevelSelector = mkSelector "resolveLevel"

-- | @Selector@ for @setResolveLevel:@
setResolveLevelSelector :: Selector '[CULong] ()
setResolveLevelSelector = mkSelector "setResolveLevel:"

-- | @Selector@ for @resolveSlice@
resolveSliceSelector :: Selector '[] CULong
resolveSliceSelector = mkSelector "resolveSlice"

-- | @Selector@ for @setResolveSlice:@
setResolveSliceSelector :: Selector '[CULong] ()
setResolveSliceSelector = mkSelector "setResolveSlice:"

-- | @Selector@ for @resolveDepthPlane@
resolveDepthPlaneSelector :: Selector '[] CULong
resolveDepthPlaneSelector = mkSelector "resolveDepthPlane"

-- | @Selector@ for @setResolveDepthPlane:@
setResolveDepthPlaneSelector :: Selector '[CULong] ()
setResolveDepthPlaneSelector = mkSelector "setResolveDepthPlane:"

-- | @Selector@ for @loadAction@
loadActionSelector :: Selector '[] MTLLoadAction
loadActionSelector = mkSelector "loadAction"

-- | @Selector@ for @setLoadAction:@
setLoadActionSelector :: Selector '[MTLLoadAction] ()
setLoadActionSelector = mkSelector "setLoadAction:"

-- | @Selector@ for @storeAction@
storeActionSelector :: Selector '[] MTLStoreAction
storeActionSelector = mkSelector "storeAction"

-- | @Selector@ for @setStoreAction:@
setStoreActionSelector :: Selector '[MTLStoreAction] ()
setStoreActionSelector = mkSelector "setStoreAction:"

-- | @Selector@ for @storeActionOptions@
storeActionOptionsSelector :: Selector '[] MTLStoreActionOptions
storeActionOptionsSelector = mkSelector "storeActionOptions"

-- | @Selector@ for @setStoreActionOptions:@
setStoreActionOptionsSelector :: Selector '[MTLStoreActionOptions] ()
setStoreActionOptionsSelector = mkSelector "setStoreActionOptions:"

