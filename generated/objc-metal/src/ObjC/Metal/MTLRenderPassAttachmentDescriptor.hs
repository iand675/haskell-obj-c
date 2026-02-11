{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPassAttachmentDescriptor
  ( MTLRenderPassAttachmentDescriptor
  , IsMTLRenderPassAttachmentDescriptor(..)
  , level
  , setLevel
  , slice
  , setSlice
  , depthPlane
  , setDepthPlane
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
  , levelSelector
  , setLevelSelector
  , sliceSelector
  , setSliceSelector
  , depthPlaneSelector
  , setDepthPlaneSelector
  , resolveLevelSelector
  , setResolveLevelSelector
  , resolveSliceSelector
  , setResolveSliceSelector
  , resolveDepthPlaneSelector
  , setResolveDepthPlaneSelector
  , loadActionSelector
  , setLoadActionSelector
  , storeActionSelector
  , setStoreActionSelector
  , storeActionOptionsSelector
  , setStoreActionOptionsSelector

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | level
--
-- The mipmap level of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- level@
level :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
level mtlRenderPassAttachmentDescriptor  =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "level") retCULong []

-- | level
--
-- The mipmap level of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- setLevel:@
setLevel :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setLevel mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setLevel:") retVoid [argCULong (fromIntegral value)]

-- | slice
--
-- The slice of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- slice@
slice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
slice mtlRenderPassAttachmentDescriptor  =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "slice") retCULong []

-- | slice
--
-- The slice of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- setSlice:@
setSlice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setSlice mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setSlice:") retVoid [argCULong (fromIntegral value)]

-- | depthPlane
--
-- The depth plane of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- depthPlane@
depthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
depthPlane mtlRenderPassAttachmentDescriptor  =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "depthPlane") retCULong []

-- | depthPlane
--
-- The depth plane of the texture to be used for rendering.  Default is zero.
--
-- ObjC selector: @- setDepthPlane:@
setDepthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setDepthPlane mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setDepthPlane:") retVoid [argCULong (fromIntegral value)]

-- | resolveLevel
--
-- The mipmap level of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- resolveLevel@
resolveLevel :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
resolveLevel mtlRenderPassAttachmentDescriptor  =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "resolveLevel") retCULong []

-- | resolveLevel
--
-- The mipmap level of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- setResolveLevel:@
setResolveLevel :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setResolveLevel mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setResolveLevel:") retVoid [argCULong (fromIntegral value)]

-- | resolveLevel
--
-- The texture slice of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- resolveSlice@
resolveSlice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
resolveSlice mtlRenderPassAttachmentDescriptor  =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "resolveSlice") retCULong []

-- | resolveLevel
--
-- The texture slice of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- setResolveSlice:@
setResolveSlice :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setResolveSlice mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setResolveSlice:") retVoid [argCULong (fromIntegral value)]

-- | resolveDepthPlane
--
-- The texture depth plane of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- resolveDepthPlane@
resolveDepthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO CULong
resolveDepthPlane mtlRenderPassAttachmentDescriptor  =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "resolveDepthPlane") retCULong []

-- | resolveDepthPlane
--
-- The texture depth plane of the resolve texture to be used for multisample resolve.  Defaults to zero.
--
-- ObjC selector: @- setResolveDepthPlane:@
setResolveDepthPlane :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> CULong -> IO ()
setResolveDepthPlane mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setResolveDepthPlane:") retVoid [argCULong (fromIntegral value)]

-- | loadAction
--
-- The action to be performed with this attachment at the beginning of a render pass.  Default is MTLLoadActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- loadAction@
loadAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO MTLLoadAction
loadAction mtlRenderPassAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLLoadAction) $ sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "loadAction") retCULong []

-- | loadAction
--
-- The action to be performed with this attachment at the beginning of a render pass.  Default is MTLLoadActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- setLoadAction:@
setLoadAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> MTLLoadAction -> IO ()
setLoadAction mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setLoadAction:") retVoid [argCULong (coerce value)]

-- | storeAction
--
-- The action to be performed with this attachment at the end of a render pass.  Default is MTLStoreActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- storeAction@
storeAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO MTLStoreAction
storeAction mtlRenderPassAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLStoreAction) $ sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "storeAction") retCULong []

-- | storeAction
--
-- The action to be performed with this attachment at the end of a render pass.  Default is MTLStoreActionDontCare unless specified by a creation or init method.
--
-- ObjC selector: @- setStoreAction:@
setStoreAction :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> MTLStoreAction -> IO ()
setStoreAction mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setStoreAction:") retVoid [argCULong (coerce value)]

-- | storeActionOptions
--
-- Optional configuration for the store action performed with this attachment at the end of a render pass.  Default is MTLStoreActionOptionNone.
--
-- ObjC selector: @- storeActionOptions@
storeActionOptions :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> IO MTLStoreActionOptions
storeActionOptions mtlRenderPassAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLStoreActionOptions) $ sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "storeActionOptions") retCULong []

-- | storeActionOptions
--
-- Optional configuration for the store action performed with this attachment at the end of a render pass.  Default is MTLStoreActionOptionNone.
--
-- ObjC selector: @- setStoreActionOptions:@
setStoreActionOptions :: IsMTLRenderPassAttachmentDescriptor mtlRenderPassAttachmentDescriptor => mtlRenderPassAttachmentDescriptor -> MTLStoreActionOptions -> IO ()
setStoreActionOptions mtlRenderPassAttachmentDescriptor  value =
  sendMsg mtlRenderPassAttachmentDescriptor (mkSelector "setStoreActionOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @slice@
sliceSelector :: Selector
sliceSelector = mkSelector "slice"

-- | @Selector@ for @setSlice:@
setSliceSelector :: Selector
setSliceSelector = mkSelector "setSlice:"

-- | @Selector@ for @depthPlane@
depthPlaneSelector :: Selector
depthPlaneSelector = mkSelector "depthPlane"

-- | @Selector@ for @setDepthPlane:@
setDepthPlaneSelector :: Selector
setDepthPlaneSelector = mkSelector "setDepthPlane:"

-- | @Selector@ for @resolveLevel@
resolveLevelSelector :: Selector
resolveLevelSelector = mkSelector "resolveLevel"

-- | @Selector@ for @setResolveLevel:@
setResolveLevelSelector :: Selector
setResolveLevelSelector = mkSelector "setResolveLevel:"

-- | @Selector@ for @resolveSlice@
resolveSliceSelector :: Selector
resolveSliceSelector = mkSelector "resolveSlice"

-- | @Selector@ for @setResolveSlice:@
setResolveSliceSelector :: Selector
setResolveSliceSelector = mkSelector "setResolveSlice:"

-- | @Selector@ for @resolveDepthPlane@
resolveDepthPlaneSelector :: Selector
resolveDepthPlaneSelector = mkSelector "resolveDepthPlane"

-- | @Selector@ for @setResolveDepthPlane:@
setResolveDepthPlaneSelector :: Selector
setResolveDepthPlaneSelector = mkSelector "setResolveDepthPlane:"

-- | @Selector@ for @loadAction@
loadActionSelector :: Selector
loadActionSelector = mkSelector "loadAction"

-- | @Selector@ for @setLoadAction:@
setLoadActionSelector :: Selector
setLoadActionSelector = mkSelector "setLoadAction:"

-- | @Selector@ for @storeAction@
storeActionSelector :: Selector
storeActionSelector = mkSelector "storeAction"

-- | @Selector@ for @setStoreAction:@
setStoreActionSelector :: Selector
setStoreActionSelector = mkSelector "setStoreAction:"

-- | @Selector@ for @storeActionOptions@
storeActionOptionsSelector :: Selector
storeActionOptionsSelector = mkSelector "storeActionOptions"

-- | @Selector@ for @setStoreActionOptions:@
setStoreActionOptionsSelector :: Selector
setStoreActionOptionsSelector = mkSelector "setStoreActionOptions:"

