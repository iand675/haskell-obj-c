{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassDepthAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPassDepthAttachmentDescriptor
  ( MTLRenderPassDepthAttachmentDescriptor
  , IsMTLRenderPassDepthAttachmentDescriptor(..)
  , clearDepth
  , setClearDepth
  , depthResolveFilter
  , setDepthResolveFilter
  , clearDepthSelector
  , depthResolveFilterSelector
  , setClearDepthSelector
  , setDepthResolveFilterSelector

  -- * Enum types
  , MTLMultisampleDepthResolveFilter(MTLMultisampleDepthResolveFilter)
  , pattern MTLMultisampleDepthResolveFilterSample0
  , pattern MTLMultisampleDepthResolveFilterMin
  , pattern MTLMultisampleDepthResolveFilterMax

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

-- | clearDepth
--
-- The clear depth value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- clearDepth@
clearDepth :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> IO CDouble
clearDepth mtlRenderPassDepthAttachmentDescriptor =
  sendMessage mtlRenderPassDepthAttachmentDescriptor clearDepthSelector

-- | clearDepth
--
-- The clear depth value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- setClearDepth:@
setClearDepth :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> CDouble -> IO ()
setClearDepth mtlRenderPassDepthAttachmentDescriptor value =
  sendMessage mtlRenderPassDepthAttachmentDescriptor setClearDepthSelector value

-- | resolveFilter
--
-- The filter to be used for depth multisample resolve.  Defaults to MTLMultisampleDepthResolveFilterSample0.
--
-- ObjC selector: @- depthResolveFilter@
depthResolveFilter :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> IO MTLMultisampleDepthResolveFilter
depthResolveFilter mtlRenderPassDepthAttachmentDescriptor =
  sendMessage mtlRenderPassDepthAttachmentDescriptor depthResolveFilterSelector

-- | resolveFilter
--
-- The filter to be used for depth multisample resolve.  Defaults to MTLMultisampleDepthResolveFilterSample0.
--
-- ObjC selector: @- setDepthResolveFilter:@
setDepthResolveFilter :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> MTLMultisampleDepthResolveFilter -> IO ()
setDepthResolveFilter mtlRenderPassDepthAttachmentDescriptor value =
  sendMessage mtlRenderPassDepthAttachmentDescriptor setDepthResolveFilterSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clearDepth@
clearDepthSelector :: Selector '[] CDouble
clearDepthSelector = mkSelector "clearDepth"

-- | @Selector@ for @setClearDepth:@
setClearDepthSelector :: Selector '[CDouble] ()
setClearDepthSelector = mkSelector "setClearDepth:"

-- | @Selector@ for @depthResolveFilter@
depthResolveFilterSelector :: Selector '[] MTLMultisampleDepthResolveFilter
depthResolveFilterSelector = mkSelector "depthResolveFilter"

-- | @Selector@ for @setDepthResolveFilter:@
setDepthResolveFilterSelector :: Selector '[MTLMultisampleDepthResolveFilter] ()
setDepthResolveFilterSelector = mkSelector "setDepthResolveFilter:"

