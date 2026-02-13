{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPassStencilAttachmentDescriptor@.
module ObjC.Metal.MTLRenderPassStencilAttachmentDescriptor
  ( MTLRenderPassStencilAttachmentDescriptor
  , IsMTLRenderPassStencilAttachmentDescriptor(..)
  , clearStencil
  , setClearStencil
  , stencilResolveFilter
  , setStencilResolveFilter
  , clearStencilSelector
  , setClearStencilSelector
  , setStencilResolveFilterSelector
  , stencilResolveFilterSelector

  -- * Enum types
  , MTLMultisampleStencilResolveFilter(MTLMultisampleStencilResolveFilter)
  , pattern MTLMultisampleStencilResolveFilterSample0
  , pattern MTLMultisampleStencilResolveFilterDepthResolvedSample

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

-- | clearStencil
--
-- The clear stencil value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- clearStencil@
clearStencil :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> IO CUInt
clearStencil mtlRenderPassStencilAttachmentDescriptor =
  sendMessage mtlRenderPassStencilAttachmentDescriptor clearStencilSelector

-- | clearStencil
--
-- The clear stencil value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- setClearStencil:@
setClearStencil :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> CUInt -> IO ()
setClearStencil mtlRenderPassStencilAttachmentDescriptor value =
  sendMessage mtlRenderPassStencilAttachmentDescriptor setClearStencilSelector value

-- | stencilResolveFilter
--
-- The filter to be used for stencil multisample resolve. Defaults to MTLMultisampleStencilResolveFilterSample0.
--
-- ObjC selector: @- stencilResolveFilter@
stencilResolveFilter :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> IO MTLMultisampleStencilResolveFilter
stencilResolveFilter mtlRenderPassStencilAttachmentDescriptor =
  sendMessage mtlRenderPassStencilAttachmentDescriptor stencilResolveFilterSelector

-- | stencilResolveFilter
--
-- The filter to be used for stencil multisample resolve. Defaults to MTLMultisampleStencilResolveFilterSample0.
--
-- ObjC selector: @- setStencilResolveFilter:@
setStencilResolveFilter :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> MTLMultisampleStencilResolveFilter -> IO ()
setStencilResolveFilter mtlRenderPassStencilAttachmentDescriptor value =
  sendMessage mtlRenderPassStencilAttachmentDescriptor setStencilResolveFilterSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clearStencil@
clearStencilSelector :: Selector '[] CUInt
clearStencilSelector = mkSelector "clearStencil"

-- | @Selector@ for @setClearStencil:@
setClearStencilSelector :: Selector '[CUInt] ()
setClearStencilSelector = mkSelector "setClearStencil:"

-- | @Selector@ for @stencilResolveFilter@
stencilResolveFilterSelector :: Selector '[] MTLMultisampleStencilResolveFilter
stencilResolveFilterSelector = mkSelector "stencilResolveFilter"

-- | @Selector@ for @setStencilResolveFilter:@
setStencilResolveFilterSelector :: Selector '[MTLMultisampleStencilResolveFilter] ()
setStencilResolveFilterSelector = mkSelector "setStencilResolveFilter:"

