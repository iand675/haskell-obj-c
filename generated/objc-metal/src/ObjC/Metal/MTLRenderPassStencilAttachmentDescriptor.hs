{-# LANGUAGE PatternSynonyms #-}
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
  , stencilResolveFilterSelector
  , setStencilResolveFilterSelector

  -- * Enum types
  , MTLMultisampleStencilResolveFilter(MTLMultisampleStencilResolveFilter)
  , pattern MTLMultisampleStencilResolveFilterSample0
  , pattern MTLMultisampleStencilResolveFilterDepthResolvedSample

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

-- | clearStencil
--
-- The clear stencil value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- clearStencil@
clearStencil :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> IO CUInt
clearStencil mtlRenderPassStencilAttachmentDescriptor  =
  sendMsg mtlRenderPassStencilAttachmentDescriptor (mkSelector "clearStencil") retCUInt []

-- | clearStencil
--
-- The clear stencil value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- setClearStencil:@
setClearStencil :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> CUInt -> IO ()
setClearStencil mtlRenderPassStencilAttachmentDescriptor  value =
  sendMsg mtlRenderPassStencilAttachmentDescriptor (mkSelector "setClearStencil:") retVoid [argCUInt (fromIntegral value)]

-- | stencilResolveFilter
--
-- The filter to be used for stencil multisample resolve. Defaults to MTLMultisampleStencilResolveFilterSample0.
--
-- ObjC selector: @- stencilResolveFilter@
stencilResolveFilter :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> IO MTLMultisampleStencilResolveFilter
stencilResolveFilter mtlRenderPassStencilAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLMultisampleStencilResolveFilter) $ sendMsg mtlRenderPassStencilAttachmentDescriptor (mkSelector "stencilResolveFilter") retCULong []

-- | stencilResolveFilter
--
-- The filter to be used for stencil multisample resolve. Defaults to MTLMultisampleStencilResolveFilterSample0.
--
-- ObjC selector: @- setStencilResolveFilter:@
setStencilResolveFilter :: IsMTLRenderPassStencilAttachmentDescriptor mtlRenderPassStencilAttachmentDescriptor => mtlRenderPassStencilAttachmentDescriptor -> MTLMultisampleStencilResolveFilter -> IO ()
setStencilResolveFilter mtlRenderPassStencilAttachmentDescriptor  value =
  sendMsg mtlRenderPassStencilAttachmentDescriptor (mkSelector "setStencilResolveFilter:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clearStencil@
clearStencilSelector :: Selector
clearStencilSelector = mkSelector "clearStencil"

-- | @Selector@ for @setClearStencil:@
setClearStencilSelector :: Selector
setClearStencilSelector = mkSelector "setClearStencil:"

-- | @Selector@ for @stencilResolveFilter@
stencilResolveFilterSelector :: Selector
stencilResolveFilterSelector = mkSelector "stencilResolveFilter"

-- | @Selector@ for @setStencilResolveFilter:@
setStencilResolveFilterSelector :: Selector
setStencilResolveFilterSelector = mkSelector "setStencilResolveFilter:"

