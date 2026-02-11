{-# LANGUAGE PatternSynonyms #-}
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
  , setClearDepthSelector
  , depthResolveFilterSelector
  , setDepthResolveFilterSelector

  -- * Enum types
  , MTLMultisampleDepthResolveFilter(MTLMultisampleDepthResolveFilter)
  , pattern MTLMultisampleDepthResolveFilterSample0
  , pattern MTLMultisampleDepthResolveFilterMin
  , pattern MTLMultisampleDepthResolveFilterMax

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

-- | clearDepth
--
-- The clear depth value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- clearDepth@
clearDepth :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> IO CDouble
clearDepth mtlRenderPassDepthAttachmentDescriptor  =
  sendMsg mtlRenderPassDepthAttachmentDescriptor (mkSelector "clearDepth") retCDouble []

-- | clearDepth
--
-- The clear depth value to be used if the loadAction property is MTLLoadActionClear
--
-- ObjC selector: @- setClearDepth:@
setClearDepth :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> CDouble -> IO ()
setClearDepth mtlRenderPassDepthAttachmentDescriptor  value =
  sendMsg mtlRenderPassDepthAttachmentDescriptor (mkSelector "setClearDepth:") retVoid [argCDouble (fromIntegral value)]

-- | resolveFilter
--
-- The filter to be used for depth multisample resolve.  Defaults to MTLMultisampleDepthResolveFilterSample0.
--
-- ObjC selector: @- depthResolveFilter@
depthResolveFilter :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> IO MTLMultisampleDepthResolveFilter
depthResolveFilter mtlRenderPassDepthAttachmentDescriptor  =
  fmap (coerce :: CULong -> MTLMultisampleDepthResolveFilter) $ sendMsg mtlRenderPassDepthAttachmentDescriptor (mkSelector "depthResolveFilter") retCULong []

-- | resolveFilter
--
-- The filter to be used for depth multisample resolve.  Defaults to MTLMultisampleDepthResolveFilterSample0.
--
-- ObjC selector: @- setDepthResolveFilter:@
setDepthResolveFilter :: IsMTLRenderPassDepthAttachmentDescriptor mtlRenderPassDepthAttachmentDescriptor => mtlRenderPassDepthAttachmentDescriptor -> MTLMultisampleDepthResolveFilter -> IO ()
setDepthResolveFilter mtlRenderPassDepthAttachmentDescriptor  value =
  sendMsg mtlRenderPassDepthAttachmentDescriptor (mkSelector "setDepthResolveFilter:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clearDepth@
clearDepthSelector :: Selector
clearDepthSelector = mkSelector "clearDepth"

-- | @Selector@ for @setClearDepth:@
setClearDepthSelector :: Selector
setClearDepthSelector = mkSelector "setClearDepth:"

-- | @Selector@ for @depthResolveFilter@
depthResolveFilterSelector :: Selector
depthResolveFilterSelector = mkSelector "depthResolveFilter"

-- | @Selector@ for @setDepthResolveFilter:@
setDepthResolveFilterSelector :: Selector
setDepthResolveFilterSelector = mkSelector "setDepthResolveFilter:"

