{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties to drive the dynamic linking process of a pipeline stage.
--
-- Generated bindings for @MTL4PipelineStageDynamicLinkingDescriptor@.
module ObjC.Metal.MTL4PipelineStageDynamicLinkingDescriptor
  ( MTL4PipelineStageDynamicLinkingDescriptor
  , IsMTL4PipelineStageDynamicLinkingDescriptor(..)
  , maxCallStackDepth
  , setMaxCallStackDepth
  , maxCallStackDepthSelector
  , setMaxCallStackDepthSelector


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
import ObjC.Foundation.Internal.Classes

-- | Limits the maximum depth of the call stack for indirect function calls in the pipeline stage function.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO CULong
maxCallStackDepth mtL4PipelineStageDynamicLinkingDescriptor  =
  sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "maxCallStackDepth") retCULong []

-- | Limits the maximum depth of the call stack for indirect function calls in the pipeline stage function.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtL4PipelineStageDynamicLinkingDescriptor  value =
  sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "setMaxCallStackDepth:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCallStackDepth@
maxCallStackDepthSelector :: Selector
maxCallStackDepthSelector = mkSelector "maxCallStackDepth"

-- | @Selector@ for @setMaxCallStackDepth:@
setMaxCallStackDepthSelector :: Selector
setMaxCallStackDepthSelector = mkSelector "setMaxCallStackDepth:"

