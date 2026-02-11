{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties that provide linking properties for render pipelines.
--
-- Generated bindings for @MTL4RenderPipelineDynamicLinkingDescriptor@.
module ObjC.Metal.MTL4RenderPipelineDynamicLinkingDescriptor
  ( MTL4RenderPipelineDynamicLinkingDescriptor
  , IsMTL4RenderPipelineDynamicLinkingDescriptor(..)
  , vertexLinkingDescriptor
  , fragmentLinkingDescriptor
  , tileLinkingDescriptor
  , objectLinkingDescriptor
  , meshLinkingDescriptor
  , vertexLinkingDescriptorSelector
  , fragmentLinkingDescriptorSelector
  , tileLinkingDescriptorSelector
  , objectLinkingDescriptorSelector
  , meshLinkingDescriptorSelector


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

-- | Controls properties for linking the vertex stage of the render pipeline.
--
-- ObjC selector: @- vertexLinkingDescriptor@
vertexLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
vertexLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor  =
  sendMsg mtL4RenderPipelineDynamicLinkingDescriptor (mkSelector "vertexLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls properties for linking the fragment stage of the render pipeline.
--
-- ObjC selector: @- fragmentLinkingDescriptor@
fragmentLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
fragmentLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor  =
  sendMsg mtL4RenderPipelineDynamicLinkingDescriptor (mkSelector "fragmentLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls properties for linking the tile stage of the render pipeline.
--
-- ObjC selector: @- tileLinkingDescriptor@
tileLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
tileLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor  =
  sendMsg mtL4RenderPipelineDynamicLinkingDescriptor (mkSelector "tileLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls properties for link the object stage of the render pipeline.
--
-- ObjC selector: @- objectLinkingDescriptor@
objectLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
objectLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor  =
  sendMsg mtL4RenderPipelineDynamicLinkingDescriptor (mkSelector "objectLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls properties for linking the mesh stage of the render pipeline.
--
-- ObjC selector: @- meshLinkingDescriptor@
meshLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
meshLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor  =
  sendMsg mtL4RenderPipelineDynamicLinkingDescriptor (mkSelector "meshLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexLinkingDescriptor@
vertexLinkingDescriptorSelector :: Selector
vertexLinkingDescriptorSelector = mkSelector "vertexLinkingDescriptor"

-- | @Selector@ for @fragmentLinkingDescriptor@
fragmentLinkingDescriptorSelector :: Selector
fragmentLinkingDescriptorSelector = mkSelector "fragmentLinkingDescriptor"

-- | @Selector@ for @tileLinkingDescriptor@
tileLinkingDescriptorSelector :: Selector
tileLinkingDescriptorSelector = mkSelector "tileLinkingDescriptor"

-- | @Selector@ for @objectLinkingDescriptor@
objectLinkingDescriptorSelector :: Selector
objectLinkingDescriptorSelector = mkSelector "objectLinkingDescriptor"

-- | @Selector@ for @meshLinkingDescriptor@
meshLinkingDescriptorSelector :: Selector
meshLinkingDescriptorSelector = mkSelector "meshLinkingDescriptor"

