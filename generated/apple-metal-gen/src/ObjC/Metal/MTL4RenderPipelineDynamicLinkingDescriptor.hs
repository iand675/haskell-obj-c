{-# LANGUAGE DataKinds #-}
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
  , fragmentLinkingDescriptorSelector
  , meshLinkingDescriptorSelector
  , objectLinkingDescriptorSelector
  , tileLinkingDescriptorSelector
  , vertexLinkingDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Controls properties for linking the vertex stage of the render pipeline.
--
-- ObjC selector: @- vertexLinkingDescriptor@
vertexLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
vertexLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor =
  sendMessage mtL4RenderPipelineDynamicLinkingDescriptor vertexLinkingDescriptorSelector

-- | Controls properties for linking the fragment stage of the render pipeline.
--
-- ObjC selector: @- fragmentLinkingDescriptor@
fragmentLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
fragmentLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor =
  sendMessage mtL4RenderPipelineDynamicLinkingDescriptor fragmentLinkingDescriptorSelector

-- | Controls properties for linking the tile stage of the render pipeline.
--
-- ObjC selector: @- tileLinkingDescriptor@
tileLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
tileLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor =
  sendMessage mtL4RenderPipelineDynamicLinkingDescriptor tileLinkingDescriptorSelector

-- | Controls properties for link the object stage of the render pipeline.
--
-- ObjC selector: @- objectLinkingDescriptor@
objectLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
objectLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor =
  sendMessage mtL4RenderPipelineDynamicLinkingDescriptor objectLinkingDescriptorSelector

-- | Controls properties for linking the mesh stage of the render pipeline.
--
-- ObjC selector: @- meshLinkingDescriptor@
meshLinkingDescriptor :: IsMTL4RenderPipelineDynamicLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor => mtL4RenderPipelineDynamicLinkingDescriptor -> IO (Id MTL4PipelineStageDynamicLinkingDescriptor)
meshLinkingDescriptor mtL4RenderPipelineDynamicLinkingDescriptor =
  sendMessage mtL4RenderPipelineDynamicLinkingDescriptor meshLinkingDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexLinkingDescriptor@
vertexLinkingDescriptorSelector :: Selector '[] (Id MTL4PipelineStageDynamicLinkingDescriptor)
vertexLinkingDescriptorSelector = mkSelector "vertexLinkingDescriptor"

-- | @Selector@ for @fragmentLinkingDescriptor@
fragmentLinkingDescriptorSelector :: Selector '[] (Id MTL4PipelineStageDynamicLinkingDescriptor)
fragmentLinkingDescriptorSelector = mkSelector "fragmentLinkingDescriptor"

-- | @Selector@ for @tileLinkingDescriptor@
tileLinkingDescriptorSelector :: Selector '[] (Id MTL4PipelineStageDynamicLinkingDescriptor)
tileLinkingDescriptorSelector = mkSelector "tileLinkingDescriptor"

-- | @Selector@ for @objectLinkingDescriptor@
objectLinkingDescriptorSelector :: Selector '[] (Id MTL4PipelineStageDynamicLinkingDescriptor)
objectLinkingDescriptorSelector = mkSelector "objectLinkingDescriptor"

-- | @Selector@ for @meshLinkingDescriptor@
meshLinkingDescriptorSelector :: Selector '[] (Id MTL4PipelineStageDynamicLinkingDescriptor)
meshLinkingDescriptorSelector = mkSelector "meshLinkingDescriptor"

