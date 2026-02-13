{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPipelineReflection@.
module ObjC.Metal.MTLRenderPipelineReflection
  ( MTLRenderPipelineReflection
  , IsMTLRenderPipelineReflection(..)
  , vertexBindings
  , fragmentBindings
  , tileBindings
  , objectBindings
  , meshBindings
  , vertexArguments
  , fragmentArguments
  , tileArguments
  , fragmentArgumentsSelector
  , fragmentBindingsSelector
  , meshBindingsSelector
  , objectBindingsSelector
  , tileArgumentsSelector
  , tileBindingsSelector
  , vertexArgumentsSelector
  , vertexBindingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- vertexBindings@
vertexBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
vertexBindings mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection vertexBindingsSelector

-- | @- fragmentBindings@
fragmentBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
fragmentBindings mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection fragmentBindingsSelector

-- | @- tileBindings@
tileBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
tileBindings mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection tileBindingsSelector

-- | @- objectBindings@
objectBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
objectBindings mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection objectBindingsSelector

-- | @- meshBindings@
meshBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
meshBindings mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection meshBindingsSelector

-- | @- vertexArguments@
vertexArguments :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
vertexArguments mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection vertexArgumentsSelector

-- | @- fragmentArguments@
fragmentArguments :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
fragmentArguments mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection fragmentArgumentsSelector

-- | @- tileArguments@
tileArguments :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
tileArguments mtlRenderPipelineReflection =
  sendMessage mtlRenderPipelineReflection tileArgumentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexBindings@
vertexBindingsSelector :: Selector '[] (Id NSArray)
vertexBindingsSelector = mkSelector "vertexBindings"

-- | @Selector@ for @fragmentBindings@
fragmentBindingsSelector :: Selector '[] (Id NSArray)
fragmentBindingsSelector = mkSelector "fragmentBindings"

-- | @Selector@ for @tileBindings@
tileBindingsSelector :: Selector '[] (Id NSArray)
tileBindingsSelector = mkSelector "tileBindings"

-- | @Selector@ for @objectBindings@
objectBindingsSelector :: Selector '[] (Id NSArray)
objectBindingsSelector = mkSelector "objectBindings"

-- | @Selector@ for @meshBindings@
meshBindingsSelector :: Selector '[] (Id NSArray)
meshBindingsSelector = mkSelector "meshBindings"

-- | @Selector@ for @vertexArguments@
vertexArgumentsSelector :: Selector '[] (Id NSArray)
vertexArgumentsSelector = mkSelector "vertexArguments"

-- | @Selector@ for @fragmentArguments@
fragmentArgumentsSelector :: Selector '[] (Id NSArray)
fragmentArgumentsSelector = mkSelector "fragmentArguments"

-- | @Selector@ for @tileArguments@
tileArgumentsSelector :: Selector '[] (Id NSArray)
tileArgumentsSelector = mkSelector "tileArguments"

