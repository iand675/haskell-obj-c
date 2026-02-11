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
  , vertexBindingsSelector
  , fragmentBindingsSelector
  , tileBindingsSelector
  , objectBindingsSelector
  , meshBindingsSelector
  , vertexArgumentsSelector
  , fragmentArgumentsSelector
  , tileArgumentsSelector


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

-- | @- vertexBindings@
vertexBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
vertexBindings mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "vertexBindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fragmentBindings@
fragmentBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
fragmentBindings mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "fragmentBindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tileBindings@
tileBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
tileBindings mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "tileBindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectBindings@
objectBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
objectBindings mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "objectBindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- meshBindings@
meshBindings :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
meshBindings mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "meshBindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- vertexArguments@
vertexArguments :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
vertexArguments mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "vertexArguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fragmentArguments@
fragmentArguments :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
fragmentArguments mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "fragmentArguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tileArguments@
tileArguments :: IsMTLRenderPipelineReflection mtlRenderPipelineReflection => mtlRenderPipelineReflection -> IO (Id NSArray)
tileArguments mtlRenderPipelineReflection  =
    sendMsg mtlRenderPipelineReflection (mkSelector "tileArguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexBindings@
vertexBindingsSelector :: Selector
vertexBindingsSelector = mkSelector "vertexBindings"

-- | @Selector@ for @fragmentBindings@
fragmentBindingsSelector :: Selector
fragmentBindingsSelector = mkSelector "fragmentBindings"

-- | @Selector@ for @tileBindings@
tileBindingsSelector :: Selector
tileBindingsSelector = mkSelector "tileBindings"

-- | @Selector@ for @objectBindings@
objectBindingsSelector :: Selector
objectBindingsSelector = mkSelector "objectBindings"

-- | @Selector@ for @meshBindings@
meshBindingsSelector :: Selector
meshBindingsSelector = mkSelector "meshBindings"

-- | @Selector@ for @vertexArguments@
vertexArgumentsSelector :: Selector
vertexArgumentsSelector = mkSelector "vertexArguments"

-- | @Selector@ for @fragmentArguments@
fragmentArgumentsSelector :: Selector
fragmentArgumentsSelector = mkSelector "fragmentArguments"

-- | @Selector@ for @tileArguments@
tileArgumentsSelector :: Selector
tileArgumentsSelector = mkSelector "tileArguments"

