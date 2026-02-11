{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPipelineReflection@.
module ObjC.Metal.MTLRenderPipelineReflection
  ( MTLRenderPipelineReflection
  , IsMTLRenderPipelineReflection(..)
  , vertexArguments
  , fragmentArguments
  , tileArguments
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

-- | @Selector@ for @vertexArguments@
vertexArgumentsSelector :: Selector
vertexArgumentsSelector = mkSelector "vertexArguments"

-- | @Selector@ for @fragmentArguments@
fragmentArgumentsSelector :: Selector
fragmentArgumentsSelector = mkSelector "fragmentArguments"

-- | @Selector@ for @tileArguments@
tileArgumentsSelector :: Selector
tileArgumentsSelector = mkSelector "tileArguments"

