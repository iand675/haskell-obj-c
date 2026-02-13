{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLRenderPipelineFunctionsDescriptor@.
module ObjC.Metal.MTLRenderPipelineFunctionsDescriptor
  ( MTLRenderPipelineFunctionsDescriptor
  , IsMTLRenderPipelineFunctionsDescriptor(..)
  , vertexAdditionalBinaryFunctions
  , setVertexAdditionalBinaryFunctions
  , fragmentAdditionalBinaryFunctions
  , setFragmentAdditionalBinaryFunctions
  , tileAdditionalBinaryFunctions
  , setTileAdditionalBinaryFunctions
  , fragmentAdditionalBinaryFunctionsSelector
  , setFragmentAdditionalBinaryFunctionsSelector
  , setTileAdditionalBinaryFunctionsSelector
  , setVertexAdditionalBinaryFunctionsSelector
  , tileAdditionalBinaryFunctionsSelector
  , vertexAdditionalBinaryFunctionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | vertexAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the vertex function in an incrementally created pipeline state.
--
-- ObjC selector: @- vertexAdditionalBinaryFunctions@
vertexAdditionalBinaryFunctions :: IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor => mtlRenderPipelineFunctionsDescriptor -> IO (Id NSArray)
vertexAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor =
  sendMessage mtlRenderPipelineFunctionsDescriptor vertexAdditionalBinaryFunctionsSelector

-- | vertexAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the vertex function in an incrementally created pipeline state.
--
-- ObjC selector: @- setVertexAdditionalBinaryFunctions:@
setVertexAdditionalBinaryFunctions :: (IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor, IsNSArray value) => mtlRenderPipelineFunctionsDescriptor -> value -> IO ()
setVertexAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor value =
  sendMessage mtlRenderPipelineFunctionsDescriptor setVertexAdditionalBinaryFunctionsSelector (toNSArray value)

-- | fragmentAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the fragment function in an incrementally created pipeline state.
--
-- ObjC selector: @- fragmentAdditionalBinaryFunctions@
fragmentAdditionalBinaryFunctions :: IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor => mtlRenderPipelineFunctionsDescriptor -> IO (Id NSArray)
fragmentAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor =
  sendMessage mtlRenderPipelineFunctionsDescriptor fragmentAdditionalBinaryFunctionsSelector

-- | fragmentAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the fragment function in an incrementally created pipeline state.
--
-- ObjC selector: @- setFragmentAdditionalBinaryFunctions:@
setFragmentAdditionalBinaryFunctions :: (IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor, IsNSArray value) => mtlRenderPipelineFunctionsDescriptor -> value -> IO ()
setFragmentAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor value =
  sendMessage mtlRenderPipelineFunctionsDescriptor setFragmentAdditionalBinaryFunctionsSelector (toNSArray value)

-- | tileAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the tile function in an incrementally created pipeline state.
--
-- ObjC selector: @- tileAdditionalBinaryFunctions@
tileAdditionalBinaryFunctions :: IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor => mtlRenderPipelineFunctionsDescriptor -> IO (Id NSArray)
tileAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor =
  sendMessage mtlRenderPipelineFunctionsDescriptor tileAdditionalBinaryFunctionsSelector

-- | tileAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the tile function in an incrementally created pipeline state.
--
-- ObjC selector: @- setTileAdditionalBinaryFunctions:@
setTileAdditionalBinaryFunctions :: (IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor, IsNSArray value) => mtlRenderPipelineFunctionsDescriptor -> value -> IO ()
setTileAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor value =
  sendMessage mtlRenderPipelineFunctionsDescriptor setTileAdditionalBinaryFunctionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexAdditionalBinaryFunctions@
vertexAdditionalBinaryFunctionsSelector :: Selector '[] (Id NSArray)
vertexAdditionalBinaryFunctionsSelector = mkSelector "vertexAdditionalBinaryFunctions"

-- | @Selector@ for @setVertexAdditionalBinaryFunctions:@
setVertexAdditionalBinaryFunctionsSelector :: Selector '[Id NSArray] ()
setVertexAdditionalBinaryFunctionsSelector = mkSelector "setVertexAdditionalBinaryFunctions:"

-- | @Selector@ for @fragmentAdditionalBinaryFunctions@
fragmentAdditionalBinaryFunctionsSelector :: Selector '[] (Id NSArray)
fragmentAdditionalBinaryFunctionsSelector = mkSelector "fragmentAdditionalBinaryFunctions"

-- | @Selector@ for @setFragmentAdditionalBinaryFunctions:@
setFragmentAdditionalBinaryFunctionsSelector :: Selector '[Id NSArray] ()
setFragmentAdditionalBinaryFunctionsSelector = mkSelector "setFragmentAdditionalBinaryFunctions:"

-- | @Selector@ for @tileAdditionalBinaryFunctions@
tileAdditionalBinaryFunctionsSelector :: Selector '[] (Id NSArray)
tileAdditionalBinaryFunctionsSelector = mkSelector "tileAdditionalBinaryFunctions"

-- | @Selector@ for @setTileAdditionalBinaryFunctions:@
setTileAdditionalBinaryFunctionsSelector :: Selector '[Id NSArray] ()
setTileAdditionalBinaryFunctionsSelector = mkSelector "setTileAdditionalBinaryFunctions:"

