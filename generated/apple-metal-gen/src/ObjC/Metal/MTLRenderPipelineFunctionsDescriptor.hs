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
  , vertexAdditionalBinaryFunctionsSelector
  , setVertexAdditionalBinaryFunctionsSelector
  , fragmentAdditionalBinaryFunctionsSelector
  , setFragmentAdditionalBinaryFunctionsSelector
  , tileAdditionalBinaryFunctionsSelector
  , setTileAdditionalBinaryFunctionsSelector


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

-- | vertexAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the vertex function in an incrementally created pipeline state.
--
-- ObjC selector: @- vertexAdditionalBinaryFunctions@
vertexAdditionalBinaryFunctions :: IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor => mtlRenderPipelineFunctionsDescriptor -> IO (Id NSArray)
vertexAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor  =
    sendMsg mtlRenderPipelineFunctionsDescriptor (mkSelector "vertexAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the vertex function in an incrementally created pipeline state.
--
-- ObjC selector: @- setVertexAdditionalBinaryFunctions:@
setVertexAdditionalBinaryFunctions :: (IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor, IsNSArray value) => mtlRenderPipelineFunctionsDescriptor -> value -> IO ()
setVertexAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlRenderPipelineFunctionsDescriptor (mkSelector "setVertexAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fragmentAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the fragment function in an incrementally created pipeline state.
--
-- ObjC selector: @- fragmentAdditionalBinaryFunctions@
fragmentAdditionalBinaryFunctions :: IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor => mtlRenderPipelineFunctionsDescriptor -> IO (Id NSArray)
fragmentAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor  =
    sendMsg mtlRenderPipelineFunctionsDescriptor (mkSelector "fragmentAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fragmentAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the fragment function in an incrementally created pipeline state.
--
-- ObjC selector: @- setFragmentAdditionalBinaryFunctions:@
setFragmentAdditionalBinaryFunctions :: (IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor, IsNSArray value) => mtlRenderPipelineFunctionsDescriptor -> value -> IO ()
setFragmentAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlRenderPipelineFunctionsDescriptor (mkSelector "setFragmentAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | tileAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the tile function in an incrementally created pipeline state.
--
-- ObjC selector: @- tileAdditionalBinaryFunctions@
tileAdditionalBinaryFunctions :: IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor => mtlRenderPipelineFunctionsDescriptor -> IO (Id NSArray)
tileAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor  =
    sendMsg mtlRenderPipelineFunctionsDescriptor (mkSelector "tileAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tileAdditionalBinaryFunctions
--
-- The set of additional binary functions to be accessed from the tile function in an incrementally created pipeline state.
--
-- ObjC selector: @- setTileAdditionalBinaryFunctions:@
setTileAdditionalBinaryFunctions :: (IsMTLRenderPipelineFunctionsDescriptor mtlRenderPipelineFunctionsDescriptor, IsNSArray value) => mtlRenderPipelineFunctionsDescriptor -> value -> IO ()
setTileAdditionalBinaryFunctions mtlRenderPipelineFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlRenderPipelineFunctionsDescriptor (mkSelector "setTileAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexAdditionalBinaryFunctions@
vertexAdditionalBinaryFunctionsSelector :: Selector
vertexAdditionalBinaryFunctionsSelector = mkSelector "vertexAdditionalBinaryFunctions"

-- | @Selector@ for @setVertexAdditionalBinaryFunctions:@
setVertexAdditionalBinaryFunctionsSelector :: Selector
setVertexAdditionalBinaryFunctionsSelector = mkSelector "setVertexAdditionalBinaryFunctions:"

-- | @Selector@ for @fragmentAdditionalBinaryFunctions@
fragmentAdditionalBinaryFunctionsSelector :: Selector
fragmentAdditionalBinaryFunctionsSelector = mkSelector "fragmentAdditionalBinaryFunctions"

-- | @Selector@ for @setFragmentAdditionalBinaryFunctions:@
setFragmentAdditionalBinaryFunctionsSelector :: Selector
setFragmentAdditionalBinaryFunctionsSelector = mkSelector "setFragmentAdditionalBinaryFunctions:"

-- | @Selector@ for @tileAdditionalBinaryFunctions@
tileAdditionalBinaryFunctionsSelector :: Selector
tileAdditionalBinaryFunctionsSelector = mkSelector "tileAdditionalBinaryFunctions"

-- | @Selector@ for @setTileAdditionalBinaryFunctions:@
setTileAdditionalBinaryFunctionsSelector :: Selector
setTileAdditionalBinaryFunctionsSelector = mkSelector "setTileAdditionalBinaryFunctions:"

