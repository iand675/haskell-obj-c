{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Allows you to specify additional binary functions to link to each stage of a render pipeline.
--
-- Generated bindings for @MTL4RenderPipelineBinaryFunctionsDescriptor@.
module ObjC.Metal.MTL4RenderPipelineBinaryFunctionsDescriptor
  ( MTL4RenderPipelineBinaryFunctionsDescriptor
  , IsMTL4RenderPipelineBinaryFunctionsDescriptor(..)
  , reset
  , vertexAdditionalBinaryFunctions
  , setVertexAdditionalBinaryFunctions
  , fragmentAdditionalBinaryFunctions
  , setFragmentAdditionalBinaryFunctions
  , tileAdditionalBinaryFunctions
  , setTileAdditionalBinaryFunctions
  , objectAdditionalBinaryFunctions
  , setObjectAdditionalBinaryFunctions
  , meshAdditionalBinaryFunctions
  , setMeshAdditionalBinaryFunctions
  , fragmentAdditionalBinaryFunctionsSelector
  , meshAdditionalBinaryFunctionsSelector
  , objectAdditionalBinaryFunctionsSelector
  , resetSelector
  , setFragmentAdditionalBinaryFunctionsSelector
  , setMeshAdditionalBinaryFunctionsSelector
  , setObjectAdditionalBinaryFunctionsSelector
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

-- | Resets this descriptor to its default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO ()
reset mtL4RenderPipelineBinaryFunctionsDescriptor =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor resetSelector

-- | Provides an array of binary functions representing additional binary vertex shader functions.
--
-- ObjC selector: @- vertexAdditionalBinaryFunctions@
vertexAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
vertexAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor vertexAdditionalBinaryFunctionsSelector

-- | Provides an array of binary functions representing additional binary vertex shader functions.
--
-- ObjC selector: @- setVertexAdditionalBinaryFunctions:@
setVertexAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setVertexAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor value =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor setVertexAdditionalBinaryFunctionsSelector (toNSArray value)

-- | Provides an array of binary functions representing additional binary fragment shader functions.
--
-- ObjC selector: @- fragmentAdditionalBinaryFunctions@
fragmentAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
fragmentAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor fragmentAdditionalBinaryFunctionsSelector

-- | Provides an array of binary functions representing additional binary fragment shader functions.
--
-- ObjC selector: @- setFragmentAdditionalBinaryFunctions:@
setFragmentAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setFragmentAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor value =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor setFragmentAdditionalBinaryFunctionsSelector (toNSArray value)

-- | Provides an array of binary functions representing additional binary tile shader functions.
--
-- ObjC selector: @- tileAdditionalBinaryFunctions@
tileAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
tileAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor tileAdditionalBinaryFunctionsSelector

-- | Provides an array of binary functions representing additional binary tile shader functions.
--
-- ObjC selector: @- setTileAdditionalBinaryFunctions:@
setTileAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setTileAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor value =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor setTileAdditionalBinaryFunctionsSelector (toNSArray value)

-- | Provides an array of binary functions representing additional binary object shader functions.
--
-- ObjC selector: @- objectAdditionalBinaryFunctions@
objectAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
objectAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor objectAdditionalBinaryFunctionsSelector

-- | Provides an array of binary functions representing additional binary object shader functions.
--
-- ObjC selector: @- setObjectAdditionalBinaryFunctions:@
setObjectAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setObjectAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor value =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor setObjectAdditionalBinaryFunctionsSelector (toNSArray value)

-- | Provides an array of binary functions representing additional binary mesh shader functions.
--
-- ObjC selector: @- meshAdditionalBinaryFunctions@
meshAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
meshAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor meshAdditionalBinaryFunctionsSelector

-- | Provides an array of binary functions representing additional binary mesh shader functions.
--
-- ObjC selector: @- setMeshAdditionalBinaryFunctions:@
setMeshAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setMeshAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor value =
  sendMessage mtL4RenderPipelineBinaryFunctionsDescriptor setMeshAdditionalBinaryFunctionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

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

-- | @Selector@ for @objectAdditionalBinaryFunctions@
objectAdditionalBinaryFunctionsSelector :: Selector '[] (Id NSArray)
objectAdditionalBinaryFunctionsSelector = mkSelector "objectAdditionalBinaryFunctions"

-- | @Selector@ for @setObjectAdditionalBinaryFunctions:@
setObjectAdditionalBinaryFunctionsSelector :: Selector '[Id NSArray] ()
setObjectAdditionalBinaryFunctionsSelector = mkSelector "setObjectAdditionalBinaryFunctions:"

-- | @Selector@ for @meshAdditionalBinaryFunctions@
meshAdditionalBinaryFunctionsSelector :: Selector '[] (Id NSArray)
meshAdditionalBinaryFunctionsSelector = mkSelector "meshAdditionalBinaryFunctions"

-- | @Selector@ for @setMeshAdditionalBinaryFunctions:@
setMeshAdditionalBinaryFunctionsSelector :: Selector '[Id NSArray] ()
setMeshAdditionalBinaryFunctionsSelector = mkSelector "setMeshAdditionalBinaryFunctions:"

