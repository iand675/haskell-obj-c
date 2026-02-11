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
  , resetSelector
  , vertexAdditionalBinaryFunctionsSelector
  , setVertexAdditionalBinaryFunctionsSelector
  , fragmentAdditionalBinaryFunctionsSelector
  , setFragmentAdditionalBinaryFunctionsSelector
  , tileAdditionalBinaryFunctionsSelector
  , setTileAdditionalBinaryFunctionsSelector
  , objectAdditionalBinaryFunctionsSelector
  , setObjectAdditionalBinaryFunctionsSelector
  , meshAdditionalBinaryFunctionsSelector
  , setMeshAdditionalBinaryFunctionsSelector


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

-- | Resets this descriptor to its default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO ()
reset mtL4RenderPipelineBinaryFunctionsDescriptor  =
    sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "reset") retVoid []

-- | Provides an array of binary functions representing additional binary vertex shader functions.
--
-- ObjC selector: @- vertexAdditionalBinaryFunctions@
vertexAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
vertexAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  =
    sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "vertexAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of binary functions representing additional binary vertex shader functions.
--
-- ObjC selector: @- setVertexAdditionalBinaryFunctions:@
setVertexAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setVertexAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "setVertexAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an array of binary functions representing additional binary fragment shader functions.
--
-- ObjC selector: @- fragmentAdditionalBinaryFunctions@
fragmentAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
fragmentAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  =
    sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "fragmentAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of binary functions representing additional binary fragment shader functions.
--
-- ObjC selector: @- setFragmentAdditionalBinaryFunctions:@
setFragmentAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setFragmentAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "setFragmentAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an array of binary functions representing additional binary tile shader functions.
--
-- ObjC selector: @- tileAdditionalBinaryFunctions@
tileAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
tileAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  =
    sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "tileAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of binary functions representing additional binary tile shader functions.
--
-- ObjC selector: @- setTileAdditionalBinaryFunctions:@
setTileAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setTileAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "setTileAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an array of binary functions representing additional binary object shader functions.
--
-- ObjC selector: @- objectAdditionalBinaryFunctions@
objectAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
objectAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  =
    sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "objectAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of binary functions representing additional binary object shader functions.
--
-- ObjC selector: @- setObjectAdditionalBinaryFunctions:@
setObjectAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setObjectAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "setObjectAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an array of binary functions representing additional binary mesh shader functions.
--
-- ObjC selector: @- meshAdditionalBinaryFunctions@
meshAdditionalBinaryFunctions :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO (Id NSArray)
meshAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  =
    sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "meshAdditionalBinaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of binary functions representing additional binary mesh shader functions.
--
-- ObjC selector: @- setMeshAdditionalBinaryFunctions:@
setMeshAdditionalBinaryFunctions :: (IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor, IsNSArray value) => mtL4RenderPipelineBinaryFunctionsDescriptor -> value -> IO ()
setMeshAdditionalBinaryFunctions mtL4RenderPipelineBinaryFunctionsDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "setMeshAdditionalBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

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

-- | @Selector@ for @objectAdditionalBinaryFunctions@
objectAdditionalBinaryFunctionsSelector :: Selector
objectAdditionalBinaryFunctionsSelector = mkSelector "objectAdditionalBinaryFunctions"

-- | @Selector@ for @setObjectAdditionalBinaryFunctions:@
setObjectAdditionalBinaryFunctionsSelector :: Selector
setObjectAdditionalBinaryFunctionsSelector = mkSelector "setObjectAdditionalBinaryFunctions:"

-- | @Selector@ for @meshAdditionalBinaryFunctions@
meshAdditionalBinaryFunctionsSelector :: Selector
meshAdditionalBinaryFunctionsSelector = mkSelector "meshAdditionalBinaryFunctions"

-- | @Selector@ for @setMeshAdditionalBinaryFunctions:@
setMeshAdditionalBinaryFunctionsSelector :: Selector
setMeshAdditionalBinaryFunctionsSelector = mkSelector "setMeshAdditionalBinaryFunctions:"

