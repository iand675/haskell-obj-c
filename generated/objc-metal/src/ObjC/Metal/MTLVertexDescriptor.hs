{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLVertexDescriptor@.
module ObjC.Metal.MTLVertexDescriptor
  ( MTLVertexDescriptor
  , IsMTLVertexDescriptor(..)
  , vertexDescriptor
  , reset
  , layouts
  , attributes
  , vertexDescriptorSelector
  , resetSelector
  , layoutsSelector
  , attributesSelector


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

-- | @+ vertexDescriptor@
vertexDescriptor :: IO (Id MTLVertexDescriptor)
vertexDescriptor  =
  do
    cls' <- getRequiredClass "MTLVertexDescriptor"
    sendClassMsg cls' (mkSelector "vertexDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reset@
reset :: IsMTLVertexDescriptor mtlVertexDescriptor => mtlVertexDescriptor -> IO ()
reset mtlVertexDescriptor  =
  sendMsg mtlVertexDescriptor (mkSelector "reset") retVoid []

-- | @- layouts@
layouts :: IsMTLVertexDescriptor mtlVertexDescriptor => mtlVertexDescriptor -> IO (Id MTLVertexBufferLayoutDescriptorArray)
layouts mtlVertexDescriptor  =
  sendMsg mtlVertexDescriptor (mkSelector "layouts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsMTLVertexDescriptor mtlVertexDescriptor => mtlVertexDescriptor -> IO (Id MTLVertexAttributeDescriptorArray)
attributes mtlVertexDescriptor  =
  sendMsg mtlVertexDescriptor (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @layouts@
layoutsSelector :: Selector
layoutsSelector = mkSelector "layouts"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

