{-# LANGUAGE DataKinds #-}
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
  , attributesSelector
  , layoutsSelector
  , resetSelector
  , vertexDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ vertexDescriptor@
vertexDescriptor :: IO (Id MTLVertexDescriptor)
vertexDescriptor  =
  do
    cls' <- getRequiredClass "MTLVertexDescriptor"
    sendClassMessage cls' vertexDescriptorSelector

-- | @- reset@
reset :: IsMTLVertexDescriptor mtlVertexDescriptor => mtlVertexDescriptor -> IO ()
reset mtlVertexDescriptor =
  sendMessage mtlVertexDescriptor resetSelector

-- | @- layouts@
layouts :: IsMTLVertexDescriptor mtlVertexDescriptor => mtlVertexDescriptor -> IO (Id MTLVertexBufferLayoutDescriptorArray)
layouts mtlVertexDescriptor =
  sendMessage mtlVertexDescriptor layoutsSelector

-- | @- attributes@
attributes :: IsMTLVertexDescriptor mtlVertexDescriptor => mtlVertexDescriptor -> IO (Id MTLVertexAttributeDescriptorArray)
attributes mtlVertexDescriptor =
  sendMessage mtlVertexDescriptor attributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector '[] (Id MTLVertexDescriptor)
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @layouts@
layoutsSelector :: Selector '[] (Id MTLVertexBufferLayoutDescriptorArray)
layoutsSelector = mkSelector "layouts"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id MTLVertexAttributeDescriptorArray)
attributesSelector = mkSelector "attributes"

